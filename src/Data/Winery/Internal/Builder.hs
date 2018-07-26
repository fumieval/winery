{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Winery.Internal.Builder
  ( Encoding
  , getSize
  , toByteString
  , hPut
  , word8
  , word16
  , word32
  , word64
  , bytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO.Buffer
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import qualified GHC.IO.BufferedIO as Buffered
import System.IO.Unsafe
import System.Endian

data Encoding = Encoding {-# UNPACK #-}!Int !Tree
  | Empty

data Tree = Bin Tree Tree
  | LWord8 {-# UNPACK #-} !Word8
  | LWord16 {-# UNPACK #-} !Word16
  | LWord32 {-# UNPACK #-} !Word32
  | LWord64 {-# UNPACK #-} !Word64
  | LBytes !B.ByteString

instance Semigroup Encoding where
  Empty <> a = a
  a <> Empty = a
  Encoding s a <> Encoding t b = Encoding (s + t) (Bin a b)

instance Monoid Encoding where
  mempty = Empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

getSize :: Encoding -> Int
getSize Empty = 0
getSize (Encoding s _) = s
{-# INLINE getSize #-}

pokeTree :: Ptr Word8 -> Tree -> IO ()
pokeTree ptr l = case l of
  LWord8 w -> poke ptr w
  LWord16 w -> poke (castPtr ptr) $ toBE16 w
  LWord32 w -> poke (castPtr ptr) $ toBE32 w
  LWord64 w -> poke (castPtr ptr) $ toBE64 w
  LBytes (B.PS fp ofs len) -> withForeignPtr fp
    $ \src -> B.memcpy ptr (src `plusPtr` ofs) len
  Bin a b -> rotate ptr a b

rotate :: Ptr Word8 -> Tree -> Tree -> IO ()
rotate ptr (LWord8 w) t = poke ptr w >> pokeTree (ptr `plusPtr` 1) t
rotate ptr (LWord16 w) t = poke (castPtr ptr) (toBE16 w) >> pokeTree (ptr `plusPtr` 2) t
rotate ptr (LWord32 w) t = poke (castPtr ptr) (toBE32 w) >> pokeTree (ptr `plusPtr` 4) t
rotate ptr (LWord64 w) t = poke (castPtr ptr) (toBE64 w) >> pokeTree (ptr `plusPtr` 8) t
rotate ptr (LBytes (B.PS fp ofs len)) t = do
  withForeignPtr fp
    $ \src -> B.memcpy ptr (src `plusPtr` ofs) len
  pokeTree (ptr `plusPtr` len) t
rotate ptr (Bin c d) t = rotate ptr c (Bin d t)

toByteString :: Encoding -> B.ByteString
toByteString Empty = B.empty
toByteString (Encoding len tree) = unsafeDupablePerformIO $ do
  fp <- B.mallocByteString len
  withForeignPtr fp $ \ptr -> pokeTree ptr tree
  return (B.PS fp 0 len)

word8 :: Word8 -> Encoding
word8 = Encoding 1 . LWord8
{-# INLINE word8 #-}

word16 :: Word16 -> Encoding
word16 = Encoding 2 . LWord16
{-# INLINE word16 #-}

word32 :: Word32 -> Encoding
word32 = Encoding 4 . LWord32
{-# INLINE word32 #-}

word64 :: Word64 -> Encoding
word64 = Encoding 8 . LWord64
{-# INLINE word64 #-}

bytes :: B.ByteString -> Encoding
bytes bs = Encoding (B.length bs) $ LBytes bs
{-# INLINE bytes #-}

pokeBuffer :: (Buffered.BufferedIO dev, Storable a) => dev -> Buffer Word8 -> a -> IO (Buffer Word8)
pokeBuffer dev buf x
    | bufferAvailable buf >= sizeOf x = bufferAdd (sizeOf x) buf
      <$ withBuffer buf (\ptr -> pokeByteOff ptr (bufR buf) x)
    | otherwise = Buffered.flushWriteBuffer dev buf
      >>= \buf' -> bufferAdd (sizeOf x) buf'
        <$ withBuffer buf' (\ptr -> pokeByteOff ptr (bufR buf') x)
{-# INLINE pokeBuffer #-}

hPut :: Handle -> Encoding -> IO ()
hPut _ Empty = return ()
hPut h (Encoding _ t0) = wantWritableHandle "Data.Winery.Intenal.Builder.hPut" h
  $ \Handle__{..} -> do
    buf0 <- readIORef haByteBuffer

    let go (LWord8 w) !buf = pokeBuffer haDevice buf w
        go (LWord16 w) buf = pokeBuffer haDevice buf (toBE16 w)
        go (LWord32 w) buf = pokeBuffer haDevice buf (toBE32 w)
        go (LWord64 w) buf = pokeBuffer haDevice buf (toBE64 w)
        go t@(LBytes (B.PS fp ofs len)) !buf
          | bufferAvailable buf >= len = (bufferAdd len buf<$) $ withBuffer buf
            $ \ptr -> withForeignPtr fp
            $ \src -> B.memcpy (ptr `plusPtr` bufR buf) (src `plusPtr` ofs) len
          | bufSize buf >= len = Buffered.flushWriteBuffer haDevice buf
            >>= go t
          | otherwise = newByteBuffer len WriteBuffer >>= go t
        go (Bin c d) buf = rot c d buf

        rot (LWord8 w) t !buf = pokeBuffer haDevice buf w >>= go t
        rot (LWord16 w) t buf = pokeBuffer haDevice buf (toBE16 w) >>= go t
        rot (LWord32 w) t buf = pokeBuffer haDevice buf (toBE32 w) >>= go t
        rot (LWord64 w) t buf = pokeBuffer haDevice buf (toBE64 w) >>= go t
        rot t@(LBytes (B.PS fp ofs len)) t' buf
          | bufferAvailable buf >= len = do
            withBuffer buf
              $ \ptr -> withForeignPtr fp
              $ \src -> B.memcpy (ptr `plusPtr` bufR buf) (src `plusPtr` ofs) len
            go t' $ bufferAdd len buf
          | bufSize buf >= len = Buffered.flushWriteBuffer haDevice buf
            >>= rot t t'
          | otherwise = do
            _ <- Buffered.flushWriteBuffer haDevice buf
            newByteBuffer len WriteBuffer >>= rot t t'
        rot (Bin c d) t buf = rot c (Bin d t) buf

    buf' <- go t0 buf0
    writeIORef haByteBuffer buf'
