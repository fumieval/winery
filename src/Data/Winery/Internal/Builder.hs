{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Data.Winery.Internal.Builder
  ( Encoding
  , getSize
  , toByteString
  , word8
  , word16
  , word32
  , word64
  , bytes
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
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

instance Monoid Encoding where
  mempty = Empty
  {-# INLINE mempty #-}
  mappend Empty a = a
  mappend a Empty = a
  mappend (Encoding s a) (Encoding t b) = Encoding (s + t) (Bin a b)
  {-# INLINE mappend #-}

getSize :: Encoding -> Int
getSize Empty = 0
getSize (Encoding s _) = s
{-# INLINE getSize #-}

toByteString :: Encoding -> B.ByteString
toByteString Empty = B.empty
toByteString (Encoding len tree) = unsafeDupablePerformIO $ do
  fp <- B.mallocByteString len
  withForeignPtr fp $ \ptr -> do
    let copyBS ofs (B.PS fp' sofs len') = withForeignPtr fp'
          $ \src -> B.memcpy (ptr `plusPtr` ofs) (src `plusPtr` sofs) len'
    let go :: Int -> Tree -> IO ()
        go ofs l = case l of
          LWord8 w -> pokeByteOff ptr ofs w
          LWord16 w -> pokeByteOff ptr ofs $ toBE16 w
          LWord32 w -> pokeByteOff ptr ofs $ toBE32 w
          LWord64 w -> pokeByteOff ptr ofs $ toBE64 w
          LBytes bs -> copyBS ofs bs
          Bin a b -> rotate ofs a b

        rotate :: Int -> Tree -> Tree -> IO ()
        rotate ofs (LWord8 w) t = pokeByteOff ptr ofs w >> go (ofs + 1) t
        rotate ofs (LWord16 w) t = pokeByteOff ptr ofs (toBE16 w) >> go (ofs + 2) t
        rotate ofs (LWord32 w) t = pokeByteOff ptr ofs (toBE32 w) >> go (ofs + 4) t
        rotate ofs (LWord64 w) t = pokeByteOff ptr ofs (toBE64 w) >> go (ofs + 8) t
        rotate ofs (LBytes bs) t = copyBS ofs bs >> go (ofs + B.length bs) t
        rotate ofs (Bin c d) t = rotate ofs c (Bin d t)
    go 0 tree
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
