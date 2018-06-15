{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Data.Winery.Internal.Builder where

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

data Tree = Bin Tree Tree | Leaf Elem

instance Monoid Encoding where
  mempty = Empty
  {-# INLINE mempty #-}
  mappend Empty a = a
  mappend a Empty = a
  mappend (Encoding s a) (Encoding t b) = Encoding (s + t) (Bin a b)
  {-# INLINE mappend #-}

singleton :: Elem -> Encoding
singleton e = Encoding (measureElem e) (Leaf e)
{-# INLINE singleton #-}

getSize :: Encoding -> Int
getSize Empty = 0
getSize (Encoding s _) = s
{-# INLINE getSize #-}

measureElem :: Elem -> Int
measureElem (EWord8 _) = 1
measureElem (EWord16 _) = 2
measureElem (EWord32 _) = 4
measureElem (EWord64 _) = 8
measureElem (EBytes bs) = B.length bs

data Elem = EWord8 {-# UNPACK #-} !Word8
  | EWord16 {-# UNPACK #-} !Word16
  | EWord32 {-# UNPACK #-} !Word32
  | EWord64 {-# UNPACK #-} !Word64
  | EBytes !B.ByteString

uncons :: Tree -> (Elem, Maybe Tree)
uncons (Leaf e) = (e, Nothing)
uncons (Bin a b) = go a b where
  go (Leaf k) t = (k, Just t)
  go (Bin c d) t = go c (Bin d t)

toByteString :: Encoding -> B.ByteString
toByteString Empty = B.empty
toByteString (Encoding len tree) = unsafeDupablePerformIO $ do
  fp <- B.mallocByteString len
  withForeignPtr fp $ \ptr -> do
    let go ofs enc = case uncons enc of
          (e, rest) -> do
            case e of
              EWord8 w -> pokeByteOff ptr ofs w
              EWord16 w -> pokeByteOff ptr ofs $ toBE16 w
              EWord32 w -> pokeByteOff ptr ofs $ toBE32 w
              EWord64 w -> pokeByteOff ptr ofs $ toBE64 w
              EBytes (B.PS fp' sofs len') -> withForeignPtr fp'
                $ \src -> B.memcpy (ptr `plusPtr` ofs) (src `plusPtr` sofs) len'
            mapM_ (go (ofs + measureElem e)) rest
    go 0 tree
  return (B.PS fp 0 len)

word8 :: Word8 -> Encoding
word8 = singleton . EWord8
{-# INLINE word8 #-}
