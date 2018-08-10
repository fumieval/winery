{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Winery.Internal
  ( Encoding
  , EncodingMulti
  , encodeMulti
  , encodeItem
  , Decoder
  , decodeAt
  , decodeVarInt
  , Offsets
  , decodeOffsets
  , getWord8
  , DecodeException(..)
  , word16be
  , word32be
  , word64be
  , unsafeIndex
  , unsafeIndexV
  , Strategy(..)
  , StrategyError
  , errorStrategy
  , TransFusion(..)
  )where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Trans.Cont
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.Winery.Internal.Builder
import Data.Bits
import Data.Dynamic
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import System.Endian

type Decoder = (->) B.ByteString

decodeAt :: (Int, Int) -> Decoder a -> Decoder a
decodeAt (i, l) m = m . B.take l . B.drop i
{-# INLINE decodeAt #-}

getWord8 :: ContT r Decoder Word8
getWord8 = ContT $ \k bs -> case B.uncons bs of
  Nothing -> throw InsufficientInput
  Just (x, bs') -> k x $! bs'
{-# INLINE getWord8 #-}

data DecodeException = InsufficientInput
  | InvalidTag deriving (Eq, Show, Read)
instance Exception DecodeException

decodeVarInt :: (Num a, Bits a) => ContT r Decoder a
decodeVarInt = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- getWord8 >>= go
      if testBit n 6
        then return $! negate $ unsafeShiftL m 6 .|. fromIntegral n .&. 0x3f
        else return $! unsafeShiftL m 6 .|. clearBit (fromIntegral n) 7
    | testBit n 6 -> return $ negate $ fromIntegral $ clearBit n 6
    | otherwise -> return $ fromIntegral n
  where
    go n
      | testBit n 7 = do
        m <- getWord8 >>= go
        return $! unsafeShiftL m 7 .|. clearBit (fromIntegral n) 7
      | otherwise = return $ fromIntegral n
{-# INLINE decodeVarInt #-}

word16be :: B.ByteString -> Word16
word16be = \s -> if B.length s >= 2
  then
    (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 8) .|.
    (fromIntegral (s `B.unsafeIndex` 1))
  else throw InsufficientInput

word32be :: B.ByteString -> Word32
word32be = \s -> if B.length s >= 4
  then
    (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 24) .|.
    (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 16) .|.
    (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL`  8) .|.
    (fromIntegral (s `B.unsafeIndex` 3) )
  else throw InsufficientInput

word64be :: B.ByteString -> Word64
word64be (B.PS fp ofs len)
  | len >= 8 = B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromBE64 <$> peekByteOff ptr ofs
  | otherwise = throw InsufficientInput

data EncodingMulti = EncodingMulti0
    | EncodingMulti !Encoding !Encoding

encodeMulti :: (EncodingMulti -> EncodingMulti) -> Encoding
encodeMulti f = case f EncodingMulti0 of
  EncodingMulti0 -> mempty
  EncodingMulti r s -> mappend r s
{-# INLINE encodeMulti #-}

encodeItem :: Encoding -> EncodingMulti -> EncodingMulti
encodeItem e EncodingMulti0 = EncodingMulti mempty e
encodeItem e (EncodingMulti a b) = EncodingMulti
  (mappend (varInt (getSize e)) a) (mappend e b)
{-# INLINE encodeItem #-}

type Offsets = U.Vector (Int, Int)

decodeOffsets :: Int -> ContT r Decoder Offsets
decodeOffsets 0 = pure U.empty
decodeOffsets n = accum <$> U.replicateM (n - 1) decodeVarInt where
  accum xs = runST $ do
    r <- UM.unsafeNew (U.length xs + 1)
    let go s i
          | i == U.length xs = do
            UM.unsafeWrite r i (s, maxBound)
            U.unsafeFreeze r
          | otherwise = do
            let x = U.unsafeIndex xs i
            let s' = s + x
            UM.unsafeWrite r i (s, x)
            go s' (i + 1)
    go 0 0

unsafeIndexV :: U.Unbox a => String -> U.Vector a -> Int -> a
unsafeIndexV err xs i
  | i >= U.length xs || i < 0 = error err
  | otherwise = U.unsafeIndex xs i
{-# INLINE unsafeIndexV #-}

unsafeIndex :: String -> [a] -> Int -> a
unsafeIndex err xs i = (xs ++ repeat (error err)) !! i

type StrategyError = Doc AnsiStyle

newtype Strategy a = Strategy { unStrategy :: [Decoder Dynamic] -> Either StrategyError a }
  deriving Functor

instance Applicative Strategy where
  pure = return
  (<*>) = ap

instance Monad Strategy where
  return = Strategy . const . Right
  m >>= k = Strategy $ \decs -> case unStrategy m decs of
    Right a -> unStrategy (k a) decs
    Left e -> Left e

instance Alternative Strategy where
  empty = Strategy $ const $ Left "empty"
  Strategy a <|> Strategy b = Strategy $ \decs -> case a decs of
    Left _ -> b decs
    Right x -> Right x

instance MonadFix Strategy where
  mfix f = Strategy $ \r -> mfix $ \a -> unStrategy (f a) r
  {-# INLINE mfix #-}

errorStrategy :: Doc AnsiStyle -> Strategy a
errorStrategy = Strategy . const . Left

newtype TransFusion f g a = TransFusion { unTransFusion :: forall h. Applicative h => (forall x. f x -> h (g x)) -> h a }

instance Functor (TransFusion f g) where
  fmap f (TransFusion m) = TransFusion $ \k -> fmap f (m k)
  {-# INLINE fmap #-}

instance Applicative (TransFusion f g) where
  pure a = TransFusion $ \_ -> pure a
  TransFusion a <*> TransFusion b = TransFusion $ \k -> a k <*> b k
  {-# INLINE (<*>) #-}
