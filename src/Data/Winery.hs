{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Winery
  ( Schema(..)
  , Serialise(..)
  , serialise
  , deserialise
  )where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Proxy
import Data.Int
import Data.Monoid
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Unsafe.Coerce

data Schema = SSchema !Word8
  | SUnit
  | SBool
  | SWord8
  | SWord16
  | SWord32
  | SWord64
  | SInt8
  | SInt16
  | SInt32
  | SInt64
  | SInteger
  | SFloat
  | SDouble
  | SBytes
  | SText
  | SList Schema
  | SProduct [Schema]
  | SSum [Schema]
  deriving (Show, Read, Eq)

type Extractor = State B.ByteString

type Encoding = (Sum Int, Builder)

encodeVarInt :: (Integral a, Bits a) => a -> Encoding
encodeVarInt n
  | n < 0x80 = (1, BB.word8 $ fromIntegral n)
  | otherwise = let (s, b) = encodeVarInt (shiftR n 7)
    in (1 + s, BB.word8 (setBit (fromIntegral n) 7) `mappend` b)

getWord8 :: Extractor Word8
getWord8 = StateT $ \bs -> case B.uncons bs of
  Nothing -> return (0, bs)
  Just (x, bs') -> return (x, bs')

getBytes :: Extractor B.ByteString
getBytes = do
  n <- decodeVarInt
  state $ B.splitAt n

decodeVarInt :: (Num a, Bits a) => Extractor a
decodeVarInt = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- decodeVarInt
      return $! shiftL m 7 .|. clearBit (fromIntegral n) 7
    | otherwise -> return $ fromIntegral n

type Decoder = ReaderT Schema (Either String)

class Serialise a where
  schema :: proxy a -> Schema
  toEncoding :: a -> Encoding
  getExtractor :: Decoder (Extractor a)

serialise :: Serialise a => a -> B.ByteString
serialise = BL.toStrict . BB.toLazyByteString . snd . toEncoding

deserialise :: Serialise a => Schema -> B.ByteString -> Either String a
deserialise sch bs = flip evalState bs <$> runReaderT getExtractor sch

instance Serialise Schema where
  schema _ = SSchema 0
  toEncoding = toEncoding . BC.pack . show
  getExtractor = ReaderT $ \case
    SSchema 0 -> Right $ read <$> BC.unpack <$> getBytes
    SSchema n -> Left $ "Unsupported schema: " ++ show n
    s -> Left $ "Expected Schema, but got " ++ show s

instance Serialise () where
  schema _ = SUnit
  toEncoding = mempty
  getExtractor = pure (pure ())

instance Serialise Bool where
  schema _ = SBool
  toEncoding False = (1, BB.word8 0)
  toEncoding True = (1, BB.word8 1)
  getExtractor = ReaderT $ \case
    SBool -> Right $ (/=0) <$> getWord8
    s -> Left $ "Expected Bool, but got " ++ show s

instance Serialise Word8 where
  schema _ = SWord8
  toEncoding x = (1, BB.word8 x)
  getExtractor = ReaderT $ \case
    SWord8 -> Right getWord8
    s -> Left $ "Expected Word8, but got " ++ show s

instance Serialise Word16 where
  schema _ = SWord16
  toEncoding x = (2, BB.word16BE x)
  getExtractor = ReaderT $ \case
    SWord16 -> Right $ do
      a <- getWord8
      b <- getWord8
      return $! fromIntegral a `unsafeShiftL` 8 .|. fromIntegral b
    s -> Left $ "Expected Word16, but got " ++ show s

instance Serialise Word32 where
  schema _ = SWord32
  toEncoding x = (4, BB.word32BE x)
  getExtractor = ReaderT $ \case
    SWord32 -> Right $ getN 4 word32be
    s -> Left $ "Expected Word32, but got " ++ show s

instance Serialise Word64 where
  schema _ = SWord64
  toEncoding x = (8, BB.word64BE x)
  getExtractor = ReaderT $ \case
    SWord64 -> Right $ getN 8 word64be
    s -> Left $ "Expected Word64, but got " ++ show s

instance Serialise Word where
  schema _ = SWord64
  toEncoding x = (8, BB.word64BE $ fromIntegral x)
  getExtractor = ReaderT $ \case
    SWord64 -> Right $ fromIntegral <$> getN 8 word64be
    s -> Left $ "Expected Word64, but got " ++ show s

instance Serialise Int8 where
  schema _ = SInt8
  toEncoding x = (1, BB.int8 x)
  getExtractor = ReaderT $ \case
    SInt8 -> Right $ fromIntegral <$> getWord8
    s -> Left $ "Expected Int8, but got " ++ show s

instance Serialise Int16 where
  schema _ = SInt16
  toEncoding x = (2, BB.int16BE x)
  getExtractor = ReaderT $ \case
    SInt16 -> Right $ fromIntegral <$> getN 2 word16be
    s -> Left $ "Expected Int16, but got " ++ show s

instance Serialise Int32 where
  schema _ = SInt32
  toEncoding x = (4, BB.int32BE x)
  getExtractor = ReaderT $ \case
    SInt32 -> Right $ fromIntegral <$> getN 4 word32be
    s -> Left $ "Expected Int32, but got " ++ show s

instance Serialise Int64 where
  schema _ = SInt64
  toEncoding x = (8, BB.int64BE x)
  getExtractor = ReaderT $ \case
    SInt64 -> Right $ fromIntegral <$> getN 8 word64be
    s -> Left $ "Expected Int64, but got " ++ show s

instance Serialise Int where
  schema _ = SInt64
  toEncoding x = (8, BB.int64BE $ fromIntegral x)
  getExtractor = ReaderT $ \case
    SInt64 -> Right $ fromIntegral <$> getN 8 word64be
    s -> Left $ "Expected Int64, but got " ++ show s

instance Serialise Float where
  schema _ = SFloat
  toEncoding x = (4, BB.word32BE $ unsafeCoerce x)
  getExtractor = ReaderT $ \case
    SFloat -> Right $ unsafeCoerce <$> getN 4 word32be
    s -> Left $ "Expected Float, but got " ++ show s

instance Serialise Double where
  schema _ = SDouble
  toEncoding x = (8, BB.word64BE $ unsafeCoerce x)
  getExtractor = ReaderT $ \case
    SDouble -> Right $ unsafeCoerce <$> getN 8 word64be
    s -> Left $ "Expected Double, but got " ++ show s

instance Serialise T.Text where
  schema _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  getExtractor = ReaderT $ \case
    SText -> Right $ T.decodeUtf8 <$> getBytes
    s -> Left $ "Expected Text, but got " ++ show s

getN :: Int -> (B.ByteString -> a) -> Extractor a
getN n k = state $ \bs -> let (b, r) = B.splitAt n bs in (k b, r)

word16be :: B.ByteString -> Word16
word16be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 8) .|.
  (fromIntegral (s `B.unsafeIndex` 1))

word32be :: B.ByteString -> Word32
word32be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 3) )

word64be :: B.ByteString -> Word64
word64be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 56) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 48) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 40) .|.
  (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 32) .|.
  (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 7) )

instance Serialise B.ByteString where
  schema _ = SBytes
  toEncoding bs = encodeVarInt (B.length bs)
    <> (Sum $ B.length bs, BB.byteString bs)
  getExtractor = ReaderT $ \case
    SBytes -> Right getBytes
    s -> Left $ "Expected SBytes, but got " ++ show s

instance Serialise a => Serialise [a] where
  schema _ = SList (schema (Proxy :: Proxy a))
  toEncoding xs = encodeVarInt (length xs)
    <> foldMap toEncoding xs
  getExtractor = ReaderT $ \case
    SList s -> do
      getItem <- runReaderT getExtractor s
      return $ do
        n <- decodeVarInt
        replicateM n getItem
    s -> Left $ "Expected Schema, but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schema _ = SProduct [schema (Proxy :: Proxy a), schema (Proxy :: Proxy b)]
  toEncoding (a, b) = encodePair (toEncoding a) (toEncoding b)
  getExtractor = decodePair (,) getExtractor getExtractor

encodePair :: Encoding -> Encoding -> Encoding
encodePair ea@(Sum oa, _) eb@(Sum ob, _) = encodeVarInt oa
  <> encodeVarInt (oa + ob)
  <> ea <> eb

decodePair :: (a -> b -> c)
  -> Decoder (Extractor a)
  -> Decoder (Extractor b)
  -> Decoder (Extractor c)
decodePair f extA extB = ReaderT $ \case
  SProduct [sa, sb] -> do
    getA <- runReaderT extA sa
    getB <- runReaderT extB sb
    return $ do
      offA <- decodeVarInt
      offAB <- decodeVarInt
      StateT $ \bs -> return
        (evalState getA bs `f` evalState getB (B.drop offA bs), B.drop offAB bs)
  s -> Left $ "Expected Product [a, b], but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schema _ = SSum [schema (Proxy :: Proxy a), schema (Proxy :: Proxy b)]
  toEncoding (Left a) = (1, BB.word8 0) <> toEncoding a
  toEncoding (Right b) = (1, BB.word8 1) <> toEncoding b
  getExtractor = ReaderT $ \case
    SSum [sa, sb] -> do
      getA <- runReaderT getExtractor sa
      getB <- runReaderT getExtractor sb
      return $ do
        t <- decodeVarInt :: Extractor Word8
        case t of
          0 -> getA
          _ -> getB
    s -> Left $ "Expected Sum [a, b], but got " ++ show s
