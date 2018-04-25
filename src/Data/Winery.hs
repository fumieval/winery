{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Winery
  ( Schema(..)
  , Serialise(..)
  , serialise
  , deserialise
  , deserialiseWith
  , Encoding
  , encodeMulti
  , Decoder
  , Plan
  , extractFieldWith
  , GSerialiseRecord
  , gschemaRecord
  , gtoEncodingRecord
  , ggetDecoderRecord
  , GSerialiseVariant
  , gschemaVariant
  , gtoEncodingVariant
  , ggetDecoderVariant
  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Functor.Identity
import Data.Proxy
import Data.Int
import Data.Monoid
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
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
  | SRecord [(String, Schema)]
  | SVariant [(String, [Schema])]
  deriving (Show, Read, Eq)

type Encoding = (Sum Int, Builder)

type Decoder = (->) B.ByteString

type Plan = ReaderT Schema (Either String)

class Serialise a where
  schema :: proxy a -> Schema
  toEncoding :: a -> Encoding
  getDecoder :: Plan (Decoder a)

serialise :: Serialise a => a -> B.ByteString
serialise = BL.toStrict . BB.toLazyByteString . snd . toEncoding

deserialiseWith :: Plan (Decoder a) -> Schema -> B.ByteString -> Either String a
deserialiseWith m sch bs = ($ bs) <$> runReaderT m sch

deserialise :: Serialise a => Schema -> B.ByteString -> Either String a
deserialise = deserialiseWith getDecoder

decodeAt :: Int -> Decoder a -> Decoder a
decodeAt i m bs = m $ B.drop i bs

encodeVarInt :: (Integral a, Bits a) => a -> Encoding
encodeVarInt n
  | n < 0x80 = (1, BB.word8 $ fromIntegral n)
  | otherwise = let (s, b) = encodeVarInt (shiftR n 7)
    in (1 + s, BB.word8 (setBit (fromIntegral n) 7) `mappend` b)

getWord8 :: ContT r Decoder Word8
getWord8 = ContT $ \k bs -> case B.uncons bs of
  Nothing -> k 0 bs
  Just (x, bs') -> k x bs'

getBytes :: Decoder B.ByteString
getBytes = runContT decodeVarInt B.take

decodeVarInt :: (Num a, Bits a) => ContT r Decoder a
decodeVarInt = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- decodeVarInt
      return $! shiftL m 7 .|. clearBit (fromIntegral n) 7
    | otherwise -> return $ fromIntegral n

instance Serialise Schema where
  schema _ = SSchema 0
  toEncoding = toEncoding . BC.pack . show
  getDecoder = ReaderT $ \case
    SSchema 0 -> Right $ read <$> BC.unpack <$> getBytes
    SSchema n -> Left $ "Unsupported schema: " ++ show n
    s -> Left $ "Expected Schema, but got " ++ show s

instance Serialise () where
  schema _ = SUnit
  toEncoding = mempty
  getDecoder = pure (pure ())

instance Serialise Bool where
  schema _ = SBool
  toEncoding False = (1, BB.word8 0)
  toEncoding True = (1, BB.word8 1)
  getDecoder = ReaderT $ \case
    SBool -> Right $ (/=0) <$> evalContT getWord8
    s -> Left $ "Expected Bool, but got " ++ show s

instance Serialise Word8 where
  schema _ = SWord8
  toEncoding x = (1, BB.word8 x)
  getDecoder = ReaderT $ \case
    SWord8 -> Right $ evalContT getWord8
    s -> Left $ "Expected Word8, but got " ++ show s

instance Serialise Word16 where
  schema _ = SWord16
  toEncoding x = (2, BB.word16BE x)
  getDecoder = ReaderT $ \case
    SWord16 -> Right $ evalContT $ do
      a <- getWord8
      b <- getWord8
      return $! fromIntegral a `unsafeShiftL` 8 .|. fromIntegral b
    s -> Left $ "Expected Word16, but got " ++ show s

instance Serialise Word32 where
  schema _ = SWord32
  toEncoding x = (4, BB.word32BE x)
  getDecoder = ReaderT $ \case
    SWord32 -> Right word32be
    s -> Left $ "Expected Word32, but got " ++ show s

instance Serialise Word64 where
  schema _ = SWord64
  toEncoding x = (8, BB.word64BE x)
  getDecoder = ReaderT $ \case
    SWord64 -> Right word64be
    s -> Left $ "Expected Word64, but got " ++ show s

instance Serialise Word where
  schema _ = SWord64
  toEncoding x = (8, BB.word64BE $ fromIntegral x)
  getDecoder = ReaderT $ \case
    SWord64 -> Right $ fromIntegral <$> word64be
    s -> Left $ "Expected Word64, but got " ++ show s

instance Serialise Int8 where
  schema _ = SInt8
  toEncoding x = (1, BB.int8 x)
  getDecoder = ReaderT $ \case
    SInt8 -> Right $ fromIntegral <$> evalContT getWord8
    s -> Left $ "Expected Int8, but got " ++ show s

instance Serialise Int16 where
  schema _ = SInt16
  toEncoding x = (2, BB.int16BE x)
  getDecoder = ReaderT $ \case
    SInt16 -> Right $ fromIntegral <$> word16be
    s -> Left $ "Expected Int16, but got " ++ show s

instance Serialise Int32 where
  schema _ = SInt32
  toEncoding x = (4, BB.int32BE x)
  getDecoder = ReaderT $ \case
    SInt32 -> Right $ fromIntegral <$> word32be
    s -> Left $ "Expected Int32, but got " ++ show s

instance Serialise Int64 where
  schema _ = SInt64
  toEncoding x = (8, BB.int64BE x)
  getDecoder = ReaderT $ \case
    SInt64 -> Right $ fromIntegral <$> word64be
    s -> Left $ "Expected Int64, but got " ++ show s

instance Serialise Int where
  schema _ = SInt64
  toEncoding x = (8, BB.int64BE $ fromIntegral x)
  getDecoder = ReaderT $ \case
    SInt64 -> Right $ fromIntegral <$> word64be
    s -> Left $ "Expected Int64, but got " ++ show s

instance Serialise Float where
  schema _ = SFloat
  toEncoding x = (4, BB.word32BE $ unsafeCoerce x)
  getDecoder = ReaderT $ \case
    SFloat -> Right $ unsafeCoerce <$> word32be
    s -> Left $ "Expected Float, but got " ++ show s

instance Serialise Double where
  schema _ = SDouble
  toEncoding x = (8, BB.word64BE $ unsafeCoerce x)
  getDecoder = ReaderT $ \case
    SDouble -> Right $ unsafeCoerce <$> word64be
    s -> Left $ "Expected Double, but got " ++ show s

instance Serialise T.Text where
  schema _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  getDecoder = ReaderT $ \case
    SText -> Right $ T.decodeUtf8 <$> getBytes
    s -> Left $ "Expected Text, but got " ++ show s

instance Serialise Integer where
  schema _ = SInteger
  toEncoding = encodeVarInt
  getDecoder = ReaderT $ \case
    SInteger -> Right $ evalContT decodeVarInt
    s -> Left $ "Expected Integer, but got " ++ show s

instance Serialise a => Serialise (Maybe a) where
  schema _ = schema (Proxy :: Proxy (Either () a))
  toEncoding = toEncoding . maybe (Left ()) Right
  getDecoder = fmap (either (\() -> Nothing) Just) <$> getDecoder

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
  getDecoder = ReaderT $ \case
    SBytes -> Right getBytes
    s -> Left $ "Expected SBytes, but got " ++ show s

instance Serialise a => Serialise [a] where
  schema _ = SList (schema (Proxy :: Proxy a))
  toEncoding xs = encodeVarInt (length xs)
    <> encodeMulti (map toEncoding xs)
  getDecoder = ReaderT $ \case
    SList s -> do
      getItem <- runReaderT getDecoder s
      return $ evalContT $ do
        n <- decodeVarInt
        offsets <- replicateM n decodeVarInt
        asks $ \bs -> [decodeAt ofs getItem bs | ofs <- 0 : init offsets]
    s -> Left $ "Expected Schema, but got " ++ show s

instance Serialise a => Serialise (Identity a) where
  schema _ = schema (Proxy :: Proxy a)
  toEncoding = toEncoding . runIdentity
  getDecoder = fmap Identity <$> getDecoder

extractFieldWith :: Plan (Decoder a) -> String -> Plan (Decoder a)
extractFieldWith g name = ReaderT $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    case lookup name schs' of
      Just (i, sch) -> do
        m <- runReaderT g sch
        return $ evalContT $ do
          offsets <- (0:) <$> mapM (const decodeVarInt) schs
          lift $ \bs -> m $ B.drop (offsets !! i) bs
      Nothing -> Left $ "Schema not found for " ++ name
  s -> Left $ "Expected Record, but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schema _ = SProduct [schema (Proxy :: Proxy a), schema (Proxy :: Proxy b)]
  toEncoding (a, b) = encodePair (toEncoding a) (toEncoding b)
  getDecoder = decodePair (,) getDecoder getDecoder

encodePair :: Encoding -> Encoding -> Encoding
encodePair ea@(Sum oa, _) eb@(Sum ob, _) = encodeVarInt oa
  <> encodeVarInt (oa + ob)
  <> ea <> eb

decodePair :: (a -> b -> c)
  -> Plan (Decoder a)
  -> Plan (Decoder b)
  -> Plan (Decoder c)
decodePair f extA extB = ReaderT $ \case
  SProduct [sa, sb] -> do
    getA <- runReaderT extA sa
    getB <- runReaderT extB sb
    return $ evalContT $ do
      offA <- decodeVarInt
      asks $ \bs -> getA bs `f` decodeAt offA getB bs
  s -> Left $ "Expected Product [a, b], but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schema _ = SSum [schema (Proxy :: Proxy a), schema (Proxy :: Proxy b)]
  toEncoding (Left a) = (1, BB.word8 0) <> toEncoding a
  toEncoding (Right b) = (1, BB.word8 1) <> toEncoding b
  getDecoder = ReaderT $ \case
    SSum [sa, sb] -> do
      getA <- runReaderT getDecoder sa
      getB <- runReaderT getDecoder sb
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> Left <$> lift getA
          _ -> Right <$> lift getB
    s -> Left $ "Expected Sum [a, b], but got " ++ show s

encodeMulti :: [Encoding] -> Encoding
encodeMulti ls = foldMap encodeVarInt offsets <> foldMap id ls where
  offsets = drop 1 $ scanl (+) 0 $ map (getSum . fst) ls

data RecordDecoder i x = Done x | forall a. More !i !(Plan (Decoder a)) (RecordDecoder i (Decoder a -> x))

deriving instance Functor (RecordDecoder i)

instance Applicative (RecordDecoder i) where
  pure = Done
  Done f <*> a = fmap f a
  More i p k <*> c = More i p (flip <$> k <*> c)

gschemaRecord :: forall proxy a. (GSerialiseRecord (Rep a), Generic a) => proxy a -> Schema
gschemaRecord _ = SRecord $ recordSchema (Proxy :: Proxy (Rep a))

gtoEncodingRecord :: (GSerialiseRecord (Rep a), Generic a) => a -> Encoding
gtoEncodingRecord = encodeMulti . recordEncoder . from

ggetDecoderRecord :: (GSerialiseRecord (Rep a), Generic a) => Plan (Decoder a)
ggetDecoderRecord = ReaderT $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    let go :: RecordDecoder String x -> Either String ([Int] -> x)
        go (Done a) = Right $ const a
        go (More name p k) = case lookup name schs' of
          Nothing -> Left $ "Schema not found for " ++ name
          Just (i, sch) -> do
            getItem <- runReaderT p sch
            r <- go k
            return $ \offsets -> r offsets (decodeAt (offsets !! i) getItem)
    m <- go recordDecoder
    return $ evalContT $ do
      offsets <- (0:) <$> mapM (const decodeVarInt) schs
      asks $ \bs -> to $ m offsets bs
  s -> Left $ "Expected Record, but got " ++ show s

class GSerialiseRecord f where
  recordSchema :: proxy f -> [(String, Schema)]
  recordEncoder :: f x -> [Encoding]
  recordDecoder :: RecordDecoder String (Decoder (f x))

instance (Serialise a, Selector c, GSerialiseRecord r) => GSerialiseRecord (S1 c (K1 i a) :*: r) where
  recordSchema _ = (selName (M1 undefined :: M1 i c (K1 i a) x), schema (Proxy :: Proxy a)) : recordSchema (Proxy :: Proxy r)
  recordEncoder (M1 (K1 a) :*: r) = toEncoding a : recordEncoder r
  recordDecoder = More (selName (M1 undefined :: M1 i c (K1 i a) x)) getDecoder
    $ fmap (\r a -> (:*:) <$> fmap (M1 . K1) a <*> r) recordDecoder

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ = [(selName (M1 undefined :: M1 i c (K1 i a) x), schema (Proxy :: Proxy a))]
  recordEncoder (M1 (K1 a)) = [toEncoding a]
  recordDecoder = More (selName (M1 undefined :: M1 i c (K1 i a) x)) getDecoder
    $ Done $ fmap $ M1 . K1

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder = fmap M1 <$> recordDecoder

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder = fmap M1 <$> recordDecoder

class GSerialiseProduct f where
  productSchema :: proxy f -> [Schema]
  productEncoder :: f x -> [Encoding]
  productDecoder :: RecordDecoder () (Decoder (f x))

instance GSerialiseProduct U1 where
  productSchema _ = []
  productEncoder _ = []
  productDecoder = Done (pure U1)

instance (Serialise a) => GSerialiseProduct (K1 i a) where
  productSchema _ = [schema (Proxy :: Proxy a)]
  productEncoder (K1 a) = [toEncoding a]
  productDecoder = More () getDecoder $ Done $ fmap K1

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ = productSchema (Proxy :: Proxy f)
  productEncoder (M1 a) = productEncoder a
  productDecoder = fmap M1 <$> productDecoder

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ = productSchema (Proxy :: Proxy f) ++ productSchema (Proxy :: Proxy g)
  productEncoder (f :*: g) = productEncoder f ++ productEncoder g
  productDecoder = liftA2 (:*:) <$> productDecoder <*> productDecoder

getDecoderProduct' :: GSerialiseProduct f => [Schema] -> Either String (Decoder (f x))
getDecoderProduct' schs0 = do
  let go :: Int -> [Schema] -> RecordDecoder () x -> Either String ([Int] -> x)
      go _ _ (Done a) = Right $ const a
      go _ [] _ = Left "Mismatching number of fields"
      go i (sch : schs) (More () p k) = do
        getItem <- runReaderT p sch
        r <- go (i + 1) schs k
        return $ \offsets -> r offsets (decodeAt (offsets !! i) getItem)
  m <- go 0 schs0 productDecoder
  return $ evalContT $ do
    offsets <- (0:) <$> mapM (const decodeVarInt) schs0
    asks $ \bs -> m offsets bs

gschemaVariant :: forall proxy a. (GSerialiseVariant (Rep a), Generic a) => proxy a -> Schema
gschemaVariant _ = SVariant $ variantSchema (Proxy :: Proxy (Rep a))

gtoEncodingVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> Encoding
gtoEncodingVariant = variantEncoder 0 . from

ggetDecoderVariant :: (GSerialiseVariant (Rep a), Generic a) => Plan (Decoder a)
ggetDecoderVariant = ReaderT $ \case
  SVariant schs0 -> do
    let ds = variantDecoder
    ds' <- sequence
      [ case lookup name ds of
          Nothing -> Left $ "Schema not found for " ++ name
          Just f -> f sch
      | (name, sch) <- schs0]
    return $ evalContT $ do
      i <- decodeVarInt
      lift $ fmap to $ ds' !! i
  s -> Left $ "Expected Variant, but got " ++ show s

class GSerialiseVariant f where
  variantSchema :: proxy f -> [(String, [Schema])]
  variantEncoder :: Int -> f x -> Encoding
  variantDecoder :: [(String, [Schema] -> Either String (Decoder (f x)))]

instance (GSerialiseProduct f, Constructor c, GSerialiseVariant r) => GSerialiseVariant (C1 c f :+: r) where
  variantSchema _ = (conName (M1 undefined :: M1 i c f x), productSchema (Proxy :: Proxy f)) : variantSchema (Proxy :: Proxy r)
  variantEncoder i (L1 (M1 a)) = encodeVarInt i <> encodeMulti (productEncoder a)
  variantEncoder i (R1 r) = variantEncoder (i + 1) r
  variantDecoder = (conName (M1 undefined :: M1 i c f x), fmap (fmap (fmap (L1 . M1))) getDecoderProduct')
    : fmap (fmap (fmap (fmap (fmap R1)))) variantDecoder

instance (GSerialiseProduct f, Constructor c) => GSerialiseVariant (C1 c f) where
  variantSchema _ = [(conName (M1 undefined :: M1 i c f x), productSchema (Proxy :: Proxy f))]
  variantEncoder i (M1 a) = encodeVarInt i <> encodeMulti (productEncoder a)
  variantDecoder = [(conName (M1 undefined :: M1 i c f x), fmap (fmap (fmap M1)) getDecoderProduct') ]

instance (GSerialiseVariant f) => GSerialiseVariant (S1 c f) where
  variantSchema _ = variantSchema (Proxy :: Proxy f)
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder = fmap (fmap (fmap (fmap M1))) <$> variantDecoder

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantSchema _ = variantSchema (Proxy :: Proxy f)
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder = fmap (fmap (fmap (fmap M1))) <$> variantDecoder
