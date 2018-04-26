{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Winery
  ( Schema(..)
  , sizeFromSchema
  , Serialise(..)
  , schema
  , getDecoder
  , serialise
  , deserialise
  , serialiseOnly
  , deserialiseWithSchema
  , deserialiseWithSchemaBy
  , Encoding
  , encodeMulti
  , Decoder
  , Plan
  , extractField
  , extractFieldWith
  , GSerialiseRecord
  , gschemaViaRecord
  , gtoEncodingRecord
  , gplanDecoderRecord
  , GSerialiseVariant
  , gschemaViaVariant
  , gtoEncodingVariant
  , gplanDecoderVariant
  -- * Preset schema
  , bootstrapSchema
  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Dynamic
import Data.Functor.Identity
import Data.Proxy
import Data.Int
import Data.List (elemIndex)
import Data.Monoid
import Data.Word
import Data.Winery.Internal
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
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
  | SRecord [(T.Text, Schema)]
  | SVariant [(T.Text, [Schema])]
  | SSelf !Word8
  | SFix Schema
  deriving (Show, Read, Eq, Generic)

sizeFromSchema :: Schema -> Maybe Int
sizeFromSchema (SSchema _) = Just 1
sizeFromSchema SUnit = Just 0
sizeFromSchema SBool = Just 1
sizeFromSchema SWord8 = Just 1
sizeFromSchema SWord16 = Just 2
sizeFromSchema SWord32 = Just 4
sizeFromSchema SWord64 = Just 8
sizeFromSchema SInt8 = Just 1
sizeFromSchema SInt16 = Just 2
sizeFromSchema SInt32 = Just 4
sizeFromSchema SInt64 = Just 8
sizeFromSchema SInteger = Nothing
sizeFromSchema SFloat = Just 4
sizeFromSchema SDouble = Just 8
sizeFromSchema SBytes = Nothing
sizeFromSchema SText = Nothing
sizeFromSchema (SList _) = Nothing
sizeFromSchema (SProduct _) = Nothing
sizeFromSchema (SRecord _) = Nothing
sizeFromSchema (SVariant _) = Nothing
sizeFromSchema (SFix s) = sizeFromSchema s
sizeFromSchema (SSelf _) = Nothing

type Plan = ReaderT Schema (ReaderT [Decoder Dynamic] (Either String))

class Typeable a => Serialise a where
  schemaVia :: proxy a -> [TypeRep] -> Schema
  toEncoding :: a -> Encoding
  planDecoder :: Plan (Decoder a)

  -- | If this is @'Just' x@, the size of `toEncoding` must be @x@.
  constantSize :: proxy a -> Maybe Int
  constantSize _ = Nothing

schema :: Serialise a => proxy a -> Schema
schema p = schemaVia p []

getDecoder :: Serialise a => Schema -> Either String (Decoder a)
getDecoder sch = runReaderT (runReaderT planDecoder sch) []

-- | Serialise a value along with a schema.
serialise :: Serialise a => a -> B.ByteString
serialise a = serialiseOnly (schema [a], a)

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialise :: Serialise a => B.ByteString -> Either String a
deserialise bs = do
  m <- getDecoder $ SSchema 0
  ($bs) $ evalContT $ do
    offB <- decodeVarInt
    sch <- lift m
    asks $ deserialiseWithSchema sch . B.drop offB

serialiseOnly :: Serialise a => a -> B.ByteString
serialiseOnly = BL.toStrict . BB.toLazyByteString . snd . toEncoding

deserialiseWithSchema :: Serialise a => Schema -> B.ByteString -> Either String a
deserialiseWithSchema = deserialiseWithSchemaBy planDecoder

deserialiseWithSchemaBy :: Plan (Decoder a) -> Schema -> B.ByteString -> Either String a
deserialiseWithSchemaBy m sch bs = ($ bs) <$> runReaderT (runReaderT m sch) []

substSchema :: Serialise a => proxy a -> [TypeRep] -> Schema
substSchema p ts
  | Just i <- elemIndex (typeRep p) ts = SSelf $ fromIntegral i
  | otherwise = schemaVia p ts

bootstrapSchema :: Word8 -> Either String Schema
bootstrapSchema 0 = Right $ SFix $ SVariant [("SSchema",[SWord8])
  ,("SUnit",[])
  ,("SBool",[])
  ,("SWord8",[])
  ,("SWord16",[])
  ,("SWord32",[])
  ,("SWord64",[])
  ,("SInt8",[])
  ,("SInt16",[])
  ,("SInt32",[])
  ,("SInt64",[])
  ,("SInteger",[])
  ,("SFloat",[])
  ,("SDouble",[])
  ,("SBytes",[])
  ,("SText",[])
  ,("SList",[SSelf 0])
  ,("SProduct",[SList (SSelf 0)])
  ,("SRecord",[SList (SProduct [SText,SSelf 0])])
  ,("SVariant",[SList (SProduct [SText,SList (SSelf 0)])])
  ,("SSelf",[SWord8])
  ,("SFix",[SSelf 0])]
bootstrapSchema n = Left $ "Unsupported version: " ++ show n

instance Serialise Schema where
  schemaVia _ _ = SSchema 0
  toEncoding = gtoEncodingVariant
  planDecoder = ReaderT $ \case
    SSchema n -> lift (bootstrapSchema n) >>= runReaderT gplanDecoderVariant
    s -> runReaderT gplanDecoderVariant s

instance Serialise () where
  schemaVia _ _ = SUnit
  toEncoding = mempty
  planDecoder = pure (pure ())
  constantSize _ = Just 0

instance Serialise Bool where
  schemaVia _ _ = SBool
  toEncoding False = (1, BB.word8 0)
  toEncoding True = (1, BB.word8 1)
  planDecoder = ReaderT $ \case
    SBool -> pure $ (/=0) <$> evalContT getWord8
    s -> lift $ Left $ "Expected Bool, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Word8 where
  schemaVia _ _ = SWord8
  toEncoding x = (1, BB.word8 x)
  planDecoder = ReaderT $ \case
    SWord8 -> pure $ evalContT getWord8
    s -> lift $ Left $ "Expected Word8, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Word16 where
  schemaVia _ _ = SWord16
  toEncoding x = (2, BB.word16BE x)
  planDecoder = ReaderT $ \case
    SWord16 -> pure $ evalContT $ do
      a <- getWord8
      b <- getWord8
      return $! fromIntegral a `unsafeShiftL` 8 .|. fromIntegral b
    s -> lift $ Left $ "Expected Word16, but got " ++ show s
  constantSize _ = Just 2

instance Serialise Word32 where
  schemaVia _ _ = SWord32
  toEncoding x = (4, BB.word32BE x)
  planDecoder = ReaderT $ \case
    SWord32 -> pure word32be
    s -> lift $ Left $ "Expected Word32, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Word64 where
  schemaVia _ _ = SWord64
  toEncoding x = (8, BB.word64BE x)
  planDecoder = ReaderT $ \case
    SWord64 -> pure word64be
    s -> lift $ Left $ "Expected Word64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Word where
  schemaVia _ _ = SWord64
  toEncoding x = (8, BB.word64BE $ fromIntegral x)
  planDecoder = ReaderT $ \case
    SWord64 -> pure $ fromIntegral <$> word64be
    s -> lift $ Left $ "Expected Word64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Int8 where
  schemaVia _ _ = SInt8
  toEncoding x = (1, BB.int8 x)
  planDecoder = ReaderT $ \case
    SInt8 -> pure $ fromIntegral <$> evalContT getWord8
    s -> lift $ Left $ "Expected Int8, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Int16 where
  schemaVia _ _ = SInt16
  toEncoding x = (2, BB.int16BE x)
  planDecoder = ReaderT $ \case
    SInt16 -> pure $ fromIntegral <$> word16be
    s -> lift $ Left $ "Expected Int16, but got " ++ show s
  constantSize _ = Just 2

instance Serialise Int32 where
  schemaVia _ _ = SInt32
  toEncoding x = (4, BB.int32BE x)
  planDecoder = ReaderT $ \case
    SInt32 -> pure $ fromIntegral <$> word32be
    s -> lift $ Left $ "Expected Int32, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Int64 where
  schemaVia _ _ = SInt64
  toEncoding x = (8, BB.int64BE x)
  planDecoder = ReaderT $ \case
    SInt64 -> pure $ fromIntegral <$> word64be
    s -> lift $ Left $ "Expected Int64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Int where
  schemaVia _ _ = SInt64
  toEncoding x = (8, BB.int64BE $ fromIntegral x)
  planDecoder = ReaderT $ \case
    SInt64 -> pure $ fromIntegral <$> word64be
    s -> lift $ Left $ "Expected Int64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Float where
  schemaVia _ _ = SFloat
  toEncoding x = (4, BB.word32BE $ unsafeCoerce x)
  planDecoder = ReaderT $ \case
    SFloat -> pure $ unsafeCoerce <$> word32be
    s -> lift $ Left $ "Expected Float, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Double where
  schemaVia _ _ = SDouble
  toEncoding x = (8, BB.word64BE $ unsafeCoerce x)
  planDecoder= ReaderT $ \case
    SDouble -> pure $ unsafeCoerce <$> word64be
    s -> lift $ Left $ "Expected Double, but got " ++ show s
  constantSize _ = Just 8

instance Serialise T.Text where
  schemaVia _ _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  planDecoder = ReaderT $ \case
    SText -> pure $ T.decodeUtf8 <$> getBytes
    s -> lift $ Left $ "Expected Text, but got " ++ show s

instance Serialise Integer where
  schemaVia _ _ = SInteger
  toEncoding = encodeVarInt
  planDecoder = ReaderT $ \case
    SInteger -> pure $ evalContT decodeVarInt
    s -> lift $ Left $ "Expected Integer, but got " ++ show s

instance Serialise a => Serialise (Maybe a) where
  schemaVia _ ts = SVariant [("Nothing", [])
    , ("Just", [substSchema (Proxy :: Proxy a) ts])]
  toEncoding Nothing = (1, BB.word8 0)
  toEncoding (Just a) = (1, BB.word8 1) <> toEncoding a
  planDecoder = ReaderT $ \case
    SVariant [_, (_, [sch])] -> do
      dec <- runReaderT planDecoder sch
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> pure Nothing
          _ -> Just <$> lift dec
    s -> lift $ Left $ "Expected Variant [a, b], but got " ++ show s
  constantSize _ = (1+) <$> constantSize (Proxy :: Proxy a)

instance Serialise B.ByteString where
  schemaVia _ _ = SBytes
  toEncoding bs = encodeVarInt (B.length bs)
    <> (Sum $ B.length bs, BB.byteString bs)
  planDecoder = ReaderT $ \case
    SBytes -> pure getBytes
    s -> lift $ Left $ "Expected SBytes, but got " ++ show s

instance Serialise a => Serialise [a] where
  schemaVia _ ts = SList (substSchema (Proxy :: Proxy a) ts)
  toEncoding xs = case constantSize xs of
    Nothing -> encodeVarInt (length xs)
      <> encodeMulti (map toEncoding xs)
    Just _ -> encodeVarInt (length xs) <> foldMap toEncoding xs
  planDecoder = ReaderT $ \case
    SList s -> do
      getItem <- runReaderT planDecoder s
      return $ evalContT $ do
        n <- decodeVarInt
        case constantSize getItem of
          Nothing -> do
            offsets <- replicateM (n - 1) decodeVarInt
            asks $ \bs -> [decodeAt ofs getItem bs | ofs <- take n $ 0 : offsets]
          Just size -> asks $ \bs -> [decodeAt (size * i) getItem bs | i <- [0..n - 1]]
    s -> lift $ Left $ "Expected Schema, but got " ++ show s

instance Serialise a => Serialise (Identity a) where
  schemaVia _ ts = schemaVia (Proxy :: Proxy a) ts
  toEncoding = toEncoding . runIdentity
  planDecoder = fmap Identity <$> planDecoder
  constantSize _ = constantSize (Proxy :: Proxy a)

extractField :: Serialise a => T.Text -> Plan (Decoder a)
extractField = extractFieldWith planDecoder

extractFieldWith :: Plan (Decoder a) -> T.Text -> Plan (Decoder a)
extractFieldWith g name = ReaderT $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    case lookup name schs' of
      Just (i, sch) -> do
        m <- runReaderT g sch
        return $ evalContT $ do
          offsets <- (0:) <$> replicateM (length schs - 1) decodeVarInt
          lift $ \bs -> m $ B.drop (offsets !! i) bs
      Nothing -> lift $ Left $ "Schema not found for " ++ T.unpack name
  s -> lift $ Left $ "Expected Record, but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schemaVia _ ts = SProduct [substSchema (Proxy :: Proxy a) ts, substSchema (Proxy :: Proxy b) ts]
  toEncoding (a, b) = case constantSize (Proxy :: Proxy (a, b)) of
    Nothing -> encodeMulti [toEncoding a, toEncoding b]
    Just _ -> toEncoding a <> toEncoding b
  planDecoder = case constantSize (Proxy :: Proxy (a, b)) of
    Nothing -> decodePair (,) planDecoder planDecoder
    Just _ -> ReaderT $ \case
      SProduct [sa, sb] -> case constantSize (Proxy :: Proxy a) of
        Just offA -> do
          getA <- runReaderT planDecoder sa
          getB <- runReaderT planDecoder sb
          return $ \bs -> (getA bs, decodeAt offA getB bs)
        Nothing -> error "Impossible"
      s -> lift $ Left $ "Expected Product [a, b], but got " ++ show s

  constantSize _ = (+) <$> constantSize (Proxy :: Proxy a) <*> constantSize (Proxy :: Proxy b)

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
  s -> lift $ Left $ "Expected Product [a, b], but got " ++ show s

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schemaVia _ ts = SVariant [("Left", [substSchema (Proxy :: Proxy a) ts])
    , ("Right", [substSchema (Proxy :: Proxy b) ts])]
  toEncoding (Left a) = (1, BB.word8 0) <> toEncoding a
  toEncoding (Right b) = (1, BB.word8 1) <> toEncoding b
  planDecoder = ReaderT $ \case
    SVariant [(_, [sa]), (_, [sb])] -> do
      getA <- runReaderT planDecoder sa
      getB <- runReaderT planDecoder sb
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> Left <$> lift getA
          _ -> Right <$> lift getB
    s -> lift $ Left $ "Expected Variant [a, b], but got " ++ show s
  constantSize _ = fmap (1+) $ max
    <$> constantSize (Proxy :: Proxy a)
    <*> constantSize (Proxy :: Proxy b)

data RecordDecoder i x = Done x | forall a. More !i !(Maybe a) !(Plan (Decoder a)) (RecordDecoder i (Decoder a -> x))

deriving instance Functor (RecordDecoder i)

instance Applicative (RecordDecoder i) where
  pure = Done
  Done f <*> a = fmap f a
  More i p d k <*> c = More i p d (flip <$> k <*> c)

gschemaViaRecord :: forall proxy a. (GSerialiseRecord (Rep a), Generic a, Typeable a) => proxy a -> [TypeRep] -> Schema
gschemaViaRecord p ts = SFix $ SRecord $ recordSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

gtoEncodingRecord :: (GSerialiseRecord (Rep a), Generic a) => a -> Encoding
gtoEncodingRecord = encodeMulti . recordEncoder . from

gplanDecoderRecord :: (GSerialiseRecord (Rep a), Generic a, Typeable a) => a -> Plan (Decoder a)
gplanDecoderRecord def = ReaderT $ \sch_ -> ReaderT $ \decs -> case sch_ of
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    let go :: RecordDecoder T.Text x -> Either String ([Int] -> x)
        go (Done a) = Right $ const a
        go (More name def' p k) = case lookup name schs' of
          Nothing -> case def' of
            Just d -> go k >>= \r -> return $ \offsets -> r offsets (pure d)
            Nothing -> Left $ "Default value not found for " ++ T.unpack name
          Just (i, sch) -> do
            getItem <- p `runReaderT` sch `runReaderT` decs
            r <- go k
            return $ \offsets -> r offsets (decodeAt (offsets !! i) getItem)
    m <- go $ recordDecoder $ from def
    return $ evalContT $ do
      offsets <- (0:) <$> replicateM (length schs - 1) decodeVarInt
      asks $ \bs -> to $ m offsets bs
  SSelf i -> return $ fmap (`fromDyn` error "Invalid recursion") $ decs !! fromIntegral i
  SFix s -> mfix $ \a -> runReaderT (gplanDecoderRecord def) s `runReaderT` (fmap toDyn a : decs)
  s -> Left $ "Expected Record, but got " ++ show s

class GSerialiseRecord f where
  recordSchema :: proxy f -> [TypeRep] -> [(T.Text, Schema)]
  recordEncoder :: f x -> [Encoding]
  recordDecoder :: f x -> RecordDecoder T.Text (Decoder (f x))

instance (Serialise a, Selector c, GSerialiseRecord r) => GSerialiseRecord (S1 c (K1 i a) :*: r) where
  recordSchema _ ts = (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), substSchema (Proxy :: Proxy a) ts)
    : recordSchema (Proxy :: Proxy r) ts
  recordEncoder (M1 (K1 a) :*: r) = toEncoding a : recordEncoder r
  recordDecoder (M1 (K1 def) :*: rest)= More (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x)) (Just def) planDecoder
    $ fmap (\r a -> (:*:) <$> fmap (M1 . K1) a <*> r) (recordDecoder rest)

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ ts = [(T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), substSchema (Proxy :: Proxy a) ts)]
  recordEncoder (M1 (K1 a)) = [toEncoding a]
  recordDecoder (M1 (K1 def)) = More (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x)) (Just def) planDecoder
    $ Done $ fmap $ M1 . K1

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder (M1 def) = fmap M1 <$> recordDecoder def

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder (M1 def) = fmap M1 <$> recordDecoder def

class GSerialiseProduct f where
  productSchema :: proxy f -> [TypeRep] -> [Schema]
  productEncoder :: f x -> [Encoding]
  productDecoder :: RecordDecoder () (Decoder (f x))

instance GSerialiseProduct U1 where
  productSchema _ _ = []
  productEncoder _ = []
  productDecoder = Done (pure U1)

instance (Serialise a) => GSerialiseProduct (K1 i a) where
  productSchema _ ts = [substSchema (Proxy :: Proxy a) ts]
  productEncoder (K1 a) = [toEncoding a]
  productDecoder = More () Nothing planDecoder $ Done $ fmap K1

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts
  productEncoder (M1 a) = productEncoder a
  productDecoder = fmap M1 <$> productDecoder

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts ++ productSchema (Proxy :: Proxy g) ts
  productEncoder (f :*: g) = productEncoder f ++ productEncoder g
  productDecoder = liftA2 (:*:) <$> productDecoder <*> productDecoder

planDecoderProduct' :: GSerialiseProduct f => [Decoder Dynamic] -> [Schema] -> Either String (Decoder (f x))
planDecoderProduct' recs schs0 = do
  let go :: Int -> [Schema] -> RecordDecoder () x -> Either String ([Int] -> x)
      go _ _ (Done a) = Right $ const a
      go _ [] _ = Left "Mismatching number of fields"
      go i (sch : schs) (More () _ p k) = do
        getItem <- runReaderT p sch `runReaderT` recs
        r <- go (i + 1) schs k
        return $ \offsets -> r offsets (decodeAt (offsets !! i) getItem)
  m <- go 0 schs0 productDecoder
  return $ evalContT $ do
    offsets <- (0:) <$> replicateM (length schs0 - 1) decodeVarInt
    asks $ \bs -> m offsets bs

gschemaViaVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> [TypeRep] -> Schema
gschemaViaVariant p ts = SFix $ SVariant $ variantSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

gtoEncodingVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> Encoding
gtoEncodingVariant = variantEncoder 0 . from

gplanDecoderVariant :: (GSerialiseVariant (Rep a), Generic a, Typeable a) => Plan (Decoder a)
gplanDecoderVariant = ReaderT $ \sch_ -> ReaderT $ \decs -> case sch_ of
  SVariant schs0 -> do
    let ds = variantDecoder decs
    ds' <- sequence
      [ case lookup name ds of
          Nothing -> Left $ "Schema not found for " ++ T.unpack name
          Just f -> f sch
      | (name, sch) <- schs0]
    return $ evalContT $ do
      i <- decodeVarInt
      lift $ fmap to $ ds' !! i
  SSelf i -> return $ fmap (`fromDyn`error "Invalid recursion") $ decs !! fromIntegral i
  SFix s -> mfix $ \a -> runReaderT gplanDecoderVariant s `runReaderT` (fmap toDyn a : decs)
  s -> Left $ "Expected Variant, but got " ++ show s

class GSerialiseVariant f where
  variantCount :: proxy f -> Int
  variantSchema :: proxy f -> [TypeRep] -> [(T.Text, [Schema])]
  variantEncoder :: Int -> f x -> Encoding
  variantDecoder :: [Decoder Dynamic] -> [(T.Text, [Schema] -> Either String (Decoder (f x)))]

instance (GSerialiseVariant f, GSerialiseVariant g) => GSerialiseVariant (f :+: g) where
  variantCount _ = variantCount (Proxy :: Proxy f) + variantCount (Proxy :: Proxy g)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts ++ variantSchema (Proxy :: Proxy g) ts
  variantEncoder i (L1 f) = variantEncoder i f
  variantEncoder i (R1 g) = variantEncoder (i + variantCount (Proxy :: Proxy f)) g
  variantDecoder recs = fmap (fmap (fmap (fmap (fmap L1)))) (variantDecoder recs)
    ++ fmap (fmap (fmap (fmap (fmap R1)))) (variantDecoder recs)

instance (GSerialiseProduct f, Constructor c) => GSerialiseVariant (C1 c f) where
  variantCount _ = 1
  variantSchema _ ts = [(T.pack $ conName (M1 undefined :: M1 i c f x), productSchema (Proxy :: Proxy f) ts)]
  variantEncoder i (M1 a) = encodeVarInt i <> encodeMulti (productEncoder a)
  variantDecoder recs = [(T.pack $ conName (M1 undefined :: M1 i c f x), fmap (fmap (fmap M1)) $ planDecoderProduct' recs) ]

instance (GSerialiseVariant f) => GSerialiseVariant (S1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder recs = fmap (fmap (fmap (fmap M1))) <$> variantDecoder recs

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder recs = fmap (fmap (fmap (fmap M1))) <$> variantDecoder recs
