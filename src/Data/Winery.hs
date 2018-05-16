{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , Serialise(..)
  , Deserialiser(..)
  , schema
  , serialise
  , serialiseOnly
  , deserialise
  , deserialiseWithSchema
  , deserialiseWithSchemaBy
  -- * Encoding
  , Encoding
  , encodeMulti
  -- * Decoding combinators
  , Decoder
  , Plan(..)
  , InnerPlan(..)
  , errorInnerPlan
  , getDecoder
  , getDecoderBy
  , extractListWith
  , extractField
  , extractFieldWith
  , extractConstructor
  , extractConstructorWith
  -- * Variable-length quantity
  , VarInt(..)
  -- * Internal
  , unwrapDeserialiser
  -- * Generics
  , GSerialiseRecord
  , gschemaViaRecord
  , gtoEncodingRecord
  , gdeserialiserRecord
  , GSerialiseVariant
  , gschemaViaVariant
  , gtoEncodingVariant
  , gdeserialiserVariant
  -- * Preset schema
  , bootstrapSchema
  )where

import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Dynamic
import Data.Functor.Identity
import Data.Foldable
import Data.Proxy
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Monoid
import Data.Word
import Data.Winery.Internal
import qualified Data.Sequence as Seq
import qualified Data.Set as S
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
  | SList !Schema
  | SArray !(VarInt Int) !Schema
  | SProduct [Schema]
  | SProductFixed [(VarInt Int, Schema)]
  | SRecord [(T.Text, Schema)]
  | SVariant [(T.Text, [Schema])]
  | SSelf !Word8
  | SFix Schema
  deriving (Show, Read, Eq, Generic)

newtype Deserialiser a = Deserialiser { getDeserialiser :: Plan (Decoder a) }
  deriving Functor

instance Applicative Deserialiser where
  pure = Deserialiser . pure . pure
  Deserialiser f <*> Deserialiser x = Deserialiser $ (<*>) <$> f <*> x

newtype InnerPlan a = InnerPlan { unInnerPlan :: [Decoder Dynamic] -> Either String a }
  deriving Functor

instance Applicative InnerPlan where
  pure = return
  (<*>) = ap

instance Monad InnerPlan where
  return = InnerPlan . const . Right
  m >>= k = InnerPlan $ \decs -> case unInnerPlan m decs of
    Right a -> unInnerPlan (k a) decs
    Left e -> Left e

instance Alternative InnerPlan where
  empty = InnerPlan $ const empty
  InnerPlan a <|> InnerPlan b = InnerPlan $ \decs -> a decs <|> b decs

instance MonadFix InnerPlan where
  mfix f = InnerPlan $ \r -> mfix $ \a -> unInnerPlan (f a) r
  {-# INLINE mfix #-}

errorInnerPlan :: String -> InnerPlan a
errorInnerPlan = InnerPlan . const . Left

newtype Plan a = Plan { unPlan :: Schema -> InnerPlan a }
  deriving Functor

instance Applicative Plan where
  pure = return
  (<*>) = ap

instance Monad Plan where
  return = Plan . const . pure
  m >>= k = Plan $ \sch -> InnerPlan $ \decs -> case unInnerPlan (unPlan m sch) decs of
    Right a -> unInnerPlan (unPlan (k a) sch) decs
    Left e -> Left e

instance Alternative Plan where
  empty = Plan $ const empty
  Plan a <|> Plan b = Plan $ \s -> a s <|> b s

unwrapDeserialiser :: Deserialiser a -> Schema -> InnerPlan (Decoder a)
unwrapDeserialiser (Deserialiser m) = unPlan m
{-# INLINE unwrapDeserialiser #-}

class Typeable a => Serialise a where
  schemaVia :: Proxy a -> [TypeRep] -> Schema
  toEncoding :: a -> Encoding
  deserialiser :: Deserialiser a

  -- | If this is @'Just' x@, the size of `toEncoding` must be @x@.
  -- `deserialiser` must not depend on this value.
  constantSize :: Proxy a -> Maybe Int
  constantSize _ = Nothing

  default schemaVia :: (Generic a, GSerialiseVariant (Rep a)) => Proxy a -> [TypeRep] -> Schema
  schemaVia = gschemaViaVariant
  default toEncoding :: (Generic a, GSerialiseVariant (Rep a)) => a -> Encoding
  toEncoding = gtoEncodingVariant
  default deserialiser :: (Generic a, GSerialiseVariant (Rep a)) => Deserialiser a
  deserialiser = gdeserialiserVariant


schema :: forall proxy a. Serialise a => proxy a -> Schema
schema _ = schemaVia (Proxy :: Proxy a) []
{-# INLINE schema #-}

getDecoder :: Serialise a => Schema -> Either String (Decoder a)
getDecoder = getDecoderBy deserialiser
{-# INLINE getDecoder #-}

getDecoderBy :: Deserialiser a -> Schema -> Either String (Decoder a)
getDecoderBy (Deserialiser plan) sch = unPlan plan sch `unInnerPlan` []
{-# INLINE getDecoderBy #-}

-- | Serialise a value along with a schema.
serialise :: Serialise a => a -> B.ByteString
serialise a = BL.toStrict $ BB.toLazyByteString $ mappend (BB.word8 0)
  $ snd $ toEncoding (schema [a], a)

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialise :: Serialise a => B.ByteString -> Either String a
deserialise bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    m <- getDecoder $ SSchema ver
    ($bs) $ evalContT $ do
      offB <- decodeVarInt
      sch <- lift m
      asks $ deserialiseWithSchema sch . B.drop offB
  Nothing -> Left "Unexpected empty string"

serialiseOnly :: Serialise a => a -> B.ByteString
serialiseOnly = BL.toStrict . BB.toLazyByteString . snd . toEncoding
{-# INLINE serialiseOnly #-}

deserialiseWithSchema :: Serialise a => Schema -> B.ByteString -> Either String a
deserialiseWithSchema = deserialiseWithSchemaBy deserialiser
{-# INLINE deserialiseWithSchema #-}

deserialiseWithSchemaBy :: Deserialiser a -> Schema -> B.ByteString -> Either String a
deserialiseWithSchemaBy m sch bs = ($ bs) <$> getDecoderBy m sch
{-# INLINE deserialiseWithSchemaBy #-}

substSchema :: Serialise a => Proxy a -> [TypeRep] -> Schema
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
  ,("SArray",[SInteger, SSelf 0])
  ,("SProduct",[SList (SSelf 0)])
  ,("SProductFixed",[SList $ SProduct [SInteger, SSelf 0]])
  ,("SRecord",[SList (SProduct [SText,SSelf 0])])
  ,("SVariant",[SList (SProduct [SText,SList (SSelf 0)])])
  ,("SFix",[SSelf 0])
  ,("SSelf",[SWord8])
  ]
bootstrapSchema n = Left $ "Unsupported version: " ++ show n

instance Serialise Schema where
  schemaVia _ _ = SSchema 0
  toEncoding = gtoEncodingVariant
  deserialiser = Deserialiser $ Plan $ \case
    SSchema n -> InnerPlan (const $ bootstrapSchema n)
      >>= unwrapDeserialiser gdeserialiserVariant
    s -> unwrapDeserialiser gdeserialiserVariant s

instance Serialise () where
  schemaVia _ _ = SUnit
  toEncoding = mempty
  deserialiser = pure ()
  constantSize _ = Just 0

instance Serialise Bool where
  schemaVia _ _ = SBool
  toEncoding False = (1, BB.word8 0)
  toEncoding True = (1, BB.word8 1)
  deserialiser = Deserialiser $ Plan $ \case
    SBool -> pure $ (/=0) <$> evalContT getWord8
    s -> errorInnerPlan $ "Expected Bool, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Word8 where
  schemaVia _ _ = SWord8
  toEncoding x = (1, BB.word8 x)
  deserialiser = Deserialiser $ Plan $ \case
    SWord8 -> pure $ evalContT getWord8
    s -> errorInnerPlan $ "Expected Word8, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Word16 where
  schemaVia _ _ = SWord16
  toEncoding x = (2, BB.word16BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SWord16 -> pure $ evalContT $ do
      a <- getWord8
      b <- getWord8
      return $! fromIntegral a `unsafeShiftL` 8 .|. fromIntegral b
    s -> errorInnerPlan $ "Expected Word16, but got " ++ show s
  constantSize _ = Just 2

instance Serialise Word32 where
  schemaVia _ _ = SWord32
  toEncoding x = (4, BB.word32BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SWord32 -> pure word32be
    s -> errorInnerPlan $ "Expected Word32, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Word64 where
  schemaVia _ _ = SWord64
  toEncoding x = (8, BB.word64BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SWord64 -> pure word64be
    s -> errorInnerPlan $ "Expected Word64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Word where
  schemaVia _ _ = SWord64
  toEncoding x = (8, BB.word64BE $ fromIntegral x)
  deserialiser = Deserialiser $ Plan $ \case
    SWord64 -> pure $ fromIntegral <$> word64be
    s -> errorInnerPlan $ "Expected Word64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Int8 where
  schemaVia _ _ = SInt8
  toEncoding x = (1, BB.int8 x)
  deserialiser = Deserialiser $ Plan $ \case
    SInt8 -> pure $ fromIntegral <$> evalContT getWord8
    s -> errorInnerPlan $ "Expected Int8, but got " ++ show s
  constantSize _ = Just 1

instance Serialise Int16 where
  schemaVia _ _ = SInt16
  toEncoding x = (2, BB.int16BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SInt16 -> pure $ fromIntegral <$> word16be
    s -> errorInnerPlan $ "Expected Int16, but got " ++ show s
  constantSize _ = Just 2

instance Serialise Int32 where
  schemaVia _ _ = SInt32
  toEncoding x = (4, BB.int32BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SInt32 -> pure $ fromIntegral <$> word32be
    s -> errorInnerPlan $ "Expected Int32, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Int64 where
  schemaVia _ _ = SInt64
  toEncoding x = (8, BB.int64BE x)
  deserialiser = Deserialiser $ Plan $ \case
    SInt64 -> pure $ fromIntegral <$> word64be
    s -> errorInnerPlan $ "Expected Int64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Int where
  schemaVia _ _ = SInt64
  toEncoding x = (8, BB.int64BE $ fromIntegral x)
  deserialiser = Deserialiser $ Plan $ \case
    SInt64 -> pure $ fromIntegral <$> word64be
    s -> errorInnerPlan $ "Expected Int64, but got " ++ show s
  constantSize _ = Just 8

instance Serialise Float where
  schemaVia _ _ = SFloat
  toEncoding x = (4, BB.word32BE $ unsafeCoerce x)
  deserialiser = Deserialiser $ Plan $ \case
    SFloat -> pure $ unsafeCoerce <$> word32be
    s -> errorInnerPlan $ "Expected Float, but got " ++ show s
  constantSize _ = Just 4

instance Serialise Double where
  schemaVia _ _ = SDouble
  toEncoding x = (8, BB.word64BE $ unsafeCoerce x)
  deserialiser = Deserialiser $ Plan $ \case
    SDouble -> pure $ unsafeCoerce <$> word64be
    s -> errorInnerPlan $ "Expected Double, but got " ++ show s
  constantSize _ = Just 8

instance Serialise T.Text where
  schemaVia _ _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  deserialiser = Deserialiser $ Plan $ \case
    SText -> pure $ T.decodeUtf8 <$> getBytes
    s -> errorInnerPlan $ "Expected Text, but got " ++ show s

newtype VarInt a = VarInt { getVarInt :: a } deriving (Show, Read, Eq, Ord, Enum
  , Bounded, Num, Real, Integral, Bits, Typeable)

instance (Typeable a, Integral a, Bits a) => Serialise (VarInt a) where
  schemaVia _ _ = SInteger
  toEncoding = encodeVarInt
  deserialiser = Deserialiser $ Plan $ \case
    SInteger -> pure $ evalContT decodeVarInt
    s -> errorInnerPlan $ "Expected Integer, but got " ++ show s

instance Serialise Integer where
  schemaVia _ _ = SInteger
  toEncoding = toEncoding . VarInt
  deserialiser = getVarInt <$> deserialiser

instance Serialise a => Serialise (Maybe a) where
  schemaVia _ ts = SVariant [("Nothing", [])
    , ("Just", [substSchema (Proxy :: Proxy a) ts])]
  toEncoding Nothing = (1, BB.word8 0)
  toEncoding (Just a) = (1, BB.word8 1) <> toEncoding a
  deserialiser = Deserialiser $ Plan $ \case
    SVariant [_, (_, [sch])] -> do
      dec <- unwrapDeserialiser deserialiser sch
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> pure Nothing
          _ -> Just <$> lift dec
    s -> errorInnerPlan $ "Expected Variant [a, b], but got " ++ show s
  constantSize _ = (1+) <$> constantSize (Proxy :: Proxy a)

instance Serialise B.ByteString where
  schemaVia _ _ = SBytes
  toEncoding bs = encodeVarInt (B.length bs)
    <> (Sum $ B.length bs, BB.byteString bs)
  deserialiser = Deserialiser $ Plan $ \case
    SBytes -> pure getBytes
    s -> errorInnerPlan $ "Expected SBytes, but got " ++ show s

instance Serialise a => Serialise [a] where
  schemaVia _ ts = case constantSize (Proxy :: Proxy a) of
    Nothing -> SList (substSchema (Proxy :: Proxy a) ts)
    Just s -> SArray (VarInt s) (substSchema (Proxy :: Proxy a) ts)
  toEncoding xs = case constantSize (Proxy :: Proxy a) of
    Nothing -> encodeVarInt (length xs)
      <> encodeMulti (map toEncoding xs)
    Just _ -> encodeVarInt (length xs) <> foldMap toEncoding xs
  deserialiser = extractListWith deserialiser

extractListWith :: Deserialiser a -> Deserialiser [a]
extractListWith (Deserialiser plan) = Deserialiser $ Plan $ \case
  SArray (VarInt size) s -> do
    getItem <- unPlan plan s
    return $ evalContT $ do
      n <- decodeVarInt
      asks $ \bs -> [decodeAt (size * i) getItem bs | i <- [0..n - 1]]
  SList s -> do
    getItem <- unPlan plan s
    return $ evalContT $ do
      n <- decodeVarInt
      offsets <- decodeOffsets n
      asks $ \bs -> [decodeAt ofs getItem bs | ofs <- offsets]
  s -> errorInnerPlan $ "Expected List or Array, but got " ++ show s

instance (Ord k, Serialise k, Serialise v) => Serialise (M.Map k v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(k, v)])
  toEncoding = toEncoding . M.toList
  deserialiser = M.fromList <$> deserialiser

instance (Eq k, Hashable k, Serialise k, Serialise v) => Serialise (HM.HashMap k v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(k, v)])
  toEncoding = toEncoding . HM.toList
  deserialiser = HM.fromList <$> deserialiser

instance (Serialise v) => Serialise (IM.IntMap v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(Int, v)])
  toEncoding = toEncoding . IM.toList
  deserialiser = IM.fromList <$> deserialiser

instance (Ord a, Serialise a) => Serialise (S.Set a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . S.toList
  deserialiser = S.fromList <$> deserialiser

instance Serialise IS.IntSet where
  schemaVia _ = schemaVia (Proxy :: Proxy [Int])
  toEncoding = toEncoding . IS.toList
  deserialiser = IS.fromList <$> deserialiser

instance Serialise a => Serialise (Seq.Seq a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . toList
  deserialiser = Seq.fromList <$> deserialiser

instance Serialise a => Serialise (Identity a) where
  schemaVia _ = schemaVia (Proxy :: Proxy a)
  toEncoding = toEncoding . runIdentity
  deserialiser = Identity <$> deserialiser
  constantSize _ = constantSize (Proxy :: Proxy a)

extractField :: Serialise a => T.Text -> Deserialiser a
extractField = extractFieldWith deserialiser
{-# INLINE extractField #-}

extractFieldWith :: Typeable a => Deserialiser a -> T.Text -> Deserialiser a
extractFieldWith (Deserialiser g) name = Deserialiser $ handleRecursion $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    case lookup name schs' of
      Just (i, sch) -> do
        m <- unPlan g sch
        return $ evalContT $ do
          offsets <- decodeOffsets (length schs)
          lift $ decodeAt (offsets !! i) m
      Nothing -> errorInnerPlan $ "Schema not found for " ++ T.unpack name
  s -> errorInnerPlan $ "Expected Record, but got " ++ show s

handleRecursion :: Typeable a => (Schema -> InnerPlan (Decoder a)) -> Plan (Decoder a)
handleRecursion k = Plan $ \sch -> InnerPlan $ \decs -> case sch of
  SSelf i -> return $ fmap (`fromDyn` error "Invalid recursion") $ decs !! fromIntegral i
  SFix s -> mfix $ \a -> unPlan (handleRecursion k) s `unInnerPlan` (fmap toDyn a : decs)
  s -> k s `unInnerPlan` decs

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schemaVia _ ts = case (constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b)) of
    (Just a, Just b) -> SProductFixed [(VarInt a, sa), (VarInt b, sb)]
    _ -> SProduct [sa, sb]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
  toEncoding (a, b) = case constantSize (Proxy :: Proxy (a, b)) of
    Nothing -> encodeMulti [toEncoding a, toEncoding b]
    Just _ -> toEncoding a <> toEncoding b
  deserialiser = Deserialiser $ Plan $ \case
    SProduct [sa, sb] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      return $ evalContT $ do
        offA <- decodeVarInt
        asks $ \bs -> (getA bs, decodeAt offA getB bs)
    SProductFixed [(VarInt la, sa), (_, sb)] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      return $ \bs -> (getA bs, decodeAt la getB bs)
    s -> errorInnerPlan $ "Expected Product or Struct, but got " ++ show s

  constantSize _ = (+) <$> constantSize (Proxy :: Proxy a) <*> constantSize (Proxy :: Proxy b)

instance (Serialise a, Serialise b, Serialise c) => Serialise (a, b, c) where
  schemaVia _ ts = case (constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b), constantSize (Proxy :: Proxy c)) of
    (Just a, Just b, Just c) -> SProductFixed [(VarInt a, sa), (VarInt b, sb), (VarInt c, sc)]
    _ -> SProduct [sa, sb, sc]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
      sc = substSchema (Proxy :: Proxy b) ts
  toEncoding (a, b, c) = case constantSize (Proxy :: Proxy (a, b, c)) of
    Nothing -> encodeMulti [toEncoding a, toEncoding b, toEncoding c]
    Just _ -> toEncoding a <> toEncoding b <> toEncoding c
  deserialiser = Deserialiser $ Plan $ \case
    SProduct [sa, sb, sc] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      getC <- unwrapDeserialiser deserialiser sc
      return $ evalContT $ do
        offA <- decodeVarInt
        offB <- decodeVarInt
        asks $ \bs -> (getA bs, decodeAt offA getB bs, decodeAt (offA + offB) getC bs)
    SProductFixed [(VarInt la, sa), (VarInt lb, sb), (_, sc)] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      getC <- unwrapDeserialiser deserialiser sc
      return $ \bs -> (getA bs, decodeAt la getB bs, decodeAt (la + lb) getC bs)
    s -> errorInnerPlan $ "Expected Product or Struct, but got " ++ show s

  constantSize _ = (+) <$> constantSize (Proxy :: Proxy a) <*> constantSize (Proxy :: Proxy b)

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schemaVia _ ts = SVariant [("Left", [substSchema (Proxy :: Proxy a) ts])
    , ("Right", [substSchema (Proxy :: Proxy b) ts])]
  toEncoding (Left a) = (1, BB.word8 0) <> toEncoding a
  toEncoding (Right b) = (1, BB.word8 1) <> toEncoding b
  deserialiser = Deserialiser $ Plan $ \case
    SVariant [(_, [sa]), (_, [sb])] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> Left <$> lift getA
          _ -> Right <$> lift getB
    s -> errorInnerPlan $ "Expected Variant [a, b], but got " ++ show s
  constantSize _ = fmap (1+) $ max
    <$> constantSize (Proxy :: Proxy a)
    <*> constantSize (Proxy :: Proxy b)

extractConstructorWith :: Typeable a => Deserialiser a -> T.Text -> Deserialiser (Maybe a)
extractConstructorWith d name = Deserialiser $ handleRecursion $ \case
  SVariant schs0 -> InnerPlan $ \decs -> do
    (j, dec) <- case [(i :: Int, ss) | (i, (k, ss)) <- zip [0..] schs0, name == k] of
      [(i, [s])] -> fmap ((,) i) $ unwrapDeserialiser d s `unInnerPlan` decs
      [(i, ss)] -> fmap ((,) i) $ unwrapDeserialiser d (SProduct ss) `unInnerPlan` decs
      _ -> Left $ "Schema not found for " ++ T.unpack name

    return $ evalContT $ do
      i <- decodeVarInt
      if i == j
        then Just <$> lift dec
        else pure Nothing
  s -> errorInnerPlan $ "Expected Variant, but got " ++ show s

extractConstructor :: (Serialise a) => T.Text -> Deserialiser (Maybe a)
extractConstructor = extractConstructorWith deserialiser
{-# INLINE extractConstructor #-}

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
{-# INLINE gtoEncodingRecord #-}

gdeserialiserRecord :: (GSerialiseRecord (Rep a), Generic a, Typeable a) => Maybe a -> Deserialiser a
gdeserialiserRecord def = Deserialiser $ handleRecursion $ \case
  SRecord schs -> InnerPlan $ \decs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    let go :: RecordDecoder T.Text x -> Either String ([Int] -> x)
        go (Done a) = Right $ const a
        go (More name def' p k) = case lookup name schs' of
          Nothing -> case def' of
            Just d -> go k >>= \r -> return $ \offsets -> r offsets (pure d)
            Nothing -> Left $ "Default value not found for " ++ T.unpack name
          Just (i, sch) -> do
            getItem <- p `unPlan` sch `unInnerPlan` decs
            r <- go k
            return $ \offsets -> r offsets (decodeAt (offsets !! i) getItem)
    m <- go $ recordDecoder $ from <$> def
    return $ evalContT $ do
      offsets <- decodeOffsets (length schs)
      asks $ \bs -> to $ m offsets bs
  s -> errorInnerPlan $ "Expected Record, but got " ++ show s

class GSerialiseRecord f where
  recordSchema :: proxy f -> [TypeRep] -> [(T.Text, Schema)]
  recordEncoder :: f x -> [Encoding]
  recordDecoder :: Maybe (f x) -> RecordDecoder T.Text (Decoder (f x))

instance (GSerialiseRecord f, GSerialiseRecord g) => GSerialiseRecord (f :*: g) where
  recordSchema _ ts = recordSchema (Proxy :: Proxy f) ts
    ++ recordSchema (Proxy :: Proxy g) ts
  recordEncoder (f :*: g) = recordEncoder f ++ recordEncoder g
  recordDecoder def = (\f g -> (:*:) <$> f <*> g)
    <$> recordDecoder ((\(x :*: _) -> x) <$> def)
    <*> recordDecoder ((\(_ :*: x) -> x) <$> def)

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ ts = [(T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), substSchema (Proxy :: Proxy a) ts)]
  recordEncoder (M1 (K1 a)) = [toEncoding a]
  recordDecoder def = More (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x)) (unK1 . unM1 <$> def) (getDeserialiser deserialiser)
    $ Done $ fmap $ M1 . K1

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder def = fmap M1 <$> recordDecoder (unM1 <$> def)

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordEncoder (M1 a) = recordEncoder a
  recordDecoder def = fmap M1 <$> recordDecoder (unM1 <$> def)

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
  productDecoder = More () Nothing (getDeserialiser deserialiser) $ Done $ fmap K1

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts
  productEncoder (M1 a) = productEncoder a
  productDecoder = fmap M1 <$> productDecoder

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts ++ productSchema (Proxy :: Proxy g) ts
  productEncoder (f :*: g) = productEncoder f ++ productEncoder g
  productDecoder = liftA2 (:*:) <$> productDecoder <*> productDecoder

deserialiserProduct' :: GSerialiseProduct f => [Decoder Dynamic] -> [Schema] -> Either String (Decoder (f x))
deserialiserProduct' recs schs0 = do
  let go :: Int -> [Schema] -> RecordDecoder () x -> Either String ([Int] -> x)
      go _ _ (Done a) = Right $ const a
      go _ [] _ = Left "Mismatching number of fields"
      go i (sch : schs) (More () _ p k) = do
        getItem <- unPlan p sch `unInnerPlan` recs
        r <- go (i + 1) schs k
        return $ \offsets -> r offsets $ decodeAt (offsets !! i) getItem
  m <- go 0 schs0 productDecoder
  return $ evalContT $ do
    offsets <- decodeOffsets (length schs0)
    asks $ \bs -> m offsets bs

gschemaViaVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> [TypeRep] -> Schema
gschemaViaVariant p ts = SFix $ SVariant $ variantSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

gtoEncodingVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> Encoding
gtoEncodingVariant = variantEncoder 0 . from
{-# INLINE gtoEncodingVariant #-}

gdeserialiserVariant :: (GSerialiseVariant (Rep a), Generic a, Typeable a) => Deserialiser a
gdeserialiserVariant = Deserialiser $ handleRecursion $ \case
  SVariant schs0 -> InnerPlan $ \decs -> do
    let ds = variantDecoder decs
    ds' <- sequence
      [ case lookup name ds of
          Nothing -> Left $ "Schema not found for " ++ T.unpack name
          Just f -> f sch
      | (name, sch) <- schs0]
    return $ evalContT $ do
      i <- decodeVarInt
      lift $ fmap to $ ds' !! i
  s -> errorInnerPlan $ "Expected Variant, but got " ++ show s

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
  variantDecoder recs = [(T.pack $ conName (M1 undefined :: M1 i c f x), fmap (fmap (fmap M1)) $ deserialiserProduct' recs) ]

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
