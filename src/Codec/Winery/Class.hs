{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
module Codec.Winery.Class (Serialise(..)
  , VarInt(..)
  , BundleSerialise(..)
  , bundleRecord
  , bundleRecordDefault
  , bundleVariant
  , getSchema
  , schema
  , unexpectedSchema
  , mkPlan
  , extractListBy
  , gschemaGenRecord
  , gtoBuilderRecord
  , gextractorRecord
  , extractorRecord'
  , gdecodeCurrentRecord
  , GEncodeProduct(..)
  , GDecodeProduct(..)
  , GSerialiseRecord(..)
  , GSerialiseProduct(..)
  , gschemaGenProduct
  , gtoBuilderProduct
  , gextractorProduct
  , gdecodeCurrentProduct
  , extractorProduct'
  , GConstructorCount(..)
  , GDecodeVariant(..)
  , GEncodeVariant(..)
  , GSerialiseVariant(..)
  , gschemaGenVariant
  , gtoBuilderVariant
  , gextractorVariant
  , gdecodeCurrentVariant
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.FastBuilder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Data.Complex
import Data.Dynamic
import Data.Fixed
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List (elemIndex)
import Data.Monoid as M
import Data.Proxy
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Scientific (Scientific, scientific, coefficient, base10Exponent)
import Data.Semigroup as S
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Ord (Down(..))
import Data.Word
import Codec.Winery.Base as W
import Codec.Winery.Internal
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Data.Text.Prettyprint.Doc hiding ((<>), SText, SChar)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Void (Void)
import Unsafe.Coerce
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Natural
import GHC.Generics
import GHC.TypeLits

-- | Serialisable datatype
--
class Typeable a => Serialise a where
  -- | Obtain the schema of the datatype.
  schemaGen :: Proxy a -> SchemaGen Schema
  schemaGen = bundleSchemaGen bundleSerialise
  {-# INLINE schemaGen #-}

  -- | Serialise a value.
  toBuilder :: a -> BB.Builder
  toBuilder = bundleToBuilder bundleSerialise
  {-# INLINE toBuilder #-}

  -- | A value of 'Extractor a' interprets a schema and builds a function from
  -- 'Term' to @a@. This must be equivalent to 'decodeCurrent' when the schema
  -- is the current one.
  --
  -- If @'extractor' s@ returns a function, the function must return a
  -- non-bottom for any 'Term' @'decodeTerm' s@ returns.
  --
  -- It must not return a function if an unsupported schema is supplied.
  --
  -- @getDecoderBy extractor (schema (Proxy @ a))@ must be @Right d@
  -- where @d@ is equivalent to 'decodeCurrent'.
  --
  extractor :: Extractor a
  extractor = bundleExtractor bundleSerialise
  {-# INLINE extractor #-}

  -- | Decode a value with the current schema.
  --
  -- @'decodeCurrent' `evalDecoder` 'toBuilder' x@ ≡ x
  decodeCurrent :: Decoder a
  decodeCurrent = bundleDecodeCurrent bundleSerialise
  {-# INLINE decodeCurrent #-}

  -- | Instead of the four methods above, you can supply a bundle.
  bundleSerialise :: BundleSerialise a
  bundleSerialise = BundleSerialise
    { bundleSchemaGen = schemaGen
    , bundleToBuilder = toBuilder
    , bundleExtractor = extractor
    , bundleDecodeCurrent = decodeCurrent
    }

  {-# MINIMAL schemaGen, toBuilder, extractor, decodeCurrent | bundleSerialise #-}

-- | A bundle of 'Serialise' methods
data BundleSerialise a = BundleSerialise
  { bundleSchemaGen :: Proxy a -> SchemaGen Schema
  , bundleToBuilder :: a -> BB.Builder
  , bundleExtractor :: Extractor a
  , bundleDecodeCurrent :: Decoder a
  }

-- | A bundle of generic implementations for records
bundleRecord :: (GEncodeProduct (Rep a), GSerialiseRecord (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a)
  => (Extractor a -> Extractor a) -- extractor modifier
  -> BundleSerialise a
bundleRecord f = BundleSerialise
  { bundleSchemaGen = gschemaGenRecord
  , bundleToBuilder = gtoBuilderRecord
  , bundleExtractor = f $ gextractorRecord Nothing
  , bundleDecodeCurrent = gdecodeCurrentRecord
  }
{-# INLINE bundleRecord #-}

-- | A bundle of generic implementations for records, with a default value
bundleRecordDefault :: (GEncodeProduct (Rep a), GSerialiseRecord (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a)
  => a -- default value
  -> (Extractor a -> Extractor a) -- extractor modifier
  -> BundleSerialise a
bundleRecordDefault def f = BundleSerialise
  { bundleSchemaGen = gschemaGenRecord
  , bundleToBuilder = gtoBuilderRecord
  , bundleExtractor = f $ gextractorRecord $ Just def
  , bundleDecodeCurrent = gdecodeCurrentRecord
  }
{-# INLINE bundleRecordDefault #-}

-- | A bundle of generic implementations for variants
bundleVariant :: (GSerialiseVariant (Rep a), GConstructorCount (Rep a), GEncodeVariant (Rep a), GDecodeVariant (Rep a), Generic a, Typeable a)
  => (Extractor a -> Extractor a) -- extractor modifier
  -> BundleSerialise a
bundleVariant f = BundleSerialise
  { bundleSchemaGen = gschemaGenVariant
  , bundleToBuilder = gtoBuilderVariant
  , bundleExtractor = f $ gextractorVariant
  , bundleDecodeCurrent = gdecodeCurrentVariant
  }
{-# INLINE bundleVariant #-}

-- | Obtain a schema on 'SchemaGen', binding a fixpoint when necessary.
-- If you are hand-rolling a definition of 'schemaGen', you should call this
-- instead of 'schemaGen'.
getSchema :: forall proxy a. Serialise a => proxy a -> SchemaGen Schema
getSchema p = SchemaGen $ \seen -> if S.member rep seen
  then (S.singleton rep, \xs -> case elemIndex rep xs of
    Just i -> SVar i
    Nothing -> error $ "getSchema: impossible " ++ show (rep, seen, xs))
    -- request a fixpoint for rep when it detects a recursion
  else case unSchemaGen (schemaGen (Proxy @ a)) (S.insert rep seen) of
    (reps, f)
      | S.member rep reps -> (reps, \xs -> SFix $ f (rep : xs))
      | otherwise -> (reps, f)
  where
    rep = typeRep p

-- | Obtain the schema of the datatype.
--
-- /"Tell me what you drink, and I will tell you what you are."/
schema :: forall proxy a. Serialise a => proxy a -> Schema
schema p = case unSchemaGen (schemaGen (Proxy @ a)) (S.singleton rep) of
  (reps, f)
    | S.member rep reps -> SFix $ f [rep]
    | otherwise -> f []
  where
    rep = typeRep p

unexpectedSchema :: forall f a. Serialise a => Doc AnsiStyle -> Schema -> Strategy' (f a)
unexpectedSchema subject actual = throwStrategy
  $ UnexpectedSchema subject (pretty $ schema (Proxy @ a)) actual

-- | Construct a plan, expanding fixpoints and let bindings.
mkPlan :: forall a. Typeable a => (Schema -> Strategy' (Term -> a)) -> Plan (Term -> a)
mkPlan k = Plan $ \sch -> Strategy $ \(StrategyEnv ofs decs) -> case sch of
  SVar i
    | point : _ <- drop i decs -> case point of
      BoundSchema ofs' sch' -> unPlan (mkPlan k) sch' `unStrategy` StrategyEnv ofs' (drop (ofs - ofs') decs)
      DynDecoder dyn -> case fromDynamic dyn of
        Nothing -> Left $ TypeMismatch i
          (typeRep (Proxy @ (Term -> a)))
          (dynTypeRep dyn)
        Just a -> Right a
    | otherwise -> Left $ UnboundVariable i
  SFix s -> mfix $ \a -> unPlan (mkPlan k) s `unStrategy` StrategyEnv (ofs + 1) (DynDecoder (toDyn a) : decs)
  SLet s t -> unPlan (mkPlan k) t `unStrategy` StrategyEnv (ofs + 1) (BoundSchema ofs s : decs)
  s -> k s `unStrategy` StrategyEnv ofs decs

instance Serialise Tag where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant

instance Serialise Schema where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant

instance Serialise () where
  schemaGen _ = pure $ SProduct mempty
  toBuilder = mempty
  {-# INLINE toBuilder #-}
  extractor = pure ()
  decodeCurrent = pure ()

instance Serialise Bool where
  schemaGen _ = pure SBool
  toBuilder False = BB.word8 0
  toBuilder True = BB.word8 1
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SBool -> pure $ \case
      TBool b -> b
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Bool" s
  decodeCurrent = (/=0) <$> getWord8

instance Serialise Word8 where
  schemaGen _ = pure SWord8
  toBuilder = BB.word8
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SWord8 -> pure $ \case
      TWord8 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word8" s
  decodeCurrent = getWord8

instance Serialise Word16 where
  schemaGen _ = pure SWord16
  toBuilder = BB.word16LE
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SWord16 -> pure $ \case
      TWord16 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word16" s
  decodeCurrent = getWord16

instance Serialise Word32 where
  schemaGen _ = pure SWord32
  toBuilder = BB.word32LE
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SWord32 -> pure $ \case
      TWord32 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word32" s
  decodeCurrent = getWord32

instance Serialise Word64 where
  schemaGen _ = pure SWord64
  toBuilder = BB.word64LE
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SWord64 -> pure $ \case
      TWord64 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word64" s
  decodeCurrent = getWord64

instance Serialise Word where
  schemaGen _ = pure SWord64
  toBuilder = BB.word64LE . fromIntegral
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SWord64 -> pure $ \case
      TWord64 i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word" s
  decodeCurrent = fromIntegral <$> getWord64

instance Serialise Int8 where
  schemaGen _ = pure SInt8
  toBuilder = BB.word8 . fromIntegral
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInt8 -> pure $ \case
      TInt8 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int8" s
  decodeCurrent = fromIntegral <$> getWord8

instance Serialise Int16 where
  schemaGen _ = pure SInt16
  toBuilder = BB.word16LE . fromIntegral
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInt16 -> pure $ \case
      TInt16 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int16" s
  decodeCurrent = fromIntegral <$> getWord16

instance Serialise Int32 where
  schemaGen _ = pure SInt32
  toBuilder = BB.word32LE . fromIntegral
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInt32 -> pure $ \case
      TInt32 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int32" s
  decodeCurrent = fromIntegral <$> getWord32

instance Serialise Int64 where
  schemaGen _ = pure SInt64
  toBuilder = BB.word64LE . fromIntegral
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInt64 -> pure $ \case
      TInt64 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int64" s
  decodeCurrent = fromIntegral <$> getWord64

instance Serialise Int where
  schemaGen _ = pure SInteger
  toBuilder = toBuilder . VarInt
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInteger -> pure $ \case
      TInteger i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int" s
  decodeCurrent = decodeVarIntFinite

instance Serialise Float where
  schemaGen _ = pure SFloat
  toBuilder = BB.floatLE
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SFloat -> pure $ \case
      TFloat x -> x
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Float" s
  decodeCurrent = castWord32ToFloat <$> getWord32

instance Serialise Double where
  schemaGen _ = pure SDouble
  toBuilder = BB.doubleLE
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SDouble -> pure $ \case
      TDouble x -> x
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Double" s
  decodeCurrent = castWord64ToDouble <$> getWord64

instance Serialise T.Text where
  schemaGen _ = pure SText
  toBuilder = toBuilder . T.encodeUtf8
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SText -> pure $ \case
      TText t -> t
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Text" s
  decodeCurrent = do
    len <- decodeVarInt
    T.decodeUtf8With T.lenientDecode <$> getBytes len

-- | Encoded in variable-length quantity.
newtype VarInt a = VarInt { getVarInt :: a } deriving (Show, Read, Eq, Ord, Enum
  , Bounded, Num, Real, Integral, Bits, Typeable)

instance (Typeable a, Bits a, Integral a) => Serialise (VarInt a) where
  schemaGen _ = pure SInteger
  toBuilder = varInt . getVarInt
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SInteger -> pure $ \case
      TInteger i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (VarInt a)" s
  decodeCurrent = VarInt <$> decodeVarInt

instance Serialise Integer where
  schemaGen _ = pure SInteger
  toBuilder = toBuilder . VarInt
  {-# INLINE toBuilder #-}
  extractor = getVarInt <$> extractor
  decodeCurrent = getVarInt <$> decodeCurrent

instance Serialise Natural where
  schemaGen _ = pure SInteger
  toBuilder = toBuilder . toInteger
  extractor = naturalFromInteger <$> extractor
  decodeCurrent = naturalFromInteger <$> decodeCurrent

instance Serialise Char where
  schemaGen _ = pure SChar
  toBuilder = toBuilder . fromEnum
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SChar -> pure $ \case
      TChar c -> c
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Char" s
  decodeCurrent = toEnum <$> decodeVarInt

instance Serialise a => Serialise (Maybe a) where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant

instance Serialise B.ByteString where
  schemaGen _ = pure SBytes
  toBuilder bs = varInt (B.length bs) <> BB.byteString bs
  {-# INLINE toBuilder #-}
  extractor = Extractor $ mkPlan $ \case
    SBytes -> pure $ \case
      TBytes bs -> bs
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise ByteString" s
  decodeCurrent = decodeVarInt >>= getBytes

instance Serialise BL.ByteString where
  schemaGen _ = pure SBytes
  toBuilder = toBuilder . BL.toStrict
  {-# INLINE toBuilder #-}
  extractor = BL.fromStrict <$> extractor
  decodeCurrent = BL.fromStrict <$> decodeCurrent

-- | time-1.9.1
nanosecondsToNominalDiffTime :: Int64 -> NominalDiffTime
nanosecondsToNominalDiffTime = unsafeCoerce . MkFixed . (*1000) . fromIntegral

instance Serialise UTCTime where
  schemaGen _ = pure SUTCTime
  toBuilder = toBuilder . utcTimeToPOSIXSeconds
  {-# INLINE toBuilder #-}
  extractor = Extractor $ Plan $ \case
    SUTCTime -> pure $ \case
      TUTCTime bs -> bs
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise UTCTime" s
  decodeCurrent = posixSecondsToUTCTime <$> decodeCurrent

instance Serialise NominalDiffTime where
  schemaGen _ = pure SInt64
  toBuilder x = case unsafeCoerce x of
    MkFixed p -> toBuilder (fromIntegral (p `div` 1000) :: Int64)
  {-# INLINE toBuilder #-}
  extractor = nanosecondsToNominalDiffTime <$> extractor
  decodeCurrent = nanosecondsToNominalDiffTime <$> decodeCurrent

-- | Extract a list or an array of values.
extractListBy :: Typeable a => Extractor a -> Extractor (V.Vector a)
extractListBy (Extractor plan) = Extractor $ mkPlan $ \case
  SVector s -> do
    getItem <- unPlan plan s
    return $ \case
      TVector xs -> V.map getItem xs
      t -> throw $ InvalidTerm t
  s -> throwStrategy $ UnexpectedSchema "extractListBy ..." "[a]" s
{-# INLINE extractListBy #-}

instance Serialise a => Serialise [a] where
  schemaGen _ = SVector <$> getSchema (Proxy @ a)
  toBuilder xs = varInt (length xs)
      <> foldMap toBuilder xs
  {-# INLINE toBuilder #-}
  extractor = V.toList <$> extractListBy extractor
  decodeCurrent = do
    n <- decodeVarInt
    replicateM n decodeCurrent

instance Serialise a => Serialise (V.Vector a) where
  schemaGen _ = SVector <$> getSchema (Proxy @ a)
  toBuilder xs = varInt (V.length xs)
    <> foldMap toBuilder xs
  {-# INLINE toBuilder #-}
  extractor = extractListBy extractor
  decodeCurrent = do
    n <- decodeVarInt
    V.replicateM n decodeCurrent

instance (SV.Storable a, Serialise a) => Serialise (SV.Vector a) where
  schemaGen _ = SVector <$> getSchema (Proxy @ a)
  toBuilder = toBuilder . (SV.convert :: SV.Vector a -> V.Vector a)
  {-# INLINE toBuilder #-}
  extractor = SV.convert <$> extractListBy extractor
  decodeCurrent = do
    n <- decodeVarInt
    SV.replicateM n decodeCurrent

instance (UV.Unbox a, Serialise a) => Serialise (UV.Vector a) where
  schemaGen _ = SVector <$> getSchema (Proxy @ a)
  toBuilder = toBuilder . (UV.convert :: UV.Vector a -> V.Vector a)
  {-# INLINE toBuilder #-}
  extractor = UV.convert <$> extractListBy extractor
  decodeCurrent = do
    n <- decodeVarInt
    UV.replicateM n decodeCurrent

instance (Ord k, Serialise k, Serialise v) => Serialise (M.Map k v) where
  schemaGen _ = schemaGen (Proxy @ [(k, v)])
  toBuilder m = toBuilder (M.size m)
    <> M.foldMapWithKey (curry toBuilder) m
  {-# INLINE toBuilder #-}
  extractor = M.fromList <$> extractor
  decodeCurrent = M.fromList <$> decodeCurrent

instance (Eq k, Hashable k, Serialise k, Serialise v) => Serialise (HM.HashMap k v) where
  schemaGen _ = schemaGen (Proxy @ [(k, v)])
  toBuilder m = toBuilder (HM.size m)
    <> HM.foldrWithKey (\k v r -> toBuilder (k, v) <> r) mempty m
  {-# INLINE toBuilder #-}
  extractor = HM.fromList <$> extractor
  decodeCurrent = HM.fromList <$> decodeCurrent

instance (Serialise v) => Serialise (IM.IntMap v) where
  schemaGen _ = schemaGen (Proxy @ [(Int, v)])
  toBuilder m = toBuilder (IM.size m)
    <> IM.foldMapWithKey (curry toBuilder) m
  {-# INLINE toBuilder #-}
  extractor = IM.fromList <$> extractor
  decodeCurrent = IM.fromList <$> decodeCurrent

instance (Ord a, Serialise a) => Serialise (S.Set a) where
  schemaGen _ = schemaGen (Proxy @ [a])
  toBuilder s = toBuilder (S.size s) <> foldMap toBuilder s
  {-# INLINE toBuilder #-}
  extractor = S.fromList <$> extractor
  decodeCurrent = S.fromList <$> decodeCurrent

instance Serialise IS.IntSet where
  schemaGen _ = schemaGen (Proxy @ [Int])
  toBuilder s = toBuilder (IS.size s) <> IS.foldr (mappend . toBuilder) mempty s
  {-# INLINE toBuilder #-}
  extractor = IS.fromList <$> extractor
  decodeCurrent = IS.fromList <$> decodeCurrent

instance Serialise a => Serialise (Seq.Seq a) where
  schemaGen _ = schemaGen (Proxy @ [a])
  toBuilder s = toBuilder (length s) <> foldMap toBuilder s
  {-# INLINE toBuilder #-}
  extractor = Seq.fromList <$> extractor
  decodeCurrent = Seq.fromList <$> decodeCurrent

instance (Integral a, Serialise a) => Serialise (Ratio a) where
  schemaGen _ = schemaGen (Proxy @ (a, a))
  toBuilder x = toBuilder (numerator x, denominator x)
  {-# INLINE toBuilder #-}
  extractor = uncurry (%) <$> extractor
  decodeCurrent = uncurry (%) <$> decodeCurrent

instance Serialise Scientific where
  schemaGen _ = schemaGen (Proxy @ (Integer, Int))
  toBuilder s = toBuilder (coefficient s, base10Exponent s)
  {-# INLINE toBuilder #-}
  extractor = Extractor $ Plan $ \s -> case s of
    SWord8 -> f (fromIntegral :: Word8 -> Scientific) s
    SWord16 -> f (fromIntegral :: Word16 -> Scientific) s
    SWord32 -> f (fromIntegral :: Word32 -> Scientific) s
    SWord64 -> f (fromIntegral :: Word64 -> Scientific) s
    SInt8 -> f (fromIntegral :: Int8 -> Scientific) s
    SInt16 -> f (fromIntegral :: Int16 -> Scientific) s
    SInt32 -> f (fromIntegral :: Int32 -> Scientific) s
    SInt64 -> f (fromIntegral :: Int64 -> Scientific) s
    SInteger -> f fromInteger s
    SFloat -> f (realToFrac :: Float -> Scientific) s
    SDouble -> f (realToFrac :: Double -> Scientific) s
    _ -> f (uncurry scientific) s
    where
      f c = unwrapExtractor (c <$> extractor)
  decodeCurrent = scientific <$> decodeCurrent <*> decodeCurrent

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance (Serialise a, Serialise b, Serialise c) => Serialise (a, b, c) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance (Serialise a, Serialise b, Serialise c, Serialise d) => Serialise (a, b, c, d) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance (Serialise a, Serialise b, Serialise c, Serialise d, Serialise e) => Serialise (a, b, c, d, e) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance (Serialise a, Serialise b, Serialise c, Serialise d, Serialise e, Serialise f) => Serialise (a, b, c, d, e, f) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant


instance Serialise Ordering where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant

deriving instance Serialise a => Serialise (Identity a)
deriving instance (Serialise a, Typeable b, Typeable k) => Serialise (Const a (b :: k))

deriving instance Serialise Any
deriving instance Serialise All
deriving instance Serialise a => Serialise (Down a)
deriving instance Serialise a => Serialise (Product a)
deriving instance Serialise a => Serialise (Sum a)
deriving instance Serialise a => Serialise (Dual a)
deriving instance Serialise a => Serialise (M.Last a)
deriving instance Serialise a => Serialise (M.First a)
deriving instance Serialise a => Serialise (S.Last a)
deriving instance Serialise a => Serialise (S.First a)
deriving instance Serialise a => Serialise (ZipList a)
deriving instance Serialise a => Serialise (Option a)
deriving instance Serialise a => Serialise (Max a)
deriving instance Serialise a => Serialise (Min a)
deriving instance (Typeable k, Typeable f, Typeable a, Serialise (f a)) => Serialise (Alt f (a :: k))
deriving instance (Typeable j, Typeable k, Typeable f, Typeable g, Typeable a, Serialise (f (g a))) => Serialise (Compose f (g :: j -> k) (a :: j))
#if MIN_VERSION_base(4,12,0)
deriving instance (Typeable k, Typeable f, Typeable a, Serialise (f a)) => Serialise (Ap f (a :: k))
#endif

instance (Typeable k, Typeable a, Typeable b, a ~ b) => Serialise ((a :: k) :~: b) where
  schemaGen _ = pure $ SProduct mempty
  toBuilder = mempty
  extractor = pure Refl
  decodeCurrent = pure Refl

instance (Serialise a, Serialise b) => Serialise (Arg a b) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance Serialise a => Serialise (Complex a) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct

instance Serialise Void where
  schemaGen _ = pure $ SVariant V.empty
  toBuilder = mempty
  extractor = Extractor $ Plan $ const $ throwStrategy "No extractor for Void"
  decodeCurrent = error "No decodeCurrent for Void"

--------------------------------------------------------------------------------

-- | Generic implementation of 'schemaGen' for a record.
gschemaGenRecord :: forall proxy a. (GSerialiseRecord (Rep a), Generic a, Typeable a) => proxy a -> SchemaGen Schema
gschemaGenRecord _ = SRecord . V.fromList <$> recordSchema (Proxy @ (Rep a))

-- | Generic implementation of 'toBuilder' for a record.
gtoBuilderRecord :: (GEncodeProduct (Rep a), Generic a) => a -> BB.Builder
gtoBuilderRecord = productEncoder . from
{-# INLINE gtoBuilderRecord #-}

data FieldDecoder i a = FieldDecoder !i !(Maybe a) !(Plan (Term -> a))

-- | Generic implementation of 'extractor' for a record.
gextractorRecord :: forall a. (GSerialiseRecord (Rep a), Generic a, Typeable a)
  => Maybe a -- ^ default value (optional)
  -> Extractor a
gextractorRecord def = Extractor $ mkPlan
  $ fmap (to .) <$> extractorRecord'
  ("gextractorRecord :: Extractor " <> viaShow (typeRep (Proxy @ a)))
  (from <$> def)

-- | Generic implementation of 'extractor' for a record.
extractorRecord' :: (GSerialiseRecord f)
  => Doc AnsiStyle
  -> Maybe (f x) -- ^ default value (optional)
  -> Schema -> Strategy' (Term -> f x)
extractorRecord' rep def (SRecord schs) = Strategy $ \decs -> do
    let go :: FieldDecoder T.Text x -> Either WineryException (Term -> x)
        go (FieldDecoder name def' p) = case lookupWithIndexV name schs of
          Nothing -> case def' of
            Just d -> Right (const d)
            Nothing -> Left $ FieldNotFound rep name (map fst $ V.toList schs)
          Just (i, sch) -> case p `unPlan` sch `unStrategy` decs of
            Right getItem -> Right $ \case
              TRecord xs -> maybe (error (show rep)) (getItem . snd) $ xs V.!? i
              t -> throw $ InvalidTerm t
            Left e -> Left e
    unTransFusion (recordExtractor def) go
extractorRecord' rep _ s = throwStrategy $ UnexpectedSchema rep "a record" s
{-# INLINE gextractorRecord #-}

-- | Synonym for 'gdecodeCurrentProduct'
gdecodeCurrentRecord :: (GDecodeProduct (Rep a), Generic a) => Decoder a
gdecodeCurrentRecord = to <$> productDecoder
{-# INLINE gdecodeCurrentRecord #-}

-- | Encode all the fields
class GEncodeProduct f where
  productEncoder :: f x -> BB.Builder

instance GEncodeProduct U1 where
  productEncoder _ = mempty
  {-# INLINE productEncoder #-}

instance (GEncodeProduct f, GEncodeProduct g) => GEncodeProduct (f :*: g) where
  productEncoder (f :*: g) = productEncoder f <> productEncoder g
  {-# INLINE productEncoder #-}

instance Serialise a => GEncodeProduct (S1 c (K1 i a)) where
  productEncoder (M1 (K1 a)) = toBuilder a
  {-# INLINE productEncoder #-}

instance GEncodeProduct f => GEncodeProduct (C1 c f) where
  productEncoder (M1 a) = productEncoder a
  {-# INLINE productEncoder #-}

instance GEncodeProduct f => GEncodeProduct (D1 c f) where
  productEncoder (M1 a) = productEncoder a
  {-# INLINE productEncoder #-}

class GDecodeProduct f where
  productDecoder :: Decoder (f x)

instance GDecodeProduct U1 where
  productDecoder = pure U1

instance Serialise a => GDecodeProduct (K1 i a) where
  productDecoder = K1 <$> decodeCurrent
  {-# INLINE productDecoder #-}

instance GDecodeProduct f => GDecodeProduct (M1 i c f) where
  productDecoder = M1 <$> productDecoder
  {-# INLINE productDecoder #-}

instance (GDecodeProduct f, GDecodeProduct g) => GDecodeProduct (f :*: g) where
  productDecoder = (:*:) <$> productDecoder <*> productDecoder
  {-# INLINE productDecoder #-}

class GSerialiseRecord f where
  recordSchema :: proxy f -> SchemaGen [(T.Text, Schema)]
  recordExtractor :: Maybe (f x) -> TransFusion (FieldDecoder T.Text) ((->) Term) (Term -> f x)

instance (GSerialiseRecord f, GSerialiseRecord g) => GSerialiseRecord (f :*: g) where
  recordSchema _ = (++) <$> recordSchema (Proxy @ f) <*> recordSchema (Proxy @ g)
  recordExtractor def = (\f g -> (:*:) <$> f <*> g)
    <$> recordExtractor ((\(x :*: _) -> x) <$> def)
    <*> recordExtractor ((\(_ :*: x) -> x) <$> def)
  {-# INLINE recordExtractor #-}

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ = do
    s <- getSchema (Proxy @ a)
    pure [(T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), s)]
  recordExtractor def = TransFusion $ \k -> fmap (fmap (M1 . K1)) $ k $ FieldDecoder
    (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x))
    (unK1 . unM1 <$> def)
    (getExtractor extractor)
  {-# INLINE recordExtractor #-}

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy @ f)
  recordExtractor def = fmap M1 <$> recordExtractor (unM1 <$> def)

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy @ f)
  recordExtractor def = fmap M1 <$> recordExtractor (unM1 <$> def)

class GSerialiseProduct f where
  productSchema :: proxy f -> SchemaGen [Schema]
  productExtractor :: Compose (State Int) (TransFusion (FieldDecoder Int) ((->) Term)) (Term -> f x)

instance GSerialiseProduct U1 where
  productSchema _ = pure []
  productExtractor = pure (pure U1)

instance (Serialise a) => GSerialiseProduct (K1 i a) where
  productSchema _ = pure <$> getSchema (Proxy @ a)
  productExtractor = Compose $ State $ \i ->
    ( TransFusion $ \k -> fmap (fmap K1) $ k $ FieldDecoder i Nothing (getExtractor extractor)
    , i + 1)

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ = productSchema (Proxy @ f)
  productExtractor = fmap M1 <$> productExtractor

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ = (++) <$> productSchema (Proxy @ f) <*> productSchema (Proxy @ g)
  productExtractor = liftA2 (:*:) <$> productExtractor <*> productExtractor

gschemaGenProduct :: forall proxy a. (Generic a, GSerialiseProduct (Rep a)) => proxy a -> SchemaGen Schema
gschemaGenProduct _ = SProduct . V.fromList <$> productSchema (Proxy @ (Rep a))
{-# INLINE gschemaGenProduct #-}

gtoBuilderProduct :: (Generic a, GEncodeProduct (Rep a)) => a -> BB.Builder
gtoBuilderProduct = productEncoder . from
{-# INLINE gtoBuilderProduct #-}

-- | Generic implementation of 'extractor' for a record.
gextractorProduct :: forall a. (GSerialiseProduct (Rep a), Generic a, Typeable a)
  => Extractor a
gextractorProduct = Extractor $ mkPlan $ fmap (to .) . extractorProduct'
{-# INLINE gextractorProduct #-}

-- | Generic implementation of 'extractor' for a record.
gdecodeCurrentProduct :: forall a. (GDecodeProduct (Rep a), Generic a)
  => Decoder a
gdecodeCurrentProduct = to <$> productDecoder
{-# INLINE gdecodeCurrentProduct #-}

extractorProduct' :: GSerialiseProduct f => Schema -> Strategy' (Term -> f x)
extractorProduct' sch
  | Just schs <- strip sch = Strategy $ \recs -> do
    let go :: FieldDecoder Int x -> Either WineryException (Term -> x)
        go (FieldDecoder i _ p) = do
          getItem <- if i < length schs
            then unPlan p (schs V.! i) `unStrategy` recs
            else Left $ ProductTooSmall $ length schs
          return $ \case
            TProduct xs -> getItem $ maybe (throw $ InvalidTerm (TProduct xs)) id
              $ xs V.!? i
            t -> throw $ InvalidTerm t
    unTransFusion (getCompose productExtractor `evalState` 0) go
  where
    strip (SProduct xs) = Just xs
    strip (SRecord xs) = Just $ V.map snd xs
    strip _ = Nothing
extractorProduct' sch = throwStrategy $ UnexpectedSchema "extractorProduct'" "a product" sch

-- | Generic implementation of 'schemaGen' for an ADT.
gschemaGenVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> SchemaGen Schema
gschemaGenVariant _ = SVariant . V.fromList <$> variantSchema (Proxy @ (Rep a))

-- | Generic implementation of 'toBuilder' for an ADT.
gtoBuilderVariant :: forall a. (GConstructorCount (Rep a), GEncodeVariant (Rep a), Generic a) => a -> BB.Builder
gtoBuilderVariant = variantEncoder (variantCount (Proxy :: Proxy (Rep a))) 0 . from
{-# INLINE gtoBuilderVariant #-}

-- | Generic implementation of 'extractor' for an ADT.
gextractorVariant :: forall a. (GSerialiseVariant (Rep a), Generic a, Typeable a)
  => Extractor a
gextractorVariant = Extractor $ mkPlan $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    ds' <- traverse (\(name, sch) -> case lookup name variantExtractor of
      Nothing -> Left $ FieldNotFound rep name (map fst $ V.toList schs0)
      Just f -> f sch `unStrategy` decs) schs0
    return $ \case
      TVariant i _ v -> to $ maybe (throw InvalidTag) ($ v) $ ds' V.!? i
      t -> throw $ InvalidTerm t
  s -> throwStrategy $ UnexpectedSchema rep "a variant" s
  where
    rep = "gextractorVariant :: Extractor "
      <> viaShow (typeRep (Proxy @ a))

gdecodeCurrentVariant :: forall a. (GConstructorCount (Rep a), GEncodeVariant (Rep a), GDecodeVariant (Rep a), Generic a) => Decoder a
gdecodeCurrentVariant = decodeVarInt >>= fmap to . variantDecoder (variantCount (Proxy :: Proxy (Rep a)))
{-# INLINE gdecodeCurrentVariant #-}

class GConstructorCount f where
  variantCount :: proxy f -> Int

instance (GConstructorCount f, GConstructorCount g) => GConstructorCount (f :+: g) where
  variantCount _ = variantCount (Proxy @ f) + variantCount (Proxy @ g)
  {-# INLINE variantCount #-}

instance GConstructorCount (C1 i f) where
  variantCount _ = 1
  {-# INLINE variantCount #-}

instance GConstructorCount f => GConstructorCount (D1 i f) where
  variantCount _ = variantCount (Proxy @ f)
  {-# INLINE variantCount #-}

class GDecodeVariant f where
  variantDecoder :: Int -> Int -> Decoder (f x)

instance (GDecodeVariant f, GDecodeVariant g) => GDecodeVariant (f :+: g) where
  variantDecoder len i
    | i < len' = L1 <$> variantDecoder len' i
    | otherwise = R1 <$> variantDecoder (len - len') (i - len')
    where
      -- Nested ':+:' are balanced
      -- cf. https://github.com/GaloisInc/cereal/blob/cereal-0.5.8.1/src/Data/Serialize.hs#L659
      len' = unsafeShiftR len 1
  {-# INLINE variantDecoder #-}

instance GDecodeProduct f => GDecodeVariant (C1 i f) where
  variantDecoder _ _ = M1 <$> productDecoder
  {-# INLINE variantDecoder #-}

instance GDecodeVariant f => GDecodeVariant (D1 i f) where
  variantDecoder len i = M1 <$> variantDecoder len i
  {-# INLINE variantDecoder #-}

class GEncodeVariant f where
  variantEncoder :: Int -> Int -> f x -> BB.Builder

instance (GEncodeVariant f, GEncodeVariant g) => GEncodeVariant (f :+: g) where
  variantEncoder len i (L1 f) = variantEncoder (unsafeShiftR len 1) i f
  variantEncoder len i (R1 g) = variantEncoder (len - len') (i + len') g
    where
      -- Nested ':+:' are balanced
      -- cf. https://github.com/GaloisInc/cereal/blob/cereal-0.5.8.1/src/Data/Serialize.hs#L659
      len' = unsafeShiftR len 1
  {-# INLINE variantEncoder #-}

instance (GEncodeProduct f) => GEncodeVariant (C1 i f) where
  variantEncoder _ !i (M1 a) = varInt i <> productEncoder a
  {-# INLINE variantEncoder #-}

instance GEncodeVariant f => GEncodeVariant (D1 i f) where
  variantEncoder len i (M1 a) = variantEncoder len i a
  {-# INLINE variantEncoder #-}

class GSerialiseVariant f where
  variantSchema :: proxy f -> SchemaGen [(T.Text, Schema)]
  variantExtractor :: [(T.Text, Schema -> Strategy' (Term -> f x))]

instance (GSerialiseVariant f, GSerialiseVariant g) => GSerialiseVariant (f :+: g) where
  variantSchema _ = (++) <$> variantSchema (Proxy @ f) <*> variantSchema (Proxy @ g)
  variantExtractor = fmap (fmap (fmap (fmap (fmap L1)))) variantExtractor
    ++ fmap (fmap (fmap (fmap (fmap R1)))) variantExtractor

instance (GSerialiseProduct f, KnownSymbol name) => GSerialiseVariant (C1 ('MetaCons name fixity 'False) f) where
  variantSchema _ = do
    s <- productSchema (Proxy @ f)
    return [(T.pack $ symbolVal (Proxy @ name), SProduct $ V.fromList s)]
  variantExtractor = [(T.pack $ symbolVal (Proxy @ name), fmap (fmap M1) . extractorProduct') ]

instance (GSerialiseRecord f, KnownSymbol name) => GSerialiseVariant (C1 ('MetaCons name fixity 'True) f) where
  variantSchema _ = do
    s <- recordSchema (Proxy @ f)
    return [(T.pack $ symbolVal (Proxy @ name), SRecord $ V.fromList s)]
  variantExtractor = [(T.pack $ symbolVal (Proxy @ name), fmap (fmap M1) . extractorRecord' "" Nothing) ]

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantSchema _ = variantSchema (Proxy @ f)
  variantExtractor = fmap (fmap (fmap (fmap M1))) <$> variantExtractor
