{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Winery
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
-- Stability   :  Provisional
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------------
module Codec.Winery
  ( Schema
  , SchemaP(..)
  , Tag(..)
  , Serialise(..)
  , testSerialise
  , DecodeException(..)
  , schema
  -- * Standalone serialisation
  , toBuilderWithSchema
  , serialise
  , deserialise
  , deserialiseBy
  , deserialiseTerm
  , splitSchema
  , writeFileSerialise
  -- * Separate serialisation
  , serialiseSchema
  , deserialiseSchema
  , Extractor(..)
  , unwrapExtractor
  , Decoder
  , evalDecoder
  , serialiseOnly
  , getDecoder
  , getDecoderBy
  -- * Decoding combinators
  , Term(..)
  , Subextractor(..)
  , buildExtractor
  , extractListBy
  , extractField
  , extractFieldBy
  , extractConstructor
  , extractConstructorBy
  , extractVoid
  , ExtractException(..)
  -- * Variable-length quantity
  , VarInt(..)
  -- * Internal
  , WineryException(..)
  , prettyWineryException
  , unexpectedSchema
  , SchemaGen
  , getSchema
  , Plan(..)
  , mkPlan
  -- * DerivingVia
  , WineryRecord(..)
  , WineryVariant(..)
  , WineryProduct(..)
  -- * Generic implementations (for old GHC / custom instances)
  , GSerialiseRecord
  , gschemaGenRecord
  , GEncodeProduct
  , gtoBuilderRecord
  , gextractorRecord
  , gdecodeCurrentRecord
  , GSerialiseVariant
  , gschemaGenVariant
  , gtoBuilderVariant
  , gextractorVariant
  , gschemaGenProduct
  , gtoBuilderProduct
  , gdecodeCurrentVariant
  , gextractorProduct
  , gdecodeCurrentProduct
  , decodeCurrentDefault
  -- * Bundles
  , BundleSerialise(..)
  , bundleRecord
  , bundleRecordDefault
  , bundleVariant
  -- * Preset schema
  , bootstrapSchema
  )where

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
import Data.Ratio
import Data.Scientific (Scientific, scientific, coefficient, base10Exponent)
import Data.Semigroup as S
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Ord
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
import Data.Void
import Unsafe.Coerce
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Natural
import GHC.Generics
import GHC.TypeLits
import System.IO
import qualified Test.QuickCheck as QC

-- | Deserialiser for a 'Term'.
--
-- /"I will read anything rather than work."/
decodeTerm :: Schema -> Decoder Term
decodeTerm = go [] where
  go points = \case
    SBool -> TBool <$> decodeCurrent
    W.SChar -> TChar <$> decodeCurrent
    SWord8 -> TWord8 <$> getWord8
    SWord16 -> TWord16 <$> getWord16
    SWord32 -> TWord32 <$> getWord32
    SWord64 -> TWord64 <$> getWord64
    SInt8 -> TInt8 <$> decodeCurrent
    SInt16 -> TInt16 <$> decodeCurrent
    SInt32 -> TInt32 <$> decodeCurrent
    SInt64 -> TInt64 <$> decodeCurrent
    SInteger -> TInteger <$> decodeVarInt
    SFloat -> TFloat <$> decodeCurrent
    SDouble -> TDouble <$> decodeCurrent
    SBytes -> TBytes <$> decodeCurrent
    W.SText -> TText <$> decodeCurrent
    SUTCTime -> TUTCTime <$> decodeCurrent
    SVector sch -> do
      n <- decodeVarInt
      TVector <$> V.replicateM n (go points sch)
    SProduct schs -> TProduct <$> traverse (go points) schs
    SRecord schs -> TRecord <$> traverse (\(k, s) -> (,) k <$> go points s) schs
    SVariant schs -> do
      let !decoders = V.map (\(name, sch) -> let !m = go points sch in (name, m)) schs
      tag <- decodeVarInt
      let (name, dec) = maybe (throw InvalidTag) id $ decoders V.!? tag
      TVariant tag name <$> dec
    SVar i -> indexDefault (throw InvalidTag) points i
    SFix s' -> fix $ \a -> go (a : points) s'
    STag _ s -> go points s
    SLet s t -> go (go points s : points) t

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialiseTerm :: B.ByteString -> Either WineryException (Schema, Term)
deserialiseTerm bs_ = do
  (sch, bs) <- splitSchema bs_
  return (sch, decodeTerm sch `evalDecoder` bs)

-- | This may be thrown if illegal 'Term' is passed to an extractor.
data ExtractException = InvalidTerm !Term deriving Show
instance Exception ExtractException

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
bundleVariant :: (GSerialiseVariant (Rep a), Generic a, Typeable a)
  => (Extractor a -> Extractor a) -- extractor modifier
  -> BundleSerialise a
bundleVariant f = BundleSerialise
  { bundleSchemaGen = gschemaGenVariant
  , bundleToBuilder = gtoBuilderVariant
  , bundleExtractor = f $ gextractorVariant
  , bundleDecodeCurrent = gdecodeCurrentVariant
  }
{-# INLINE bundleVariant #-}

-- | Check the integrity of a Serialise instance.
--
-- /"No tears in the writer, no tears in the reader. No surprise in the writer, no surprise in the reader."/
testSerialise :: forall a. (Eq a, Show a, Serialise a) => a -> QC.Property
testSerialise x = case getDecoderBy extractor (schema (Proxy @ a)) of
  Left e -> QC.counterexample (show e) False
  Right f -> QC.counterexample "extractor" (evalDecoder f b QC.=== x)
    QC..&&. QC.counterexample "decodeCurrent" (evalDecoder decodeCurrent b QC.=== x)
  where
    b = serialiseOnly x

-- | 'decodeCurrent' in terms of 'extractor'; note that it's very slow.
decodeCurrentDefault :: forall a. Serialise a => Decoder a
decodeCurrentDefault = case getDecoderBy extractor (schema (Proxy @ a)) of
  Left err -> error $ "decodeCurrentDefault: failed to get a decoder from the current schema"
    ++ show err
  Right a -> a

-- | Schema generator
newtype SchemaGen a = SchemaGen { unSchemaGen :: S.Set TypeRep -> (S.Set TypeRep, [TypeRep] -> a) }

instance Functor SchemaGen where
  fmap f m = SchemaGen $ \s -> case unSchemaGen m s of
    (rep, k) -> (rep, f . k)

instance Applicative SchemaGen where
  pure a = SchemaGen $ const (S.empty, const a)
  m <*> n = SchemaGen $ \s -> case unSchemaGen m s of
    (rep, f) -> case unSchemaGen n s of
      (rep', g) -> (mappend rep rep', f <*> g)

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

-- | Obtain a decoder from a schema.
--
-- /"A reader lives a thousand lives before he dies... The man who never reads lives only one."/
getDecoder :: forall a. Serialise a => Schema -> Either WineryException (Decoder a)
getDecoder sch
  | sch == schema (Proxy @ a) = Right decodeCurrent
  | otherwise = getDecoderBy extractor sch
{-# INLINE getDecoder #-}

-- | Get a decoder from a `Extractor` and a schema.
getDecoderBy :: Extractor a -> Schema -> Either WineryException (Decoder a)
getDecoderBy (Extractor plan) sch = (\f -> f <$> decodeTerm sch)
  <$> unPlan plan sch `unStrategy` StrategyEnv 0 []
{-# INLINE getDecoderBy #-}

-- | Serialise a value along with its schema.
--
-- /"Write the vision, and make it plain upon tables, that he may run that readeth it."/
serialise :: Serialise a => a -> B.ByteString
serialise = BL.toStrict . BB.toLazyByteString . toBuilderWithSchema
{-# INLINE serialise #-}

-- | 'serialise' then write it to a file.
writeFileSerialise :: Serialise a => FilePath -> a -> IO ()
writeFileSerialise path a = withBinaryFile path WriteMode
  $ \h -> BB.hPutBuilder h $ toBuilderWithSchema a
{-# INLINE writeFileSerialise #-}

-- | Serialise a value with the schema.
toBuilderWithSchema :: forall a. Serialise a => a -> BB.Builder
toBuilderWithSchema a = mappend (BB.word8 currentSchemaVersion)
  $ toBuilder (schema (Proxy @ a), a)
{-# INLINE toBuilderWithSchema #-}

-- | Split a 'Schema' from a 'B.ByteString'.
splitSchema :: B.ByteString -> Either WineryException (Schema, B.ByteString)
splitSchema bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    m <- bootstrapSchema ver >>= getDecoder
    return $ flip evalDecoder bs $ do
      sch <- m
      Decoder $ \bs' i -> DecoderResult (B.length bs') (sch, B.drop i bs')
  Nothing -> Left EmptyInput

-- | Serialise a schema.
serialiseSchema :: Schema -> B.ByteString
serialiseSchema = BL.toStrict . BB.toLazyByteString
  . mappend (BB.word8 currentSchemaVersion) . toBuilder

-- | Deserialise a 'serialise'd 'B.Bytestring'.
--
-- /"Old wood to burn! Old wine to drink! Old friends to trust! Old authors to read!"/
deserialise :: Serialise a => B.ByteString -> Either WineryException a
deserialise bs_ = do
  (sch, bs) <- splitSchema bs_
  dec <- getDecoder sch
  return $ evalDecoder dec bs
{-# INLINE deserialise #-}

-- | Deserialise a 'serialise'd 'B.Bytestring' using an 'Extractor'.
deserialiseBy :: Extractor a -> B.ByteString -> Either WineryException a
deserialiseBy e bs_ = do
  (sch, bs) <- splitSchema bs_
  dec <- getDecoderBy e sch
  return $ evalDecoder dec bs

-- | Deserialise a schema.
deserialiseSchema :: B.ByteString -> Either WineryException Schema
deserialiseSchema bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    m <- bootstrapSchema ver >>= getDecoder
    return $ evalDecoder m bs
  Nothing -> Left EmptyInput

-- | Serialise a value without its schema.
--
-- /"Any unsaved progress will be lost."/
serialiseOnly :: Serialise a => a -> B.ByteString
serialiseOnly = BL.toStrict . BB.toLazyByteString . toBuilder
{-# INLINE serialiseOnly #-}

unexpectedSchema :: forall f a. Serialise a => Doc AnsiStyle -> Schema -> Strategy' (f a)
unexpectedSchema subject actual = throwStrategy
  $ UnexpectedSchema subject (pretty $ schema (Proxy @ a)) actual

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
  schemaGen _ = pure $ SProduct []
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
  decodeCurrent = decodeCurrentDefault

-- | Build an extractor from a 'Subextractor'.
buildExtractor :: Typeable a => Subextractor a -> Extractor a
buildExtractor (Subextractor e) = Extractor $ mkPlan $ unwrapExtractor e
{-# INLINE buildExtractor #-}

-- | An extractor for individual fields. This distinction is required for
-- handling recursions correctly.
--
-- Recommended extension: ApplicativeDo
newtype Subextractor a = Subextractor { unSubextractor :: Extractor a }
  deriving (Functor, Applicative, Alternative)

-- | Extract a field of a record.
extractField :: Serialise a => T.Text -> Subextractor a
extractField = extractFieldBy extractor
{-# INLINE extractField #-}

-- | Extract a field using the supplied 'Extractor'.
extractFieldBy :: Extractor a -> T.Text -> Subextractor a
extractFieldBy (Extractor g) name = Subextractor $ Extractor $ Plan $ \case
  SRecord schs -> case lookupWithIndexV name schs of
    Just (i, sch) -> do
      m <- unPlan g sch
      return $ \case
        TRecord xs -> maybe (error msg) (m . snd) $ xs V.!? i
        t -> throw $ InvalidTerm t
    _ -> throwStrategy $ FieldNotFound rep name (map fst $ V.toList schs)
  s -> throwStrategy $ UnexpectedSchema rep "a record" s
  where
    rep = "extractFieldBy ... " <> dquotes (pretty name)
    msg = "Codec.Winery.extractFieldBy ... " <> show name <> ": impossible"

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

-- | Tries to extract a specific constructor of a variant. Useful for
-- implementing backward-compatible extractors.
extractConstructorBy :: Typeable a => (Extractor a, T.Text, a -> r) -> Subextractor r -> Subextractor r
extractConstructorBy (d, name, f) cont = Subextractor $ Extractor $ Plan $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    let run :: Extractor x -> Schema -> Either WineryException (Term -> x)
        run e s = unwrapExtractor e s `unStrategy` decs
    case lookupWithIndexV name schs0 of
      Just (i, s) -> do
        (j, dec) <- fmap ((,) i) $ run d $ case s of
          SProduct [s'] -> s'
          s' -> s'
        let rest = SVariant $ V.filter ((/=name) . fst) schs0
        k <- run (unSubextractor cont) rest
        return $ \case
          TVariant tag _ v
            | tag == j -> f $ dec v
          t -> k t
      _ -> run (unSubextractor cont) (SVariant schs0)
  s -> throwStrategy $ UnexpectedSchema rep "a variant" s
  where
    rep = "extractConstructorBy ... " <> dquotes (pretty name)

-- | Tries to match on a constructor. If it doesn't match (or constructor
-- doesn't exist at all), leave it to the successor.
--
-- @extractor = ("Just", Just) `extractConstructor` ("Nothing", \() -> Nothing) `extractConstructor` extractVoid@
extractConstructor :: (Serialise a) => (T.Text, a -> r) -> Subextractor r -> Subextractor r
extractConstructor (name, f) = extractConstructorBy (extractor, name, f)
{-# INLINE extractConstructor #-}

-- | No constructors remaining.
extractVoid :: Typeable r => Subextractor r
extractVoid = Subextractor $ Extractor $ mkPlan $ \case
  SVariant schs0
    | V.null schs0 -> return $ throw . InvalidTerm
  s -> throwStrategy $ UnexpectedSchema "extractVoid" "no constructors" s

infixr 1 `extractConstructorBy`
infixr 1 `extractConstructor`

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
  $ fmap (fmap (to .)) $ extractorRecord'
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

-- | The 'Serialise' instance is generically defined for records.
--
-- /"Remember thee! Yea, from the table of my memory I'll wipe away all trivial fond records."/
newtype WineryRecord a = WineryRecord { unWineryRecord :: a }

instance (GEncodeProduct (Rep a), GSerialiseRecord (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a) => Serialise (WineryRecord a) where
  schemaGen _ = gschemaGenRecord (Proxy @ a)
  toBuilder = gtoBuilderRecord . unWineryRecord
  extractor = WineryRecord <$> gextractorRecord Nothing
  decodeCurrent = WineryRecord <$> gdecodeCurrentRecord

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

-- | Serialise a value as a product (omits field names).
--
-- /"I get ideas about what's essential when packing my suitcase."/
newtype WineryProduct a = WineryProduct { unWineryProduct :: a }

instance (GEncodeProduct (Rep a), GSerialiseProduct (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a) => Serialise (WineryProduct a) where
  schemaGen _ = gschemaGenProduct (Proxy @ a)
  toBuilder = gtoBuilderProduct . unWineryProduct
  extractor = WineryProduct <$> gextractorProduct
  decodeCurrent = WineryProduct <$> gdecodeCurrentProduct

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

-- | The 'Serialise' instance is generically defined for variants.
--
-- /"The one so like the other as could not be distinguish'd but by names."/
newtype WineryVariant a = WineryVariant { unWineryVariant :: a }

instance (GSerialiseVariant (Rep a), Generic a, Typeable a) => Serialise (WineryVariant a) where
  schemaGen _ = gschemaGenVariant (Proxy @ a)
  toBuilder = gtoBuilderVariant . unWineryVariant
  extractor = WineryVariant <$> gextractorVariant
  decodeCurrent = WineryVariant <$> gdecodeCurrentVariant

-- | Generic implementation of 'schemaGen' for an ADT.
gschemaGenVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> SchemaGen Schema
gschemaGenVariant _ = SVariant . V.fromList <$> variantSchema (Proxy @ (Rep a))

-- | Generic implementation of 'toBuilder' for an ADT.
gtoBuilderVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> BB.Builder
gtoBuilderVariant = variantEncoder 0 . from
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

gdecodeCurrentVariant :: (GSerialiseVariant (Rep a), Generic a) => Decoder a
gdecodeCurrentVariant = decodeVarInt >>= maybe (throw InvalidTag) (fmap to) . (decs V.!?)
  where
    decs = V.fromList variantDecoder

class GSerialiseVariant f where
  variantCount :: proxy f -> Int
  variantSchema :: proxy f -> SchemaGen [(T.Text, Schema)]
  variantEncoder :: Int -> f x -> BB.Builder
  variantExtractor :: [(T.Text, Schema -> Strategy' (Term -> f x))]
  variantDecoder :: [Decoder (f x)]

instance (GSerialiseVariant f, GSerialiseVariant g) => GSerialiseVariant (f :+: g) where
  variantCount _ = variantCount (Proxy @ f) + variantCount (Proxy @ g)
  variantSchema _ = (++) <$> variantSchema (Proxy @ f) <*> variantSchema (Proxy @ g)
  variantEncoder i (L1 f) = variantEncoder i f
  variantEncoder i (R1 g) = variantEncoder (i + variantCount (Proxy @ f)) g
  variantExtractor = fmap (fmap (fmap (fmap (fmap L1)))) variantExtractor
    ++ fmap (fmap (fmap (fmap (fmap R1)))) variantExtractor
  variantDecoder = fmap (fmap L1) variantDecoder ++ fmap (fmap R1) variantDecoder

instance (GSerialiseProduct f, GEncodeProduct f, GDecodeProduct f, KnownSymbol name) => GSerialiseVariant (C1 ('MetaCons name fixity 'False) f) where
  variantCount _ = 1
  variantSchema _ = do
    s <- productSchema (Proxy @ f)
    return [(T.pack $ symbolVal (Proxy @ name), SProduct $ V.fromList s)]
  variantEncoder i (M1 a) = varInt i <> productEncoder a
  variantExtractor = [(T.pack $ symbolVal (Proxy @ name), fmap (fmap M1) . extractorProduct') ]
  variantDecoder = [M1 <$> productDecoder]
  {-# INLINE variantDecoder #-}

instance (GSerialiseRecord f, GEncodeProduct f, GDecodeProduct f, KnownSymbol name) => GSerialiseVariant (C1 ('MetaCons name fixity 'True) f) where
  variantCount _ = 1
  variantSchema _ = do
    s <- recordSchema (Proxy @ f)
    return [(T.pack $ symbolVal (Proxy @ name), SRecord $ V.fromList s)]
  variantEncoder i (M1 a) = varInt i <> productEncoder a
  variantExtractor = [(T.pack $ symbolVal (Proxy @ name), fmap (fmap M1) . extractorRecord' "" Nothing) ]
  variantDecoder = [M1 <$> productDecoder]

instance (GSerialiseVariant f) => GSerialiseVariant (S1 c f) where
  variantCount _ = variantCount (Proxy @ f)
  variantSchema _ = variantSchema (Proxy @ f)
  variantEncoder i (M1 a) = variantEncoder i a
  variantExtractor = fmap (fmap (fmap (fmap M1))) <$> variantExtractor
  variantDecoder = fmap M1 <$> variantDecoder

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantCount _ = variantCount (Proxy @ f)
  variantSchema _ = variantSchema (Proxy @ f)
  variantEncoder i (M1 a) = variantEncoder i a
  variantExtractor = fmap (fmap (fmap (fmap M1))) <$> variantExtractor
  variantDecoder = fmap M1 <$> variantDecoder

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
  schemaGen _ = pure $ SProduct []
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
