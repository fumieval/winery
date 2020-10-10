{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , readFileDeserialise
  -- * Separate serialisation
  , serialiseSchema
  , schemaToBuilder
  , deserialiseSchema
  , Extractor(..)
  , mkExtractor
  , unwrapExtractor
  , Decoder
  , evalDecoder
  , serialiseOnly
  , getDecoder
  , getDecoderBy
  -- * Decoding combinators
  , Term(..)
  , encodeTerm
  , Subextractor(..)
  , buildExtractor
  , extractListBy
  , extractField
  , extractFieldBy
  , extractConstructor
  , extractConstructorBy
  , extractProductItemBy
  , extractVoid
  , buildVariantExtractor
  , ExtractException(..)
  , SingleField(..)
  -- * Variable-length quantity
  , VarInt(..)
  -- * Internal
  , WineryException(..)
  , prettyWineryException
  , unexpectedSchema
  , SchemaGen
  , getSchema
  -- * DerivingVia
  , WineryRecord(..)
  , WineryVariant(..)
  , WineryProduct(..)
  -- * Generic implementations (for old GHC / custom instances)
  , GSerialiseRecord
  , gschemaGenRecord
  , gtoBuilderRecord
  , gextractorRecord
  , gdecodeCurrentRecord
  , GSerialiseVariant
  , GConstructorCount
  , GEncodeVariant
  , GDecodeVariant
  , gschemaGenVariant
  , gtoBuilderVariant
  , gextractorVariant
  , gdecodeCurrentVariant
  , gvariantExtractors
  , GEncodeProduct
  , GDecodeProduct
  , gschemaGenProduct
  , gtoBuilderProduct
  , gextractorProduct
  , gdecodeCurrentProduct
  , decodeCurrentDefault
  -- * Bundles
  , BundleSerialise(..)
  , bundleRecord
  , bundleRecordDefault
  , bundleVariant
  , bundleVia
  -- * Preset schema
  , bootstrapSchema
  ) where

import Codec.Winery.Base as W
import Codec.Winery.Class
import Codec.Winery.Internal
import Control.Applicative
import Control.Exception (throw, throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.FastBuilder as BB
import Data.Coerce
import Data.Function (fix)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector as V
import GHC.Generics (Generic, Rep)
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

encodeTerm :: Term -> BB.Builder
encodeTerm = \case
  TBool b -> toBuilder b
  TChar x -> toBuilder x
  TWord8 x -> toBuilder x
  TWord16 x -> toBuilder x
  TWord32 x -> toBuilder x
  TWord64 x -> toBuilder x
  TInt8 x -> toBuilder x
  TInt16 x -> toBuilder x
  TInt32 x -> toBuilder x
  TInt64 x -> toBuilder x
  TInteger x -> toBuilder x
  TFloat x -> toBuilder x
  TDouble x -> toBuilder x
  TBytes x -> toBuilder x
  TText x -> toBuilder x
  TUTCTime x -> toBuilder x
  TVector xs -> foldMap encodeTerm xs
  TProduct xs -> foldMap encodeTerm xs
  TRecord xs -> foldMap (encodeTerm . snd) xs
  TVariant tag _ t -> toBuilder tag <> encodeTerm t

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialiseTerm :: B.ByteString -> Either WineryException (Schema, Term)
deserialiseTerm bs_ = do
  (sch, bs) <- splitSchema bs_
  return (sch, decodeTerm sch `evalDecoder` bs)

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
  <$> plan sch `unStrategy` StrategyEnv 0 []
{-# INLINE getDecoderBy #-}

-- | Serialise a value along with its schema.
--
-- /"Write the vision, and make it plain upon tables, that he may run that readeth it."/
serialise :: Serialise a => a -> B.ByteString
serialise = BB.toStrictByteString . toBuilderWithSchema
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

-- | Serialise a schema (prefix with the version number only).
serialiseSchema :: Schema -> B.ByteString
serialiseSchema = BB.toStrictByteString . schemaToBuilder

schemaToBuilder :: Schema -> BB.Builder
schemaToBuilder = mappend (BB.word8 currentSchemaVersion) . toBuilder

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

-- | Deserialise a file. Throws 'WineryException'
readFileDeserialise :: Serialise a => FilePath -> IO a
readFileDeserialise path = B.readFile path >>= either throwIO pure . deserialise

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
serialiseOnly = BB.toStrictByteString . toBuilder
{-# INLINE serialiseOnly #-}

-- | Build an extractor from a 'Subextractor'.
buildExtractor :: Typeable a => Subextractor a -> Extractor a
buildExtractor (Subextractor e) = mkExtractor $ runExtractor e
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
extractFieldBy (Extractor g) name = Subextractor $ Extractor $ \case
  SRecord schs -> case lookupWithIndexV name schs of
    Just (i, sch) -> do
      m <- g sch
      return $ \case
        TRecord xs -> maybe (throw $ InvalidTerm (TRecord xs)) (m . snd) $ xs V.!? i
        t -> throw $ InvalidTerm t
    _ -> throwStrategy $ FieldNotFound [] name (map fst $ V.toList schs)
  s -> throwStrategy $ UnexpectedSchema [] "a record" s

-- | Extract a field using the supplied 'Extractor'.
extractProductItemBy :: Extractor a -> Int -> Subextractor a
extractProductItemBy (Extractor g) i = Subextractor $ Extractor $ \case
  SProduct schs -> case schs V.!? i of
    Just sch -> do
      m <- g sch
      return $ \case
        t@(TProduct xs) -> maybe (throw $ InvalidTerm t) m $ xs V.!? i
        t -> throw $ InvalidTerm t
    _ -> throwStrategy $ ProductTooSmall [] i
  s -> throwStrategy $ UnexpectedSchema [] "a record" s

-- | Tries to extract a specific constructor of a variant. Useful for
-- implementing backward-compatible extractors.
extractConstructorBy :: Typeable a => (Extractor a, T.Text, a -> r) -> Subextractor r -> Subextractor r
extractConstructorBy (d, name, f) cont = Subextractor $ Extractor $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    let run :: Extractor x -> Schema -> Either WineryException (Term -> x)
        run e s = runExtractor e s `unStrategy` decs
    case lookupWithIndexV name schs0 of
      Just (i, s) -> do
        dec <- case s of
          -- Unwrap single-field constructor
          SProduct [s'] -> do
            dec <- runExtractor d s' `unStrategy` decs
            pure $ \case
              TProduct [v] -> dec v
              t -> throw $ InvalidTerm t
          _ -> runExtractor d s `unStrategy` decs
        let rest = SVariant $ V.filter ((/=name) . fst) schs0
        k <- run (unSubextractor cont) rest
        return $ \case
          TVariant tag name' v
            | tag == i -> f $ dec v
            -- rest has fewer constructors
            | tag > i -> k (TVariant (tag - 1) name' v)
          t -> k t
      _ -> run (unSubextractor cont) (SVariant schs0)
  s -> throwStrategy $ UnexpectedSchema [] "a variant" s

-- | Tries to match on a constructor. If it doesn't match (or constructor
-- doesn't exist at all), leave it to the successor.
--
-- @extractor = ("Just", Just) `extractConstructor` ("Nothing", \() -> Nothing) `extractConstructor` extractVoid@
extractConstructor :: (Serialise a) => (T.Text, a -> r) -> Subextractor r -> Subextractor r
extractConstructor (name, f) = extractConstructorBy (extractor, name, f)
{-# INLINE extractConstructor #-}

-- | No constructors remaining.
extractVoid :: Typeable r => Subextractor r
extractVoid = Subextractor $ mkExtractor $ \case
  SVariant schs0
    | V.null schs0 -> return $ throw . InvalidTerm
  s -> throwStrategy $ UnexpectedSchema [] "no constructors" s

infixr 1 `extractConstructorBy`
infixr 1 `extractConstructor`

-- | The 'Serialise' instance is generically defined for records.
--
-- /"Remember thee! Yea, from the table of my memory I'll wipe away all trivial fond records."/
newtype WineryRecord a = WineryRecord { unWineryRecord :: a }

instance (GEncodeProduct (Rep a), GSerialiseRecord (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a) => Serialise (WineryRecord a) where
  schemaGen _ = gschemaGenRecord (Proxy @ a)
  toBuilder = gtoBuilderRecord . unWineryRecord
  extractor = WineryRecord <$> gextractorRecord Nothing
  decodeCurrent = WineryRecord <$> gdecodeCurrentRecord
  {-# INLINE toBuilder #-}
  {-# INLINE decodeCurrent #-}

-- | Serialise a value as a product (omits field names).
--
-- /"I get ideas about what's essential when packing my suitcase."/
newtype WineryProduct a = WineryProduct { unWineryProduct :: a }

instance (GEncodeProduct (Rep a), GSerialiseProduct (Rep a), GDecodeProduct (Rep a), Generic a, Typeable a) => Serialise (WineryProduct a) where
  schemaGen _ = gschemaGenProduct (Proxy @ a)
  toBuilder = gtoBuilderProduct . unWineryProduct
  extractor = WineryProduct <$> gextractorProduct
  decodeCurrent = WineryProduct <$> gdecodeCurrentProduct
  {-# INLINE toBuilder #-}
  {-# INLINE decodeCurrent #-}

-- | The 'Serialise' instance is generically defined for variants.
--
-- /"The one so like the other as could not be distinguish'd but by names."/
newtype WineryVariant a = WineryVariant { unWineryVariant :: a }

instance (GConstructorCount (Rep a), GSerialiseVariant (Rep a), GEncodeVariant (Rep a), GDecodeVariant (Rep a), Generic a, Typeable a) => Serialise (WineryVariant a) where
  schemaGen _ = gschemaGenVariant (Proxy @ a)
  toBuilder = gtoBuilderVariant . unWineryVariant
  extractor = WineryVariant <$> gextractorVariant
  decodeCurrent = WineryVariant <$> gdecodeCurrentVariant
  {-# INLINE toBuilder #-}
  {-# INLINE decodeCurrent #-}

-- | A product with one field. Useful when creating a custom extractor for constructors.
newtype SingleField a = SingleField { getSingleField :: a }
  deriving (Show, Eq, Ord, Generic)

instance Serialise a => Serialise (SingleField a) where
  schemaGen = gschemaGenProduct
  toBuilder = gtoBuilderProduct
  extractor = gextractorProduct
  decodeCurrent = gdecodeCurrentProduct
  {-# INLINE toBuilder #-}
  {-# INLINE decodeCurrent #-}

bundleVia :: forall a t. (Coercible a t, Serialise t) => (a -> t) -> BundleSerialise a
bundleVia _ = BundleSerialise
  { bundleSchemaGen = coerce (schemaGen @t)
  , bundleToBuilder = coerce (toBuilder @t)
  , bundleExtractor = coerce (extractor @t)
  , bundleDecodeCurrent = coerce (decodeCurrent @t)
  }
{-# INLINE bundleVia #-}