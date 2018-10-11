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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Winery
  ( Schema(..)
  , Serialise(..)
  , DecodeException(..)
  , schema
  -- * Standalone serialisation
  , toEncodingWithSchema
  , serialise
  , deserialise
  , deserialiseBy
  , deserialiseTerm
  , splitSchema
  , writeFileSerialise
  -- * Separate serialisation
  , Extractor(..)
  , Decoder
  , serialiseOnly
  , getDecoder
  , getDecoderBy
  , decodeCurrent
  -- * Encoding combinators
  , Encoding
  , BB.getSize
  , BB.toByteString
  , BB.hPutEncoding
  -- * Decoding combinators
  , Plan(..)
  , extractListBy
  , extractField
  , extractFieldBy
  , extractConstructor
  , extractConstructorBy
  -- * Variable-length quantity
  , VarInt(..)
  -- * Internal
  , unwrapExtractor
  , Strategy
  , StrategyError
  , unexpectedSchema
  , unexpectedSchema'
  -- * Generics
  , GSerialiseRecord
  , gschemaViaRecord
  , GEncodeRecord
  , gtoEncodingRecord
  , gextractorRecord
  , GSerialiseVariant
  , gschemaViaVariant
  , gtoEncodingVariant
  , gextractorVariant
  -- * Preset schema
  , bootstrapSchema
  )where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.State.Strict
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Winery.Internal.Builder as BB
import qualified Data.Aeson as J
import Data.Bits
import Data.Dynamic
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Foldable
import Data.Proxy
import Data.Scientific (Scientific, scientific, coefficient, base10Exponent)
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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Data.Text.Prettyprint.Doc hiding ((<>), SText, SChar)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable
import GHC.Generics
import System.IO
import Unsafe.Coerce

data Schema = SFix Schema -- ^ binds a fixpoint
  | SSelf !Word8 -- ^ @SSelf n@ refers to the n-th innermost fixpoint
  | SVector !Schema
  | SProduct [Schema]
  | SRecord [(T.Text, Schema)]
  | SVariant [(T.Text, Schema)]
  | SSchema !Word8
  | SBool
  | SChar
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
  | SUTCTime
  deriving (Show, Read, Eq, Generic)

instance Pretty Schema where
  pretty = \case
    SSchema v -> "Schema " <> pretty v
    SProduct [] -> "()"
    SBool -> "Bool"
    SChar -> "Char"
    SWord8 -> "Word8"
    SWord16 -> "Word16"
    SWord32 -> "Word32"
    SWord64 -> "Word64"
    SInt8 -> "Int8"
    SInt16 -> "Int16"
    SInt32 -> "Int32"
    SInt64 -> "Int64"
    SInteger -> "Integer"
    SFloat -> "Float"
    SDouble -> "Double"
    SBytes -> "ByteString"
    SText -> "Text"
    SUTCTime -> "UTCTime"
    SVector s -> "[" <> pretty s <> "]"
    SProduct ss -> tupled $ map pretty ss
    SRecord ss -> align $ encloseSep "{ " " }" ", " [pretty k <+> "::" <+> pretty v | (k, v) <- ss]
    SVariant ss -> align $ encloseSep "( " " )" (flatAlt "| " " | ")
      [ nest 2 $ sep [pretty k, pretty vs] | (k, vs) <- ss]
    SFix sch -> group $ nest 2 $ sep ["μ", pretty sch]
    SSelf i -> "Self" <+> pretty i

-- | Common representation for any winery data.
-- Handy for prettyprinting winery-serialised data.
data Term = TBool !Bool
  | TChar !Char
  | TWord8 !Word8
  | TWord16 !Word16
  | TWord32 !Word32
  | TWord64 !Word64
  | TInt8 !Int8
  | TInt16 !Int16
  | TInt32 !Int32
  | TInt64 !Int64
  | TInteger !Integer
  | TFloat !Float
  | TDouble !Double
  | TBytes !B.ByteString
  | TText !T.Text
  | TUTCTime !UTCTime
  | TVector (V.Vector Term)
  | TProduct [Term]
  | TRecord (V.Vector (T.Text, Term))
  | TVariant !Int !T.Text Term
  deriving Show

data ExtractException = InvalidTerm !Term deriving Show
instance Exception ExtractException

instance J.ToJSON Term where
  toJSON (TBool b) = J.toJSON b
  toJSON (TChar c) = J.toJSON c
  toJSON (TWord8 w) = J.toJSON w
  toJSON (TWord16 w) = J.toJSON w
  toJSON (TWord32 w) = J.toJSON w
  toJSON (TWord64 w) = J.toJSON w
  toJSON (TInt8 w) = J.toJSON w
  toJSON (TInt16 w) = J.toJSON w
  toJSON (TInt32 w) = J.toJSON w
  toJSON (TInt64 w) = J.toJSON w
  toJSON (TInteger w) = J.toJSON w
  toJSON (TFloat x) = J.toJSON x
  toJSON (TDouble x) = J.toJSON x
  toJSON (TBytes bs) = J.toJSON (B.unpack bs)
  toJSON (TText t) = J.toJSON t
  toJSON (TUTCTime t) = J.toJSON t
  toJSON (TVector xs) = J.toJSON xs
  toJSON (TProduct xs) = J.toJSON xs
  toJSON (TRecord xs) = J.toJSON $ HM.fromList $ V.toList xs
  toJSON (TVariant _ "Just" x) = J.toJSON x
  toJSON (TVariant _ "Nothing" _) = J.Null
  toJSON (TVariant _ t x) = J.object ["tag" J..= J.toJSON t, "contents" J..= J.toJSON x]

-- | Deserialiser for a 'Term'.
decodeTerm :: Schema -> Decoder Term
decodeTerm = go [] where
  go points = \case
    SSchema ver -> go points (bootstrapSchema ver)
    SBool -> TBool . (/=0) <$> getWord8
    Data.Winery.SChar -> TChar . toEnum <$> decodeVarInt
    SWord8 -> TWord8 <$> getWord8
    SWord16 -> TWord16 <$> getWord16
    SWord32 -> TWord32 <$> getWord32
    SWord64 -> TWord64 <$> getWord64
    SInt8 -> TInt8 . fromIntegral <$> getWord8
    SInt16 -> TInt16 . fromIntegral <$> getWord16
    SInt32 -> TInt32 . fromIntegral <$> getWord32
    SInt64 -> TInt64 . fromIntegral <$> getWord64
    SInteger -> TInteger <$> decodeVarInt
    SFloat -> TFloat <$> unsafeCoerce getWord32
    SDouble -> TDouble <$> unsafeCoerce getWord64
    SBytes -> do
      len <- decodeVarInt
      TBytes <$> state (B.splitAt len)
    Data.Winery.SText -> do
      len <- decodeVarInt
      TText . T.decodeUtf8 <$> state (B.splitAt len)
    SUTCTime -> TUTCTime . posixSecondsToUTCTime <$> unsafeCoerce getWord64
    SVector sch -> do
      n <- decodeVarInt
      TVector <$> V.replicateM n (go points sch)
    SProduct schs -> TProduct <$> traverse (go points) schs
    SRecord schs -> TRecord <$> traverse (\(k, s) -> (,) k <$> go points s) (V.fromList schs)
    SVariant schs -> do
      let decoders = map (\(name, sch) -> (name, go points sch)) schs
      tag <- decodeVarInt
      let (name, dec) = unsafeIndex (throw InvalidTag) decoders tag
      TVariant tag name <$> dec
    SSelf i -> unsafeIndex (throw InvalidTag) points $ fromIntegral i
    SFix s' -> fix $ \a -> go (a : points) s'

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialiseTerm :: B.ByteString -> Either (Doc AnsiStyle) (Schema, Term)
deserialiseTerm bs_ = do
  (sch, bs) <- splitSchema bs_
  return (sch, decodeTerm sch `evalState` bs)

instance Pretty Term where
  pretty (TWord8 i) = pretty i
  pretty (TWord16 i) = pretty i
  pretty (TWord32 i) = pretty i
  pretty (TWord64 i) = pretty i
  pretty (TInt8 i) = pretty i
  pretty (TInt16 i) = pretty i
  pretty (TInt32 i) = pretty i
  pretty (TInt64 i) = pretty i
  pretty (TInteger i) = pretty i
  pretty (TBytes s) = pretty $ show s
  pretty (TText s) = pretty s
  pretty (TVector xs) = list $ map pretty (V.toList xs)
  pretty (TBool x) = pretty x
  pretty (TChar x) = pretty x
  pretty (TFloat x) = pretty x
  pretty (TDouble x) = pretty x
  pretty (TProduct xs) = tupled $ map pretty xs
  pretty (TRecord xs) = align $ encloseSep "{ " " }" ", " [group $ nest 2 $ vsep [pretty k <+> "=", pretty v] | (k, v) <- V.toList xs]
  pretty (TVariant _ tag x) = group $ nest 2 $ sep [pretty tag, pretty x]
  pretty (TUTCTime t) = pretty (show t)

-- | 'Extractor' is a 'Plan' that creates a function from Term.
newtype Extractor a = Extractor { getExtractor :: Plan (Term -> a) }
  deriving Functor

instance Applicative Extractor where
  pure = Extractor . pure . pure
  Extractor f <*> Extractor x = Extractor $ (<*>) <$> f <*> x

instance Alternative Extractor where
  empty = Extractor empty
  Extractor f <|> Extractor g = Extractor $ f <|> g

type Strategy' = Strategy (Term -> Dynamic)

newtype Plan a = Plan { unPlan :: Schema -> Strategy' a }
  deriving Functor

instance Applicative Plan where
  pure = return
  (<*>) = ap

instance Monad Plan where
  return = Plan . const . pure
  m >>= k = Plan $ \sch -> Strategy $ \decs -> case unStrategy (unPlan m sch) decs of
    Right a -> unStrategy (unPlan (k a) sch) decs
    Left e -> Left e

instance Alternative Plan where
  empty = Plan $ const empty
  Plan a <|> Plan b = Plan $ \s -> a s <|> b s

unwrapExtractor :: Extractor a -> Schema -> Strategy' (Term -> a)
unwrapExtractor (Extractor m) = unPlan m
{-# INLINE unwrapExtractor #-}

-- | Serialisable datatype
class Typeable a => Serialise a where
  -- | Obtain the schema of the datatype. @[TypeRep]@ is for handling recursion.
  schemaVia :: Proxy a -> [TypeRep] -> Schema

  -- | Serialise a value.
  toEncoding :: a -> Encoding

  -- | The 'Extractor'
  extractor :: Extractor a

  default schemaVia :: (Generic a, GSerialiseVariant (Rep a)) => Proxy a -> [TypeRep] -> Schema
  schemaVia = gschemaViaVariant
  default toEncoding :: (Generic a, GSerialiseVariant (Rep a)) => a -> Encoding
  toEncoding = gtoEncodingVariant
  default extractor :: (Generic a, GSerialiseVariant (Rep a)) => Extractor a
  extractor = gextractorVariant

-- | Obtain the schema of the datatype.
schema :: forall proxy a. Serialise a => proxy a -> Schema
schema _ = schemaVia (Proxy :: Proxy a) []
{-# INLINE schema #-}

-- | Obtain a decoder from a schema.
getDecoder :: Serialise a => Schema -> Either StrategyError (Decoder a)
getDecoder = getDecoderBy extractor
{-# INLINE getDecoder #-}

-- | Get a decoder from a `Extractor` and a schema.
getDecoderBy :: Extractor a -> Schema -> Either StrategyError (Decoder a)
getDecoderBy (Extractor plan) sch = (\f -> f <$> decodeTerm sch)
  <$> unPlan plan sch `unStrategy` []
{-# INLINE getDecoderBy #-}

-- | Decode a value with the current schema.
decodeCurrent :: forall a. Serialise a => Decoder a
decodeCurrent = case getDecoder (schema (Proxy :: Proxy a)) of
  Left err -> error $ show $ "decodeCurrent: failed to get a decoder from the current schema"
    <+> parens err
  Right a -> a

-- | Serialise a value along with its schema.
serialise :: Serialise a => a -> B.ByteString
serialise a = BB.toByteString $ toEncodingWithSchema a
{-# INLINE serialise #-}

-- | Serialise a value along with its schema.
writeFileSerialise :: Serialise a => FilePath -> a -> IO ()
writeFileSerialise path a = withFile path WriteMode
  $ \h -> BB.hPutEncoding h $ toEncodingWithSchema a
{-# INLINE writeFileSerialise #-}

toEncodingWithSchema :: Serialise a => a -> Encoding
toEncodingWithSchema a = mappend (BB.word8 currentSchemaVersion)
  $ toEncoding (schema [a], a)
{-# INLINE toEncodingWithSchema #-}

splitSchema :: B.ByteString -> Either StrategyError (Schema, B.ByteString)
splitSchema bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    m <- getDecoder $ SSchema ver
    return $ flip evalState bs $ do
      sch <- m
      state $ \bs' -> ((sch, bs'), mempty)
  Nothing -> Left "Unexpected empty string"

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialise :: Serialise a => B.ByteString -> Either StrategyError a
deserialise = deserialiseBy extractor
{-# INLINE deserialise #-}

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialiseBy :: Extractor a -> B.ByteString -> Either StrategyError a
deserialiseBy d bs_ = do
  (sch, bs) <- splitSchema bs_
  ext <- d `unwrapExtractor` sch `unStrategy` []
  return $ ext $ evalState (decodeTerm sch) bs

-- | Serialise a value without its schema.
serialiseOnly :: Serialise a => a -> B.ByteString
serialiseOnly = BB.toByteString . toEncoding
{-# INLINE serialiseOnly #-}

substSchema :: Serialise a => Proxy a -> [TypeRep] -> Schema
substSchema p ts
  | Just i <- elemIndex (typeRep p) ts = SSelf $ fromIntegral i
  | otherwise = schemaVia p ts

currentSchemaVersion :: Word8
currentSchemaVersion = 3

bootstrapSchema :: Word8 -> Schema
bootstrapSchema 3 = SFix $ SVariant [("SFix",SProduct [SSelf 0])
  ,("SSelf",SProduct [SWord8])
  ,("SVector",SProduct [SSelf 0])
  ,("SProduct",SProduct [SVector (SSelf 0)])
  ,("SRecord",SProduct [SVector (SProduct [SText,SSelf 0])])
  ,("SVariant",SProduct [SVector (SProduct [SText,SSelf 0])])
  ,("SSchema",SProduct [SWord8])
  ,("SBool",SProduct [])
  ,("SChar",SProduct [])
  ,("SWord8",SProduct [])
  ,("SWord16",SProduct [])
  ,("SWord32",SProduct [])
  ,("SWord64",SProduct [])
  ,("SInt8",SProduct [])
  ,("SInt16",SProduct [])
  ,("SInt32",SProduct [])
  ,("SInt64",SProduct [])
  ,("SInteger",SProduct [])
  ,("SFloat",SProduct [])
  ,("SDouble",SProduct [])
  ,("SBytes",SProduct [])
  ,("SText",SProduct [])
  ,("SUTCTime",SProduct [])]
bootstrapSchema n = error $ "Unsupported version: " <> show n

unexpectedSchema :: forall f a. Serialise a => Doc AnsiStyle -> Schema -> Strategy' (f a)
unexpectedSchema subject actual = unexpectedSchema' subject
  (pretty $ schema (Proxy :: Proxy a)) actual

unexpectedSchema' :: Doc AnsiStyle -> Doc AnsiStyle -> Schema -> Strategy' a
unexpectedSchema' subject expected actual = errorStrategy
  $ annotate bold subject
  <+> "expects" <+> annotate (color Green <> bold) expected
  <+> "but got " <+> pretty actual

instance Serialise Schema where
  schemaVia _ _ = SSchema currentSchemaVersion
  toEncoding = gtoEncodingVariant
  extractor = Extractor $ Plan $ \case
    SSchema n -> unwrapExtractor gextractorVariant (bootstrapSchema n)
    s -> unwrapExtractor gextractorVariant s

instance Serialise () where
  schemaVia _ _ = SProduct []
  toEncoding = mempty
  extractor = pure ()

instance Serialise Bool where
  schemaVia _ _ = SBool
  toEncoding False = BB.word8 0
  toEncoding True = BB.word8 1
  extractor = Extractor $ Plan $ \case
    SBool -> pure $ \case
      TBool b -> b
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Bool" s

instance Serialise Word8 where
  schemaVia _ _ = SWord8
  toEncoding = BB.word8
  extractor = Extractor $ Plan $ \case
    SWord8 -> pure $ \case
      TWord8 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word8" s

instance Serialise Word16 where
  schemaVia _ _ = SWord16
  toEncoding = BB.word16
  extractor = Extractor $ Plan $ \case
    SWord16 -> pure $ \case
      TWord16 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word16" s

instance Serialise Word32 where
  schemaVia _ _ = SWord32
  toEncoding = BB.word32
  extractor = Extractor $ Plan $ \case
    SWord32 -> pure $ \case
      TWord32 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word32" s

instance Serialise Word64 where
  schemaVia _ _ = SWord64
  toEncoding = BB.word64
  extractor = Extractor $ Plan $ \case
    SWord64 -> pure $ \case
      TWord64 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word64" s

instance Serialise Word where
  schemaVia _ _ = SWord64
  toEncoding = BB.word64 . fromIntegral
  extractor = Extractor $ Plan $ \case
    SWord64 -> pure $ \case
      TWord64 i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Word" s

instance Serialise Int8 where
  schemaVia _ _ = SInt8
  toEncoding = BB.word8 . fromIntegral
  extractor = Extractor $ Plan $ \case
    SInt8 -> pure $ \case
      TInt8 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int8" s

instance Serialise Int16 where
  schemaVia _ _ = SInt16
  toEncoding = BB.word16 . fromIntegral
  extractor = Extractor $ Plan $ \case
    SInt16 -> pure $ \case
      TInt16 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int16" s

instance Serialise Int32 where
  schemaVia _ _ = SInt32
  toEncoding = BB.word32 . fromIntegral
  extractor = Extractor $ Plan $ \case
    SInt32 -> pure $ \case
      TInt32 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int32" s

instance Serialise Int64 where
  schemaVia _ _ = SInt64
  toEncoding = BB.word64 . fromIntegral
  extractor = Extractor $ Plan $ \case
    SInt64 -> pure $ \case
      TInt64 i -> i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int64" s

instance Serialise Int where
  schemaVia _ _ = SInteger
  toEncoding = toEncoding . VarInt
  extractor = Extractor $ Plan $ \case
    SInteger -> pure $ \case
      TInteger i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Int" s

instance Serialise Float where
  schemaVia _ _ = SFloat
  toEncoding = BB.word32 . unsafeCoerce
  extractor = Extractor $ Plan $ \case
    SFloat -> pure $ \case
      TFloat x -> x
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Float" s

instance Serialise Double where
  schemaVia _ _ = SDouble
  toEncoding = BB.word64 . unsafeCoerce
  extractor = Extractor $ Plan $ \case
    SDouble -> pure $ \case
      TDouble x -> x
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Double" s

instance Serialise T.Text where
  schemaVia _ _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  extractor = Extractor $ Plan $ \case
    SText -> pure $ \case
      TText t -> t
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Text" s

-- | Encoded in variable-length quantity.
newtype VarInt a = VarInt { getVarInt :: a } deriving (Show, Read, Eq, Ord, Enum
  , Bounded, Num, Real, Integral, Bits, Typeable)

instance (Typeable a, Bits a, Integral a) => Serialise (VarInt a) where
  schemaVia _ _ = SInteger
  toEncoding = BB.varInt . getVarInt
  {-# INLINE toEncoding #-}
  extractor = Extractor $ Plan $ \case
    SInteger -> pure $ \case
      TInteger i -> fromIntegral i
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (VarInt a)" s

instance Serialise Integer where
  schemaVia _ _ = SInteger
  toEncoding = toEncoding . VarInt
  extractor = getVarInt <$> extractor

instance Serialise Char where
  schemaVia _ _ = SChar
  toEncoding = toEncoding . fromEnum
  extractor = Extractor $ Plan $ \case
    SChar -> pure $ \case
      TChar c -> c
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise Char" s

instance Serialise a => Serialise (Maybe a) where
  schemaVia _ ts = SVariant [("Nothing", SProduct [])
    , ("Just", substSchema (Proxy :: Proxy a) ts)]
  toEncoding Nothing = BB.varInt (0 :: Word8)
  toEncoding (Just a) = BB.varInt (1 :: Word8) <> toEncoding a
  extractor = Extractor $ Plan $ \case
    SVariant [_, (_, sch)] -> do
      dec <- unwrapExtractor extractor sch
      return $ \case
        TVariant 0 _ _ -> Nothing
        TVariant _ _ v -> dec v
        t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (Maybe a)" s

instance Serialise B.ByteString where
  schemaVia _ _ = SBytes
  toEncoding bs = BB.varInt (B.length bs) <> BB.bytes bs
  extractor = Extractor $ Plan $ \case
    SBytes -> pure $ \case
      TBytes bs -> bs
      t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise ByteString" s

instance Serialise BL.ByteString where
  schemaVia _ _ = SBytes
  toEncoding = toEncoding . BL.toStrict
  extractor = BL.fromStrict <$> extractor

instance Serialise UTCTime where
  schemaVia _ _ = SUTCTime
  toEncoding = toEncoding . utcTimeToPOSIXSeconds
  extractor = Extractor $ Plan $ \case
    SUTCTime -> unwrapExtractor
      (posixSecondsToUTCTime <$> extractor)
      (schema (Proxy :: Proxy Double))
    s -> unexpectedSchema "Serialise UTCTime" s

instance Serialise NominalDiffTime where
  schemaVia _ = schemaVia (Proxy :: Proxy Double)
  toEncoding = toEncoding . (realToFrac :: NominalDiffTime -> Double)
  extractor = (realToFrac :: Double -> NominalDiffTime) <$> extractor

instance Serialise a => Serialise [a] where
  schemaVia _ ts = SVector (substSchema (Proxy :: Proxy a) ts)
  toEncoding xs = BB.varInt (length xs)
    <> foldMap toEncoding xs
  extractor = V.toList <$> extractListBy extractor

instance Serialise a => Serialise (V.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding xs = BB.varInt (V.length xs)
    <> foldMap toEncoding xs
  extractor = extractListBy extractor

instance (SV.Storable a, Serialise a) => Serialise (SV.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . (SV.convert :: SV.Vector a -> V.Vector a)
  extractor = SV.convert <$> extractListBy extractor

instance (UV.Unbox a, Serialise a) => Serialise (UV.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . (UV.convert :: UV.Vector a -> V.Vector a)
  extractor = UV.convert <$> extractListBy extractor

-- | Extract a list or an array of values.
extractListBy :: Extractor a -> Extractor (V.Vector a)
extractListBy (Extractor plan) = Extractor $ Plan $ \case
  SVector s -> do
    getItem <- unPlan plan s
    return $ \case
      TVector xs -> V.map getItem xs
      t -> throw $ InvalidTerm t
  s -> unexpectedSchema' "extractListBy ..." "[a]" s
{-# INLINE extractListBy #-}

instance (Ord k, Serialise k, Serialise v) => Serialise (M.Map k v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(k, v)])
  toEncoding = toEncoding . M.toList
  extractor = M.fromList <$> extractor

instance (Eq k, Hashable k, Serialise k, Serialise v) => Serialise (HM.HashMap k v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(k, v)])
  toEncoding = toEncoding . HM.toList
  extractor = HM.fromList <$> extractor

instance (Serialise v) => Serialise (IM.IntMap v) where
  schemaVia _ = schemaVia (Proxy :: Proxy [(Int, v)])
  toEncoding = toEncoding . IM.toList
  extractor = IM.fromList <$> extractor

instance (Ord a, Serialise a) => Serialise (S.Set a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . S.toList
  extractor = S.fromList <$> extractor

instance Serialise IS.IntSet where
  schemaVia _ = schemaVia (Proxy :: Proxy [Int])
  toEncoding = toEncoding . IS.toList
  extractor = IS.fromList <$> extractor

instance Serialise a => Serialise (Seq.Seq a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . toList
  extractor = Seq.fromList <$> extractor

instance Serialise Scientific where
  schemaVia _ = schemaVia (Proxy :: Proxy (Integer, Int))
  toEncoding s = toEncoding (coefficient s, base10Exponent s)
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

-- | Extract a field of a record.
extractField :: Serialise a => T.Text -> Extractor a
extractField = extractFieldBy extractor
{-# INLINE extractField #-}

-- | Extract a field using the supplied 'Extractor'.
extractFieldBy :: Typeable a => Extractor a -> T.Text -> Extractor a
extractFieldBy (Extractor g) name = Extractor $ handleRecursion $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    case lookup name schs' of
      Just (i, sch) -> do
        m <- unPlan g sch
        return $ \case
          TRecord xs -> maybe (error msg) (m . snd) $ xs V.!? i
          t -> throw $ InvalidTerm t
      Nothing -> errorStrategy $ rep <> ": Schema not found in " <> pretty (map fst schs)
  s -> unexpectedSchema' rep "a record" s
  where
    rep = "extractFieldBy ... " <> dquotes (pretty name)
    msg = "Data.Winery.extractFieldBy ... " <> show name <> ": impossible"

handleRecursion :: Typeable a => (Schema -> Strategy' (Term -> a)) -> Plan (Term -> a)
handleRecursion k = Plan $ \sch -> Strategy $ \decs -> case sch of
  SSelf i -> return $ fmap (`fromDyn` throw InvalidTag)
    $ unsafeIndex (error "Data.Winery.handleRecursion: unbound fixpoint") decs (fromIntegral i)
  SFix s -> mfix $ \a -> unPlan (handleRecursion k) s `unStrategy` (fmap toDyn a : decs)
  s -> k s `unStrategy` decs

instance (Serialise a, Serialise b) => Serialise (a, b) where
  schemaVia _ ts = SProduct [substSchema (Proxy :: Proxy a) ts, substSchema (Proxy :: Proxy b) ts]
  toEncoding (a, b) = toEncoding a <> toEncoding b
  extractor = Extractor $ Plan $ \case
    SProduct [sa, sb] -> do
      getA <- unwrapExtractor extractor sa
      getB <- unwrapExtractor extractor sb
      return $ \case
        TProduct [a, b] -> (getA a, getB b)
        t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (a, b)" s

instance (Serialise a, Serialise b, Serialise c) => Serialise (a, b, c) where
  schemaVia _ ts = SProduct [sa, sb, sc]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
      sc = substSchema (Proxy :: Proxy c) ts
  toEncoding (a, b, c) = toEncoding a <> toEncoding b <> toEncoding c
  extractor = Extractor $ Plan $ \case
    SProduct [sa, sb, sc] -> do
      getA <- unwrapExtractor extractor sa
      getB <- unwrapExtractor extractor sb
      getC <- unwrapExtractor extractor sc
      return $ \case
        TProduct [a, b, c] -> (getA a, getB b, getC c)
        t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (a, b, c)" s

instance (Serialise a, Serialise b, Serialise c, Serialise d) => Serialise (a, b, c, d) where
  schemaVia _ ts = SProduct [sa, sb, sc, sd]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
      sc = substSchema (Proxy :: Proxy c) ts
      sd = substSchema (Proxy :: Proxy d) ts
  toEncoding (a, b, c, d) = toEncoding a <> toEncoding b <> toEncoding c <> toEncoding d
  extractor = Extractor $ Plan $ \case
    SProduct [sa, sb, sc, sd] -> do
      getA <- unwrapExtractor extractor sa
      getB <- unwrapExtractor extractor sb
      getC <- unwrapExtractor extractor sc
      getD <- unwrapExtractor extractor sd
      return $ \case
        TProduct [a, b, c, d] -> (getA a, getB b, getC c, getD d)
        t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Serialise (a, b, c, d)" s

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schemaVia _ ts = SVariant [("Left", substSchema (Proxy :: Proxy a) ts)
    , ("Right", substSchema (Proxy :: Proxy b) ts)]
  toEncoding (Left a) = BB.word8 0 <> toEncoding a
  toEncoding (Right b) = BB.word8 1 <> toEncoding b
  extractor = Extractor $ Plan $ \case
    SVariant [(_, sa), (_, sb)] -> do
      getA <- unwrapExtractor extractor sa
      getB <- unwrapExtractor extractor sb
      return $ \case
        TVariant 0 _ v -> Left $ getA v
        TVariant _ _ v -> Right $ getB v
        t -> throw $ InvalidTerm t
    s -> unexpectedSchema "Either (a, b)" s

-- | Tries to extract a specific constructor of a variant. Useful for
-- implementing backward-compatible extractors.
extractConstructorBy :: Typeable a => Extractor a -> T.Text -> Extractor (Maybe a)
extractConstructorBy d name = Extractor $ handleRecursion $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    (j, dec) <- case [(i :: Int, ss) | (i, (k, ss)) <- zip [0..] schs0, name == k] of
      [(i, s)] -> fmap ((,) i) $ unwrapExtractor d s `unStrategy` decs
      _ -> Left $ rep <> ": Schema not found in " <> pretty (map fst schs0)

    return $ \case
      TVariant i _ v
        | i == j -> Just $ dec v
        | otherwise -> Nothing
      t -> throw $ InvalidTerm t
  s -> unexpectedSchema' rep "a variant" s
  where
    rep = "extractConstructorBy ... " <> dquotes (pretty name)

extractConstructor :: (Serialise a) => T.Text -> Extractor (Maybe a)
extractConstructor = extractConstructorBy extractor
{-# INLINE extractConstructor #-}

-- | Generic implementation of 'schemaVia' for a record.
gschemaViaRecord :: forall proxy a. (GSerialiseRecord (Rep a), Generic a, Typeable a) => proxy a -> [TypeRep] -> Schema
gschemaViaRecord p ts = SFix $ SRecord $ recordSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

-- | Generic implementation of 'toEncoding' for a record.
gtoEncodingRecord :: (GEncodeRecord (Rep a), Generic a) => a -> Encoding
gtoEncodingRecord = recordEncoder . from
{-# INLINE gtoEncodingRecord #-}

data FieldDecoder i a = FieldDecoder !i !(Maybe a) !(Plan (Term -> a))

-- | Generic implementation of 'extractor' for a record.
gextractorRecord :: forall a. (GSerialiseRecord (Rep a), Generic a, Typeable a)
  => Maybe a -- ^ default value (optional)
  -> Extractor a
gextractorRecord def = Extractor $ handleRecursion $ \case
  SRecord schs -> Strategy $ \decs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    let go :: FieldDecoder T.Text x -> Either StrategyError (Term -> x)
        go (FieldDecoder name def' p) = case lookup name schs' of
          Nothing -> case def' of
            Just d -> Right (const d)
            Nothing -> Left $ rep <> ": Default value not found for " <> pretty name
          Just (i, sch) -> case p `unPlan` sch `unStrategy` decs of
            Right getItem -> Right $ \case
              TRecord xs -> maybe (error (show rep)) (getItem . snd) $ xs V.!? i
              t -> throw $ InvalidTerm t
            Left e -> Left e
    m <- unTransFusion (recordExtractor $ from <$> def) go
    return (to . m)
  s -> unexpectedSchema' rep "a record" s
  where
    rep = "gextractorRecord :: Extractor "
      <> viaShow (typeRep (Proxy :: Proxy a))
{-# INLINE gextractorRecord #-}

class GEncodeRecord f where
  recordEncoder :: f x -> Encoding

instance (GEncodeRecord f, GEncodeRecord g) => GEncodeRecord (f :*: g) where
  recordEncoder (f :*: g) = recordEncoder f <> recordEncoder g
  {-# INLINE recordEncoder #-}

instance Serialise a => GEncodeRecord (S1 c (K1 i a)) where
  recordEncoder (M1 (K1 a)) = toEncoding a
  {-# INLINE recordEncoder #-}

instance GEncodeRecord f => GEncodeRecord (C1 c f) where
  recordEncoder (M1 a) = recordEncoder a
  {-# INLINE recordEncoder #-}

instance GEncodeRecord f => GEncodeRecord (D1 c f) where
  recordEncoder (M1 a) = recordEncoder a
  {-# INLINE recordEncoder #-}

class GSerialiseRecord f where
  recordSchema :: proxy f -> [TypeRep] -> [(T.Text, Schema)]
  recordExtractor :: Maybe (f x) -> TransFusion (FieldDecoder T.Text) ((->) Term) (Term -> f x)

instance (GSerialiseRecord f, GSerialiseRecord g) => GSerialiseRecord (f :*: g) where
  recordSchema _ ts = recordSchema (Proxy :: Proxy f) ts
    ++ recordSchema (Proxy :: Proxy g) ts
  recordExtractor def = (\f g -> (:*:) <$> f <*> g)
    <$> recordExtractor ((\(x :*: _) -> x) <$> def)
    <*> recordExtractor ((\(_ :*: x) -> x) <$> def)

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ ts = [(T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), substSchema (Proxy :: Proxy a) ts)]
  recordExtractor def = TransFusion $ \k -> fmap (fmap (M1 . K1)) $ k $ FieldDecoder
    (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x))
    (unK1 . unM1 <$> def)
    (getExtractor extractor)

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordExtractor def = fmap M1 <$> recordExtractor (unM1 <$> def)

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordExtractor def = fmap M1 <$> recordExtractor (unM1 <$> def)

class GSerialiseProduct f where
  productSchema :: proxy f -> [TypeRep] -> [Schema]
  productEncoder :: f x -> Encoding
  productExtractor :: Compose (State Int) (TransFusion (FieldDecoder Int) ((->) Term)) (Term -> f x)

instance GSerialiseProduct U1 where
  productSchema _ _ = []
  productEncoder _ = mempty
  productExtractor = pure (pure U1)

instance (Serialise a) => GSerialiseProduct (K1 i a) where
  productSchema _ ts = [substSchema (Proxy :: Proxy a) ts]
  productEncoder (K1 a) = toEncoding a
  productExtractor = Compose $ state $ \i ->
    ( TransFusion $ \k -> fmap (fmap K1) $ k $ FieldDecoder i Nothing (getExtractor extractor)
    , i + 1)

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts
  productEncoder (M1 a) = productEncoder a
  productExtractor = fmap M1 <$> productExtractor

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts ++ productSchema (Proxy :: Proxy g) ts
  productEncoder (f :*: g) = productEncoder f <> productEncoder g
  productExtractor = liftA2 (:*:) <$> productExtractor <*> productExtractor

extractorProduct' :: GSerialiseProduct f => Schema -> Strategy' (Term -> f x)
extractorProduct' (SProduct schs) = Strategy $ \recs -> do
  let go :: FieldDecoder Int x -> Either StrategyError (Term -> x)
      go (FieldDecoder i _ p) = do
        getItem <- if i < length schs
          then unPlan p (schs !! i) `unStrategy` recs
          else Left "Data.Winery.gextractorProduct: insufficient fields"
        return $ \case
          TProduct xs -> getItem $ unsafeIndex (throw $ InvalidTerm (TProduct xs)) xs i
          t -> throw $ InvalidTerm t
  m <- unTransFusion (getCompose productExtractor `evalState` 0) go
  return m
extractorProduct' sch = unexpectedSchema' "extractorProduct'" "a product" sch

-- | Generic implementation of 'schemaVia' for an ADT.
gschemaViaVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> [TypeRep] -> Schema
gschemaViaVariant p ts = SFix $ SVariant $ variantSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

-- | Generic implementation of 'toEncoding' for an ADT.
gtoEncodingVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> Encoding
gtoEncodingVariant = variantEncoder 0 . from
{-# INLINE gtoEncodingVariant #-}

-- | Generic implementation of 'extractor' for an ADT.
gextractorVariant :: forall a. (GSerialiseVariant (Rep a), Generic a, Typeable a)
  => Extractor a
gextractorVariant = Extractor $ handleRecursion $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    ds' <- V.fromList <$> sequence
      [ case lookup name variantExtractor of
          Nothing -> Left $ rep <> ": Schema not found for " <> pretty name
          Just f -> f sch `unStrategy` decs
      | (name, sch) <- schs0]
    return $ \case
      TVariant i _ v -> to $ maybe (throw InvalidTag) ($ v) $ ds' V.!? i
      t -> throw $ InvalidTerm t
  s -> unexpectedSchema' rep "a variant" s
  where
    rep = "gextractorVariant :: Extractor "
      <> viaShow (typeRep (Proxy :: Proxy a))

class GSerialiseVariant f where
  variantCount :: proxy f -> Int
  variantSchema :: proxy f -> [TypeRep] -> [(T.Text, Schema)]
  variantEncoder :: Int -> f x -> Encoding
  variantExtractor :: [(T.Text, Schema -> Strategy' (Term -> f x))]

instance (GSerialiseVariant f, GSerialiseVariant g) => GSerialiseVariant (f :+: g) where
  variantCount _ = variantCount (Proxy :: Proxy f) + variantCount (Proxy :: Proxy g)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts ++ variantSchema (Proxy :: Proxy g) ts
  variantEncoder i (L1 f) = variantEncoder i f
  variantEncoder i (R1 g) = variantEncoder (i + variantCount (Proxy :: Proxy f)) g
  variantExtractor = fmap (fmap (fmap (fmap (fmap L1)))) variantExtractor
    ++ fmap (fmap (fmap (fmap (fmap R1)))) variantExtractor

instance (GSerialiseProduct f, Constructor c) => GSerialiseVariant (C1 c f) where
  variantCount _ = 1
  variantSchema _ ts = [(T.pack $ conName (M1 undefined :: M1 i c f x), SProduct $ productSchema (Proxy :: Proxy f) ts)]
  variantEncoder i (M1 a) = BB.varInt i <> productEncoder a
  variantExtractor = [(T.pack $ conName (M1 undefined :: M1 i c f x)
    , fmap (fmap M1) . extractorProduct') ]

instance (GSerialiseVariant f) => GSerialiseVariant (S1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantExtractor = fmap (fmap (fmap (fmap M1))) <$> variantExtractor

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantExtractor = fmap (fmap (fmap (fmap M1))) <$> variantExtractor

instance Serialise Ordering
deriving instance Serialise a => Serialise (Identity a)
deriving instance (Serialise a, Typeable b) => Serialise (Const a (b :: *))
