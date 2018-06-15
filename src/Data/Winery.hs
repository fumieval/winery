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
module Data.Winery
  ( Schema(..)
  , Serialise(..)
  , schema
  -- * Standalone serialisation
  , serialise
  , deserialise
  -- * Separate serialisation
  , Deserialiser(..)
  , Decoder
  , serialiseOnly
  , getDecoder
  , getDecoderBy
  -- * Encoding combinators
  , Encoding
  , encodeMulti
  -- * Decoding combinators
  , Plan(..)
  , extractListWith
  , extractField
  , extractFieldWith
  , extractConstructor
  , extractConstructorWith
  -- * Variable-length quantity
  , VarInt(..)
  -- * Internal
  , unwrapDeserialiser
  , Strategy
  , StrategyError
  -- * Generics
  , GSerialiseRecord
  , gschemaViaRecord
  , GEncodeRecord
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
import qualified Data.Winery.Internal.Builder as BB
import Data.Bits
import Data.Dynamic
import Data.Functor.Compose
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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Data.Text.Prettyprint.Doc hiding ((<>), SText, SChar)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Typeable
import GHC.Generics
import Unsafe.Coerce

data Schema = SSchema !Word8
  | SUnit
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
  | SList !Schema
  | SArray !(VarInt Int) !Schema -- fixed size
  | SProduct [Schema]
  | SProductFixed [(VarInt Int, Schema)] -- fixed size
  | SRecord [(T.Text, Schema)]
  | SVariant [(T.Text, [Schema])]
  | SFix Schema -- ^ binds a fixpoint
  | SSelf !Word8 -- ^ @SSelf n@ refers to the n-th innermost fixpoint
  deriving (Show, Read, Eq, Generic)

instance Pretty Schema where
  pretty = \case
    SSchema v -> "Schema " <> pretty v
    SUnit -> "()"
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
    SList s -> "[" <> pretty s <> "]"
    SArray _ s -> "[" <> pretty s <> "]"
    SProduct ss -> tupled $ map pretty ss
    SProductFixed ss -> tupled $ map (pretty . snd) ss
    SRecord ss -> align $ encloseSep "{ " " }" ", " [pretty k <+> "::" <+> pretty v | (k, v) <- ss]
    SVariant ss -> align $ encloseSep "( " " )" (flatAlt "| " " | ")
      [ if null vs
          then pretty k
          else nest 2 $ sep $ pretty k : map pretty vs | (k, vs) <- ss]
    SFix sch -> group $ nest 2 $ sep ["μ", pretty sch]
    SSelf i -> "Self" <+> pretty i

-- | 'Deserialiser' is a 'Plan' that creates a 'Decoder'.
newtype Deserialiser a = Deserialiser { getDeserialiser :: Plan (Decoder a) }
  deriving Functor

instance Applicative Deserialiser where
  pure = Deserialiser . pure . pure
  Deserialiser f <*> Deserialiser x = Deserialiser $ (<*>) <$> f <*> x

newtype Plan a = Plan { unPlan :: Schema -> Strategy a }
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

unwrapDeserialiser :: Deserialiser a -> Schema -> Strategy (Decoder a)
unwrapDeserialiser (Deserialiser m) = unPlan m
{-# INLINE unwrapDeserialiser #-}

-- | Serialisable datatype
class Typeable a => Serialise a where
  -- | Obtain the schema of the datatype. @[TypeRep]@ is for handling recursion.
  schemaVia :: Proxy a -> [TypeRep] -> Schema

  -- | Serialise a value.
  toEncoding :: a -> Encoding

  -- | The 'Deserialiser'
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

-- | Obtain the schema of the datatype.
schema :: forall proxy a. Serialise a => proxy a -> Schema
schema _ = schemaVia (Proxy :: Proxy a) []
{-# INLINE schema #-}

-- | Obtain a decoder from a schema.
getDecoder :: Serialise a => Schema -> Either StrategyError (Decoder a)
getDecoder = getDecoderBy deserialiser
{-# INLINE getDecoder #-}

-- | Get a decoder from a `Deserialiser` and a schema.
getDecoderBy :: Deserialiser a -> Schema -> Either StrategyError (Decoder a)
getDecoderBy (Deserialiser plan) sch = unPlan plan sch `unStrategy` []
{-# INLINE getDecoderBy #-}

-- | Serialise a value along with its schema.
serialise :: Serialise a => a -> B.ByteString
serialise a = BB.toByteString $ mappend (BB.word8 currentSchemaVersion)
  $ toEncoding (schema [a], a)

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialise :: Serialise a => B.ByteString -> Either StrategyError a
deserialise bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    m <- getDecoder $ SSchema ver
    ($bs) $ evalContT $ do
      sizA <- decodeVarInt
      sch <- lift $ m . B.take sizA
      asks $ \bs' -> ($ B.drop sizA bs') <$> getDecoderBy deserialiser sch
  Nothing -> Left "Unexpected empty string"

-- | Serialise a value without its schema.
serialiseOnly :: Serialise a => a -> B.ByteString
serialiseOnly = BB.toByteString . toEncoding
{-# INLINE serialiseOnly #-}

substSchema :: Serialise a => Proxy a -> [TypeRep] -> Schema
substSchema p ts
  | Just i <- elemIndex (typeRep p) ts = SSelf $ fromIntegral i
  | otherwise = schemaVia p ts

currentSchemaVersion :: Word8
currentSchemaVersion = 1

bootstrapSchema :: Word8 -> Either StrategyError Schema
bootstrapSchema 1 = Right $ SFix $ SVariant [("SSchema",[SWord8])
  ,("SUnit",[])
  ,("SBool",[])
  ,("SChar",[])
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
bootstrapSchema n = Left $ "Unsupported version: " <> pretty n

unexpectedSchema :: forall a. Serialise a => Doc AnsiStyle -> Schema -> Strategy (Decoder a)
unexpectedSchema subject actual = unexpectedSchema' subject
  (pretty $ schema (Proxy :: Proxy a)) actual

unexpectedSchema' :: Doc AnsiStyle -> Doc AnsiStyle -> Schema -> Strategy a
unexpectedSchema' subject expected actual = errorStrategy
  $ annotate bold subject
  <+> "expects" <+> annotate (color Green <> bold) expected
  <+> "but got " <+> pretty actual

instance Serialise Schema where
  schemaVia _ _ = SSchema currentSchemaVersion
  toEncoding = gtoEncodingVariant
  deserialiser = Deserialiser $ Plan $ \case
    SSchema n -> Strategy (const $ bootstrapSchema n)
      >>= unwrapDeserialiser gdeserialiserVariant
    s -> unwrapDeserialiser gdeserialiserVariant s

instance Serialise () where
  schemaVia _ _ = SUnit
  toEncoding = mempty
  deserialiser = pure ()
  constantSize _ = Just 0

instance Serialise Bool where
  schemaVia _ _ = SBool
  toEncoding False = BB.word8 0
  toEncoding True = BB.word8 1
  deserialiser = Deserialiser $ Plan $ \case
    SBool -> pure $ (/=0) <$> evalContT getWord8
    s -> unexpectedSchema "Serialise Bool" s
  constantSize _ = Just 1

instance Serialise Word8 where
  schemaVia _ _ = SWord8
  toEncoding = BB.word8
  deserialiser = Deserialiser $ Plan $ \case
    SWord8 -> pure $ evalContT getWord8
    s -> unexpectedSchema "Serialise Word8" s
  constantSize _ = Just 1

instance Serialise Word16 where
  schemaVia _ _ = SWord16
  toEncoding = BB.word16
  deserialiser = Deserialiser $ Plan $ \case
    SWord16 -> pure $ evalContT $ do
      a <- getWord8
      b <- getWord8
      return $! fromIntegral a `unsafeShiftL` 8 .|. fromIntegral b
    s -> unexpectedSchema "Serialise Word16" s
  constantSize _ = Just 2

instance Serialise Word32 where
  schemaVia _ _ = SWord32
  toEncoding = BB.word32
  deserialiser = Deserialiser $ Plan $ \case
    SWord32 -> pure word32be
    s -> unexpectedSchema "Serialise Word32" s
  constantSize _ = Just 4

instance Serialise Word64 where
  schemaVia _ _ = SWord64
  toEncoding = BB.word64
  deserialiser = Deserialiser $ Plan $ \case
    SWord64 -> pure word64be
    s -> unexpectedSchema "Serialise Word64" s
  constantSize _ = Just 8

instance Serialise Word where
  schemaVia _ _ = SWord64
  toEncoding = BB.word64 . fromIntegral
  deserialiser = Deserialiser $ Plan $ \case
    SWord64 -> pure $ fromIntegral <$> word64be
    s -> unexpectedSchema "Serialise Word" s
  constantSize _ = Just 8

instance Serialise Int8 where
  schemaVia _ _ = SInt8
  toEncoding = BB.word8 . fromIntegral
  deserialiser = Deserialiser $ Plan $ \case
    SInt8 -> pure $ fromIntegral <$> evalContT getWord8
    s -> unexpectedSchema "Serialise Int8" s
  constantSize _ = Just 1

instance Serialise Int16 where
  schemaVia _ _ = SInt16
  toEncoding = BB.word16 . fromIntegral
  deserialiser = Deserialiser $ Plan $ \case
    SInt16 -> pure $ fromIntegral <$> word16be
    s -> unexpectedSchema "Serialise Int16" s
  constantSize _ = Just 2

instance Serialise Int32 where
  schemaVia _ _ = SInt32
  toEncoding = BB.word32 . fromIntegral
  deserialiser = Deserialiser $ Plan $ \case
    SInt32 -> pure $ fromIntegral <$> word32be
    s -> unexpectedSchema "Serialise Int32" s
  constantSize _ = Just 4

instance Serialise Int64 where
  schemaVia _ _ = SInt64
  toEncoding = BB.word64 . fromIntegral
  deserialiser = Deserialiser $ Plan $ \case
    SInt64 -> pure $ fromIntegral <$> word64be
    s -> unexpectedSchema "Serialise Int64" s
  constantSize _ = Just 8

instance Serialise Int where
  schemaVia _ _ = SInteger
  toEncoding = toEncoding . VarInt
  deserialiser = Deserialiser $ Plan $ \case
    SInteger -> pure $ getVarInt . evalContT decodeVarInt
    s -> unexpectedSchema "Serialise Int" s

instance Serialise Float where
  schemaVia _ _ = SFloat
  toEncoding = BB.word32 . unsafeCoerce
  deserialiser = Deserialiser $ Plan $ \case
    SFloat -> pure $ unsafeCoerce <$> word32be
    s -> unexpectedSchema "Serialise Float" s
  constantSize _ = Just 4

instance Serialise Double where
  schemaVia _ _ = SDouble
  toEncoding = BB.word64 . unsafeCoerce
  deserialiser = Deserialiser $ Plan $ \case
    SDouble -> pure $ unsafeCoerce <$> word64be
    s -> unexpectedSchema "Serialise Double" s
  constantSize _ = Just 8

instance Serialise T.Text where
  schemaVia _ _ = SText
  toEncoding = toEncoding . T.encodeUtf8
  deserialiser = Deserialiser $ Plan $ \case
    SText -> pure T.decodeUtf8
    s -> unexpectedSchema "Serialise Text" s

-- | Encoded in variable-length quantity.
newtype VarInt a = VarInt { getVarInt :: a } deriving (Show, Read, Eq, Ord, Enum
  , Bounded, Num, Real, Integral, Bits, Typeable)

instance (Typeable a, Bits a, Integral a) => Serialise (VarInt a) where
  schemaVia _ _ = SInteger
  toEncoding = encodeVarInt . getVarInt
  {-# INLINE toEncoding #-}
  deserialiser = Deserialiser $ Plan $ \case
    SInteger -> pure $ evalContT decodeVarInt
    s -> unexpectedSchema "Serialise (VarInt a)" s

instance Serialise Integer where
  schemaVia _ _ = SInteger
  toEncoding = toEncoding . VarInt
  deserialiser = getVarInt <$> deserialiser

instance Serialise Char where
  schemaVia _ _ = SChar
  toEncoding = toEncoding . fromEnum
  deserialiser = Deserialiser $ Plan $ \case
    SChar -> pure $ toEnum . evalContT decodeVarInt
    s -> unexpectedSchema "Serialise Char" s

instance Serialise a => Serialise (Maybe a) where
  schemaVia _ ts = SVariant [("Nothing", [])
    , ("Just", [substSchema (Proxy :: Proxy a) ts])]
  toEncoding Nothing = encodeVarInt (0 :: Word8)
  toEncoding (Just a) = encodeVarInt (1 :: Word8) <> toEncoding a
  deserialiser = Deserialiser $ Plan $ \case
    SVariant [_, (_, [sch])] -> do
      dec <- unwrapDeserialiser deserialiser sch
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> pure Nothing
          _ -> Just <$> lift dec
    s -> unexpectedSchema "Serialise (Maybe a)" s
  constantSize _ = (1+) <$> constantSize (Proxy :: Proxy a)

instance Serialise B.ByteString where
  schemaVia _ _ = SBytes
  toEncoding = BB.bytes
  deserialiser = Deserialiser $ Plan $ \case
    SBytes -> pure id
    s -> unexpectedSchema "Serialise ByteString" s

instance Serialise a => Serialise [a] where
  schemaVia _ ts = case constantSize (Proxy :: Proxy a) of
    Nothing -> SList (substSchema (Proxy :: Proxy a) ts)
    Just s -> SArray (VarInt s) (substSchema (Proxy :: Proxy a) ts)
  toEncoding xs = case constantSize (Proxy :: Proxy a) of
    Nothing -> encodeVarInt (length xs)
      <> encodeMulti (map toEncoding xs)
    Just _ -> encodeVarInt (length xs) <> foldMap toEncoding xs
  deserialiser = extractListWith deserialiser

instance Serialise a => Serialise (V.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . V.toList
  deserialiser = V.fromList <$> deserialiser

instance (SV.Storable a, Serialise a) => Serialise (SV.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . SV.toList
  deserialiser = SV.fromList <$> deserialiser

instance (UV.Unbox a, Serialise a) => Serialise (UV.Vector a) where
  schemaVia _ = schemaVia (Proxy :: Proxy [a])
  toEncoding = toEncoding . UV.toList
  deserialiser = UV.fromList <$> deserialiser

-- | Extract a list or an array of values.
extractListWith :: Deserialiser a -> Deserialiser [a]
extractListWith (Deserialiser plan) = Deserialiser $ Plan $ \case
  SArray (VarInt size) s -> do
    getItem <- unPlan plan s
    return $ evalContT $ do
      n <- decodeVarInt
      asks $ \bs -> [decodeAt (size * i, size) getItem bs | i <- [0..n - 1]]
  SList s -> do
    getItem <- unPlan plan s
    return $ evalContT $ do
      n <- decodeVarInt
      offsets <- decodeOffsets n
      asks $ \bs -> [decodeAt ofs getItem bs | ofs <- UV.toList offsets]
  s -> unexpectedSchema' "extractListWith ..." "[a]" s

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

-- | Extract a field of a record.
extractField :: Serialise a => T.Text -> Deserialiser a
extractField = extractFieldWith deserialiser
{-# INLINE extractField #-}

-- | Extract a field using the supplied 'Deserialiser'.
extractFieldWith :: Typeable a => Deserialiser a -> T.Text -> Deserialiser a
extractFieldWith (Deserialiser g) name = Deserialiser $ handleRecursion $ \case
  SRecord schs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    case lookup name schs' of
      Just (i, sch) -> do
        m <- unPlan g sch
        return $ evalContT $ do
          offsets <- decodeOffsets (length schs)
          lift $ decodeAt (unsafeIndexV msg offsets i) m
      Nothing -> errorStrategy $ rep <> ": Schema not found in " <> pretty (map fst schs)
  s -> unexpectedSchema' rep "a record" s
  where
    rep = "extractFieldWith ... " <> dquotes (pretty name)
    msg = "Data.Winery.extractFieldWith ... " <> show name <> ": impossible"

handleRecursion :: Typeable a => (Schema -> Strategy (Decoder a)) -> Plan (Decoder a)
handleRecursion k = Plan $ \sch -> Strategy $ \decs -> case sch of
  SSelf i -> return $ fmap (`fromDyn` error "Invalid recursion")
    $ unsafeIndex "Data.Winery.handleRecursion: unbound fixpoint" decs (fromIntegral i)
  SFix s -> mfix $ \a -> unPlan (handleRecursion k) s `unStrategy` (fmap toDyn a : decs)
  s -> k s `unStrategy` decs

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
        asks $ \bs -> (decodeAt (0, offA) getA bs, decodeAt (offA, maxBound) getB bs)
    SProductFixed [(VarInt la, sa), (VarInt lb, sb)] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      return $ \bs -> (decodeAt (0, la) getA bs, decodeAt (la, lb) getB bs)
    s -> unexpectedSchema "Serialise (a, b)" s

  constantSize _ = (+) <$> constantSize (Proxy :: Proxy a) <*> constantSize (Proxy :: Proxy b)

instance (Serialise a, Serialise b, Serialise c) => Serialise (a, b, c) where
  schemaVia _ ts = case (constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b), constantSize (Proxy :: Proxy c)) of
    (Just a, Just b, Just c) -> SProductFixed [(VarInt a, sa), (VarInt b, sb), (VarInt c, sc)]
    _ -> SProduct [sa, sb, sc]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
      sc = substSchema (Proxy :: Proxy c) ts
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
        asks $ \bs -> (decodeAt (0, offA) getA bs, decodeAt (offA, offB) getB bs, decodeAt (offA + offB, maxBound) getC bs)
    SProductFixed [(VarInt la, sa), (VarInt lb, sb), (VarInt lc, sc)] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      getC <- unwrapDeserialiser deserialiser sc
      return $ \bs -> (decodeAt (0, la) getA bs, decodeAt (la, lb) getB bs, decodeAt (la + lb, lc) getC bs)
    s -> unexpectedSchema "Serialise (a, b, c)" s

  constantSize _ = fmap sum $ sequence [constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b), constantSize (Proxy :: Proxy c)]

instance (Serialise a, Serialise b, Serialise c, Serialise d) => Serialise (a, b, c, d) where
  schemaVia _ ts = case (constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b), constantSize (Proxy :: Proxy c), constantSize (Proxy :: Proxy d)) of
    (Just a, Just b, Just c, Just d) -> SProductFixed [(VarInt a, sa), (VarInt b, sb), (VarInt c, sc), (VarInt d, sd)]
    _ -> SProduct [sa, sb, sc, sd]
    where
      sa = substSchema (Proxy :: Proxy a) ts
      sb = substSchema (Proxy :: Proxy b) ts
      sc = substSchema (Proxy :: Proxy c) ts
      sd = substSchema (Proxy :: Proxy d) ts
  toEncoding (a, b, c, d) = case constantSize (Proxy :: Proxy (a, b, c)) of
    Nothing -> encodeMulti [toEncoding a, toEncoding b, toEncoding c, toEncoding d]
    Just _ -> toEncoding a <> toEncoding b <> toEncoding c <> toEncoding d
  deserialiser = Deserialiser $ Plan $ \case
    SProduct [sa, sb, sc, sd] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      getC <- unwrapDeserialiser deserialiser sc
      getD <- unwrapDeserialiser deserialiser sd
      return $ evalContT $ do
        offA <- decodeVarInt
        offB <- decodeVarInt
        offC <- decodeVarInt
        asks $ \bs -> (decodeAt (0, offA) getA bs, decodeAt (offB, offA) getB bs, decodeAt (offA + offB, offC) getC bs, decodeAt (offA + offB + offC, maxBound) getD bs)
    SProductFixed [(VarInt la, sa), (VarInt lb, sb), (VarInt lc, sc), (VarInt ld, sd)] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      getC <- unwrapDeserialiser deserialiser sc
      getD <- unwrapDeserialiser deserialiser sd
      return $ \bs -> (decodeAt (0, la) getA bs, decodeAt (la, lb) getB bs, decodeAt (la + lb, lc) getC bs, decodeAt (la + lb + lc, ld) getD bs)
    s -> unexpectedSchema "Serialise (a, b, c, d)" s

  constantSize _ = fmap sum $ sequence [constantSize (Proxy :: Proxy a), constantSize (Proxy :: Proxy b), constantSize (Proxy :: Proxy c), constantSize (Proxy :: Proxy d)]

instance (Serialise a, Serialise b) => Serialise (Either a b) where
  schemaVia _ ts = SVariant [("Left", [substSchema (Proxy :: Proxy a) ts])
    , ("Right", [substSchema (Proxy :: Proxy b) ts])]
  toEncoding (Left a) = BB.word8 0 <> toEncoding a
  toEncoding (Right b) = BB.word8 1 <> toEncoding b
  deserialiser = Deserialiser $ Plan $ \case
    SVariant [(_, [sa]), (_, [sb])] -> do
      getA <- unwrapDeserialiser deserialiser sa
      getB <- unwrapDeserialiser deserialiser sb
      return $ evalContT $ do
        t <- decodeVarInt
        case t :: Word8 of
          0 -> Left <$> lift getA
          _ -> Right <$> lift getB
    s -> unexpectedSchema "Either (a, b)" s
  constantSize _ = fmap (1+) $ max
    <$> constantSize (Proxy :: Proxy a)
    <*> constantSize (Proxy :: Proxy b)

-- | Tries to extract a specific constructor of a variant. Useful for
-- implementing backward-compatible deserialisers.
extractConstructorWith :: Typeable a => Deserialiser a -> T.Text -> Deserialiser (Maybe a)
extractConstructorWith d name = Deserialiser $ handleRecursion $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    (j, dec) <- case [(i :: Int, ss) | (i, (k, ss)) <- zip [0..] schs0, name == k] of
      [(i, [s])] -> fmap ((,) i) $ unwrapDeserialiser d s `unStrategy` decs
      [(i, ss)] -> fmap ((,) i) $ unwrapDeserialiser d (SProduct ss) `unStrategy` decs
      _ -> Left $ rep <> ": Schema not found in " <> pretty (map fst schs0)

    return $ evalContT $ do
      i <- decodeVarInt
      if i == j
        then Just <$> lift dec
        else pure Nothing
  s -> unexpectedSchema' rep "a variant" s
  where
    rep = "extractConstructorWith ... " <> dquotes (pretty name)

extractConstructor :: (Serialise a) => T.Text -> Deserialiser (Maybe a)
extractConstructor = extractConstructorWith deserialiser
{-# INLINE extractConstructor #-}

-- | Generic implementation of 'schemaVia' for a record.
gschemaViaRecord :: forall proxy a. (GSerialiseRecord (Rep a), Generic a, Typeable a) => proxy a -> [TypeRep] -> Schema
gschemaViaRecord p ts = SFix $ SRecord $ recordSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

-- | Generic implementation of 'toEncoding' for a record.
gtoEncodingRecord :: (GEncodeRecord (Rep a), Generic a) => a -> Encoding
gtoEncodingRecord = encodeMulti . recordEncoder . from
{-# INLINE gtoEncodingRecord #-}

data FieldDecoder i a = FieldDecoder !i !(Maybe a) !(Plan (Decoder a))

-- | Generic implementation of 'deserialiser' for a record.
gdeserialiserRecord :: forall a. (GSerialiseRecord (Rep a), Generic a, Typeable a)
  => Maybe a -- ^ default value (optional)
  -> Deserialiser a
gdeserialiserRecord def = Deserialiser $ handleRecursion $ \case
  SRecord schs -> Strategy $ \decs -> do
    let schs' = [(k, (i, s)) | (i, (k, s)) <- zip [0..] schs]
    let go :: FieldDecoder T.Text x -> Compose (Either StrategyError) ((->) Offsets) (Decoder x)
        go (FieldDecoder name def' p) = case lookup name schs' of
          Nothing -> Compose $ case def' of
            Just d -> Right (pure (pure d))
            Nothing -> Left $ rep <> ": Default value not found for " <> pretty name
          Just (i, sch) -> Compose $ case p `unPlan` sch `unStrategy` decs of
            Right getItem -> Right $ \offsets -> decodeAt (unsafeIndexV "Data.Winery.gdeserialiserRecord: impossible" offsets i) getItem
            Left e -> Left e
    m <- getCompose $ unTransFusion (recordDecoder $ from <$> def) go
    return $ evalContT $ do
      offsets <- decodeOffsets (length schs)
      asks $ \bs -> to $ m offsets bs
  s -> unexpectedSchema' rep "a record" s
  where
    rep = "gdeserialiserRecord :: Deserialiser "
      <> viaShow (typeRep (Proxy :: Proxy a))

class GEncodeRecord f where
  recordEncoder :: f x -> [Encoding]

instance (GEncodeRecord f, GEncodeRecord g) => GEncodeRecord (f :*: g) where
  recordEncoder (f :*: g) = recordEncoder f ++ recordEncoder g
  {-# INLINE recordEncoder #-}

instance Serialise a => GEncodeRecord (S1 c (K1 i a)) where
  recordEncoder (M1 (K1 a)) = pure $! toEncoding a
  {-# INLINE recordEncoder #-}

instance GEncodeRecord f => GEncodeRecord (C1 c f) where
  recordEncoder (M1 a) = recordEncoder a
  {-# INLINE recordEncoder #-}

instance GEncodeRecord f => GEncodeRecord (D1 c f) where
  recordEncoder (M1 a) = recordEncoder a
  {-# INLINE recordEncoder #-}

class GSerialiseRecord f where
  recordSchema :: proxy f -> [TypeRep] -> [(T.Text, Schema)]
  recordDecoder :: Maybe (f x) -> TransFusion (FieldDecoder T.Text) Decoder (Decoder (f x))

instance (GSerialiseRecord f, GSerialiseRecord g) => GSerialiseRecord (f :*: g) where
  recordSchema _ ts = recordSchema (Proxy :: Proxy f) ts
    ++ recordSchema (Proxy :: Proxy g) ts
  recordDecoder def = (\f g -> (:*:) <$> f <*> g)
    <$> recordDecoder ((\(x :*: _) -> x) <$> def)
    <*> recordDecoder ((\(_ :*: x) -> x) <$> def)

instance (Serialise a, Selector c) => GSerialiseRecord (S1 c (K1 i a)) where
  recordSchema _ ts = [(T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x), substSchema (Proxy :: Proxy a) ts)]
  recordDecoder def = TransFusion $ \k -> fmap (fmap (M1 . K1)) $ k $ FieldDecoder
    (T.pack $ selName (M1 undefined :: M1 i c (K1 i a) x))
    (unK1 . unM1 <$> def)
    (getDeserialiser deserialiser)

instance (GSerialiseRecord f) => GSerialiseRecord (C1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordDecoder def = fmap M1 <$> recordDecoder (unM1 <$> def)

instance (GSerialiseRecord f) => GSerialiseRecord (D1 c f) where
  recordSchema _ = recordSchema (Proxy :: Proxy f)
  recordDecoder def = fmap M1 <$> recordDecoder (unM1 <$> def)

class GSerialiseProduct f where
  productSchema :: proxy f -> [TypeRep] -> [Schema]
  productEncoder :: f x -> [Encoding]
  productDecoder :: TransFusion (FieldDecoder ()) Decoder (Decoder (f x))

instance GSerialiseProduct U1 where
  productSchema _ _ = []
  productEncoder _ = []
  productDecoder = pure (pure U1)

instance (Serialise a) => GSerialiseProduct (K1 i a) where
  productSchema _ ts = [substSchema (Proxy :: Proxy a) ts]
  productEncoder (K1 a) = [toEncoding a]
  productDecoder = TransFusion $ \k -> fmap (fmap K1) $ k $ FieldDecoder () Nothing (getDeserialiser deserialiser)

instance GSerialiseProduct f => GSerialiseProduct (M1 i c f) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts
  productEncoder (M1 a) = productEncoder a
  productDecoder = fmap M1 <$> productDecoder

instance (GSerialiseProduct f, GSerialiseProduct g) => GSerialiseProduct (f :*: g) where
  productSchema _ ts = productSchema (Proxy :: Proxy f) ts ++ productSchema (Proxy :: Proxy g) ts
  productEncoder (f :*: g) = productEncoder f ++ productEncoder g
  productDecoder = liftA2 (:*:) <$> productDecoder <*> productDecoder

deserialiserProduct' :: GSerialiseProduct f => [Schema] -> Strategy (Decoder (f x))
deserialiserProduct' schs0 = Strategy $ \recs -> do
  let go :: Int -> [Schema] -> TransList (FieldDecoder ()) Decoder x -> Either StrategyError (Offsets -> x)
      go _ _ (Done a) = Right $ const a
      go _ [] _ = Left "deserialiserProduct': Mismatching number of fields"
      go i (sch : schs) (More (FieldDecoder () _ p) k) = do
        getItem <- unPlan p sch `unStrategy` recs
        r <- go (i + 1) schs k
        return $ \offsets -> r offsets $ decodeAt (unsafeIndexV "Data.Winery.gdeserialiserProduct: impossible" offsets i) getItem
  m <- go 0 schs0 $ runTransFusion productDecoder
  return $ evalContT $ do
    offsets <- decodeOffsets (length schs0)
    asks $ \bs -> m offsets bs

-- | Generic implementation of 'schemaVia' for an ADT.
gschemaViaVariant :: forall proxy a. (GSerialiseVariant (Rep a), Typeable a, Generic a) => proxy a -> [TypeRep] -> Schema
gschemaViaVariant p ts = SFix $ SVariant $ variantSchema (Proxy :: Proxy (Rep a)) (typeRep p : ts)

-- | Generic implementation of 'toEncoding' for an ADT.
gtoEncodingVariant :: (GSerialiseVariant (Rep a), Generic a) => a -> Encoding
gtoEncodingVariant = variantEncoder 0 . from
{-# INLINE gtoEncodingVariant #-}

-- | Generic implementation of 'deserialiser' for an ADT.
gdeserialiserVariant :: forall a. (GSerialiseVariant (Rep a), Generic a, Typeable a)
  => Deserialiser a
gdeserialiserVariant = Deserialiser $ handleRecursion $ \case
  SVariant schs0 -> Strategy $ \decs -> do
    ds' <- V.fromList <$> sequence
      [ case lookup name variantDecoder of
          Nothing -> Left $ rep <> ": Schema not found for " <> pretty name
          Just f -> f sch `unStrategy` decs
      | (name, sch) <- schs0]
    return $ evalContT $ do
      i <- decodeVarInt
      lift $ fmap to $ ds' V.! i
  s -> unexpectedSchema' rep "a variant" s
  where
    rep = "gdeserialiserVariant :: Deserialiser "
      <> viaShow (typeRep (Proxy :: Proxy a))

class GSerialiseVariant f where
  variantCount :: proxy f -> Int
  variantSchema :: proxy f -> [TypeRep] -> [(T.Text, [Schema])]
  variantEncoder :: Int -> f x -> Encoding
  variantDecoder :: [(T.Text, [Schema] -> Strategy (Decoder (f x)))]

instance (GSerialiseVariant f, GSerialiseVariant g) => GSerialiseVariant (f :+: g) where
  variantCount _ = variantCount (Proxy :: Proxy f) + variantCount (Proxy :: Proxy g)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts ++ variantSchema (Proxy :: Proxy g) ts
  variantEncoder i (L1 f) = variantEncoder i f
  variantEncoder i (R1 g) = variantEncoder (i + variantCount (Proxy :: Proxy f)) g
  variantDecoder = fmap (fmap (fmap (fmap (fmap L1)))) variantDecoder
    ++ fmap (fmap (fmap (fmap (fmap R1)))) variantDecoder

instance (GSerialiseProduct f, Constructor c) => GSerialiseVariant (C1 c f) where
  variantCount _ = 1
  variantSchema _ ts = [(T.pack $ conName (M1 undefined :: M1 i c f x), productSchema (Proxy :: Proxy f) ts)]
  variantEncoder i (M1 a) = encodeVarInt i <> encodeMulti (productEncoder a)
  variantDecoder = [(T.pack $ conName (M1 undefined :: M1 i c f x)
    , fmap (fmap M1) . deserialiserProduct') ]

instance (GSerialiseVariant f) => GSerialiseVariant (S1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder = fmap (fmap (fmap (fmap M1))) <$> variantDecoder

instance (GSerialiseVariant f) => GSerialiseVariant (D1 c f) where
  variantCount _ = variantCount (Proxy :: Proxy f)
  variantSchema _ ts = variantSchema (Proxy :: Proxy f) ts
  variantEncoder i (M1 a) = variantEncoder i a
  variantDecoder = fmap (fmap (fmap (fmap M1))) <$> variantDecoder
