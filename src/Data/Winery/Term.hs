{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Data.Winery.Term where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Functor.Compose
import Data.Int
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Winery
import Data.Winery.Internal
import Data.Word

data Term = TUnit
  | TBool !Bool
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
  | TList [Term]
  | TProduct [Term]
  | TRecord [(T.Text, Term)]
  | TVariant !T.Text [Term]
  deriving Show

decodeTerm :: Deserialiser Term
decodeTerm = go [] where
  go points = Compose $ ReaderT $ \s -> case s of
    SSchema ver -> lift (bootstrapSchema ver) >>= unwrapDeserialiser (go points)
    SUnit -> pure (pure TUnit)
    SBool -> p s TBool
    SWord8 -> p s TWord8
    SWord16 -> p s TWord16
    SWord32 -> p s TWord32
    SWord64 -> p s TWord64
    SInt8 -> p s TInt8
    SInt16 -> p s TInt16
    SInt32 -> p s TInt32
    SInt64 -> p s TInt64
    SInteger -> p s TInteger
    SFloat -> p s TFloat
    SDouble -> p s TDouble
    SBytes -> p s TBytes
    Data.Winery.SText -> p s TText
    SArray siz sch -> fmap TList <$> extractListWith (go points) `unwrapDeserialiser` SArray siz sch
    SList sch -> fmap TList <$> extractListWith (go points) `unwrapDeserialiser` SList sch
    SProduct schs -> do
      decoders <- traverse (unwrapDeserialiser $ go points) schs
      return $ evalContT $ do
        offsets <- replicateM (length decoders - 1) decodeVarInt
        asks $ \bs -> TProduct [decodeAt ofs dec bs | (dec, ofs) <- zip decoders $ 0 : offsets]
    SProductFixed schs -> do
      decoders <- traverse (\(VarInt n, sch) -> (,) n <$> unwrapDeserialiser (go points) sch) schs
      let f bs ((n, dec) : decs) = dec bs : f (B.drop n bs) decs
          f _ [] = []
      return $ \bs -> TProduct $ f bs decoders
    SRecord schs -> do
      decoders <- traverse (\(name, sch) -> (,) name <$> unwrapDeserialiser (go points) sch) schs
      return $ evalContT $ do
        offsets <- replicateM (length decoders - 1) decodeVarInt
        asks $ \bs -> TRecord [(name, decodeAt ofs dec bs) | ((name, dec), ofs) <- zip decoders $ 0 : offsets]
    SVariant schs -> do
      decoders <- traverse (\(name, sch) -> (,) name <$> traverse (unwrapDeserialiser (go points)) sch) schs
      return $ evalContT $ do
        tag <- decodeVarInt
        let (name, decs) = decoders !! tag
        offsets <- replicateM (length decs - 1) decodeVarInt
        asks $ \bs -> TVariant name [decodeAt ofs dec bs | (dec, ofs) <- zip decs $ 0 : offsets]
    SSelf i -> return $ points !! fromIntegral i
    SFix s' -> mfix $ \a -> go (a : points) `unwrapDeserialiser` s'

  p s f = fmap f <$> unwrapDeserialiser deserialiser s

-- | Deserialise a 'serialise'd 'B.Bytestring'.
deserialiseTerm :: B.ByteString -> Either String (Term, Term)
deserialiseTerm bs_ = case B.uncons bs_ of
  Just (ver, bs) -> do
    getSchema <- getDecoder $ SSchema ver
    getSchemaTerm <- getDecoderBy decodeTerm $ SSchema 0
    ($bs) $ evalContT $ do
      offB <- decodeVarInt
      sch <- lift getSchema
      schT <- lift getSchemaTerm
      body <- asks $ deserialiseWithSchemaBy decodeTerm sch . B.drop offB
      return ((,) schT <$> body)
  Nothing -> Left "Unexpected empty string"

instance Pretty Term where
  pretty TUnit = "()"
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
  pretty (TList xs) = list $ map pretty xs
  pretty (TBool x) = pretty x
  pretty (TFloat x) = pretty x
  pretty (TDouble x) = pretty x
  pretty (TProduct xs) = tupled $ map pretty xs
  pretty (TRecord xs) = encloseSep "{" "}" ", " [pretty k <> ": " <> pretty v | (k, v) <- xs]
  pretty (TVariant tag xs) = pretty tag <> ": " <> sep (map pretty xs)
