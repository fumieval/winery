{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Winery.Base
  ( Tag(..)
  , Schema
  , SchemaP(..)
  , isBigSchema
  , currentSchemaVersion
  , bootstrapSchema
  , unexpectedSchema'
  , Term(..)
  , Extractor(..)
  , Strategy'
  , Plan(..)
  , unwrapExtractor)
  where

import Control.Applicative
import Data.Aeson as J
import qualified Data.ByteString as B
import Data.Dynamic
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.String
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>), SText, SChar)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Time
import qualified Data.Vector as V
import Data.Winery.Internal
import Data.Word
import GHC.Generics (Generic)
import GHC.Exts (IsList(..))

-- | Tag is an extra value that can be attached to a schema.
data Tag = TagInt !Int
  | TagStr !T.Text
  | TagList ![Tag]
  deriving (Show, Read, Eq, Generic)

instance IsString Tag where
  fromString = TagStr . fromString

instance IsList Tag where
  type Item Tag = Tag
  fromList = TagList
  toList (TagList xs) = xs
  toList _ = []

instance Pretty Tag where
  pretty (TagInt i) = pretty i
  pretty (TagStr s) = pretty s
  pretty (TagList xs) = list (map pretty xs)

currentSchemaVersion :: Word8
currentSchemaVersion = 4

type Schema = SchemaP Int

data SchemaP a = SFix !(SchemaP a) -- ^ binds a fixpoint
  | SVar !a -- ^ @SVar n@ refers to the n-th innermost fixpoint
  | SVector !(SchemaP a)
  | SProduct !(V.Vector (SchemaP a))
  | SRecord !(V.Vector (T.Text, SchemaP a))
  | SVariant !(V.Vector (T.Text, SchemaP a))
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
  | SUTCTime -- ^ nanoseconds from POSIX epoch
  | STag !Tag !(SchemaP a)
  | SLet !(SchemaP a) !(SchemaP a)
  deriving (Show, Read, Eq, Generic, Functor, Foldable, Traversable)

-- | Somewhat arbitrary predicate that a schema should be floated out
isBigSchema :: SchemaP a -> Bool
isBigSchema (SProduct _) = True
isBigSchema (SRecord _) = True
isBigSchema (SVariant _) = True
isBigSchema _ = False

instance Pretty a => Pretty (SchemaP a) where
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
    SProduct ss -> tupled $ map pretty (V.toList ss)
    SRecord ss -> align $ encloseSep "{ " " }" ", " [pretty k <+> "::" <+> pretty v | (k, v) <- V.toList ss]
    SVariant ss -> align $ encloseSep "( " " )" (flatAlt "| " " | ")
      [ nest 2 $ sep [pretty k, pretty vs] | (k, vs) <- V.toList ss]
    SFix sch -> group $ nest 2 $ sep ["μ", pretty sch]
    SVar i -> "Self" <+> pretty i
    STag t s -> nest 2 $ sep [pretty t <> ":", pretty s]
    SLet s t -> nest 2 $ sep ["let" <+> pretty s, pretty t]

bootstrapSchema :: Word8 -> Schema
bootstrapSchema 4 = SFix
  $ SVariant
  [("SFix",SProduct [SVar 0])
  ,("SVar",SProduct [SInteger])
  ,("SVector",SProduct [SVar 0])
  ,("SProduct",SProduct [SVector (SVar 0)])
  ,("SRecord",SProduct [SVector (SProduct [SText,SVar 0])])
  ,("SVariant",SProduct [SVector (SProduct [SText,SVar 0])])
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
  ,("SUTCTime",SProduct [])
  ,("STag",SProduct
    [SFix $ SVariant
      [("TagInt",SProduct [SInteger])
      ,("TagStr",SProduct [SText])
      ,("TagList",SProduct [SVector (SVar 0)])]
    ,SVar 0])
  ,("SLet",SProduct [SVar 0, SVar 0])]
bootstrapSchema n = error $ "Unsupported version: " <> show n

unexpectedSchema' :: Doc AnsiStyle -> Doc AnsiStyle -> Schema -> Strategy' a
unexpectedSchema' subject expected actual = errorStrategy
  $ annotate bold subject
  <+> "expects" <+> annotate (color Green <> bold) expected
  <+> "but got " <+> pretty actual

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
  | TVector !(V.Vector Term)
  | TProduct !(V.Vector Term)
  | TRecord !(V.Vector (T.Text, Term))
  | TVariant !Int !T.Text Term
  deriving Show

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
  pretty (TProduct xs) = tupled $ map pretty (V.toList xs)
  pretty (TRecord xs) = align $ encloseSep "{ " " }" ", " [group $ nest 2 $ vsep [pretty k <+> "=", pretty v] | (k, v) <- V.toList xs]
  pretty (TVariant _ tag (TProduct xs)) = group $ nest 2 $ sep $ pretty tag : map pretty (V.toList xs)
  pretty (TVariant _ tag x) = group $ nest 2 $ sep [pretty tag, pretty x]
  pretty (TUTCTime t) = pretty (show t)

-- | 'Extractor' is a 'Plan' that creates a function to extract a value Term.
newtype Extractor a = Extractor { getExtractor :: Plan (Term -> a) }
  deriving Functor

instance Applicative Extractor where
  pure = Extractor . pure . pure
  Extractor f <*> Extractor x = Extractor $ (<*>) <$> f <*> x

instance Alternative Extractor where
  empty = Extractor empty
  Extractor f <|> Extractor g = Extractor $ f <|> g

type Strategy' = Strategy (Either Schema Dynamic)

-- | Plan is a monad for computations which interpret 'Schema'.
newtype Plan a = Plan { unPlan :: Schema -> Strategy' a }
  deriving Functor

instance Applicative Plan where
  pure = Plan . const . pure
  m <*> k = Plan $ \sch -> Strategy $ \decs -> case unStrategy (unPlan m sch) decs of
    Right f -> fmap f $ unStrategy (unPlan k sch) decs
    Left e -> Left e

instance Monad Plan where
  m >>= k = Plan $ \sch -> Strategy $ \decs -> case unStrategy (unPlan m sch) decs of
    Right a -> unStrategy (unPlan (k a) sch) decs
    Left e -> Left e

instance Alternative Plan where
  empty = Plan $ const empty
  Plan a <|> Plan b = Plan $ \s -> a s <|> b s

unwrapExtractor :: Extractor a -> Schema -> Strategy' (Term -> a)
unwrapExtractor (Extractor m) = unPlan m
{-# INLINE unwrapExtractor #-}
