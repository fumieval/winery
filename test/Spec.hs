{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}
import Control.Monad
import Barbies.Bare
import Barbies.TH
import Data.ByteString (ByteString)
import Data.Fixed (Pico)
import Data.Functor.Identity
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Codec.Winery
import Codec.Winery.Internal
import Data.Word
import qualified Data.ByteString.FastBuilder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import GHC.Generics
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances ()
import Control.Applicative ((<|>))

prop_VarInt :: Int -> Property
prop_VarInt i = evalDecoder decodeVarInt
  (B.toStrictByteString $ varInt i) === i

newtype Nano = Nano NominalDiffTime deriving (Show, Eq, Serialise)

instance Arbitrary Nano where
  arbitrary = Nano . realToFrac . (1000*) <$> (arbitrary :: Gen Pico)

prop_Unit = testSerialise @ ()
prop_Bool = testSerialise @ Bool
prop_Word8 = testSerialise @ Word8
prop_Word16 = testSerialise @ Word16
prop_Word32 = testSerialise @ Word32
prop_Word64 = testSerialise @ Word64
prop_Word = testSerialise @ Word
prop_Int8 = testSerialise @ Int8
prop_Int16 = testSerialise @ Int16
prop_Int32 = testSerialise @ Int32
prop_Int64 = testSerialise @ Int64
prop_Int = testSerialise @ Int
prop_Float = testSerialise @ Float
prop_Double = testSerialise @ Double
prop_Text = testSerialise @ Text
prop_Integer = testSerialise @ Integer
prop_Char = testSerialise @ Char
prop_Maybe_Int = testSerialise @ (Maybe Int)
prop_ByteString = testSerialise @ ByteString
prop_ByteString_Lazy = testSerialise @ BL.ByteString
-- prop_UTCTime = testSerialise @ UTCTime
prop_NominalDiffTime = testSerialise @ Nano
prop_List_Int = testSerialise @ [Int]
prop_Vector_Int = testSerialise @ (V.Vector Int) . V.fromList
prop_Vector_Storable_Int = testSerialise @ (SV.Vector Int) . SV.fromList
prop_Vector_Unboxed_Int = testSerialise @ (UV.Vector Int) . UV.fromList
prop_Map_Int_Int = testSerialise @ (Map Int Int)
prop_HashMap_String_Int = testSerialise @ (HM.HashMap String Int) . HM.fromList
prop_IntMap_Int = testSerialise @ (IntMap Int)
prop_Set_Int = testSerialise @ (Set Int)
prop_IntSet = testSerialise @ IntSet
prop_Seq_Int = testSerialise @ (Seq Int)
prop_Scientific = testSerialise @ Scientific
prop_Tuple2 = testSerialise @ (Bool, Int)
prop_Tuple3 = testSerialise @ (Bool, Int, Text)
prop_Tuple4 = testSerialise @ (Bool, Int, Text, Double)
prop_Either_String_Int = testSerialise @ (Either String Int)
prop_Ordering = testSerialise @ Ordering

data TRec = TRec
  { foo :: !Int
  , bar :: !Text
  } deriving (Show, Eq, Generic)

instance Arbitrary TRec where
  arbitrary = TRec <$> arbitrary <*> arbitrary

instance Serialise TRec where
  bundleSerialise = bundleVia WineryRecord

prop_TRec = testSerialise @ TRec

data TList a = TCons a (TList a) | TNil deriving (Show, Eq, Generic)

instance Serialise a => Serialise (TList a) where
  bundleSerialise = bundleVia WineryVariant

instance Arbitrary a => Arbitrary (TList a) where
  arbitrary = sized $ \n -> if n <= 0
    then pure TNil
    else TCons <$> arbitrary <*> scale pred arbitrary

prop_TList_Int = testSerialise @ (TList Int)

data Tree
  = Leaf
  | Branch Node
  deriving (Show, Eq, Generic)

data Node = Node { left :: !Tree, value :: !Int, right :: !Tree }
  deriving (Show, Eq, Generic)

instance Arbitrary Tree where
  arbitrary = sized $ \n -> if n <= 0
    then pure Leaf
    else Branch <$> arbitrary

instance Arbitrary Node where
  arbitrary = sized $ \n -> do
    leftSize <- Gen.choose (0, max 0 $ n - 1)
    let rightSize = max 0 $ n - 1 - leftSize
    Node <$> resize leftSize arbitrary <*> arbitrary <*> resize rightSize arbitrary

instance Serialise Tree where
  bundleSerialise = bundleVia WineryVariant

instance Serialise Node where
  bundleSerialise = (bundleVia WineryRecord)
    { bundleExtractor = buildExtractor $ Node
      <$> (extractField "left" <|> extractField "leftChild")
      <*> extractField "value"
      <*> (extractField "right" <|> extractField "rightChild")
    }

prop_tree = testSerialise @ Tree
prop_node = testSerialise @ Node

data Foo = Foo | Bar | Baz | Qux | FooBar | FooBaz | FooQux deriving (Generic, Eq, Show, Enum)

instance Arbitrary Foo where
  arbitrary = toEnum <$> Gen.choose (0, 6)

instance Serialise Foo where
  bundleSerialise = bundleVia WineryVariant

prop_Foo = testSerialise @ Foo

data Soup = Shoyu | Miso | Tonkotsu deriving (Generic, Eq, Show, Enum)

instance Arbitrary Soup where
  arbitrary = toEnum <$> Gen.choose (0, 2)

instance Serialise Soup where
  bundleSerialise = bundleVia WineryVariant

data Food = Rice | Ramen Soup | Pasta Text Text deriving (Generic, Eq, Show)

instance Arbitrary Food where
  arbitrary = Gen.oneof
    [ pure Rice
    , Ramen <$> arbitrary
    , Pasta
      <$> Gen.elements ["Spaghetti", "Rigatoni", "Linguine"]
      <*> Gen.elements ["Aglio e olio", "L'amatriciana", "Carbonara"]
    ]

instance Serialise Food where
  bundleSerialise = bundleVia WineryVariant
  extractor = buildExtractor
    $ ("Rice", \() -> Rice)
    `extractConstructor` ("Ramen", Ramen)
    `extractConstructor` ("Pasta", uncurry Pasta)
    `extractConstructor` extractVoid

prop_Food = testSerialise @ Food

declareBareB [d|
  data HRecB = HRec
    { baz :: !Int
    , qux :: !Text
    } deriving Generic
    |]
type HRec = HRecB Bare Identity

instance Serialise HRec where
  bundleSerialise = bundleVia WineryRecord
  extractor = fmap bstrip $ buildRecordExtractor bextractors
    { qux = extractField "qux" <|> extractField "oldQux" }

deriving instance Show HRec
deriving instance Eq HRec

instance Arbitrary HRec where
  arbitrary = HRec <$> arbitrary <*> arbitrary
prop_HRec = testSerialise @ HRec
return []
main = $quickCheckAll >>= flip unless (error "Failed")
