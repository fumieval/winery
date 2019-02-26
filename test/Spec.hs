{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures#-}
import Control.Monad
import Data.ByteString (ByteString)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Winery
import Data.Winery.Internal
import Data.Word
import qualified Data.ByteString.FastBuilder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

prop_VarInt :: Int -> Property
prop_VarInt i = evalDecoder decodeVarInt
  (B.toStrictByteString $ varInt i) === i

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
prop_UTCTime = testSerialise @ UTCTime
prop_NominalDiffTime = testSerialise @ NominalDiffTime
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

data TList a = TCons a (TList a) | TNil deriving (Show, Eq, Generic)

instance Serialise a => Serialise (TList a) where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant

instance Arbitrary a => Arbitrary (TList a) where
  arbitrary = sized $ \n -> if n <= 0
    then pure TNil
    else TCons <$> arbitrary <*> scale pred arbitrary

prop_TList_Int = testSerialise @ (TList Int)

return []
main = void $ $quickCheckAll
