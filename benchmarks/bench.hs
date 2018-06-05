{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS -ddump-simpl -dsuppress-all -ddump-to-file #-}
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Binary as B
import Data.Winery
import Data.Word
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Gauge.Main
import qualified Codec.Serialise as CBOR
import qualified Data.Csv as CSV

data Gender = Male | Female deriving (Show, Generic)

instance Serialise Gender
instance CBOR.Serialise Gender
instance B.Binary Gender
instance CSV.FromField Gender where
  parseField "Male" = pure Male
  parseField "Female" = pure Female
  parseField _ = fail "Unknwon gender"

data TestRec = TestRec
  { id_ :: !Int
  , first_name :: !Text
  , last_name :: !Text
  , email :: !Text
  , gender :: !Gender
  , num :: !Int
  , latitude :: !Double
  , longitude :: !Double
  } deriving (Show, Generic)

instance Serialise TestRec where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  deserialiser = gdeserialiserRecord Nothing

instance B.Binary TestRec
instance CBOR.Serialise TestRec

instance CSV.FromRecord TestRec

main = do
  Right values_ <- CSV.decode CSV.HasHeader <$> BL.readFile "benchmarks/data.csv"
  let values = V.toList values_ :: [TestRec]
  B.writeFile "benchmarks/data.winery" $ serialise values
  BL.writeFile "benchmarks/data.binary" $ B.encode values
  BL.writeFile "benchmarks/data.cbor" $ CBOR.serialise values
  defaultMain
    [ bench "serialise/winery" $ whnf serialiseOnly values
    , bench "serialise/binary" $ whnf (BL.toStrict . B.encode) values
    , bench "serialise/serialise" $ whnf (BL.toStrict . CBOR.serialise) values
    ]
