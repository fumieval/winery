{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
import Control.DeepSeq
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Binary as B
import Data.Either
import Data.Winery
import Data.Word
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Gauge.Main
import qualified Codec.Serialise as CBOR
import qualified Data.Csv as CSV
import Data.Winery.Term

data Gender = Male | Female deriving (Show, Generic)

instance Serialise Gender
instance CBOR.Serialise Gender
instance B.Binary Gender

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

instance NFData TestRec where
  rnf TestRec{} = ()

instance Serialise TestRec where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  deserialiser = gdeserialiserRecord Nothing

instance B.Binary TestRec
instance CBOR.Serialise TestRec

main = do
  winery <- B.readFile "benchmarks/data.winery"
  binary <- B.readFile "benchmarks/data.binary"
  cbor <- B.readFile "benchmarks/data.cbor"
  values :: [TestRec] <- return $ B.decode $ BL.fromStrict binary
  let aValue = head values
  defaultMain
    [ bgroup "serialise/list"
      [ bench "winery" $ nf serialiseOnly values
      , bench "binary" $ nf (BL.toStrict . B.encode) values
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) values
      ]
    , bgroup "serialise/item"
      [ bench "winery" $ nf serialiseOnly aValue
      , bench "binary" $ nf (BL.toStrict . B.encode) aValue
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) aValue
      ]
    , bgroup "deserialise"
      [ bench "winery" $ nf (fromRight undefined . deserialise :: B.ByteString -> [TestRec]) winery
      , bench "binary" $ nf (B.decode . BL.fromStrict :: B.ByteString -> [TestRec]) binary
      , bench "serialise" $ nf (CBOR.deserialise . BL.fromStrict :: B.ByteString -> [TestRec]) cbor
      ]
    ]
