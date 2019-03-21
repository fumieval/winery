{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}
import Control.DeepSeq
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Binary as B
import Data.Either
import Data.Winery
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as C
import GHC.Generics (Generic)
import Gauge.Main
import qualified Codec.Serialise as CBOR

data Gender = Male | Female deriving (Show, Generic)

instance Serialise Gender where
  schemaGen = gschemaGenVariant
  toBuilder = gtoBuilderVariant
  extractor = gextractorVariant
  decodeCurrent = gdecodeCurrentVariant
instance CBOR.Serialise Gender
instance B.Binary Gender
instance C.Serialize Gender
instance J.FromJSON Gender
instance J.ToJSON Gender

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
  schemaGen = gschemaGenRecord
  toBuilder = gtoBuilderRecord
  extractor = gextractorRecord Nothing
  decodeCurrent = gdecodeCurrentRecord

instance NFData TestRec where
  rnf TestRec{} = ()

instance B.Binary TestRec
instance C.Serialize TestRec
instance CBOR.Serialise TestRec
instance J.FromJSON TestRec
instance J.ToJSON TestRec

instance C.Serialize Text where
    put = C.put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> C.get

main :: IO ()
main = do
  winery <- B.readFile "benchmarks/data.winery"
  binary <- B.readFile "benchmarks/data.binary"
  cbor <- B.readFile "benchmarks/data.cbor"
  cereal <- B.readFile "benchmarks/data.cereal"
  json <- B.readFile "benchmarks/data.json"
  values :: [TestRec] <- return $ B.decode $ BL.fromStrict binary
  let aValue = head values
  let serialisedInts = serialiseOnly [floor (2**x) :: Int | x <- [0 :: Double, 0.5..62]]
  deepseq values $ defaultMain
    [ bgroup "serialise/list"
      [ bench "winery" $ nf serialise values
      , bench "binary" $ nf (BL.toStrict . B.encode) values
      , bench "cereal" $ nf C.encode values
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) values
      , bench "aeson" $ nf (BL.toStrict . J.encode) values
      ]
    , bgroup "serialise/item"
      [ bench "winery" $ nf serialiseOnly aValue
      , bench "binary" $ nf (BL.toStrict . B.encode) aValue
      , bench "cereal" $ nf C.encode aValue
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) aValue
      , bench "aeson" $ nf (BL.toStrict . J.encode) aValue
      ]
    , bgroup "deserialise"
      [ bench "winery" $ nf (fromRight undefined . deserialise :: B.ByteString -> [TestRec]) winery
      , bench "binary" $ nf (B.decode . BL.fromStrict :: B.ByteString -> [TestRec]) binary
      , bench "cereal" $ nf (C.decode :: B.ByteString -> Either String [TestRec]) cereal
      , bench "serialise" $ nf (CBOR.deserialise . BL.fromStrict :: B.ByteString -> [TestRec]) cbor
      , bench "aeson" $ nf (J.decode . BL.fromStrict :: B.ByteString -> Maybe [TestRec]) json
      ]
    , bgroup "deserialise/Int"
      [ bench "winery" $ nf (evalDecoder decodeCurrent :: B.ByteString -> [Int]) serialisedInts
      ]
    ]
