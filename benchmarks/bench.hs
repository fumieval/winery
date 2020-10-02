{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wno-orphans #-}
module Main (main) where

import Control.DeepSeq
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Binary as B
import qualified Data.Store as S
import Data.Either
import Codec.Winery
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as C
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import GHC.Generics (Generic)
import Gauge.Main
import qualified Codec.Serialise as CBOR

data Gender = Male | Female deriving (Show, Generic)

instance Serialise Gender where
  bundleSerialise = bundleVariant id

instance CBOR.Serialise Gender
instance B.Binary Gender
instance C.Serialize Gender
instance J.FromJSON Gender
instance J.ToJSON Gender
instance S.Store Gender

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
  bundleSerialise = bundleVia WineryRecord

instance NFData TestRec where
  rnf TestRec{} = ()

instance B.Binary TestRec
instance C.Serialize TestRec
instance CBOR.Serialise TestRec
instance J.FromJSON TestRec
instance J.ToJSON TestRec
instance S.Store TestRec

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
  B.writeFile "benchmarks/data.store" $ S.encode values

  store <- B.readFile "benchmarks/data.store"
  let serialisedInts = serialiseOnly [floor (2**x) :: Int | x <- [0 :: Double, 0.5..62]]

      intVec :: VG.Vector v Int => Int -> v Int
      intVec n = VG.replicate n 7

      vecSizes :: [Int]
      vecSizes = [5, 50, 500, 5000]

  deepseq values $ defaultMain
    [ bgroup "serialise/list"
      [ bench "winery" $ nf serialise values
      , bench "binary" $ nf (BL.toStrict . B.encode) values
      , bench "cereal" $ nf C.encode values
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) values
      , bench "store" $ nf S.encode values
      , bench "aeson" $ nf (BL.toStrict . J.encode) values
      ]
    , bgroup "serialise/item"
      [ bench "winery" $ nf serialiseOnly aValue
      , bench "binary" $ nf (BL.toStrict . B.encode) aValue
      , bench "cereal" $ nf C.encode aValue
      , bench "serialise" $ nf (BL.toStrict . CBOR.serialise) aValue
      , bench "store" $ nf S.encode aValue
      , bench "aeson" $ nf (BL.toStrict . J.encode) aValue
      ]
    , bgroup "serialise/Vector" $
      let vecGroup
              :: forall v. (VG.Vector v Int, NFData (v Int), Serialise (v Int))
              => String -> Benchmark
          vecGroup lbl = bgroup lbl $ flip fmap vecSizes $ \n ->
              bench (show n) $ nf serialiseOnly (intVec @v n)
      in [ vecGroup @V.Vector "boxed"
         , vecGroup  @VS.Vector "storable"
         , vecGroup @VU.Vector "unboxed"
         ]

    , bgroup "deserialise"
      [ bench "winery" $ nf (fromRight undefined . deserialise :: B.ByteString -> [TestRec]) winery
      , bench "binary" $ nf (B.decode . BL.fromStrict :: B.ByteString -> [TestRec]) binary
      , bench "cereal" $ nf (C.decode :: B.ByteString -> Either String [TestRec]) cereal
      , bench "serialise" $ nf (CBOR.deserialise . BL.fromStrict :: B.ByteString -> [TestRec]) cbor
      , bench "store" $ nf (S.decodeEx :: B.ByteString -> [TestRec]) store
      , bench "aeson" $ nf (J.decode . BL.fromStrict :: B.ByteString -> Maybe [TestRec]) json
      ]
    , bgroup "deserialise/Int"
      [ bench "winery" $ nf (evalDecoder decodeCurrent :: B.ByteString -> [Int]) serialisedInts
      ]

    , bgroup "deserialise/Vector" $
      let vecGroup
              :: forall v. (VG.Vector v Int, NFData (v Int), Serialise (v Int))
              => String -> Benchmark
          vecGroup lbl = bgroup lbl $ flip fmap vecSizes $ \n ->
              bench (show n) $ nf
                  (evalDecoder decodeCurrent :: B.ByteString -> v Int)
                  (serialiseOnly $ intVec @v n)
      in [ vecGroup @V.Vector "boxed"
         , vecGroup  @VS.Vector "storable"
         , vecGroup @VU.Vector "unboxed"
         ]
    ]
