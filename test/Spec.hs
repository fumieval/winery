{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}
import Data.ByteString (ByteString)
import Data.Winery
import Data.Winery.Term
import GHC.Generics

data TestRec = TestRec
    { bar :: [ByteString]
    , baz :: Double
    , foo :: Maybe Int
    } deriving (Show, Generic)

instance Serialise TestRec where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  planDecoder = gplanDecoderRecord defTest

defTest :: TestRec
defTest = TestRec ["hello"] pi (Just 42)

main :: IO ()
main = return ()

data TestVar = VFoo | VBar !Int | VBaz !Bool !Bool deriving (Show, Generic)

instance Serialise TestVar where
  schemaVia = gschemaViaVariant
  toEncoding = gtoEncodingVariant
  planDecoder = gplanDecoderVariant
