{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}
import Control.Monad.Fix
import Data.ByteString (ByteString)
import Data.Winery
import Data.Winery.Term
import GHC.Generics

data TestRec = TestRec
    { foo :: Maybe Int
    , bar :: [ByteString]
    , nodes :: [TestRec]
    } deriving (Show, Generic)

instance Serialise TestRec where
  schemaVia = gschemaViaRecord
  toEncoding = gtoEncodingRecord
  planDecoder = do
    gfoo <- extractField "foo"
    gbar <- extractField "bar"
    gnodes <- extractFieldWith (extractListWith planDecoder) "nodes"
    return $ TestRec <$> gfoo <*> gbar <*> gnodes

defTest :: TestRec
defTest = TestRec Nothing ["hello"] [TestRec (Just 42) ["world"] []]

main :: IO ()
main = return ()

data TestVar = VFoo | VBar !Int | VBaz !Bool !Bool deriving (Show, Generic)

instance Serialise TestVar where
  schemaVia = gschemaViaVariant
  toEncoding = gtoEncodingVariant
  planDecoder = gplanDecoderVariant
