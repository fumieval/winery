{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators, GeneralizedNewtypeDeriving #-}
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
  deserialiser = TestRec
    <$> extractField "foo"
    <*> extractField "bar"
    <*> extractFieldWith (extractListWith deserialiser) "nodes"

defTest :: TestRec
defTest = TestRec Nothing ["hello"] [TestRec (Just 42) ["world"] []]

main :: IO ()
main = return ()

data TestVar = VFoo | VBar !Int | VBaz !Bool !Bool deriving (Show, Generic)

instance Serialise TestVar

newtype UserId = UserId Int deriving Serialise
