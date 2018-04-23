{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.ByteString (ByteString)
import Data.Winery
import Data.Proxy
import GHC.Generics

data Test = Test
    { foo :: Int
    , bar :: ByteString
    , baz :: Double
    } deriving (Show, Generic)
instance Serialise Test

def = Test 42 "bar" pi

main :: IO ()
main = do
  let sch = schema (Proxy :: Proxy Test)
  print sch
  let bs = serialise def
  print bs
  print (deserialise sch bs :: Either String Test)
