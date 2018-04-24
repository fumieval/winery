{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}
import Data.ByteString (ByteString)
import Data.Extensible
import Data.Winery
import Data.Proxy
import GHC.Generics

type Test = Record ["foo" >: Maybe Int, "bar" >: [ByteString], "baz" >: Double]

data Test' = Test'
    { bar :: [ByteString]
    , baz :: Double
    , foo :: Maybe Int
    } deriving (Show, Generic)

def :: Test
def = #foo @= Just 42 <: #bar @= ["hell", "world"] <: #baz @= pi <: emptyRecord

main :: IO ()
main = do
  let sch = schema (Proxy :: Proxy Test)
  print sch
  let bs = serialise def
  print bs
  print (deserialiseWith ggetDecoderForRecord sch bs :: Either String Test')
