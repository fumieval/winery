{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}
import Data.ByteString (ByteString, unpack)
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
  let sch = schema (Proxy :: Proxy TestVar)
  print sch
  let bs = serialise (VBaz True False)
  print $ unpack bs
  print (deserialiseWith ggetDecoderVariant sch bs :: Either String TestVar)

data TestVar = VFoo | VBar !Int | VBaz !Bool !Bool deriving (Show, Generic)

instance Serialise TestVar where
  schema = gschemaVariant
  toEncoding = gtoEncodingVariant
  getDecoder = ggetDecoderVariant
