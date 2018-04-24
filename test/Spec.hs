{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}
import Data.ByteString (ByteString)
import Data.Extensible
import Data.Winery
import Data.Proxy
import GHC.Generics

type Test = Record ["foo" >: Maybe Int, "bar" >: [ByteString], "baz" >: Double]

type Test' = Record ["bar" >: [ByteString], "baz" >: Double, "foo" >: Maybe Int]

def :: Test
def = #foo @= Just 42 <: #bar @= ["hell", "world"] <: #baz @= pi <: emptyRecord

main :: IO ()
main = do
  let sch = schema (Proxy :: Proxy Test)
  print sch
  let bs = serialise def
  print bs
  print (deserialise sch bs :: Either String Test')
