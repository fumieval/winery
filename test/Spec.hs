{-# LANGUAGE TemplateHaskell #-}
import Data.Coerce
import Data.Winery
import Test.QuickCheck
import Control.Monad

prop_VarInt :: [Large Int] -> Property
prop_VarInt xs0 = evalDecoder decodeCurrent (serialiseOnly xs) === xs
  where
    xs = coerce xs0 :: [Int]

return []

main :: IO ()
main = void $ $quickCheckAll
