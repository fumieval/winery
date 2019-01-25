{-# LANGUAGE TemplateHaskell #-}
import qualified Data.ByteString.FastBuilder as B
import Data.Winery
import Data.Winery.Internal
import Test.QuickCheck
import Control.Monad

prop_VarInt :: Int -> Property
prop_VarInt i = evalDecoder decodeVarInt
  (B.toStrictByteString $ varInt i) === i

return []
main = void $ $quickCheckAll
