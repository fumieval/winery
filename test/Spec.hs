{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.Fix
import qualified Data.ByteString as B
import Data.Winery
import qualified Data.Winery.Internal.Builder as WB
import Test.QuickCheck
import Control.Monad

prop_VarInt :: [Int] -> Property
prop_VarInt i = B.length bs === WB.getSize e .&&. decodeCurrent bs === i
  where
    bs = WB.toByteString e
    e = toEncoding i

return []
main = void $ $quickCheckAll
