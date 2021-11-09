{-# LANGUAGE TypeApplications, DeriveGeneric #-}

import Codec.Winery
import Data.List (intercalate)
import Data.Proxy
import GHC.Generics
import Prettyprinter
import Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

data BigUnion = BU0 | BU1 | BU2 | BU3 | BU4 deriving (Show, Eq, Generic, Enum)
instance Serialise BigUnion where
    bundleSerialise = bundleVia WineryVariant

instance Arbitrary BigUnion where
  arbitrary = toEnum <$> Gen.choose (0, 4)

data BigRecord = BigRecord
    { foo :: BigUnion
    , bar :: BigUnion
    , baz :: BigUnion
    , qux :: BigUnion
    } deriving (Show, Eq, Generic)

instance Arbitrary BigRecord where
  arbitrary = BigRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data BiggerRecord = BiggerRecord
    { union :: BigUnion
    , alpha :: BigRecord
    , bravo :: BigRecord
    } deriving (Show, Eq, Generic)

instance Arbitrary BiggerRecord where
  arbitrary = BiggerRecord <$> arbitrary <*> arbitrary <*> arbitrary

instance Serialise BigRecord where
    bundleSerialise = alterSchemaGen (withSchema @BigUnion)
        $ bundleVia WineryRecord

instance Serialise BiggerRecord where
    bundleSerialise = alterSchemaGen (withSchema @BigUnion . withSchema @BigRecord)
        $ bundleVia WineryRecord

main :: IO ()
main = do
    let expected = intercalate "\n"
            [ "let BU0 | BU1 | BU2 | BU3 | BU4"
            , "  let { foo :: $0, bar :: $0, baz :: $0, qux :: $0 }"
            , "    { union :: $1, alpha :: $0, bravo :: $0 }"
            ]
    let actual = show $ pretty $ schema (Proxy @BiggerRecord)
    if expected == actual
        then quickCheck $ testSerialise @BiggerRecord
        else fail $ "Schema mismatch: \n" <> actual