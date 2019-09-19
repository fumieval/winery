{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Winery.Test
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
-- Stability   :  Experimental
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- A test framework that allows you to test all related Serialise instances
-- with automatically-generated test cases
--
-----------------------------------------------------------------------------
module Codec.Winery.Test
  ( -- * Generating tests
  TestGen(..)
  , printTests
  , buildTestGroups
  -- * Test cases
  , Tested(..)
  , testCase
  -- * Running tests
  , allTests
  , mergeTests
  ) where

import Test.HUnit
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import Data.Functor.Identity
import Data.Hashable
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Typeable
import Codec.Winery
import Data.Word
import Data.Void
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics
import Text.Show
import qualified Data.Text as T

-- | Construct a test case.
testCase :: (Show a, Eq a, Serialise a)
  => Schema -- ^ the schema
  -> B.ByteString -- ^ serialised
  -> a -- ^ expected
  -> Test
testCase sch bs expected = case getDecoder sch of
  Left err -> TestCase $ assertFailure (show err)
  Right f -> expected ~=? evalDecoder f bs

-- | Merge multiple tests into one.
mergeTests :: M.Map TypeRep [Test] -> Test
mergeTests = TestList . concatMap (\(k, v) -> map (show k ~:) v) . M.toList

-- | Gather all test cases involved in the specified type.
allTests :: forall a. (TestGen a, Tested a) => M.Map TypeRep [Test]
allTests = M.insertWith (++) (typeRep (Proxy @ a)) (testCases @ a) (inheritedTests (Proxy @ a))

-- | Types with concrete test cases.
--
-- /"Doubt is useful, it keeps faith a living thing. After all, you cannot know
-- the strength of your faith until it has been tested."/
class TestGen a => Tested a where
  -- | List of test cases for the type.
  testCases :: [Test]

  default testCases :: (Serialise a, Eq a, Show a) => [Test]
  testCases = [testCase sch b a | (sch, xs) <- testGroups @ a, (b, a) <- xs]

  testGroups :: [(Schema, [(B.ByteString, a)])]
  testGroups = []
  {-# MINIMAL testGroups | testCases #-}

-- these are already covered by the QuickCheck tests from winery
instance Tested Void where testCases = []
instance Tested Bool where testCases = []
instance Tested Int where testCases = []
instance Tested Word8 where testCases = []
instance Tested Word16 where testCases = []
instance Tested Word32 where testCases = []
instance Tested Word64 where testCases = []
instance Tested Float where testCases = []
instance Tested Double where testCases = []
instance Tested () where testCases = []
instance Tested Char where testCases = []
instance Tested T.Text where testCases = []
instance Tested B.ByteString where testCases = []
instance Tested a => Tested (Identity a) where
  testCases = testCases @ a
instance Tested a => Tested (S.Seq a) where testCases = []
instance (Ord a, Tested a) => Tested (Set.Set a) where testCases = []
instance Tested a => Tested [a] where testCases = []
instance (Tested a, Tested b) => Tested (Either a b) where testCases = []
instance (Tested a, Tested b) => Tested (a, b) where testCases = []
instance (Tested a, Tested b, Tested c) => Tested (a, b, c) where testCases = []
instance (Tested a, Tested b, Tested c, Tested d) => Tested (a, b, c, d) where testCases = []
instance Tested a => Tested (V.Vector a) where testCases = []
instance (UV.Unbox a, Tested a) => Tested (UV.Vector a) where testCases = []
instance (Hashable k, Tested k, Tested a) => Tested (HM.HashMap k a) where testCases = []
instance (Ord k, Tested k, Tested a) => Tested (M.Map k a) where testCases = []
instance Tested a => TestGen (Maybe a)
instance Tested a => Tested (Maybe a) where testCases = []

-- | Generate test cases and print them to the standard output.
printTests :: forall a. (TestGen a, Serialise a, Show a) => IO ()
printTests = putStrLn $ showTests (genTestCases :: [a])

buildTestGroups :: forall a. (TestGen a, Serialise a) => [(Schema, [(B.ByteString, a)])]
buildTestGroups = [(schema (Proxy @ a), [(serialiseOnly a, a) | a <- genTestCases :: [a]])]

showTests :: (Serialise a, Show a) => [a] -> String
showTests xs = showListWith ppTest xs ""

ppTest :: (Serialise a, Show a) => a -> ShowS
ppTest a = showString "testCase "
  . showsPrec 11 (schema [a])
  . showChar ' '
  . showsPrec 11 (serialiseOnly a)
  . showChar ' '
  . showsPrec 11 a

-- | A class to provide test values and gather tests for its components.
-- It is recommended to use the generic default methods.
class Typeable a => TestGen a where
  -- | A list of values that can be used as test cases.
  -- It should contain at least one value as long as there is a non-bottom value
  -- in the type.
  genTestCases :: [a]

  -- | Inherited set of test cases for each type it involves.
  inheritedTests :: Proxy a -> M.Map TypeRep [Test]

  default genTestCases :: (Generic a, GTestGen (Rep a)) => [a]
  genTestCases = fmap to ggenTestCases

  default inheritedTests :: (GTestGen (Rep a)) => Proxy a -> M.Map TypeRep [Test]
  inheritedTests _ = ginheritedTests (Proxy @ (Rep a))

class GTestGen f where
  ggenTestCases :: [f x]
  ginheritedTests :: proxy f -> M.Map TypeRep [Test]

instance GTestGen V1 where
  ggenTestCases = mempty
  ginheritedTests _ = mempty

instance GTestGen U1 where
  ggenTestCases = [U1]
  ginheritedTests _ = mempty

instance GTestGen f => GTestGen (Rec1 f) where
  ggenTestCases = fmap Rec1 ggenTestCases
  ginheritedTests _ = ginheritedTests (Proxy @ f)

instance (Tested c, TestGen c) => GTestGen (K1 i c) where
  ggenTestCases = fmap K1 genTestCases
  ginheritedTests _ = allTests @ c

instance GTestGen f => GTestGen (M1 i c f) where
  ggenTestCases = fmap M1 ggenTestCases
  ginheritedTests _ = ginheritedTests (Proxy @ f)

instance (GTestGen f, GTestGen g) => GTestGen (f :+: g) where
  ggenTestCases = fmap L1 ggenTestCases ++ fmap R1 ggenTestCases
  ginheritedTests _ = ginheritedTests (Proxy @ f)
    `mappend` ginheritedTests (Proxy @ g)

instance (GTestGen f, GTestGen g) => GTestGen (f :*: g) where
  ggenTestCases = ((:*:) <$> ggenTestCases <*> xs)
    ++ ((:*:) <$> take 1 ggenTestCases <*> ys)
    where
      (xs, ys) = splitAt 1 ggenTestCases
  ginheritedTests _ = ginheritedTests (Proxy @ f)
    `mappend` ginheritedTests (Proxy @ g)

deriving instance TestGen a => TestGen (Identity a)
instance TestGen ()
instance TestGen Bool
instance (Tested a, Tested b) => TestGen (a, b)
instance (Tested a, Tested b, Tested c) => TestGen (a, b, c)
instance (Tested a, Tested b, Tested c, Tested d) => TestGen (a, b, c, d)
instance (Tested a, Tested b) => TestGen (Either a b)

instance Tested a => TestGen [a] where
  genTestCases = [[]]
  inheritedTests _ = allTests @ a

instance (Ord a, Tested a) => TestGen (Set.Set a) where
  genTestCases = [mempty]
  inheritedTests _ = allTests @ a

instance Tested a => TestGen (S.Seq a) where
  genTestCases = [mempty]
  inheritedTests _ = allTests @ a

instance Tested a => TestGen (V.Vector a) where
  genTestCases = [V.empty]
  inheritedTests _ = allTests @ a

instance (UV.Unbox a, Tested a) => TestGen (UV.Vector a) where
  genTestCases = [UV.empty]
  inheritedTests _ = allTests @ a

instance (Hashable k, Tested k, Tested a) => TestGen (HM.HashMap k a) where
  genTestCases = HM.singleton <$> genTestCases <*> genTestCases
  inheritedTests _ = allTests @ k `mappend` allTests @ a

instance (Ord k, Tested k, Tested a) => TestGen (M.Map k a) where
  genTestCases = [M.empty]
  inheritedTests _ = allTests @ k `mappend` allTests @ a

instance TestGen Void where
  genTestCases = []
  inheritedTests = mempty

instance TestGen Int where
  genTestCases = [42]
  inheritedTests = mempty

instance TestGen Word8 where
  genTestCases = [8]
  inheritedTests = mempty

instance TestGen Word16 where
  genTestCases = [16]
  inheritedTests = mempty

instance TestGen Word32 where
  genTestCases = [32]
  inheritedTests = mempty

instance TestGen Word64 where
  genTestCases = [64]
  inheritedTests = mempty

instance TestGen Float where
  genTestCases = [0.1]

  inheritedTests = mempty
instance TestGen Double where
  genTestCases = [0.2]
  inheritedTests = mempty

instance TestGen Char where
  genTestCases = ['X']
  inheritedTests = mempty

instance TestGen T.Text where
  genTestCases = ["葡"]
  inheritedTests = mempty

instance TestGen B.ByteString where
  genTestCases = ["B"]
  inheritedTests = mempty
