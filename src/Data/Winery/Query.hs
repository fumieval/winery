{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.Winery.Query
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
-- Stability   :  Experimental
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Building blocks for winery queries.
--
-----------------------------------------------------------------------------
module Data.Winery.Query (Query(..)
  , invalid
  , list
  , range
  , field
  , con
  , select) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Data.Winery
import Data.Winery.Internal
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Vector as V

-- | The type of winery queries.
--
-- Like jq, this returns a list of values.
newtype Query a b = Query
  { runQuery :: Extractor [a] -> Extractor [b] }
  deriving Functor

instance Category Query where
  id = Query id
  Query f . Query g = Query $ f . g

instance Applicative (Query a) where
  pure a = Query $ const $ pure [a]
  Query f <*> Query g = Query $ \d -> (<*>) <$> f d <*> g d

instance Alternative (Query a) where
  empty = Query $ const $ pure []
  Query f <|> Query g = Query $ \d -> (++) <$> f d <*> g d

-- | Throw an error.
invalid :: StrategyError -> Query a b
invalid = Query . const . Extractor . Plan . const . errorStrategy

-- | Takes a list and traverses on it.
list :: Query a a
list = Query $ \d -> concat <$> extractListBy d

-- | Takes a list and enumerates elements in the specified range.
-- Like Python's array slicing, negative numbers counts from the last element.
range :: Int -> Int -> Query a a
range i j = Query $ \d -> (\v -> foldMap id
  $ V.backpermute v (V.enumFromTo (i `mod` V.length v) (j `mod` V.length v)))
  <$> extractListBy d

-- | Takes a record and extracts the specified field.
field :: Typeable a => T.Text -> Query a a
field name = Query $ \d -> extractFieldBy d name

-- | Takes a variant and returns a value when the constructor matches.
con :: Typeable a => T.Text -> Query a a
con name = Query $ \d -> maybe [] id <$> extractConstructorBy d name

-- | Propagate values if the supplied 'Query' doesn't return False.
select :: Query a Bool -> Query a a
select qp = Query $ \d -> Extractor $ Plan $ \sch -> do
  p <- unwrapExtractor (runQuery qp d) sch
  dec <- unwrapExtractor d sch
  return $ \bs -> [x | and $ p bs, x <- dec bs]
