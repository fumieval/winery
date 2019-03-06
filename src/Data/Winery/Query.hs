{-# LANGUAGE DeriveFunctor #-}
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

-- | Query is a transformation between 'Extractor's.
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
invalid :: WineryException -> Query a b
invalid = Query . const . Extractor . Plan . const . throwStrategy

-- | Takes a list and traverses on it.
list :: Typeable a => Query a a
list = Query $ fmap concat . extractListBy

-- | Takes a list and enumerates elements in the specified range.
-- Like Python's array slicing, negative numbers counts from the last element.
range :: Typeable a => Int -> Int -> Query a a
range i j = Query $ fmap (\v -> foldMap id
  $ V.backpermute v (V.enumFromTo (i `mod` V.length v) (j `mod` V.length v)))
  . extractListBy

-- | Takes a record and extracts the specified field.
field :: Typeable a => T.Text -> Query a a
field name = Query $ \d -> buildExtractor $ extractFieldBy d name

-- | Takes a variant and returns a value when the constructor matches.
con :: Typeable a => T.Text -> Query a a
con name = Query $ \d -> buildExtractor $ extractConstructorBy (d, name, id) (pure [])

-- | Propagate values if the supplied 'Query' doesn't return False.
select :: Query a Bool -> Query a a
select qp = Query $ \d -> Extractor $ Plan $ \sch -> do
  p <- unwrapExtractor (runQuery qp d) sch
  dec <- unwrapExtractor d sch
  return $ \bs -> [x | and $ p bs, x <- dec bs]
