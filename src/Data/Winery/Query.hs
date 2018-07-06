{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
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

newtype Query a b = Query
  { runQuery :: Deserialiser [a] -> Deserialiser [b] }
  deriving Functor

instance Category Query where
  id = Query $ fmap id
  Query f . Query g = Query $ f . g

instance Applicative (Query a) where
  pure a = Query $ const $ pure [a]
  Query f <*> Query g = Query $ \d -> (<*>) <$> f d <*> g d

instance Alternative (Query a) where
  empty = Query $ const $ pure []
  Query f <|> Query g = Query $ \d -> (++) <$> f d <*> g d

invalid :: StrategyError -> Query a b
invalid = Query . const . Deserialiser . Plan . const . errorStrategy

list :: Query a a
list = Query $ \d -> concat <$> extractListWith d

range :: Int -> Int -> Query a a
range i j = Query $ \d -> (\(n, f) -> concatMap f [mod i n..mod j n]) <$> extractArrayWith d

field :: Typeable a => T.Text -> Query a a
field name = Query $ \d -> extractFieldWith d name

con :: Typeable a => T.Text -> Query a a
con name = Query $ \d -> maybe [] id <$> extractConstructorWith d name

select :: Query a Bool -> Query a a
select qp = Query $ \d -> Deserialiser $ Plan $ \sch -> do
  p <- unwrapDeserialiser (runQuery qp d) sch
  dec <- unwrapDeserialiser d sch
  return $ \bs -> [x | and $ p bs, x <- dec bs]
