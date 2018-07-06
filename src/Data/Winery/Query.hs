{-# LANGUAGE OverloadedStrings #-}
module Data.Winery.Query where

import Control.Monad
import Data.Maybe
import Data.Winery
import qualified Data.ByteString as B
import qualified Data.Text as T

newtype Query = Query
  { runQuery :: Schema -> Strategy (Schema, B.ByteString -> [B.ByteString]) }

instance Monoid Query where
  mempty = Query $ \sch -> pure (sch, pure)
  mappend f g = Query $ \sch -> do
    (sch', f') <- runQuery f sch
    (sch'', g') <- runQuery g sch'
    return (sch'', f' >=> g')

raw :: Deserialiser (Schema, B.ByteString)
raw = Deserialiser $ Plan $ \sch -> pure $ \bs -> (sch, bs)

list :: Query
list = Query $ \sch -> unwrapDeserialiser (extractListWith raw) sch
  >>= \dec -> case dec "\1" of -- FIXME: hacky
    (sch', _) : _ -> pure (sch', fmap snd . dec)
    [] -> error "Impossible"

field :: T.Text -> Query
field name = Query $ \sch -> unwrapDeserialiser (extractFieldWith raw name) sch
  >>= \dec -> case dec B.empty of -- FIXME: hacky
    (sch', _) -> pure (sch', pure . snd . dec)

con :: T.Text -> Query
con name = Query $ \sch -> unwrapDeserialiser (extractConstructorWith raw name) sch
  >>= \dec -> case dec B.empty of -- FIXME: hacky
    Just (sch', _) -> pure (sch', maybeToList . fmap snd . dec)
    Nothing -> error "Impossible"

fromQuery :: Deserialiser a -> Query -> Deserialiser [a]
fromQuery d q = Deserialiser $ Plan $ \sch -> do
  (sch', f) <- runQuery q sch
  dec <- unwrapDeserialiser d sch'
  return (map dec . f)
