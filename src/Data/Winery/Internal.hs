{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Winery.Internal
  ( Encoding(..)
  , encodeMulti
  , encodeVarInt
  , Decoder
  , decodeAt
  , decodeVarInt
  , Offsets
  , decodeOffsets
  , getWord8
  , word16be
  , word32be
  , word64be
  , unsafeIndex
  , unsafeIndexV
  , Strategy(..)
  , StrategyError
  , errorStrategy
  , TransList(..)
  , TransFusion(..)
  , runTransFusion
  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Trans.Cont
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Dynamic
import Data.List (foldl')
import Data.Monoid
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word

data Encoding = Encoding
  { encodingLength :: !Int, encodingBuilder :: !Builder }

instance Monoid Encoding where
  mempty = Encoding 0 mempty
  mappend (Encoding m a) (Encoding n b) = Encoding (m + n) (mappend a b)
  mconcat = foldl' mappend mempty

type Decoder = (->) B.ByteString

decodeAt :: (Int, Int) -> Decoder a -> Decoder a
decodeAt (i, l) m bs = m $ B.take l $ B.drop i bs

encodeVarInt :: (Bits a, Integral a) => a -> Encoding
encodeVarInt n
  | n < 0 = case negate n of
    n'
      | n' < 0x40 -> e1 (fromIntegral n' `setBit` 6)
      | otherwise -> go (e1 (0xc0 .|. fromIntegral n')) (shiftR n' 6)
  | n < 0x40 = e1 (fromIntegral n)
  | otherwise = go (e1 (fromIntegral n `setBit` 7 `clearBit` 6)) (shiftR n 6)
  where
  e1 = Encoding 1 . BB.word8
  go !acc m
    | m < 0x80 = acc `mappend` e1 (fromIntegral m)
    | otherwise = go (acc <> e1 (setBit (fromIntegral m) 7)) (shiftR m 7)
{-# INLINE encodeVarInt #-}

getWord8 :: ContT r Decoder Word8
getWord8 = ContT $ \k bs -> case B.uncons bs of
  Nothing -> k 0 bs
  Just (x, bs') -> k x $! bs'
{-# INLINE getWord8 #-}

decodeVarInt :: (Num a, Bits a) => ContT r Decoder a
decodeVarInt = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- getWord8 >>= go
      if testBit n 6
        then return $! negate $ shiftL m 6 .|. fromIntegral n .&. 0x3f
        else return $! shiftL m 6 .|. clearBit (fromIntegral n) 7
    | testBit n 6 -> return $ negate $ fromIntegral $ clearBit n 6
    | otherwise -> return $ fromIntegral n
  where
    go n
      | testBit n 7 = do
        m <- getWord8 >>= go
        return $! shiftL m 7 .|. clearBit (fromIntegral n) 7
      | otherwise = return $ fromIntegral n
{-# INLINE decodeVarInt #-}

word16be :: B.ByteString -> Word16
word16be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 8) .|.
  (fromIntegral (s `B.unsafeIndex` 1))

word32be :: B.ByteString -> Word32
word32be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 3) )

word64be :: B.ByteString -> Word64
word64be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 56) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 48) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 40) .|.
  (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 32) .|.
  (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 7) )

encodeMulti :: [Encoding] -> Encoding
encodeMulti = uncurry mappend . foldl'
  (\(p, b) e -> let !p' = p <> encodeVarInt (encodingLength e)
                    !b' = b <> e
                in (p', b'))
  (mempty, mempty)
{-# INLINE encodeMulti #-}

type Offsets = U.Vector (Int, Int)

decodeOffsets :: Int -> ContT r Decoder Offsets
decodeOffsets n = mapAccumLV (\ofs s -> (s + ofs, (ofs, s))) 0
  <$> U.replicateM n decodeVarInt

unsafeIndexV :: U.Unbox a => String -> U.Vector a -> Int -> a
unsafeIndexV err xs i
  | i >= U.length xs || i < 0 = error err
  | otherwise = U.unsafeIndex xs i
{-# INLINE unsafeIndexV #-}

unsafeIndex :: String -> [a] -> Int -> a
unsafeIndex err xs i = (xs ++ repeat (error err)) !! i

type StrategyError = Doc AnsiStyle

newtype Strategy a = Strategy { unStrategy :: [Decoder Dynamic] -> Either StrategyError a }
  deriving Functor

instance Applicative Strategy where
  pure = return
  (<*>) = ap

instance Monad Strategy where
  return = Strategy . const . Right
  m >>= k = Strategy $ \decs -> case unStrategy m decs of
    Right a -> unStrategy (k a) decs
    Left e -> Left e

instance Alternative Strategy where
  empty = Strategy $ const $ Left "empty"
  Strategy a <|> Strategy b = Strategy $ \decs -> case a decs of
    Left _ -> b decs
    Right x -> Right x

instance MonadFix Strategy where
  mfix f = Strategy $ \r -> mfix $ \a -> unStrategy (f a) r
  {-# INLINE mfix #-}

errorStrategy :: Doc AnsiStyle -> Strategy a
errorStrategy = Strategy . const . Left

newtype TransFusion f g a = TransFusion { unTransFusion :: forall h. Applicative h => (forall x. f x -> h (g x)) -> h a }

runTransFusion :: TransFusion f g a -> TransList f g a
runTransFusion (TransFusion k) = k (\f -> More f (Done id))

instance Functor (TransFusion f g) where
  fmap f (TransFusion m) = TransFusion $ \k -> fmap f (m k)
  {-# INLINE fmap #-}

instance Applicative (TransFusion f g) where
  pure a = TransFusion $ const $ pure a
  TransFusion a <*> TransFusion b = TransFusion $ \k -> a k <*> b k
  {-# INLINE (<*>) #-}

data TransList f g a = Done a | forall x. More (f x) (TransList f g (g x -> a))

deriving instance Functor (TransList f g)

instance Applicative (TransList f g) where
  pure = Done
  Done f <*> a = fmap f a
  More i k <*> c = More i (flip <$> k <*> c)

mapAccumLV :: (U.Unbox b, U.Unbox c) => (a -> b -> (a, c)) -> a -> U.Vector b -> U.Vector c
mapAccumLV f s0 xs = runST $ do
  r <- UM.unsafeNew (U.length xs)
  let go s i
        | i == U.length xs = U.unsafeFreeze r
        | otherwise = do
          let (s', y) = f s $ U.unsafeIndex xs i
          UM.write r i y
          go s' (i + 1)
  go s0 0
