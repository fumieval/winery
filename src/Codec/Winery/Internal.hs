{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Winery.Internal
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
-- Stability   :  Experimental
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Internal functions and datatypes
--
-----------------------------------------------------------------------------
module Codec.Winery.Internal
  ( unsignedVarInt
  , varInt
  , Decoder(..)
  , DecoderResult(..)
  , evalDecoder
  , State(..)
  , evalState
  , decodeVarInt
  , decodeVarIntFinite
  , getWord8
  , getWord16
  , getWord32
  , getWord64
  , getBytes
  , DecodeException(..)
  , indexDefault
  , unsafeIndexV
  , lookupWithIndexV
  , Strategy(..)
  , throwStrategy
  , TransFusion(..)
  )where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.ByteString.FastBuilder as BB
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Prim.Internal as BPI
import Data.Bits
import Data.String
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import System.Endian

unsignedVarInt :: (Bits a, Integral a) => a -> BB.Builder
unsignedVarInt n
  | n < 0x80 = BB.word8 (fromIntegral n)
  | otherwise = BB.word8 (fromIntegral n `setBit` 7) <> uvarInt (unsafeShiftR n 7)
{-# INLINE unsignedVarInt #-}

varInt :: (Bits a, Integral a) => a -> BB.Builder
varInt n
  | n < 0 = case negate n of
    n'
      | n' < 0x40 -> BB.word8 (fromIntegral n' `setBit` 6)
      | otherwise -> BB.word8 (0xc0 .|. fromIntegral n') <> uvarInt (unsafeShiftR n' 6)
  | n < 0x40 = BB.word8 (fromIntegral n)
  | otherwise = BB.word8 (fromIntegral n `setBit` 7 `clearBit` 6) <> uvarInt (unsafeShiftR n 6)
{-# RULES "varInt/Int" varInt = varIntFinite #-}
{-# INLINEABLE[1] varInt #-}

varIntFinite :: Int -> BB.Builder
varIntFinite = BB.primBounded (BPI.boundedPrim 10 writeIntFinite)

writeWord8 :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
writeWord8 w p = do
  poke p w
  return $! plusPtr p 1

writeIntFinite :: Int -> Ptr Word8 -> IO (Ptr Word8)
writeIntFinite !n
  | n < 0 = case negate n of
    n'
      | n' < 0x40 -> writeWord8 (fromIntegral n' `setBit` 6)
      | otherwise ->
          writeWord8 (0xc0 .|. fromIntegral n') >=>
            writeUnsignedFinite pure (unsafeShiftR n' 6)
  | n < 0x40 = writeWord8 (fromIntegral n)
  | otherwise = writeWord8 (fromIntegral n `setBit` 7 `clearBit` 6) >=>
      writeUnsignedFinite pure (unsafeShiftR n 6)
{-# INLINE writeIntFinite #-}

writeUnsignedFinite :: (Ptr Word8 -> IO r) -> Int -> Ptr Word8 -> IO r
writeUnsignedFinite k = go
  where
    go m
      | m < 0x80 = writeWord8 (fromIntegral m) >=> k
      | otherwise = writeWord8 (setBit (fromIntegral m) 7) >=> go (unsafeShiftR m 7)
{-# INLINE writeUnsignedFinite #-}

uvarInt :: (Bits a, Integral a) => a -> BB.Builder
uvarInt = go where
  go m
    | m < 0x80 = BB.word8 (fromIntegral m)
    | otherwise = BB.word8 (setBit (fromIntegral m) 7) <> go (unsafeShiftR m 7)
{-# INLINE uvarInt #-}

-- | A state monad. The reason being not @State@ from transformers is to
-- allow coercion for newtype deriving and DerivingVia.
newtype State s a = State { runState :: s -> (a, s) }
  deriving Functor

evalState :: State s a -> s -> a
evalState m = fst . runState m
{-# INLINE evalState #-}

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  m <*> k = State $ \s -> case runState m s of
    (f, s') -> case runState k s' of
      (a, s'') -> (f a, s'')

instance Monad (State s) where
  m >>= k = State $ \s -> case runState m s of
    (a, s') -> runState (k a) s'

instance MonadFix (State s) where
  mfix f = State $ \s -> fix $ \ ~(a, _) -> runState (f a) s

data DecoderResult a = DecoderResult {-# UNPACK #-} !Int a deriving Functor

-- | The Decoder monad
newtype Decoder a = Decoder { unDecoder :: B.ByteString -> Int -> DecoderResult a }
  deriving Functor

instance Applicative Decoder where
  pure a = Decoder $ \_ i -> DecoderResult i a
  {-# INLINE pure #-}
  Decoder m <*> Decoder n = Decoder $ \bs i -> case m bs i of
    DecoderResult j f -> f <$> n bs j
  {-# INLINE (<*>) #-}
instance Monad Decoder where
  Decoder m >>= k = Decoder $ \bs i -> case m bs i of
    DecoderResult j a -> unDecoder (k a) bs j
  {-# INLINE (>>=) #-}

-- | Run a 'Decoder'
evalDecoder :: Decoder a -> B.ByteString -> a
evalDecoder m bs = case unDecoder m bs 0 of
  DecoderResult _ a -> a
{-# INLINE evalDecoder #-}

getWord8 :: Decoder Word8
getWord8 = Decoder $ \(B.PS fp ofs len) i -> if i >= len
  then throw InsufficientInput
  else DecoderResult (i + 1)
    $! B.accursedUnutterablePerformIO $ withForeignPtr fp $ \p -> peekByteOff p (ofs + i)
{-# INLINE getWord8 #-}

-- | Exceptions thrown by a 'Decoder'
data DecodeException = InsufficientInput
  | IntegerOverflow
  | InvalidTag deriving (Eq, Show, Read)
instance Exception DecodeException

decodeVarIntBase :: (Num a, Bits a) => Decoder a -> Decoder a
decodeVarIntBase body = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- body
      if testBit n 6
        then return $! negate $ unsafeShiftL m 6 .|. fromIntegral n .&. 0x3f
        else return $! unsafeShiftL m 6 .|. clearBit (fromIntegral n) 7
    | testBit n 6 -> return $ negate $ fromIntegral $ clearBit n 6
    | otherwise -> return $ fromIntegral n
{-# INLINE decodeVarIntBase #-}

decodeVarInt :: (Num a, Bits a) => Decoder a
decodeVarInt = decodeVarIntBase $ getWord8 >>= go
  where
    go n
      | testBit n 7 = do
        m <- getWord8 >>= go
        return $! unsafeShiftL m 7 .|. clearBit (fromIntegral n) 7
      | otherwise = return $ fromIntegral n
{-# INLINE decodeVarInt #-}

decodeVarIntFinite :: forall a. (Num a, FiniteBits a) => Decoder a
decodeVarIntFinite = decodeVarIntBase $ getWord8 >>= go 7
  where
    go w n
      | testBit n 7 = do
        m <- getWord8 >>= go (w + 7)
        return $! unsafeShiftL m 7 .|. clearBit (fromIntegral n) 7
      | w + 7 - countLeadingZeros n < finiteBitSize (0 :: a) = return $ fromIntegral n
      | otherwise = throw IntegerOverflow
{-# INLINABLE[1] decodeVarIntFinite #-}
{-# SPECIALISE decodeVarIntFinite :: Decoder Int #-}

getWord16 :: Decoder Word16
getWord16 = Decoder $ \(B.PS fp ofs len) i -> if i + 2 <= len
  then DecoderResult (i + 2)
    $ B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE16 <$> peekByteOff ptr (ofs + i)
  else throw InsufficientInput
{-# INLINE getWord16 #-}

getWord32 :: Decoder Word32
getWord32 = Decoder $ \(B.PS fp ofs len) i -> if i + 4 <= len
  then DecoderResult (i + 4)
    $ B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE32 <$> peekByteOff ptr (ofs + i)
  else throw InsufficientInput
{-# INLINE getWord32 #-}

getWord64 :: Decoder Word64
getWord64 = Decoder $ \(B.PS fp ofs len) i -> if i + 8 <= len
  then DecoderResult (i + 8)
    $ B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE64 <$> peekByteOff ptr (ofs + i)
  else throw InsufficientInput
{-# INLINE getWord64 #-}

getBytes :: Int -> Decoder B.ByteString
getBytes len = Decoder $ \bs i -> DecoderResult (i + len)
  $ B.take len $ B.drop i bs
{-# INLINE getBytes #-}

unsafeIndexV :: U.Unbox a => String -> U.Vector a -> Int -> a
unsafeIndexV err xs i
  | i >= U.length xs || i < 0 = error err
  | otherwise = U.unsafeIndex xs i
{-# INLINE unsafeIndexV #-}

lookupWithIndexV :: Eq k => k -> V.Vector (k, v) -> Maybe (Int, v)
lookupWithIndexV k v = (\i -> (i, snd $ V.unsafeIndex v i))
  <$> V.findIndex ((k==) . fst) v
{-# INLINE lookupWithIndexV #-}

indexDefault :: a -> [a] -> Int -> a
indexDefault err xs i = case drop i xs of
  x : _ -> x
  _ -> err

-- | A monad with @Reader [r]@ and @Either WineryException@ combined, used internally
-- to build an extractor.
-- @r@ is used to share environment such as extractors for fixpoints.
newtype Strategy e r a = Strategy { unStrategy :: r -> Either e a }
  deriving Functor

instance Applicative (Strategy e r) where
  pure = Strategy . const . Right
  (<*>) = ap

instance Monad (Strategy e r) where
  m >>= k = Strategy $ \decs -> case unStrategy m decs of
    Right a -> unStrategy (k a) decs
    Left e -> Left e

instance IsString e => Alternative (Strategy e r) where
  empty = Strategy $ const $ Left "empty"
  Strategy a <|> Strategy b = Strategy $ \decs -> case a decs of
    Left _ -> b decs
    Right x -> Right x

instance MonadFix (Strategy e r) where
  mfix f = Strategy $ \r -> mfix $ \a -> unStrategy (f a) r
  {-# INLINE mfix #-}

throwStrategy :: e -> Strategy e r a
throwStrategy = Strategy . const . Left

-- | A Bazaar (chain of indexed store comonad)-like structure which instead
-- works for natural transformations.
newtype TransFusion f g a = TransFusion { unTransFusion :: forall h. Applicative h => (forall x. f x -> h (g x)) -> h a }

instance Functor (TransFusion f g) where
  fmap f (TransFusion m) = TransFusion $ \k -> fmap f (m k)
  {-# INLINE fmap #-}

instance Applicative (TransFusion f g) where
  pure a = TransFusion $ \_ -> pure a
  TransFusion a <*> TransFusion b = TransFusion $ \k -> a k <*> b k
  {-# INLINE (<*>) #-}
