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
  ( varInt
  , Decoder(..)
  , evalDecoder
  , decodeVarInt
  , getWord8
  , getWord16
  , getWord32
  , getWord64
  , DecodeException(..)
  , indexDefault
  , unsafeIndexV
  , Strategy(..)
  , StrategyError
  , errorStrategy
  , TransFusion(..)
  )where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.ByteString.FastBuilder as BB
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Internal as B
import Data.Bits
import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Vector.Unboxed as U
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import System.Endian

varInt :: Int -> BB.Builder
varInt n
  | bits > 56 = BB.word8 0 <> BB.int64LE (fromIntegral n)
  | otherwise = BB.primFixed (BP.fixedPrim (1 + bytes) (go 0)) ((2 * n + 1) `unsafeShiftL` bytes)
  where
    bits = 64 - countLeadingZeros (n .|. 1)
    bytes = (bits - 1) `div` 7
    go !i !x !ptr = do
      pokeByteOff ptr i (fromIntegral (x .&. 0xff) :: Word8)
      when (i <= bytes) $ go (i + 1) (x `unsafeShiftR` 8) ptr
{-# INLINABLE varInt #-}

-- | The decoder monad. The reason being not @State@ from transformers is to
-- allow coercion for newtype deriving and DerivingVia.
newtype Decoder a = Decoder { runDecoder :: B.ByteString -> (a, B.ByteString) }
  deriving Functor

evalDecoder :: Decoder a -> B.ByteString -> a
evalDecoder m = fst . runDecoder m
{-# INLINE evalDecoder #-}

instance Applicative Decoder where
  pure a = Decoder $ \bs -> (a, bs)
  m <*> k = Decoder $ \bs -> case runDecoder m bs of
    (f, bs') -> case runDecoder k bs' of
      (a, bs'') -> (f a, bs'')

instance Monad Decoder where
  m >>= k = Decoder $ \bs -> case runDecoder m bs of
    (a, bs') -> runDecoder (k a) bs'

getWord8 :: Decoder Word8
getWord8 = Decoder $ \bs -> case B.uncons bs of
  Nothing -> throw InsufficientInput
  Just (x, bs') -> (x, bs')
{-# INLINE getWord8 #-}

data DecodeException = InsufficientInput
  | InvalidTag deriving (Eq, Show, Read)
instance Exception DecodeException

decodeVarInt :: Decoder Int
decodeVarInt = Decoder $ \(B.PS fp ofs len) -> B.accursedUnutterablePerformIO
  $ withForeignPtr fp $ \ptr -> do
    when (len == 0) $ throwIO InsufficientInput
    prefix <- peekByteOff ptr ofs
    let bytes = countTrailingZeros (prefix :: Word8)
    when (bytes >= len) $ throwIO InsufficientInput
    let go !n !x
          | n == 0 = return (x `unsafeShiftL` (7 - bytes)
            .|. fromIntegral (prefix `unsafeShiftR` (bytes + 1))
            , B.PS fp (ofs + bytes + 1) (len - bytes - 1))
          | otherwise = do
            w <- peekByteOff ptr (ofs + n)
            go (n - 1) (unsafeShiftL x 8 .|. fromIntegral (w :: Word8))
    if bytes == 8
      then do
        x <- fromIntegral . fromLE64 <$> peekByteOff ptr (ofs + 1)
        return (x, B.PS fp (ofs + 9) (len - 9))
      else go bytes 0

getWord16 :: Decoder Word16
getWord16 = Decoder $ \(B.PS fp ofs len) -> if len >= 2
  then (B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE16 <$> peekByteOff ptr ofs, B.PS fp (ofs + 2) (len - 2))
  else throw InsufficientInput

getWord32 :: Decoder Word32
getWord32 = Decoder $ \(B.PS fp ofs len) -> if len >= 4
  then (B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE32 <$> peekByteOff ptr ofs, B.PS fp (ofs + 4) (len - 4))
  else throw InsufficientInput

getWord64 :: Decoder Word64
getWord64 = Decoder $ \(B.PS fp ofs len) -> if len >= 8
  then (B.accursedUnutterablePerformIO $ withForeignPtr fp
    $ \ptr -> fromLE64 <$> peekByteOff ptr ofs, B.PS fp (ofs + 8) (len - 8))
  else throw InsufficientInput

unsafeIndexV :: U.Unbox a => String -> U.Vector a -> Int -> a
unsafeIndexV err xs i
  | i >= U.length xs || i < 0 = error err
  | otherwise = U.unsafeIndex xs i
{-# INLINE unsafeIndexV #-}

indexDefault :: a -> [a] -> Int -> a
indexDefault err xs i = case drop i xs of
  x : _ -> x
  _ -> err

type StrategyError = Doc AnsiStyle

-- | A monad with @Reader [r]@ and @Either StrategyError@ combined, used internally
-- to build an extractor.
-- @r@ is used to share environment such as extractors for fixpoints.
newtype Strategy r a = Strategy { unStrategy :: [r] -> Either StrategyError a }
  deriving Functor

instance Applicative (Strategy r) where
  pure = return
  (<*>) = ap

instance Monad (Strategy r) where
  return = Strategy . const . Right
  m >>= k = Strategy $ \decs -> case unStrategy m decs of
    Right a -> unStrategy (k a) decs
    Left e -> Left e

instance Alternative (Strategy r) where
  empty = Strategy $ const $ Left "empty"
  Strategy a <|> Strategy b = Strategy $ \decs -> case a decs of
    Left _ -> b decs
    Right x -> Right x

instance MonadFix (Strategy r) where
  mfix f = Strategy $ \r -> mfix $ \a -> unStrategy (f a) r
  {-# INLINE mfix #-}

errorStrategy :: Doc AnsiStyle -> Strategy r a
errorStrategy = Strategy . const . Left

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
