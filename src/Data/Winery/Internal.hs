{-# LANGUAGE LambdaCase #-}
module Data.Winery.Internal
  ( Encoding
  , encodeMulti
  , encodeVarInt
  , Decoder
  , decodeAt
  , decodeVarInt
  , decodeOffsets
  , getWord8
  , getBytes
  , word16be
  , word32be
  , word64be
  , unsafeIndex
  )where

import Control.Monad
import Control.Monad.Trans.Cont
import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Monoid
import Data.Word

type Encoding = (Sum Int, Builder)

type Decoder = (->) B.ByteString

decodeAt :: Int -> Decoder a -> Decoder a
decodeAt i m bs = m $ B.drop i bs

encodeVarInt :: (Integral a, Bits a) => a -> Encoding
encodeVarInt n
  | n < 0x80 = (1, BB.word8 $ fromIntegral n)
  | otherwise = let (s, b) = encodeVarInt (shiftR n 7)
    in (1 + s, BB.word8 (setBit (fromIntegral n) 7) `mappend` b)

getWord8 :: ContT r Decoder Word8
getWord8 = ContT $ \k bs -> case B.uncons bs of
  Nothing -> k 0 bs
  Just (x, bs') -> k x bs'

getBytes :: Decoder B.ByteString
getBytes = runContT decodeVarInt B.take

decodeVarInt :: (Num a, Bits a) => ContT r Decoder a
decodeVarInt = getWord8 >>= \case
  n | testBit n 7 -> do
      m <- decodeVarInt
      return $! shiftL m 7 .|. clearBit (fromIntegral n) 7
    | otherwise -> return $ fromIntegral n

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
encodeMulti ls = foldMap encodeVarInt offsets <> foldMap id ls where
  offsets = take (length ls - 1) $ map (getSum . fst) ls

decodeOffsets :: Int -> ContT r Decoder [Int]
decodeOffsets 0 = pure []
decodeOffsets n = scanl (+) 0 <$> replicateM (n - 1) decodeVarInt

unsafeIndex :: String -> [a] -> Int -> a
unsafeIndex err xs i = (xs ++ repeat (error err)) !! i
