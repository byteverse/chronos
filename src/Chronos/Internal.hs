{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chronos.Internal where

import Data.Attoparsec.Text (Parser)
import Data.Vector (Vector)
import Data.Text.Lazy.Builder (Builder)
import Data.Word
import Data.Int
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Read as Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector

parseFixedDigits :: Integral i => Int -> Parser i
parseFixedDigits n = do
  t <- Atto.take n
  case Text.decimal t of
    Left err -> fail err
    Right (i,r) -> if Text.null r
      then return i
      else fail "datetime decoding could not parse integral"

raiseTenTo :: Int -> Int64
raiseTenTo i = if i > 15
  then 10 ^ i
  else UVector.unsafeIndex tenRaisedToSmallPowers i

tenRaisedToSmallPowers :: UVector.Vector Int64
tenRaisedToSmallPowers = UVector.fromList $ map (10 ^) [0 :: Int ..15]

-- | Only provide positive numbers to this function.
indexTwoDigitTextBuilder :: Integral i => i -> Builder
indexTwoDigitTextBuilder i = if i < 100
  then Vector.unsafeIndex twoDigitTextBuilder (fromIntegral i)
  else Builder.decimal i
{-# INLINE indexTwoDigitTextBuilder #-}

twoDigitTextBuilder :: Vector Builder
twoDigitTextBuilder = Vector.fromList $ map Builder.fromText
  [ "00","01","02","03","04","05","06","07","08","09"
  , "10","11","12","13","14","15","16","17","18","19"
  , "20","21","22","23","24","25","26","27","28","29"
  , "30","31","32","33","34","35","36","37","38","39"
  , "40","41","42","43","44","45","46","47","48","49"
  , "50","51","52","53","54","55","56","57","58","59"
  , "60","61","62","63","64","65","66","67","68","69"
  , "70","71","72","73","74","75","76","77","78","79"
  , "80","81","82","83","84","85","86","87","88","89"
  , "90","91","92","93","94","95","96","97","98","99"
  ]
{-# NOINLINE twoDigitTextBuilder #-}

countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

