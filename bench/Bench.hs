{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Vector.Unboxed as UVector
import Criterion.Main
import Data.Int
import Data.Maybe (fromMaybe)
import Control.DeepSeq (($!!), NFData(..), deepseq)
import qualified Data.ByteString.Char8 as BS8
import Test.QuickCheck (arbitrary, generate)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char)
import qualified Data.Attoparsec.Zepto as Z
import Data.Foldable (traverse_)
import Data.Word
import Control.Monad (when)
import Control.Applicative

import qualified Data.Time as Time
import qualified Data.Thyme as Thyme
import qualified System.Locale as Thyme
import qualified Chronos

main :: IO ()
main = do
  utcthyme <- generate arbitrary :: IO Thyme.UTCTime
  let
    isoFormatString = "%Y-%m-%dT%H:%M:%S"
    renderIsoTime = Thyme.formatTime Thyme.defaultTimeLocale isoFormatString
    timeParser :: String -> Time.UTCTime
    timeParser =
      fromMaybe (error "Failed to parse in timeParser")
      . Time.parseTimeM True Time.defaultTimeLocale isoFormatString
    thymeParser :: String -> Thyme.UTCTime
    thymeParser =
      fromMaybe (error "Failed to parse in thymeParser")
      . Thyme.parseTime Thyme.defaultTimeLocale isoFormatString
    thymeAttoparsec :: BS8.ByteString -> Thyme.UTCTime
    thymeAttoparsec =
      Thyme.buildTime @Thyme.UTCTime
      . either error id
      . parseOnly (Thyme.timeParser Thyme.defaultTimeLocale isoFormatString)
    chronosAttoparsec :: BS8.ByteString -> Chronos.Datetime
    chronosAttoparsec =
      either error id
      . parseOnly (Chronos.parserUtf8_YmdHMS Chronos.w3c)
    chronosZepto :: BS8.ByteString -> Chronos.Datetime
    chronosZepto =
      either error id
      . Z.parse (zparserUtf8_YmdHMS Chronos.w3c)


  string <- return $!! renderIsoTime utcthyme
  bytestring <- return $!! BS8.pack (renderIsoTime utcthyme)

  defaultMain
    [ bgroup "parsing"
      [ bench "Time.parseTimeM"           $ nf timeParser        string
      , bench "Thyme.parseTime"           $ nf thymeParser       string
      , bench "Thyme.timeParser"          $ nf thymeAttoparsec   bytestring
      , bench "Chronos.parserUtf8_YmdHMS" $ nf chronosAttoparsec bytestring
      , bench "Chronos.zeptoUtf8_YmdHMS"  $ nf chronosZepto      bytestring
      ]
    ]

zparserUtf8_YmdHMS :: Chronos.DatetimeFormat -> Z.Parser Chronos.Datetime
zparserUtf8_YmdHMS (Chronos.DatetimeFormat mdateSep msep' mtimeSep) = do
  date <- zparserUtf8_Ymd mdateSep
  let msep = BS8.singleton <$> msep'
  traverse_ Z.string msep
  time <- zparserUtf8_HMS mtimeSep
  return (Chronos.Datetime date time)


countZeroes :: Z.Parser Int
countZeroes = do
    bs <- Z.takeWhile (0x30 ==)
    pure $!! BS8.length bs

zparserUtf8_Ymd :: Maybe Char -> Z.Parser Chronos.Date
zparserUtf8_Ymd msep' = do
  y <- parseFixedDigitsIntBS 4
  let msep = BS8.singleton <$> msep'
  traverse_ Z.string msep
  m <- parseFixedDigitsIntBS 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ Z.string msep
  d <- parseFixedDigitsIntBS 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  return (Chronos.Date (Chronos.Year y) (Chronos.Month $ m - 1) (Chronos.DayOfMonth d))

zparserUtf8_HMS :: Maybe Char -> Z.Parser Chronos.TimeOfDay
zparserUtf8_HMS msep' = do
  h <- parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  let msep = BS8.singleton <$> msep'
  traverse_ Z.string msep
  m <- parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ Z.string msep
  ns <- parseSecondsAndNanosecondsUtf8
  return (Chronos.TimeOfDay h m ns)

parseFixedDigitsIntBS :: Int -> Z.Parser Int
parseFixedDigitsIntBS n = do
  t <- Z.take n
  case BS8.readInt t of
    Nothing -> fail "datetime decoding could not parse integral bytestring (a)"
    Just (i,r) -> if BS8.null r
      then return i
      else fail "datetime decoding could not parse integral bytestring (b)"


parseSecondsAndNanosecondsUtf8 :: Z.Parser Int64
parseSecondsAndNanosecondsUtf8 = do
  s' <- parseFixedDigitsIntBS 2
  let s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- Z.string "."
         numberOfZeroes <- countZeroes
         x <- zdecimal
         let totalDigits = countDigits x + numberOfZeroes
             result = if totalDigits == 9
               then x
               else if totalDigits < 9
                 then x * raiseTenTo (9 - totalDigits)
                 else quot x (raiseTenTo (totalDigits - 9))
         return (fromIntegral result)
    ) <|> return 0
  return (s * 1000000000 + nanoseconds)

zdecimal :: Z.Parser Int64
zdecimal = do
    digits <- Z.takeWhile wordIsDigit
    case BS8.readInt digits of
      Nothing -> fail "somehow this didn't work"
      Just (i,_) -> pure $!! fromIntegral i

wordIsDigit :: Word8 -> Bool
wordIsDigit a = 0x30 <= a && a <= 0x39

instance NFData Chronos.Datetime where
  rnf (Chronos.Datetime a b) = a `deepseq` b `deepseq` ()

instance NFData Chronos.Date where
  rnf (Chronos.Date y m d) = y `deepseq` m `deepseq` d `deepseq` ()

instance NFData Chronos.TimeOfDay where
  rnf (Chronos.TimeOfDay h m s) = h `deepseq` m `deepseq` s `deepseq` ()

deriving instance NFData Chronos.DayOfMonth
deriving instance NFData Chronos.Month
deriving instance NFData Chronos.Year
deriving instance NFData Chronos.Day


countDigits :: (Integral a) => a -> Int
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

raiseTenTo :: Int -> Int64
raiseTenTo i = if i > 15
  then 10 ^ i
  else UVector.unsafeIndex tenRaisedToSmallPowers i


tenRaisedToSmallPowers :: UVector.Vector Int64
tenRaisedToSmallPowers = UVector.fromList $ map (10 ^) [0 :: Int ..15]
