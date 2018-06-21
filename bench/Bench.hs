{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Control.DeepSeq (($!!), NFData(..), deepseq)
import qualified Data.ByteString.Char8 as BS8
import Test.QuickCheck (arbitrary, generate)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char)

import qualified Data.Time as Time
import qualified Data.Thyme as Thyme
import qualified System.Locale as Thyme
import qualified Chronos

main :: IO ()
main = do
  utcthyme <- generate arbitrary :: IO Thyme.UTCTime
  let
    isoFormatString = "%Y-%m-%dT%H:%M:%S%N"
    renderIsoTime = Thyme.formatTime Thyme.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%N"
  string <- return $!! renderIsoTime utcthyme
  bytestring <- return $!! BS8.pack (renderIsoTime utcthyme)

  either (error . show) pure (parseOnly parseUtcTime bytestring)
  defaultMain
    [ bgroup "parsing"
      [ bench "Thyme.parseTime" $ nf
        ((Thyme.parseTime Thyme.defaultTimeLocale isoFormatString :: String -> Maybe Thyme.UTCTime))
        string
      , bench "Time.parseTimeM" $ nf
        (Time.parseTimeM True Time.defaultTimeLocale isoFormatString :: String -> Maybe Time.UTCTime)
        string
      , bench "Thyme.timeParser" $ nf
        (fmap (Thyme.buildTime @Thyme.UTCTime) . parseOnly (Thyme.timeParser Thyme.defaultTimeLocale isoFormatString))
        bytestring
      , bench "Chronos.parser" $ nf
        (parseOnly parseUtcTime)
        bytestring
      ]
    ]

parseUtcTime :: Parser Chronos.UtcTime
parseUtcTime = do
  date <- Chronos.parserUtf8_Ymd (Just '-')
  char 'T'
  timeOfDay <- Chronos.parserUtf8_HMS (Just ':')
  pure $!!
    Chronos.UtcTime
      (Chronos.dateToDay date)
      (Chronos.timeOfDayToNanosecondsSinceMidnight timeOfDay)

instance NFData Chronos.UtcTime where
  rnf (Chronos.UtcTime a b) = a `deepseq` b `deepseq` ()

instance NFData Chronos.Day where
  rnf (Chronos.Day i) = rnf i
