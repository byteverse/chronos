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
import Data.Text.Lazy.Builder (toLazyText)

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
    -- With chronos, we explicitly convert the ymdhms time to the
    -- nanoseconds since the epoch. This is done to make the benchmark
    -- compare apples to apples. Both thyme and time are computing epoch
    -- times, so we want chronos to do the same.
    chronosAttoparsec :: BS8.ByteString -> Chronos.Time
    chronosAttoparsec =
      either error Chronos.datetimeToTime
      . parseOnly (Chronos.parserUtf8_YmdHMS Chronos.w3c)
    chronosZepto :: BS8.ByteString -> Chronos.Time
    chronosZepto =
      either error Chronos.datetimeToTime
      . Z.parse (Chronos.zeptoUtf8_YmdHMS Chronos.w3c)

    dmy              = "%d:%m:%y."
    hms              = "%%H:%M:%S."
    dmyhms           = isoFormatString
    timePretty       = Time.formatTime Time.defaultTimeLocale
    thymePretty      = Thyme.formatTime Thyme.defaultTimeLocale
    chronosPrettyDmy = toLazyText
      . Chronos.builder_Dmy (Just ':')
      . Chronos.datetimeDate
      . Chronos.timeToDatetime
    chronosPrettyHMS = toLazyText
      . Chronos.builder_HMS Chronos.SubsecondPrecisionAuto (Just ':')
      . Chronos.datetimeTime
      . Chronos.timeToDatetime
    chronosPrettyYmdHMS = toLazyText
      . Chronos.builder_YmdHMS Chronos.SubsecondPrecisionAuto Chronos.w3c
      . Chronos.timeToDatetime

  timeTime  <- Time.getCurrentTime
  thymeTime <- Thyme.getCurrentTime
  chronosTime <- Chronos.now

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

    , bgroup "prettyPrint"
      [ bgroup "dmy"
        [ bench "Time.formatTime"     $ nf (timePretty dmy)  timeTime
        , bench "Thyme.formatTime"    $ nf (thymePretty dmy) thymeTime
        , bench "Chronos.builder_Dmy" $ nf chronosPrettyDmy  chronosTime
        ]
      , bgroup "HMS"
        [ bench "Time.formatTime"     $ nf (timePretty hms)  timeTime
        , bench "Thyme.formatTime"    $ nf (thymePretty hms) thymeTime
        , bench "Chronos.builder_HMS" $ nf chronosPrettyHMS  chronosTime
        ]
      , bgroup "YmdHMS"
        [ bench "Time.formatTime"        $ nf (timePretty dmyhms)  timeTime
        , bench "Thyme.formatTime"       $ nf (thymePretty dmyhms) thymeTime
        , bench "Chronos.builder_YmdHMS" $ nf chronosPrettyYmdHMS  chronosTime
        ]
      ]
    ]

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
deriving instance NFData Chronos.Time
