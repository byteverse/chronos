{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main

import Control.DeepSeq (($!!))
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Short (ShortText)
import Test.QuickCheck (arbitrary, generate)

import qualified Chronos
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Short as TS
import qualified Data.Thyme as Thyme
import qualified Data.Time as Time
import qualified System.Locale as Thyme

main :: IO ()
main = do
  utcthyme <- generate arbitrary :: IO Thyme.UTCTime
  let
    renderIsoTime = Thyme.formatTime Thyme.defaultTimeLocale isoFormatString
    -- With chronos, we explicitly convert the ymdhms time to the
    -- nanoseconds since the epoch. This is done to make the benchmark
    -- compare apples to apples. Both thyme and time are computing epoch
    -- times, so we want chronos to do the same.

    dmy              = "%d:%m:%y."
    hms              = "%H:%M:%S."
    dmyhms           = isoFormatString
    timePretty       = Time.formatTime Time.defaultTimeLocale
    thymePretty      = Thyme.formatTime Thyme.defaultTimeLocale

  timeTime  <- Time.getCurrentTime
  thymeTime <- Thyme.getCurrentTime
  chronosTime <- Chronos.now

  string <- return $!! renderIsoTime utcthyme
  bytestring <- return $!! BS8.pack string
  shortText <- return $!! TS.pack string

  defaultMain
    [ bgroup "parsing"
      [ bench "Time.parseTimeM"           $ nf timeParser        string
      , bench "Thyme.parseTime"           $ nf thymeParser       string
      , bench "Thyme.timeParser"          $ nf thymeAttoparsec   bytestring
      , bench "Chronos.parserUtf8_YmdHMS" $ nf chronosAttoparsec bytestring
      , bench "Chronos.zeptoUtf8_YmdHMS"  $ nf chronosZepto      bytestring
      , bench "Chronos.decodeShortTextIso8601" $ nf chronosIso8601 shortText
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
      , bgroup "ISO-8601-Zulu"
        [ bench "Chronos.encodeShortTextIso8601Zulu" $ nf encodeChronosIso8601Zulu chronosTime
        ]
      ]
    ]

encodeChronosIso8601Zulu :: Chronos.Time -> ShortText
encodeChronosIso8601Zulu !t =
  Chronos.encodeShortTextIso8601Zulu (Chronos.timeToDatetime t)

chronosIso8601 :: ShortText -> Chronos.Time
{-# noinline chronosIso8601 #-}
chronosIso8601 !t = case Chronos.decodeShortTextIso8601Zoneless t of
  Just x -> Chronos.datetimeToTime x
  Nothing -> errorWithoutStackTrace "chronosIso8601: decode failure"

chronosZepto :: BS8.ByteString -> Chronos.Time
{-# noinline chronosZepto #-}
chronosZepto !bs = either error Chronos.datetimeToTime
  (Z.parse (Chronos.zeptoUtf8_YmdHMS Chronos.w3c) bs)


chronosPrettyYmdHMS :: Chronos.Time -> LT.Text
{-# noinline chronosPrettyYmdHMS #-}
chronosPrettyYmdHMS = toLazyText
  . Chronos.builder_YmdHMS Chronos.SubsecondPrecisionAuto Chronos.w3c
  . Chronos.timeToDatetime

chronosPrettyHMS :: Chronos.Time -> LT.Text
{-# noinline chronosPrettyHMS #-}
chronosPrettyHMS = toLazyText
  . Chronos.builder_HMS Chronos.SubsecondPrecisionAuto (Just ':')
  . Chronos.datetimeTime
  . Chronos.timeToDatetime

chronosPrettyDmy :: Chronos.Time -> LT.Text
{-# noinline chronosPrettyDmy #-}
chronosPrettyDmy = toLazyText
  . Chronos.builder_Dmy (Just ':')
  . Chronos.datetimeDate
  . Chronos.timeToDatetime

chronosAttoparsec :: BS8.ByteString -> Chronos.Time
{-# noinline chronosAttoparsec #-}
chronosAttoparsec =
    either error Chronos.datetimeToTime
  . parseOnly (Chronos.parserUtf8_YmdHMS Chronos.w3c)

timeParser :: String -> Time.UTCTime
{-# noinline timeParser #-}
timeParser =
  fromMaybe (error "Failed to parse in timeParser")
  . Time.parseTimeM True Time.defaultTimeLocale isoFormatString

thymeParser :: String -> Thyme.UTCTime
{-# noinline thymeParser #-}
thymeParser =
  fromMaybe (error "Failed to parse in thymeParser")
  . Thyme.parseTime Thyme.defaultTimeLocale isoFormatString

thymeAttoparsec :: BS8.ByteString -> Thyme.UTCTime
{-# noinline thymeAttoparsec #-}
thymeAttoparsec =
  Thyme.buildTime @Thyme.UTCTime
  . either error id
  . parseOnly (Thyme.timeParser Thyme.defaultTimeLocale isoFormatString)

isoFormatString :: String
{-# noinline isoFormatString #-}
isoFormatString = "%Y-%m-%dT%H:%M:%S"
