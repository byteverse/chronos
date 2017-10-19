{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}

module Chronos
  ( -- * Functions
    epoch
  , now
    -- ** Conversion
  , timeToDatetime
  , datetimeToTime
  , timeToOffsetDatetime
  , offsetDatetimeToTime
  , truncateTimeToDay
  , dayToTime
    -- ** Build Timespan
  , second
  , minute
  , hour
  , day
  , week
    -- * Textual Conversion
    -- ** Date
    -- *** Text
  , builder_Ymd
  , builder_Dmy
  , parser_Ymd
  , parser_Mdy
  , parser_Dmy
    -- *** UTF-8 ByteString
  , builderUtf8_Ymd
  , parserUtf8_Ymd
    -- ** Time of Day
    -- *** Text
  , builder_IMS_p
  , builder_IMSp
  , parser_HMS
  , parser_HMS_opt_S
    -- *** UTF-8 ByteString
  , builderUtf8_HMS
  , builderUtf8_IMS_p
  , builderUtf8_IMSp
  , parserUtf8_HMS
  , parserUtf8_HMS_opt_S
    -- ** Datetime
    -- *** Text
  , builder_DmyHMS
  , builder_DmyIMSp
  , builder_DmyIMS_p
  , builder_YmdHMS
  , builder_YmdIMSp
  , builder_YmdIMS_p
  , builderW3
  , encode_DmyHMS
  , encode_DmyIMS_p
  , encode_YmdHMS
  , encode_YmdIMS_p
  , parser_DmyHMS
  , parser_YmdHMS
  , parser_YmdHMS_opt_S
  , parser_DmyHMS_opt_S
  , decode_DmyHMS
  , decode_YmdHMS
  , decode_YmdHMS_opt_S
  , decode_DmyHMS_opt_S
  ) where

import Chronos.Types
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Data.Foldable
import Control.Applicative
import Data.Int (Int64)
import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Chronos.Internal.Format as Format
import qualified Chronos.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as LT

second :: Timespan
second = Timespan 1000000000

minute :: Timespan
minute = Timespan 60000000000

hour :: Timespan
hour = Timespan 3600000000000

day :: Timespan
day = Timespan 86400000000000

week :: Timespan
week = Timespan 604800000000000

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_Ymd :: Maybe Char -> Date -> TB.Builder
builder_Ymd msep (Date (Year y) m d) = case msep of
  Nothing ->
       TB.decimal y
    <> Format.monthToZeroPaddedDigit m
    <> Format.zeroPadDayOfMonth d
  Just sep -> let sepBuilder = TB.singleton sep in
       TB.decimal y
    <> sepBuilder
    <> Format.monthToZeroPaddedDigit m
    <> sepBuilder
    <> Format.zeroPadDayOfMonth d

builder_Dmy :: Maybe Char -> Date -> TB.Builder
builder_Dmy msep (Date (Year y) m d) = case msep of
  Nothing ->
       Format.zeroPadDayOfMonth d
    <> Format.monthToZeroPaddedDigit m
    <> TB.decimal y
  Just sep -> let sepBuilder = TB.singleton sep in
       Format.zeroPadDayOfMonth d
    <> sepBuilder
    <> Format.monthToZeroPaddedDigit m
    <> sepBuilder
    <> TB.decimal y

parser_Ymd :: Maybe Char -> Parser Date
parser_Ymd msep = do
  y <- I.parseFixedDigits 4
  traverse_ AT.char msep
  m <- I.parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  d <- I.parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))

parser_Mdy :: Maybe Char -> Parser Date
parser_Mdy msep = do
  m <- I.parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  d <- I.parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  traverse_ AT.char msep
  y <- I.parseFixedDigits 4
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))

parser_Dmy :: Maybe Char -> Parser Date
parser_Dmy msep = do
  d <- I.parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  traverse_ AT.char msep
  m <- I.parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  y <- I.parseFixedDigits 4
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))

builderUtf8_Ymd :: Maybe Char -> Date -> BB.Builder
builderUtf8_Ymd msep (Date (Year y) m d) = case msep of
  Nothing ->
       BB.intDec y
    <> Format.monthToZeroPaddedDigitBS m
    <> Format.zeroPadDayOfMonthBS d
  Just sep -> let sepBuilder = BB.char7 sep in
       BB.intDec y
    <> sepBuilder
    <> Format.monthToZeroPaddedDigitBS m
    <> sepBuilder
    <> Format.zeroPadDayOfMonthBS d

parserUtf8_Ymd :: Maybe Char -> AB.Parser Date
parserUtf8_Ymd msep = do
  y <- I.parseFixedDigitsIntBS 4
  traverse_ AB.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AB.char msep
  d <- I.parseFixedDigitsIntBS 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))


builder_HMS :: SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_HMS sp msep (TimeOfDay h m ns) =
     I.indexTwoDigitTextBuilder h
  <> internalBuilder_NS sp msep m ns

builder_IMS_p :: MeridiemLocale Text -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_IMS_p meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep m ns
  <> " "
  <> internalBuilder_p meridiemLocale h

internalBuilder_I :: Int -> TB.Builder
internalBuilder_I h =
  I.indexTwoDigitTextBuilder $ if h > 12
    then h - 12
    else if h == 0
      then 12
      else h

internalBuilder_p :: MeridiemLocale Text -> Int -> TB.Builder
internalBuilder_p (MeridiemLocale am pm) h = if h > 11
  then TB.fromText pm
  else TB.fromText am

builder_IMSp :: MeridiemLocale Text -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_IMSp meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep m ns
  <> internalBuilder_p meridiemLocale h

parser_HMS :: Maybe Char -> Parser TimeOfDay
parser_HMS msep = do
  h <- I.parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AT.char msep
  m <- I.parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ AT.char msep
  ns <- parseSecondsAndNanoseconds
  return (TimeOfDay h m ns)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_HMS_opt_S :: Maybe Char -> Parser TimeOfDay
parser_HMS_opt_S msep = do
  h <- I.parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AT.char msep
  m <- I.parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- AT.peekChar
  case mc of
    Nothing -> return (TimeOfDay h m 0)
    Just c -> case msep of
      Just sep -> if c == sep
        then do
          _ <- AT.anyChar -- should be the separator
          ns <- parseSecondsAndNanoseconds
          return (TimeOfDay h m ns)
        else return (TimeOfDay h m 0)
      -- if there is no separator, we will try to parse the
      -- remaining part as seconds. We commit to trying to
      -- parse as seconds if we see any number as the next
      -- character.
      Nothing -> if isDigit c
        then do
          ns <- parseSecondsAndNanoseconds
          return (TimeOfDay h m ns)
        else return (TimeOfDay h m 0)

parseSecondsAndNanoseconds :: Parser Int64
parseSecondsAndNanoseconds = do
  s' <- I.parseFixedDigits 2
  let s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- AT.char '.'
         numberOfZeroes <- countZeroes
         x <- AT.decimal
         let totalDigits = I.countDigits x + numberOfZeroes
             result = if totalDigits == 9
               then x
               else if totalDigits < 9
                 then x * I.raiseTenTo (9 - totalDigits)
                 else quot x (I.raiseTenTo (totalDigits - 9))
         return (fromIntegral result)
    ) <|> return 0
  return (s * 1000000000 + nanoseconds)

countZeroes :: AT.Parser Int
countZeroes = go 0 where
  go !i = do
    m <- AT.peekChar
    case m of
      Nothing -> return i
      Just c -> if c == '0'
        then AT.anyChar *> go (i + 1)
        else return i

nanosecondsBuilder :: Int64 -> TB.Builder
nanosecondsBuilder w
  | w == 0 = mempty
  | w > 99999999 = "." <> TB.decimal w
  | w > 9999999 = ".0" <> TB.decimal w
  | w > 999999 = ".00" <> TB.decimal w
  | w > 99999 = ".000" <> TB.decimal w
  | w > 9999 = ".0000" <> TB.decimal w
  | w > 999 = ".00000" <> TB.decimal w
  | w > 99 = ".000000" <> TB.decimal w
  | w > 9 = ".0000000" <> TB.decimal w
  | otherwise = ".00000000" <> TB.decimal w

microsecondsBuilder :: Int64 -> TB.Builder
microsecondsBuilder w
  | w == 0 = mempty
  | w > 99999 = "." <> TB.decimal w
  | w > 9999 = ".0" <> TB.decimal w
  | w > 999 = ".00" <> TB.decimal w
  | w > 99 = ".000" <> TB.decimal w
  | w > 9 = ".0000" <> TB.decimal w
  | otherwise = ".00000" <> TB.decimal w

millisecondsBuilder :: Int64 -> TB.Builder
millisecondsBuilder w
  | w == 0 = mempty
  | w > 99 = "." <> TB.decimal w
  | w > 9 = ".0" <> TB.decimal w
  | otherwise = ".00" <> TB.decimal w

prettyNanosecondsBuilder :: SubsecondPrecision -> Int64 -> TB.Builder
prettyNanosecondsBuilder sp nano = case sp of
  SubsecondPrecisionAuto
    | milliRem == 0 -> millisecondsBuilder milli
    | microRem == 0 -> microsecondsBuilder micro
    | otherwise -> nanosecondsBuilder nano
  SubsecondPrecisionFixed d -> if d == 0
    then mempty
    else
      let newSubsecondPart = quot nano (I.raiseTenTo (9 - d))
       in "."
          <> TB.fromText (Text.replicate (d - I.countDigits newSubsecondPart) "0")
          <> TB.decimal newSubsecondPart
  where
  (milli,milliRem) = quotRem nano 1000000
  (micro,microRem) = quotRem nano 1000

internalBuilder_NS :: SubsecondPrecision -> Maybe Char -> Int -> Int64 -> TB.Builder
internalBuilder_NS sp msep m ns = case msep of
  Nothing -> I.indexTwoDigitTextBuilder m
          <> I.indexTwoDigitTextBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  Just sep -> let sepBuilder = TB.singleton sep in
             sepBuilder
          <> I.indexTwoDigitTextBuilder m
          <> sepBuilder
          <> I.indexTwoDigitTextBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000
  !s = fromIntegral sInt64




builder_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) = 
  case msep of
    Nothing -> builder_Dmy mdateSep date
            <> builder_HMS sp mtimeSep time
    Just sep -> builder_Dmy mdateSep date
             <> TB.singleton sep
             <> builder_HMS sp mtimeSep time

builder_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Dmy mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

builder_DmyIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Dmy mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time


encode_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyHMS sp format = 
  LT.toStrict . TB.toLazyText . builder_DmyHMS sp format

encode_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyIMS_p a sp b = LT.toStrict . TB.toLazyText . builder_DmyIMS_p a sp b

encode_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdHMS sp format =
  LT.toStrict . TB.toLazyText . builder_YmdHMS sp format

encode_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdIMS_p a sp b = LT.toStrict . TB.toLazyText . builder_YmdIMS_p a sp b

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> builder_Ymd mdateSep date
            <> builder_HMS sp mtimeSep time
    Just sep -> builder_Ymd mdateSep date
             <> TB.singleton sep
             <> builder_HMS sp mtimeSep time

builder_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Ymd mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

builder_YmdIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Ymd mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time


builderW3 :: Datetime -> TB.Builder
builderW3 = builder_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

decode_YmdHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS format =
  either (const Nothing) Just . AT.parseOnly (parser_YmdHMS format)

parser_DmyHMS :: DatetimeFormat -> Parser Datetime
parser_DmyHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Dmy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS mtimeSep
  return (Datetime date time)

parser_DmyHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_DmyHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Dmy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS_opt_S mtimeSep
  return (Datetime date time)

decode_DmyHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS format =
  either (const Nothing) Just . AT.parseOnly (parser_DmyHMS format)

decode_DmyHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS_opt_S format =
  either (const Nothing) Just . AT.parseOnly (parser_DmyHMS_opt_S format)

parser_YmdHMS :: DatetimeFormat -> Parser Datetime
parser_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Ymd mdateSep
  traverse_ AT.char msep
  time <- parser_HMS mtimeSep
  return (Datetime date time)

parser_YmdHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_YmdHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Ymd mdateSep
  traverse_ AT.char msep
  time <- parser_HMS_opt_S mtimeSep
  return (Datetime date time)

decode_YmdHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS_opt_S format =
  either (const Nothing) Just . AT.parseOnly (parser_YmdHMS_opt_S format)


---------------
-- ByteString stuff
---------------

builderUtf8_HMS :: SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_HMS sp msep (TimeOfDay h m ns) =
     I.indexTwoDigitByteStringBuilder h
  <> internalBuilderUtf8_NS sp msep m ns

builderUtf8_IMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_IMS_p meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilderUtf8_I h
  <> internalBuilderUtf8_NS sp msep m ns
  <> " "
  <> internalBuilderUtf8_p meridiemLocale h

internalBuilderUtf8_I :: Int -> BB.Builder
internalBuilderUtf8_I h =
  I.indexTwoDigitByteStringBuilder $ if h > 12
    then h - 12
    else if h == 0
      then 12
      else h

internalBuilderUtf8_p :: MeridiemLocale ByteString -> Int -> BB.Builder
internalBuilderUtf8_p (MeridiemLocale am pm) h = if h > 11
  then BB.byteString pm
  else BB.byteString am

builderUtf8_IMSp :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_IMSp meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilderUtf8_I h
  <> internalBuilderUtf8_NS sp msep m ns
  <> internalBuilderUtf8_p meridiemLocale h

parserUtf8_HMS :: Maybe Char -> AB.Parser TimeOfDay
parserUtf8_HMS msep = do
  h <- I.parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AB.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ AB.char msep
  ns <- parseSecondsAndNanosecondsUtf8
  return (TimeOfDay h m ns)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parserUtf8_HMS_opt_S :: Maybe Char -> AB.Parser TimeOfDay
parserUtf8_HMS_opt_S msep = do
  h <- I.parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AB.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- AB.peekChar
  case mc of
    Nothing -> return (TimeOfDay h m 0)
    Just c -> case msep of
      Just sep -> if c == sep
        then do
          _ <- AB.anyChar -- should be the separator
          ns <- parseSecondsAndNanosecondsUtf8
          return (TimeOfDay h m ns)
        else return (TimeOfDay h m 0)
      -- if there is no separator, we will try to parse the
      -- remaining part as seconds. We commit to trying to
      -- parse as seconds if we see any number as the next
      -- character.
      Nothing -> if isDigit c
        then do
          ns <- parseSecondsAndNanosecondsUtf8
          return (TimeOfDay h m ns)
        else return (TimeOfDay h m 0)

parseSecondsAndNanosecondsUtf8 :: AB.Parser Int64
parseSecondsAndNanosecondsUtf8 = do
  s' <- I.parseFixedDigitsIntBS 2
  let !s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- AB.char '.'
         numberOfZeroes <- countZeroesUtf8
         x <- AB.decimal
         let totalDigits = I.countDigits x + numberOfZeroes
             result = if totalDigits == 9
               then x
               else if totalDigits < 9
                 then x * I.raiseTenTo (9 - totalDigits)
                 else quot x (I.raiseTenTo (totalDigits - 9))
         return (fromIntegral result)
    ) <|> return 0
  return (s * 1000000000 + nanoseconds)

countZeroesUtf8 :: AB.Parser Int
countZeroesUtf8 = go 0 where
  go !i = do
    m <- AB.peekChar
    case m of
      Nothing -> return i
      Just c -> if c == '0'
        then AB.anyChar *> go (i + 1)
        else return i

nanosecondsBuilderUtf8 :: Int64 -> BB.Builder
nanosecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99999999 = "." <> int64Builder w
  | w > 9999999 = ".0" <> int64Builder w
  | w > 999999 = ".00" <> int64Builder w
  | w > 99999 = ".000" <> int64Builder w
  | w > 9999 = ".0000" <> int64Builder w
  | w > 999 = ".00000" <> int64Builder w
  | w > 99 = ".000000" <> int64Builder w
  | w > 9 = ".0000000" <> int64Builder w
  | otherwise = ".00000000" <> int64Builder w

microsecondsBuilderUtf8 :: Int64 -> BB.Builder
microsecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99999 = "." <> int64Builder w
  | w > 9999 = ".0" <> int64Builder w
  | w > 999 = ".00" <> int64Builder w
  | w > 99 = ".000" <> int64Builder w
  | w > 9 = ".0000" <> int64Builder w
  | otherwise = ".00000" <> int64Builder w

millisecondsBuilderUtf8 :: Int64 -> BB.Builder
millisecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99 = "." <> int64Builder w
  | w > 9 = ".0" <> int64Builder w
  | otherwise = ".00" <> int64Builder w

prettyNanosecondsBuilderUtf8 :: SubsecondPrecision -> Int64 -> BB.Builder
prettyNanosecondsBuilderUtf8 sp nano = case sp of
  SubsecondPrecisionAuto
    | milliRem == 0 -> millisecondsBuilderUtf8 milli
    | microRem == 0 -> microsecondsBuilderUtf8 micro
    | otherwise -> nanosecondsBuilderUtf8 nano
  SubsecondPrecisionFixed d -> if d == 0
    then mempty
    else
      let newSubsecondPart = quot nano (I.raiseTenTo (9 - d))
       in "."
          <> BB.byteString (BC.replicate (d - I.countDigits newSubsecondPart) '0')
          <> int64Builder newSubsecondPart
  where
  (milli,milliRem) = quotRem nano 1000000
  (micro,microRem) = quotRem nano 1000

int64Builder :: Int64 -> BB.Builder
int64Builder = BB.integerDec . fromIntegral

internalBuilderUtf8_NS :: SubsecondPrecision -> Maybe Char -> Int -> Int64 -> BB.Builder
internalBuilderUtf8_NS sp msep m ns = case msep of
  Nothing -> I.indexTwoDigitByteStringBuilder m
          <> I.indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilderUtf8 sp nsRemainder
  Just sep -> let sepBuilder = BB.char7 sep in
             sepBuilder
          <> I.indexTwoDigitByteStringBuilder m
          <> sepBuilder
          <> I.indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilderUtf8 sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000
  !s = fromIntegral sInt64

