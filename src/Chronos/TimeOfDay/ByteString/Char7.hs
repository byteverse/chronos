{-| Functions for encoding 'TimeOfDay' to 'ByteString'. Any encoding
    that is a superset of ASCII is compatible with the functions in
    this module. This includes UTF-8 and ISO-8859-15 but does not
    include UTF-16 or UTF-32.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Chronos.TimeOfDay.ByteString.Char7 where

import Chronos.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.ByteString.Char8 (Parser)
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Word
import Data.Int
import Data.Char (isDigit)
import qualified Data.ByteString.Char8 as ByteString
import qualified Chronos.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as Vector
import qualified Data.ByteString.Builder as Builder

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_HMS :: SubsecondPrecision -> Maybe Char -> TimeOfDay -> Builder
builder_HMS sp msep (TimeOfDay h m ns) =
     I.indexTwoDigitByteStringBuilder h
  <> internalBuilder_NS sp msep m ns

builder_IMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> Builder
builder_IMS_p meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep h ns
  <> " "
  <> internalBuilder_p meridiemLocale h

internalBuilder_I :: Int -> Builder
internalBuilder_I h =
  I.indexTwoDigitByteStringBuilder $ if h > 12
    then h - 12
    else if h == 0
      then 12
      else h

internalBuilder_p :: MeridiemLocale ByteString -> Int -> Builder
internalBuilder_p (MeridiemLocale am pm) h = if h > 11
  then Builder.byteString pm
  else Builder.byteString am

builder_IMSp :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> Builder
builder_IMSp meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep h ns
  <> internalBuilder_p meridiemLocale h

parser_HMS :: Maybe Char -> Parser TimeOfDay
parser_HMS msep = do
  h <- I.parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ Atto.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ Atto.char msep
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
  h <- I.parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ Atto.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- Atto.peekChar
  case mc of
    Nothing -> return (TimeOfDay h m 0)
    Just c -> case msep of
      Just sep -> if c == sep
        then do
          _ <- Atto.anyChar -- should be the separator
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
  s' <- I.parseFixedDigitsIntBS 2
  let !s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  let nanoseconds = 1
  nanoseconds <-
    ( do _ <- Atto.char '.'
         numberOfZeroes <- countZeroes
         x <- Atto.decimal
         let totalDigits = I.countDigits x + numberOfZeroes
             result = if totalDigits == 9
               then x
               else if totalDigits < 9
                 then x * I.raiseTenTo (9 - totalDigits)
                 else quot x (I.raiseTenTo (totalDigits - 9))
         return (fromIntegral result)
    ) <|> return 0
  return (s * 1000000000 + nanoseconds)

countZeroes :: Parser Int
countZeroes = go 0 where
  go !i = do
    m <- Atto.peekChar
    case m of
      Nothing -> return i
      Just c -> if c == '0'
        then Atto.anyChar *> go (i + 1)
        else return i

nanosecondsBuilder :: Int64 -> Builder
nanosecondsBuilder w
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

microsecondsBuilder :: Int64 -> Builder
microsecondsBuilder w
  | w == 0 = mempty
  | w > 99999 = "." <> int64Builder w
  | w > 9999 = ".0" <> int64Builder w
  | w > 999 = ".00" <> int64Builder w
  | w > 99 = ".000" <> int64Builder w
  | w > 9 = ".0000" <> int64Builder w
  | otherwise = ".00000" <> int64Builder w

millisecondsBuilder :: Int64 -> Builder
millisecondsBuilder w
  | w == 0 = mempty
  | w > 99 = "." <> int64Builder w
  | w > 9 = ".0" <> int64Builder w
  | otherwise = ".00" <> int64Builder w

prettyNanosecondsBuilder :: SubsecondPrecision -> Int64 -> Builder
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
          <> Builder.byteString (ByteString.replicate (d - I.countDigits newSubsecondPart) '0')
          <> int64Builder newSubsecondPart
  where
  (milli,milliRem) = quotRem nano 1000000
  (micro,microRem) = quotRem nano 1000

int64Builder :: Int64 -> Builder
int64Builder = Builder.integerDec . fromIntegral

internalBuilder_NS :: SubsecondPrecision -> Maybe Char -> Int -> Int64 -> Builder
internalBuilder_NS sp msep m ns = case msep of
  Nothing -> I.indexTwoDigitByteStringBuilder m
          <> I.indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  Just sep -> let sepBuilder = Builder.char7 sep in
             sepBuilder
          <> I.indexTwoDigitByteStringBuilder m
          <> sepBuilder
          <> I.indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000
  !s = fromIntegral sInt64


