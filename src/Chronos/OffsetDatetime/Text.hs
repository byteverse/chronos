{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | The naming conventions for offsets that are used in
--   function names are as follows:
--
--   * @%z@ - @z@ +hhmm numeric time zone (e.g., -0400)
--   * @%:z@ - @z1@ +hh:mm numeric time zone (e.g., -04:00)
--   * @%::z@ - @z2@ +hh:mm:ss numeric time zone (e.g., -04:00:00)
--   * @%:::z@ - @z3@ numeric time zone with : to necessary precision (e.g., -04, +05:30)

module Chronos.OffsetDatetime.Text where

import Chronos.Types
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Data.Foldable
import Data.Int
import qualified Chronos.Internal as I
import qualified Chronos.Datetime.Text as Datetime
import qualified Chronos.TimeOfDay.Text as TimeOfDay
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

builder_YmdHMSz :: OffsetFormat -> DatetimeFormat Char -> OffsetDatetime -> Builder
builder_YmdHMSz offsetFormat datetimeFormat (OffsetDatetime datetime offset) =
     Datetime.builder_YmdHMS datetimeFormat datetime
  <> offsetBuilder offsetFormat offset

parser_YmdHMSz :: OffsetFormat -> DatetimeFormat Char -> Parser OffsetDatetime
parser_YmdHMSz offsetFormat datetimeFormat = OffsetDatetime
  <$> Datetime.parser_YmdHMS datetimeFormat
  <*> offsetParser offsetFormat

builderW3 :: OffsetDatetime -> Builder
builderW3 = builder_YmdHMSz
  OffsetFormatColonOn
  (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

offsetBuilder :: OffsetFormat -> Offset -> Builder
offsetBuilder x = case x of
  OffsetFormatColonOff -> buildOffset_z
  OffsetFormatColonOn -> buildOffset_z1
  OffsetFormatSecondsPrecision -> buildOffset_z2
  OffsetFormatColonAuto -> buildOffset_z3

offsetParser :: OffsetFormat -> Parser Offset
offsetParser x = case x of
  OffsetFormatColonOff -> parseOffset_z
  OffsetFormatColonOn -> parseOffset_z1
  OffsetFormatSecondsPrecision -> parseOffset_z2
  OffsetFormatColonAuto -> parseOffset_z3

-- | True means positive, false means negative
parseSignedness :: Parser Bool
parseSignedness = do
  c <- Atto.anyChar
  if c == '-'
    then return False
    else if c == '+'
      then return True
      else fail "while parsing offset, expected [+] or [-]"
{-# INLINE parseSignedness #-}

parseOffset_z :: Parser Offset
parseOffset_z = do
  pos <- parseSignedness
  h <- I.parseFixedDigits 2
  m <- I.parseFixedDigits 2
  let !res = h * 60 + m
  return . Offset $ if pos
    then res
    else negate res

parseOffset_z1 :: Parser Offset
parseOffset_z1 = do
  pos <- parseSignedness
  h <- I.parseFixedDigits 2
  _ <- Atto.char ':'
  m <- I.parseFixedDigits 2
  let !res = h * 60 + m
  return . Offset $ if pos
    then res
    else negate res

parseOffset_z2 :: Parser Offset
parseOffset_z2 = do
  pos <- parseSignedness
  h <- I.parseFixedDigits 2
  _ <- Atto.char ':'
  m <- I.parseFixedDigits 2
  _ <- Atto.string ":00"
  let !res = h * 60 + m
  return . Offset $ if pos
    then res
    else negate res

-- | This is generous in what it accepts. If you give
--   something like +04:00 as the offset, it will be
--   allowed, even though it could be shorter.
parseOffset_z3 :: Parser Offset
parseOffset_z3 = do
  pos <- parseSignedness
  h <- I.parseFixedDigits 2
  mc <- Atto.peekChar
  case mc of
    Just ':' -> do
      _ <- Atto.anyChar -- should be a colon
      m <- I.parseFixedDigits 2
      let !res = h * 60 + m
      return . Offset $ if pos
        then res
        else negate res
    _ -> return . Offset $ if pos
      then h * 60
      else h * (-60)

buildOffset_z :: Offset -> Builder
buildOffset_z (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> I.indexTwoDigitTextBuilder a
      <> I.indexTwoDigitTextBuilder b

buildOffset_z1 :: Offset -> Builder
buildOffset_z1 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> I.indexTwoDigitTextBuilder a
      <> ":"
      <> I.indexTwoDigitTextBuilder b

buildOffset_z2 :: Offset -> Builder
buildOffset_z2 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> I.indexTwoDigitTextBuilder a
      <> ":"
      <> I.indexTwoDigitTextBuilder b
      <> ":00"

buildOffset_z3 :: Offset -> Builder
buildOffset_z3 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in if b == 0
        then prefix
          <> I.indexTwoDigitTextBuilder a
        else prefix
          <> I.indexTwoDigitTextBuilder a
          <> ":"
          <> I.indexTwoDigitTextBuilder b
