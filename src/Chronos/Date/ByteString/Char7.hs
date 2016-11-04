{-# LANGUAGE OverloadedStrings #-}

module Chronos.Date.ByteString.Char7 where

import Chronos.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.ByteString.Char8 (Parser)
import Control.Monad
import Data.Foldable
import qualified Chronos.Internal.Format as Format
import qualified Chronos.Internal as I
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as Vector
import qualified Data.ByteString.Builder as Builder

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'ByteString' will be.
builder_Ymd :: Maybe Char -> Date -> Builder
builder_Ymd msep (Date (Year y) m d) = case msep of
  Nothing ->
       Builder.intDec y
    <> Format.monthToZeroPaddedDigitBS m
    <> Format.zeroPadDayOfMonthBS d
  Just sep -> let sepBuilder = Builder.char7 sep in
       Builder.intDec y
    <> sepBuilder
    <> Format.monthToZeroPaddedDigitBS m
    <> sepBuilder
    <> Format.zeroPadDayOfMonthBS d

parser_Ymd :: Maybe Char -> Parser Date
parser_Ymd msep = do
  y <- I.parseFixedDigitsIntBS 4
  traverse_ Atto.char msep
  m <- I.parseFixedDigitsIntBS 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ Atto.char msep
  d <- I.parseFixedDigitsIntBS 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))



