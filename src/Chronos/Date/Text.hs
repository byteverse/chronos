{-# LANGUAGE OverloadedStrings #-}

module Chronos.Date.Text where

import Chronos.Types
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Data.Foldable
import qualified Chronos.Internal.Format as Format
import qualified Chronos.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_Ymd :: Maybe Char -> Date -> Builder
builder_Ymd msep (Date (Year y) m d) = case msep of
  Nothing ->
       Builder.decimal y
    <> Format.monthToZeroPaddedDigit m
    <> Format.zeroPadDayOfMonth d
  Just sep -> let sepBuilder = Builder.singleton sep in
       Builder.decimal y
    <> sepBuilder
    <> Format.monthToZeroPaddedDigit m
    <> sepBuilder
    <> Format.zeroPadDayOfMonth d

parser_Ymd :: Maybe Char -> Parser Date
parser_Ymd msep = do
  y <- I.parseFixedDigits 4
  traverse_ Atto.char msep
  m <- I.parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ Atto.char msep
  d <- I.parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  return (Date (Year y) (Month $ m - 1) (DayOfMonth d))


