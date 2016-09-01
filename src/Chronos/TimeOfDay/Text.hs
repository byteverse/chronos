{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Chronos.TimeOfDay.Text where

import Chronos.Types
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Word
import Data.Char (isDigit)
import qualified Chronos.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_HMS :: Maybe Char -> TimeOfDay -> Builder
builder_HMS msep (TimeOfDay h m us) = case msep of
  Nothing -> I.indexTwoDigitTextBuilder h
          <> I.indexTwoDigitTextBuilder m
          <> I.indexTwoDigitTextBuilder s
          <> microsecondsBuilder usRemainder
  Just sep -> let sepBuilder = Builder.singleton sep in
             I.indexTwoDigitTextBuilder h
          <> sepBuilder
          <> I.indexTwoDigitTextBuilder m
          <> sepBuilder
          <> I.indexTwoDigitTextBuilder s
          <> microsecondsBuilder usRemainder
  where
  (!s,!usRemainder) = quotRem us 1000000000

parser_HMS :: Maybe Char -> Parser TimeOfDay
parser_HMS msep = do
  h <- I.parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ Atto.char msep
  m <- I.parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ Atto.char msep
  s <- I.parseFixedDigits 2
  when (s > 60) (fail "seconds must be between 0 and 60")
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
  return (TimeOfDay h m (s * 1000000000 + nanoseconds))

countZeroes :: Parser Int
countZeroes = go 0 where
  go !i = do
    m <- Atto.peekChar
    case m of
      Nothing -> return i
      Just c -> if c == '0'
        then Atto.anyChar *> go (i + 1)
        else return i

microsecondsBuilder :: Word64 -> Builder
microsecondsBuilder w
  | w == 0 = mempty
  | w > 99999999 = "." <> Builder.decimal w
  | w > 9999999 = ".0" <> Builder.decimal w
  | w > 999999 = ".00" <> Builder.decimal w
  | w > 99999 = ".000" <> Builder.decimal w
  | w > 9999 = ".0000" <> Builder.decimal w
  | w > 999 = ".00000" <> Builder.decimal w
  | w > 99 = ".000000" <> Builder.decimal w
  | w > 9 = ".0000000" <> Builder.decimal w
  | otherwise = ".00000000" <> Builder.decimal w

