{-# LANGUAGE OverloadedStrings #-}

module Chronos.Datetime.Text where

import Chronos.Types
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Data.Foldable
import qualified Chronos.Date.Text as Date
import qualified Chronos.TimeOfDay.Text as TimeOfDay
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

encode_YmdHMS :: DatetimeFormat Char -> Datetime -> Text
encode_YmdHMS format =
  LText.toStrict . Builder.toLazyText . builder_YmdHMS format

encode_YmdIMS_p :: MeridiemLocale Text -> DatetimeFormat Char -> Datetime -> Text
encode_YmdIMS_p a b = LText.toStrict . Builder.toLazyText . builder_YmdIMS_p a b

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_YmdHMS :: DatetimeFormat Char -> Datetime -> Builder
builder_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> Date.builder_Ymd mdateSep date
            <> TimeOfDay.builder_HMS mtimeSep time
    Just sep -> Date.builder_Ymd mdateSep date
             <> Builder.singleton sep
             <> TimeOfDay.builder_HMS mtimeSep time

builder_YmdIMS_p :: MeridiemLocale Text -> DatetimeFormat Char -> Datetime -> Builder
builder_YmdIMS_p locale (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale mtimeSep time

builder_YmdIMSp :: MeridiemLocale Text -> DatetimeFormat Char -> Datetime -> Builder
builder_YmdIMSp locale (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale mtimeSep time


builderW3 :: Datetime -> Builder
builderW3 = builder_YmdHMS (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

decode_YmdHMS :: DatetimeFormat Char -> Text -> Maybe Datetime
decode_YmdHMS format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS format)

parser_YmdHMS :: DatetimeFormat Char -> Parser Datetime
parser_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Ymd mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS mtimeSep
  return (Datetime date time)

parser_YmdHMS_opt_S :: DatetimeFormat Char -> Parser Datetime
parser_YmdHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Ymd mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS_opt_S mtimeSep
  return (Datetime date time)

decode_YmdHMS_opt_S :: DatetimeFormat Char -> Text -> Maybe Datetime
decode_YmdHMS_opt_S format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS_opt_S format)




