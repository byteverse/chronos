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

builder_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_DmyHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) = 
  case msep of
    Nothing -> Date.builder_Dmy mdateSep date
            <> TimeofDay.builder_HMS sp mtimeSep time
    Just sep -> Date.builder_Dmy mdateSep date
             <> Builder.singleton sep
             <> TimeOfDay.builder_HMS sp mtimeSep time

builder_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_DmyIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Dmy mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time

builder_DmyIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_DmyIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Dmy mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time


encode_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyHMS sp format = 
  LText.toStrict . Builder.toLazyText . builder_DmyHMS sp format

encode_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyIMS_p a sp b = LText.toStrict . Builder.toLazyText . builder_DmyIMS_p a sp b

encode_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdHMS sp format =
  LText.toStrict . Builder.toLazyText . builder_YmdHMS sp format

encode_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdIMS_p a sp b = LText.toStrict . Builder.toLazyText . builder_YmdIMS_p a sp b

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> Date.builder_Ymd mdateSep date
            <> TimeOfDay.builder_HMS sp mtimeSep time
    Just sep -> Date.builder_Ymd mdateSep date
             <> Builder.singleton sep
             <> TimeOfDay.builder_HMS sp mtimeSep time

builder_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time

builder_YmdIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.singleton msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time


builderW3 :: Datetime -> Builder
builderW3 = builder_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

decode_YmdHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS format)

parser_DmyHMS :: DatetimeForat -> Parser Datetime
parser_DmyHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Dmy mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS mtimeSep
  return (Datetime date time)

parser_DmyHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_DmyHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Dmy mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS_opt_S mtimeSep
  return (Datetime date time)

decode_DmyHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS format =
  either (const Nothing) Just . Atto.parseOnly (parser_DmyHMS format)

decode_DmyHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS_opt_S format =
  either (const Nothing) Just . Atto.parseOnly (parser_DmyHMS_opt_S format)

parser_YmdHMS :: DatetimeFormat -> Parser Datetime
parser_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Ymd mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS mtimeSep
  return (Datetime date time)

parser_YmdHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_YmdHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- Date.parser_Ymd mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS_opt_S mtimeSep
  return (Datetime date time)

decode_YmdHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS_opt_S format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS_opt_S format)




