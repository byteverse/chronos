{-# LANGUAGE OverloadedStrings #-}

module Chronos.Datetime.ByteString.Char7 where

import Chronos.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.ByteString (Parser)
import Control.Monad
import Data.Foldable
import qualified Chronos.Date.ByteString.Char7 as Date
import qualified Chronos.TimeOfDay.ByteString.Char7 as TimeOfDay
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy.Builder as Builder

encode_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> ByteString
encode_YmdHMS sp format =
  LByteString.toStrict . Builder.toLazyByteString . builder_YmdHMS sp format

encode_YmdIMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> ByteString
encode_YmdIMS_p a sp b = LByteString.toStrict . Builder.toLazyByteString . builder_YmdIMS_p a sp b

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> Date.builder_Ymd mdateSep date
            <> TimeOfDay.builder_HMS sp mtimeSep time
    Just sep -> Date.builder_Ymd mdateSep date
             <> Builder.char7 sep
             <> TimeOfDay.builder_HMS sp mtimeSep time

builder_YmdIMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.char7 msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time

builder_YmdIMSp :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Builder
builder_YmdIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     Date.builder_Ymd mdateSep date
  <> maybe mempty Builder.char7 msep
  <> TimeOfDay.builder_IMS_p locale sp mtimeSep time


builderW3 :: Datetime -> Builder
builderW3 = builder_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

decode_YmdHMS :: DatetimeFormat -> ByteString -> Maybe Datetime
decode_YmdHMS format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS format)

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

decode_YmdHMS_opt_S :: DatetimeFormat -> ByteString -> Maybe Datetime
decode_YmdHMS_opt_S format =
  either (const Nothing) Just . Atto.parseOnly (parser_YmdHMS_opt_S format)





