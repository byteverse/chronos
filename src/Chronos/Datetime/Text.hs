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
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

-- | This could be written much more efficiently since we know the
--   exact size the resulting 'Text' will be.
builder_YmdHMS ::
     Maybe Char -- ^ Separator in the date
  -> Maybe Char -- ^ Separator between date and time
  -> Maybe Char -- ^ Separator in the time
  -> Datetime
  -> Builder
builder_YmdHMS mdateSep msep mtimeSep (Datetime date time) = case msep of
  Nothing -> Date.builder_Ymd mdateSep date
          <> TimeOfDay.builder_HMS mtimeSep time
  Just sep -> Date.builder_Ymd mdateSep date
           <> Builder.singleton sep
           <> TimeOfDay.builder_HMS mtimeSep time

builderW3 :: Datetime -> Builder
builderW3 = builder_YmdHMS (Just '-') (Just 'T') (Just ':')

parser_YmdHMS ::
     Maybe Char -- ^ Separator in the date
  -> Maybe Char -- ^ Separator between date and time
  -> Maybe Char -- ^ Separator in the time
  -> Parser Datetime
parser_YmdHMS mdateSep msep mtimeSep = do
  date <- Date.parser_Ymd mdateSep
  traverse_ Atto.char msep
  time <- TimeOfDay.parser_HMS mtimeSep
  return (Datetime date time)

