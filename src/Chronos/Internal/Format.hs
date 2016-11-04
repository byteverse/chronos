module Chronos.Internal.Format where

import Chronos.Types
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Control.Monad
import Data.Foldable
import qualified Data.ByteString.Builder as BBuilder
import qualified Chronos.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Vector as Vector
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

monthToZeroPaddedDigit :: Month -> Builder
monthToZeroPaddedDigit (Month x) =
  I.indexTwoDigitTextBuilder (x + 1)

zeroPadDayOfMonth :: DayOfMonth -> Builder
zeroPadDayOfMonth (DayOfMonth d) = I.indexTwoDigitTextBuilder d

monthToZeroPaddedDigitBS :: Month -> BBuilder.Builder
monthToZeroPaddedDigitBS (Month x) =
  I.indexTwoDigitByteStringBuilder (x + 1)

zeroPadDayOfMonthBS :: DayOfMonth -> BBuilder.Builder
zeroPadDayOfMonthBS (DayOfMonth d) = I.indexTwoDigitByteStringBuilder d
