{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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

builderW3 :: OffsetDatetime -> Builder
builderW3 (OffsetDatetime datetime offset) =
  Datetime.builderW3 datetime <> buildOffsetW3 offset

buildOffsetW3 :: Int16 -> Builder
buildOffsetW3 i =
  let (!a,!b) = divMod i 60
   in if a < 0
     then "-"
       <> I.indexTwoDigitTextBuilder (-a)
       <> ":"
       <> I.indexTwoDigitTextBuilder b
     else "+"
       <> I.indexTwoDigitTextBuilder a
       <> ":"
       <> I.indexTwoDigitTextBuilder b


