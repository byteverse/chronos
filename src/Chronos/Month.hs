module Chronos.Month where

import Chronos.Types
import qualified Chronos.Internal.Conversion as Conv

month :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Month -> a
month a b c d e f g h i j k l =
  let theMatch = match a b c d e f g h i j k l
   in \m -> deconstruct theMatch m

match :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> MonthMatch a
match = Conv.internalBuildMonthMatch

deconstruct :: MonthMatch a -> Month -> a
deconstruct = Conv.internalMatchMonth

