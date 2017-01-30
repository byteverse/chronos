module Chronos.Date
  ( toDay
  , fromDay
  ) where

import qualified Chronos.Internal.Conversion as Conv

toDay :: Date -> Day
toDay = Conv.dateToDay

fromDay :: Day -> Date
fromDay = Conv.dayToDate

