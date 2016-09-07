module Chronos.Datetime where

import Chronos.Types
import qualified Chronos.Internal.Conversion as Conv

fromYmdhms :: Int -> Int -> Int -> Int -> Int -> Int -> Datetime
fromYmdhms y m d h m' s = Datetime
  (Date
     (Year $ fromIntegral y)
     (Month mx)
     (DayOfMonth $ fromIntegral d)
  )
  (TimeOfDay
     (fromIntegral h)
     (fromIntegral m')
     (fromIntegral s * 1000000000)
  )
  where
  mx = if m >= 1 && m <= 12
    then fromIntegral (m - 1)
    else error "fromYmdhms: month must be between 1 and 12"


-- toOffsetDatetime :: Offset -> Datetime -> OffsetDatetime
-- toOffsetDatetime =
