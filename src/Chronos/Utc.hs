module Chronos.Utc where

import Chronos.Types

toDatetime :: UtcTime -> Datetime
toDatetime = toDatetimeOffset 0

-- WORKING HERE
--
-- toDatetimeOffset :: Int8 -> UtcTime -> Datetime
-- toDatetimeOffset offset (UtcTime d ns) (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
--     m' = m + timeZoneMinutes zone
--     h' = h + (div m' 60)



