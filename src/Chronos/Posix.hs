{-# LANGUAGE BangPatterns #-}

module Chronos.Posix
  ( epoch
  , epochDay
  , dayLength
  , add
  , diff
  , now
  , toUtc
  , fromUtc
  , toDatetime
  , toOffsetDatetime
  , fromDatetime
  , fromOffsetDatetime
  , truncateToDay
  , fromDay
  ) where

import Chronos.Types
import Chronos.Internal.CTimespec
import Chronos.Internal.Conversion as Conv
import Foreign.C.Types (CLong(..),CTime(..))
import Data.Word
import Data.Int
import qualified Chronos.Nanoseconds as Nanoseconds

epoch :: PosixTime
epoch = PosixTime 0

epochDay :: Day
epochDay = Day 40587

dayLength :: Nanoseconds
dayLength = Nanoseconds 86400000000000

add :: Nanoseconds -> PosixTime -> PosixTime
add (Nanoseconds a) (PosixTime b) = PosixTime (a + b)

diff :: PosixTime -> PosixTime -> Nanoseconds
diff (PosixTime a) (PosixTime b) = Nanoseconds (a - b)

-- | This probably needs to be wrapped in a bunch of CPP like
--   the one in @time@ is.
now :: IO PosixTime
now = fmap PosixTime getPosixNanoseconds

-- | This may be wrong for dates before what we count as the
--   first modified julian day.
toUtc :: PosixTime -> UtcTime
toUtc (PosixTime i) = let (d,t) = divMod i (getNanoseconds dayLength)
 in UtcTime (Conv.addDay (fromIntegral d) epochDay) (fromIntegral t)

fromUtc :: UtcTime -> PosixTime
fromUtc (UtcTime d ns') = PosixTime $ getNanoseconds $ Nanoseconds.add
  (Nanoseconds.scale (fromIntegral (Conv.diffDay d epochDay)) dayLength)
  (if ns > dayLength then dayLength else ns)
  where ns = Nanoseconds (fromIntegral ns')

toDatetime :: PosixTime -> Datetime
toDatetime = Conv.utcTimeToDatetime . toUtc

toOffsetDatetime :: Offset -> PosixTime -> OffsetDatetime
toOffsetDatetime offset = Conv.utcTimeToOffsetDatetime offset . toUtc

fromDatetime :: Datetime -> PosixTime
fromDatetime = fromUtc . Conv.datetimeToUtcTime

fromOffsetDatetime :: OffsetDatetime -> PosixTime
fromOffsetDatetime = fromUtc . Conv.offsetDatetimeToUtcTime

truncateToDay :: PosixTime -> Day
truncateToDay (PosixTime i) = Day (fromIntegral (div i 86400000000000) + 40587)

-- | The 'PosixTime' at midnight on the given day.
fromDay :: Day -> PosixTime
fromDay (Day d) = PosixTime (fromIntegral (d - 40587) * 86400000000000)

-- | Convert a 'PosixTime' to a UTC 'Datetime'.
-- toDatetime :: PosixTime -> Datetime
-- toDatetime (PosixTime i) = let (d,t) = divMod i (getNanoseconds dayLength)
--  in UtcTime (Day.add (fromIntegral d) epochDay) (fromIntegral t)

-- ctimespecToPosixSeconds :: CTimespec -> PosixTime
-- ctimespecToPosixSeconds (MkCTimespec (CTime s) (CLong ns)) =
--   PosixTime ((s * 1000000000) + ns)

