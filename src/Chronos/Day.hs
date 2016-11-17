module Chronos.Day
  ( add
  , diff
  , today
  , tomorrow
  , yesterday
  ) where

import Chronos.Types
import Data.Int
import Chronos.Internal.Conversion as Conv
import qualified Chronos.Posix as Posix

-- | Offset by the given number of days. To accomplish
--   subtraction, provide a negative number.
add :: Int -> Day -> Day
add = Conv.addDay

-- | Take the difference between two days.
diff :: Day -> Day -> Int
diff = Conv.diffDay

-- | Gets the current POSIX day. This does not take the user\'s
--   time zone into account.
today :: IO Day
today = fmap Posix.truncateToDay Posix.now

tomorrow :: IO Day
tomorrow = fmap (add 1 . Posix.truncateToDay) Posix.now

yesterday :: IO Day
yesterday = fmap (add (-1) . Posix.truncateToDay) Posix.now

