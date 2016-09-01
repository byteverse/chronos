module Chronos.Calendar
  ( -- * Enumerations
    months
  , weekdays
    -- * Pattern Matching
  , month
  , dayOfWeek
    -- * Days of Week
  , sunday
  , monday
  , tuesday
  , wednesday
  , thursday
  , friday
  , saturday
    -- * Months
  , january
  , february
  , march
  , april
  , may
  , june
  , july
  , august
  , september
  , october
  , november
  , december
  ) where

import Chronos.Types
import qualified Data.Vector as Vector

month :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Month -> a
month a b c d e f g h i j k l =
  let v = Vector.fromList [a,b,c,d,e,f,g,h,i,j,k,l]
   in \(Month ix) -> Vector.unsafeIndex v (fromIntegral ix)

dayOfWeek :: a -> a -> a -> a -> a -> a -> a -> DayOfWeek -> a
dayOfWeek a b c d e f g =
  let v = Vector.fromList [a,b,c,d,e,f,g]
   in \(DayOfWeek ix) -> Vector.unsafeIndex v (fromIntegral ix)

months :: [Month]
months = map Month [0..11]

weekdays :: [DayOfWeek]
weekdays = map DayOfWeek [0..6]

january :: Month
january = Month 0

february :: Month
february = Month 1

march :: Month
march = Month 2

april :: Month
april = Month 3

may :: Month
may = Month 4

june :: Month
june = Month 5

july :: Month
july = Month 6

august :: Month
august = Month 7

september :: Month
september = Month 8

october :: Month
october = Month 9

november :: Month
november = Month 10

december :: Month
december = Month 11

sunday :: DayOfWeek
sunday = DayOfWeek 0

monday :: DayOfWeek
monday = DayOfWeek 1

tuesday :: DayOfWeek
tuesday = DayOfWeek 2

wednesday :: DayOfWeek
wednesday = DayOfWeek 3

thursday :: DayOfWeek
thursday = DayOfWeek 4

friday :: DayOfWeek
friday = DayOfWeek 5

saturday :: DayOfWeek
saturday = DayOfWeek 6

