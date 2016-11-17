{-# LANGUAGE BangPatterns #-}

module Chronos.Internal.Conversion where

import Chronos.Types
import Data.Word
import Data.Int
import qualified Chronos.Internal as I
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector

dayLengthInt64 :: Int64
dayLengthInt64 = 86400000000000

nanosecondsInMinute :: Int64
nanosecondsInMinute = 60000000000

-- | The first argument in the resulting tuple in a day
--   adjustment. It should be either -1, 0, or 1, as no
--   offset should ever exceed 24 hours.
offsetTimeOfDay :: Offset -> TimeOfDay -> (Days, TimeOfDay)
offsetTimeOfDay (Offset offset) (TimeOfDay h m s) =
  (Days dayAdjustment,TimeOfDay h'' m'' s)
  where
  (!dayAdjustment, !h'') = divMod h' 24
  (!hourAdjustment, !m'') = divMod m' 60
  m' = m + offset
  h' = h + hourAdjustment

nanosecondsSinceMidnightToTimeOfDay :: Int64 -> TimeOfDay
nanosecondsSinceMidnightToTimeOfDay ns =
  if ns >= dayLengthInt64
    then TimeOfDay 23 59 (nanosecondsInMinute + (ns - dayLengthInt64))
    else TimeOfDay h' m' ns'
  where
  (!mInt64,!ns') = quotRem ns nanosecondsInMinute
  !m = fromIntegral mInt64
  (!h',!m')  = quotRem m 60

timeOfDayToNanosecondsSinceMidnight :: TimeOfDay -> Int64
timeOfDayToNanosecondsSinceMidnight (TimeOfDay h m ns) =
  fromIntegral h * 3600000000000 + fromIntegral m * 60000000000 + ns

dayToDate :: Day -> Date
dayToDate day = Date year month dayOfMonth
  where
  OrdinalDate year yd = dayToOrdinalDate day
  MonthDate month dayOfMonth = dayOfYearToMonthAndDay (isLeapYear year) yd

-- datetimeToOffsetDatetime :: Offset -> Datetime -> OffsetDatetime
-- datetimeToOffsetDatetime offset

utcTimeToOffsetDatetime :: Offset -> UtcTime -> OffsetDatetime
utcTimeToOffsetDatetime offset (UtcTime (Day d) nanoseconds) =
  let (!(Days dayAdjustment),!tod) = offsetTimeOfDay offset (nanosecondsSinceMidnightToTimeOfDay nanoseconds)
      !date = dayToDate (Day (d + dayAdjustment))
   in OffsetDatetime (Datetime date tod) offset

utcTimeToDatetime :: UtcTime -> Datetime
utcTimeToDatetime (UtcTime d nanoseconds) =
  let !tod = nanosecondsSinceMidnightToTimeOfDay nanoseconds
      !date = dayToDate d
   in Datetime date tod

datetimeToUtcTime :: Datetime -> UtcTime
datetimeToUtcTime (Datetime date timeOfDay) =
  UtcTime (dateToDay date) (timeOfDayToNanosecondsSinceMidnight timeOfDay)

offsetDatetimeToUtcTime :: OffsetDatetime -> UtcTime
offsetDatetimeToUtcTime (OffsetDatetime (Datetime date timeOfDay) (Offset off)) =
  let (!(Days !dayAdjustment),!tod) =
        offsetTimeOfDay (Offset $ negate off) timeOfDay
      !(Day !day) = dateToDay date
   in UtcTime
        (Day (day + dayAdjustment))
        (timeOfDayToNanosecondsSinceMidnight tod)

dateToDay :: Date -> Day
dateToDay (Date y m d) = ordinalDateToDay $ OrdinalDate y
  (monthDateToDayOfYear (isLeapYear y) (MonthDate m d))

monthDateToDayOfYear :: Bool -> MonthDate -> DayOfYear
monthDateToDayOfYear isLeap (MonthDate month@(Month m) (DayOfMonth dayOfMonth)) =
  DayOfYear ((div (367 * (fromIntegral m + 1) - 362) 12) + k + day')
  where
  day' = fromIntegral $ I.clip 1 (monthLength isLeap month) dayOfMonth
  k = if month < Month 2 then 0 else if isLeap then -1 else -2

ordinalDateToDay :: OrdinalDate -> Day
ordinalDateToDay (OrdinalDate year@(Year y') day) = Day mjd where
  y = y' - 1
  mjd = (fromIntegral . getDayOfYear $
           (I.clip (DayOfYear 1) (if isLeapYear year then DayOfYear 366 else DayOfYear 365) day)
        )
      + (365 * y)
      + (div y 4) - (div y 100)
      + (div y 400) - 678576

isLeapYear :: Year -> Bool
isLeapYear (Year year) = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

dayOfYearToMonthAndDay :: Bool -> DayOfYear -> MonthDate
dayOfYearToMonthAndDay isLeap dayOfYear =
  let (!upperBound,!monthTable,!dayTable) =
        if isLeap
          then (DayOfYear 366, leapYearDayOfYearMonthTable, leapYearDayOfYearDayOfMonthTable)
          else (DayOfYear 365, normalYearDayOfYearMonthTable, normalYearDayOfYearDayOfMonthTable)
      DayOfYear clippedDay = I.clip (DayOfYear 1) upperBound dayOfYear
      clippedDayInt = fromIntegral clippedDay :: Int
      month = UVector.unsafeIndex monthTable clippedDayInt
      day = UVector.unsafeIndex dayTable clippedDayInt
   in MonthDate month day

dayToOrdinalDate :: Day -> OrdinalDate
dayToOrdinalDate (Day mjd) = OrdinalDate (Year $ fromIntegral year) (DayOfYear $ fromIntegral yd) where
  a = (fromIntegral mjd :: Int64) + 678575
  quadcent = div a 146097
  b = mod a 146097
  cent = min (div b 36524) 3
  c = b - (cent * 36524)
  quad = div c 1461
  d = mod c 1461
  y = min (div d 365) 3
  yd = (d - (y * 365) + 1)
  year = quadcent * 400 + cent * 100 + quad * 4 + y + 1

internalBuildDayOfWeekMatch :: a -> a -> a -> a -> a -> a -> a -> DayOfWeekMatch a
internalBuildDayOfWeekMatch a b c d e f g =
  DayOfWeekMatch (Vector.fromList [a,b,c,d,e,f,g])

internalBuildMonthMatch :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> MonthMatch a
internalBuildMonthMatch a b c d e f g h i j k l =
  MonthMatch (Vector.fromList [a,b,c,d,e,f,g,h,i,j,k,l])

internalMatchMonth :: MonthMatch a -> Month -> a
internalMatchMonth (MonthMatch v) (Month ix) = Vector.unsafeIndex v (fromIntegral ix)

monthLength :: Bool -> Month -> Int
monthLength isLeap m = if isLeap
  then internalMatchMonth leapYearMonthLength m
  else internalMatchMonth leapYearMonthLength m

leapYearMonthLength :: MonthMatch Int
leapYearMonthLength = internalBuildMonthMatch 31 29 31 30 31 30 31 31 30 31 30 31

normalYearMonthLength :: MonthMatch Int
normalYearMonthLength = internalBuildMonthMatch 31 30 31 30 31 30 31 31 30 31 30 31

leapYearDayOfYearDayOfMonthTable :: UVector.Vector DayOfMonth
leapYearDayOfYearDayOfMonthTable = UVector.fromList $ (DayOfMonth 1:) $ concat
  [ enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 29)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  ]
{-# NOINLINE leapYearDayOfYearDayOfMonthTable #-}

normalYearDayOfYearDayOfMonthTable :: UVector.Vector DayOfMonth
normalYearDayOfYearDayOfMonthTable = UVector.fromList $ (DayOfMonth 1:) $concat
  [ enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 28)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 30)
  , enumFromTo (DayOfMonth 1) (DayOfMonth 31)
  ]
{-# NOINLINE normalYearDayOfYearDayOfMonthTable #-}

leapYearDayOfYearMonthTable :: UVector.Vector Month
leapYearDayOfYearMonthTable = UVector.fromList $ (Month 0:) $ concat
  [ replicate 31 (Month 0)
  , replicate 29 (Month 1)
  , replicate 31 (Month 2)
  , replicate 30 (Month 3)
  , replicate 31 (Month 4)
  , replicate 30 (Month 5)
  , replicate 31 (Month 6)
  , replicate 31 (Month 7)
  , replicate 30 (Month 8)
  , replicate 31 (Month 9)
  , replicate 30 (Month 10)
  , replicate 31 (Month 11)
  ]
{-# NOINLINE leapYearDayOfYearMonthTable #-}

normalYearDayOfYearMonthTable :: UVector.Vector Month
normalYearDayOfYearMonthTable = UVector.fromList $ (Month 0:) $ concat
  [ replicate 31 (Month 0)
  , replicate 28 (Month 1)
  , replicate 31 (Month 2)
  , replicate 30 (Month 3)
  , replicate 31 (Month 4)
  , replicate 30 (Month 5)
  , replicate 31 (Month 6)
  , replicate 31 (Month 7)
  , replicate 30 (Month 8)
  , replicate 31 (Month 9)
  , replicate 30 (Month 10)
  , replicate 31 (Month 11)
  ]
{-# NOINLINE normalYearDayOfYearMonthTable #-}

addDay :: Int -> Day -> Day
addDay a (Day b) = Day (a + b)
{-# INLINE addDay #-}

diffDay :: Day -> Day -> Int
diffDay (Day a) (Day b) = a - b
{-# INLINE diffDay #-}

