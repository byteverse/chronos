{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wall -Werror #-}

{- | Data types for representing different date and time-related
     information.

     Internally, the types 'Int' and 'Int64' are used to
     represent everything. These are used even when negative
     values are not appropriate and even if a smaller fixed-size
     integer could hold the information. The only cases when
     'Int64' is used are when it is neccessary to represent values
     with numbers @2^29@ or higher. These are typically fields
     that represent nanoseconds.

     Unlike the types in the venerable @time@ library, the types
     here do not allow the user to work with all dates. Since this
     library uses fixed-precision integral values instead of 'Integer',
     all of the usual problems with overflow should be considered. Notably,
     'PosixTime' and 'TaiTime' can only be used to represent time between the years
     1680 and 2260. All other types in this library correctly represent time
     a million years before or after 1970.

     The vector unbox instances, not yet available, will store
     data in a reasonably compact manner. For example, the instance
     for 'Day' has three unboxed vectors: 'Int' for the year, 'Int8'
     for the month, and 'Int8' for the day. This only causes
     corruption of data if the user is trying to use out-of-bounds
     values for the month and the day. Users are advised to not
     use the data types provided here to model non-existent times.

-}

module Chronos.Types
  ( Day(..)
  , DayOfWeek(..)
  , DayOfMonth(..)
  , DayOfYear(..)
  , Month(..)
  , Year(..)
  , Offset(..)
  , Time(..)
  , DayOfWeekMatch(..)
  , MonthMatch(..)
  , UnboxedMonthMatch(..)
  , Timespan(..)
  , SubsecondPrecision(..)
  , Date(..)
  , OrdinalDate(..)
  , MonthDate(..)
  , Datetime(..)
  , OffsetDatetime(..)
  , TimeOfDay(..)
  , DatetimeFormat(..)
  , OffsetFormat(..)
  , DatetimeLocale(..)
  , MeridiemLocale(..)
  ) where

import Torsor
import Data.Int
import Data.Vector (Vector)
import Data.Aeson (FromJSON,ToJSON)
import Data.Hashable (Hashable)
import Data.Primitive
import Foreign.Storable
import Control.Monad
import GHC.Generics (Generic)
import Data.Monoid (Monoid(..))
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.Vector.Generic.Mutable as MGVector

-- | A day represented as the modified Julian date, the number of days
--   since midnight on November 17, 1858.
newtype Day = Day { getDay :: Int }
  deriving (Show,Read,Eq,Ord,Hashable,Enum,ToJSON,FromJSON,Storable,Prim)

instance Torsor Day Int where
  add i (Day d) = Day (d + i)
  difference (Day a) (Day b) = a - b

-- | The day of the week.
newtype DayOfWeek = DayOfWeek { getDayOfWeek :: Int }
  deriving (Show,Read,Eq,Ord,Hashable)

-- | The day of the month.
newtype DayOfMonth = DayOfMonth { getDayOfMonth :: Int }
  deriving (Show,Read,Eq,Ord,Prim,Enum)

-- | The day of the year.
newtype DayOfYear = DayOfYear { getDayOfYear :: Int }
  deriving (Show,Read,Eq,Ord,Prim)

-- | The month of the year.
newtype Month = Month { getMonth :: Int }
  deriving (Show,Read,Eq,Ord,Prim)

instance Bounded Month where
  minBound = Month 0
  maxBound = Month 11

-- | The number of years elapsed since the beginning
--   of the Common Era.
newtype Year = Year { getYear :: Int }
  deriving (Show,Read,Eq,Ord)

newtype Offset = Offset { getOffset :: Int }
  deriving (Show,Read,Eq,Ord)

-- | POSIX time with nanosecond resolution.
newtype Time = Time { getTime :: Int64 }
  deriving (FromJSON,ToJSON,Hashable,Eq,Ord,Show,Read,Storable,Prim)

newtype DayOfWeekMatch a = DayOfWeekMatch { getDayOfWeekMatch :: Vector a }

newtype MonthMatch a = MonthMatch { getMonthMatch :: Vector a }

newtype UnboxedMonthMatch a = UnboxedMonthMatch { getUnboxedMonthMatch :: UVector.Vector a }

-- | A timespan. This is represented internally as a number
--   of nanoseconds.
newtype Timespan = Timespan { getTimespan :: Int64 }
  deriving (Show,Read,Eq,Ord,ToJSON,FromJSON,Additive)

instance Monoid Timespan where
  mempty = Timespan 0
  mappend (Timespan a) (Timespan b) = Timespan (a + b)

instance Torsor Time Timespan where
  add (Timespan ts) (Time t) = Time (t + ts)
  difference (Time t) (Time s) = Timespan (t - s)

instance Scaling Timespan Int64 where
  scale i (Timespan ts) = Timespan (i * ts)

-- | The precision used when encoding seconds to a human-readable format.
data SubsecondPrecision
  = SubsecondPrecisionAuto -- ^ Rounds to second, millisecond, microsecond, or nanosecond
  | SubsecondPrecisionFixed {-# UNPACK #-} !Int -- ^ Specify number of places after decimal

-- | A date as represented by the Gregorian calendar.
data Date = Date
  { dateYear  :: {-# UNPACK #-} !Year
  , dateMonth :: {-# UNPACK #-} !Month
  , dateDay   :: {-# UNPACK #-} !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

-- | The year and number of days elapsed since the beginning it began.
data OrdinalDate = OrdinalDate
  { ordinalDateYear :: {-# UNPACK #-} !Year
  , ordinalDateDayOfYear :: {-# UNPACK #-} !DayOfYear
  } deriving (Show,Read,Eq,Ord)

-- | A month and the day of the month. This does not actually represent
--   a specific date, since this recurs every year.
data MonthDate = MonthDate
  { monthDateMonth :: {-# UNPACK #-} !Month
  , monthDateDay :: {-# UNPACK #-} !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

-- | A date as represented by the Gregorian calendar
--   and a time of day.
data Datetime = Datetime
  { datetimeDate :: {-# UNPACK #-} !Date
  , datetimeTime :: {-# UNPACK #-} !TimeOfDay
  } deriving (Show,Read,Eq,Ord)

data OffsetDatetime = OffsetDatetime
  { offsetDatetimeDatetime :: {-# UNPACK #-} !Datetime
  , offsetDatetimeOffset :: {-# UNPACK #-} !Offset
  } deriving (Show,Read,Eq,Ord)

-- | A time of day with nanosecond resolution.
data TimeOfDay = TimeOfDay
  { timeOfDayHour :: {-# UNPACK #-} !Int
  , timeOfDayMinute :: {-# UNPACK #-} !Int
  , timeOfDayNanoseconds :: {-# UNPACK #-} !Int64
  } deriving (Show,Read,Eq,Ord)

data DatetimeFormat = DatetimeFormat
  { datetimeFormatDateSeparator :: !(Maybe Char)
    -- ^ Separator in the date
  , datetimeFormatSeparator :: !(Maybe Char)
    -- ^ Separator between date and time
  , datetimeFormatTimeSeparator :: !(Maybe Char)
    -- ^ Separator in the time
  } deriving (Show,Read,Eq,Ord)

-- | Formatting settings for a timezone offset.
data OffsetFormat
  = OffsetFormatColonOff -- ^ @%z@ (e.g., -0400)
  | OffsetFormatColonOn -- ^ @%:z@ (e.g., -04:00)
  | OffsetFormatSecondsPrecision -- ^ @%::z@ (e.g., -04:00:00)
  | OffsetFormatColonAuto -- ^ @%:::z@ (e.g., -04, +05:30)
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

-- | Locale-specific formatting for weekdays and months. The
--   type variable will likely be instantiated to @Text@
--   or @ByteString@.
data DatetimeLocale a = DatetimeLocale
  { datetimeLocaleDaysOfWeekFull :: !(DayOfWeekMatch a)
    -- ^ full weekdays starting with Sunday, 7 elements
  , datetimeLocaleDaysOfWeekAbbreviated :: !(DayOfWeekMatch a)
    -- ^ abbreviated weekdays starting with Sunday, 7 elements
  , datetimeLocaleMonthsFull :: !(MonthMatch a)
    -- ^ full months starting with January, 12 elements
  , datetimeLocaleMonthsAbbreviated :: !(MonthMatch a)
    -- ^ abbreviated months starting with January, 12 elements
  }

-- | Locale-specific formatting for AM and PM.
data MeridiemLocale a = MeridiemLocale
  { meridiemLocaleAm :: !a
  , meridiemLocalePm :: !a
  } deriving (Read,Show,Eq,Ord)

newtype instance UVector.MVector s Month = MV_Month (PVector.MVector s Month)
newtype instance UVector.Vector Month = V_Month (PVector.Vector Month)

instance UVector.Unbox Month

instance MGVector.MVector UVector.MVector Month where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Month v) = MGVector.basicLength v
  basicUnsafeSlice i n (MV_Month v) = MV_Month $ MGVector.basicUnsafeSlice i n v
  basicOverlaps (MV_Month v1) (MV_Month v2) = MGVector.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Month `liftM` MGVector.basicUnsafeNew n
  basicInitialize (MV_Month v) = MGVector.basicInitialize v
  basicUnsafeReplicate n x = MV_Month `liftM` MGVector.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Month v) i = MGVector.basicUnsafeRead v i
  basicUnsafeWrite (MV_Month v) i x = MGVector.basicUnsafeWrite v i x
  basicClear (MV_Month v) = MGVector.basicClear v
  basicSet (MV_Month v) x = MGVector.basicSet v x
  basicUnsafeCopy (MV_Month v1) (MV_Month v2) = MGVector.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Month v1) (MV_Month v2) = MGVector.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Month v) n = MV_Month `liftM` MGVector.basicUnsafeGrow v n

instance GVector.Vector UVector.Vector Month where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Month v) = V_Month `liftM` GVector.basicUnsafeFreeze v
  basicUnsafeThaw (V_Month v) = MV_Month `liftM` GVector.basicUnsafeThaw v
  basicLength (V_Month v) = GVector.basicLength v
  basicUnsafeSlice i n (V_Month v) = V_Month $ GVector.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Month v) i = GVector.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Month mv) (V_Month v) = GVector.basicUnsafeCopy mv v
  elemseq _ = seq

newtype instance UVector.MVector s DayOfMonth = MV_DayOfMonth (PVector.MVector s DayOfMonth)
newtype instance UVector.Vector DayOfMonth = V_DayOfMonth (PVector.Vector DayOfMonth)

instance UVector.Unbox DayOfMonth

instance MGVector.MVector UVector.MVector DayOfMonth where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_DayOfMonth v) = MGVector.basicLength v
  basicUnsafeSlice i n (MV_DayOfMonth v) = MV_DayOfMonth $ MGVector.basicUnsafeSlice i n v
  basicOverlaps (MV_DayOfMonth v1) (MV_DayOfMonth v2) = MGVector.basicOverlaps v1 v2
  basicUnsafeNew n = MV_DayOfMonth `liftM` MGVector.basicUnsafeNew n
  basicInitialize (MV_DayOfMonth v) = MGVector.basicInitialize v
  basicUnsafeReplicate n x = MV_DayOfMonth `liftM` MGVector.basicUnsafeReplicate n x
  basicUnsafeRead (MV_DayOfMonth v) i = MGVector.basicUnsafeRead v i
  basicUnsafeWrite (MV_DayOfMonth v) i x = MGVector.basicUnsafeWrite v i x
  basicClear (MV_DayOfMonth v) = MGVector.basicClear v
  basicSet (MV_DayOfMonth v) x = MGVector.basicSet v x
  basicUnsafeCopy (MV_DayOfMonth v1) (MV_DayOfMonth v2) = MGVector.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_DayOfMonth v1) (MV_DayOfMonth v2) = MGVector.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_DayOfMonth v) n = MV_DayOfMonth `liftM` MGVector.basicUnsafeGrow v n

instance GVector.Vector UVector.Vector DayOfMonth where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_DayOfMonth v) = V_DayOfMonth `liftM` GVector.basicUnsafeFreeze v
  basicUnsafeThaw (V_DayOfMonth v) = MV_DayOfMonth `liftM` GVector.basicUnsafeThaw v
  basicLength (V_DayOfMonth v) = GVector.basicLength v
  basicUnsafeSlice i n (V_DayOfMonth v) = V_DayOfMonth $ GVector.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_DayOfMonth v) i = GVector.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_DayOfMonth mv) (V_DayOfMonth v) = GVector.basicUnsafeCopy mv v
  elemseq _ = seq
