{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Chronos.Types where

import Data.Int
import Data.Word
import Data.Vector (Vector)
import Data.Aeson (FromJSON,ToJSON)
import Data.Hashable (Hashable)
import Data.Primitive
import Control.Monad
import qualified Data.Vector.Generic            as GVector
import qualified Data.Vector.Unboxed            as UVector
import qualified Data.Vector.Primitive          as PVector
import qualified Data.Vector.Generic.Mutable    as MGVector
import qualified Data.Vector.Unboxed.Mutable    as MUVector
import qualified Data.Vector.Primitive.Mutable  as MPVector

newtype Day = Day { getDay :: Int32 }
  deriving (Show,Read,Eq,Ord)

-- | A duration of days
newtype Days = Days { getDays :: Int32 }
  deriving (Show,Read,Eq,Ord)

newtype DayOfWeek = DayOfWeek { getDayOfWeek :: Word8 }

newtype DayOfMonth = DayOfMonth { getDayOfMonth :: Word8 }
  deriving (Show,Read,Eq,Ord,Prim,Enum)

newtype DayOfYear = DayOfYear { getDayOfYear :: Word16 }
  deriving (Show,Read,Eq,Ord,Prim)

newtype Month = Month { getMonth :: Word8 }
  deriving (Show,Read,Eq,Ord,Prim)

newtype Year = Year { getYear :: Int32 }
  deriving (Show,Read,Eq,Ord)

-- This is a Modified Julian Day.
-- newtype Date = Date { getDate :: Int32 }

-- | TAI time with nanosecond resolution.
newtype TaiTime = TaiTime { getTaiTime :: Int64 }
  deriving (FromJSON,ToJSON,Hashable,Eq,Ord,Show,Read)

-- | POSIX time with nanosecond resolution.
newtype PosixTime = PosixTime { getPosixTime :: Int64 }
  deriving (FromJSON,ToJSON,Hashable,Eq,Ord,Show,Read)

-- newtype Day = Day { getDay :: Word8 }
-- newtype Week = Week { getWeek :: Word8 }

newtype DayOfWeekMatch a = DayOfWeekMatch { getDayOfWeekMatch :: Vector a }

newtype MonthMatch a = MonthMatch { getMonthMatch :: Vector a }

newtype UnboxedMonthMatch a = UnboxedMonthMatch { getUnboxedMonthMatch :: UVector.Vector a }

newtype Nanoseconds = Nanoseconds { getNanoseconds :: Int64 }
  deriving (Show,Read,Eq,Ord)

-- | A date as represented by the Gregorian calendar.
data Date = Date
  { dateYear  :: !Year
  , dateMonth :: !Month
  , dateDay   :: !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

data OrdinalDate = OrdinalDate
  { ordinalDateYear  :: !Year
  , ordinalDateMonth :: !DayOfYear
  } deriving (Show,Read,Eq,Ord)

data MonthDate = MonthDate
  { monthDateMonth :: !Month
  , monthDateDay   :: !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

-- | A date as represented by the Gregorian calendar
--   and a time of day.
data Datetime = Datetime
  { datetimeDate :: !Date
  , datetimeTime :: !TimeOfDay
  } deriving (Show,Read,Eq,Ord)

data OffsetDatetime = OffsetDatetime
  { offsetDatetimeDatetime :: !Datetime
  , offsetDatetimeOffset :: !Int16
  }

-- | A time of day, including the possibility of leap seconds.
data TimeOfDay = TimeOfDay
  { timeOfDayHour :: !Word8
  , timeOfDayMinute :: !Word8
  , timeOfDayNanoseconds :: !Word64
  } deriving (Show,Read,Eq,Ord)

data UtcTime = UtcTime
  { utcTimeDate :: !Day
  , utcTimeNanoseconds :: !Word64
  } deriving (Show,Read,Eq,Ord)

data DatetimeLocale a = DatetimeLocale
  { datetimeLocaleDaysOfWeekFull :: !(DayOfWeekMatch a)
    -- ^ full weekdays starting with Sunday, 7 elements
  , datetimeLocaleDaysOfWeekAbbreviated :: !(DayOfWeekMatch a)
    -- ^ abbreviated weekdays starting with Sunday, 7 elements
  , datetimeLocaleMonthsFull :: !(MonthMatch a)
    -- ^ full months starting with January, 12 elements
  , datetimeLocaleMonthsAbbreviated :: !(MonthMatch a)
    -- ^ abbreviated months starting with January, 12 elements
  , datetimeLocaleAm :: !a
    -- ^ Symbol for AM
  , datetimeLocalePm :: !a
    -- ^ Symbol for PM
  }

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
