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

import           Chronos
