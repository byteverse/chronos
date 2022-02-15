{-# language
    BangPatterns
  , CPP
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , NumericUnderscores
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeInType
  , UnboxedTuples
  #-}

{-| Chronos is a performance-oriented time library for Haskell, with a
    straightforward API. The main differences between this
    and the <http://hackage.haskell.org/package/time time> library
    are:

      * Chronos uses machine integers where possible. This means
        that time-related arithmetic should be faster, with the
        drawback that the types are incapable of representing times
        that are very far in the future or the past (because Chronos
        provides nanosecond, rather than picosecond, resolution).
        For most users, this is not a hindrance.
      * Chronos provides 'ToJSON'/'FromJSON' instances for serialisation.
      * Chronos provides 'UVector.Unbox' instances for working with unboxed vectors.
      * Chronos provides 'Prim' instances for working with byte arrays/primitive arrays.
      * Chronos uses normal non-overloaded haskell functions for
        encoding and decoding time. It provides <http://hackage.haskell.org/package/attoparsec attoparsec> parsers for both 'Text' and
        'ByteString'. Additionally, Chronos provides functions for
        encoding time to 'Text' or 'ByteString'. The <http://hackage.haskell.org/package/time time> library accomplishes these with the
        <http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html Data.Time.Format> module, which uses UNIX-style datetime
        format strings. The approach taken by Chronos is faster and
        catches more mistakes at compile time, at the cost of being
        less expressive.
 -}

module Chronos
  ( -- * Functions
    -- ** Current
    now
  , today
  , tomorrow
  , yesterday
  , todayDayOfWeek
  , yesterdayDayOfWeek
  , tomorrowDayOfWeek
  , timeToDayOfWeek
  , epoch
    -- ** Duration
  , stopwatch
  , stopwatch_
    -- ** Construction
  , datetimeFromYmdhms
  , timeFromYmdhms
    -- ** Conversion
  , timeToDatetime
  , datetimeToTime
  , datetimeToDayOfWeek
  , timeToOffsetDatetime
  , offsetDatetimeToTime
  , timeToDayTruncate
  , dayToTimeMidnight
  , dayToDate
  , dateToDay
  , dayToOrdinalDate
  , ordinalDateToDay
  , monthDateToDayOfYear
  , dayOfYearToMonthDay
    -- ** Build Timespan
  , second
  , minute
  , hour
  , day
  , week
    -- ** Matching
  , buildDayOfWeekMatch
  , buildMonthMatch
  , buildUnboxedMonthMatch
  , caseDayOfWeek
  , caseMonth
  , caseUnboxedMonth
    -- ** Format
    -- $format
  , w3c
  , slash
  , hyphen
  , compact
  , timeParts
    -- ** Months
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
    -- ** Days of Week
  , sunday
  , monday
  , tuesday
  , wednesday
  , thursday
  , friday
  , saturday
    -- ** Utility
  , daysInMonth
  , isLeapYear
  , observedOffsets
    -- * Textual Conversion
    -- ** Date
    -- *** Text
  , builder_Ymd
  , builder_Dmy
  , builder_HMS
  , parser_Ymd
  , parser_Ymd_lenient
  , parser_Mdy
  , parser_Mdy_lenient
  , parser_Dmy
  , parser_Dmy_lenient
    -- *** UTF-8 ByteString
  , builderUtf8_Ymd
  , parserUtf8_Ymd
    -- ** Time of Day
    -- *** Text
  , builder_IMS_p
  , builder_IMSp
  , parser_HMS
  , parser_HMS_opt_S
    -- *** UTF-8 ByteString
  , builderUtf8_HMS
  , builderUtf8_IMS_p
  , builderUtf8_IMSp
  , parserUtf8_HMS
  , parserUtf8_HMS_opt_S
  , zeptoUtf8_HMS
    -- ** Datetime
    -- *** Text
  , builder_DmyHMS
  , builder_DmyIMSp
  , builder_DmyIMS_p
  , builder_YmdHMS
  , builder_YmdIMSp
  , builder_YmdIMS_p
  , builderW3C
  , builderIso8601
  , encodeIso8601
  , encode_DmyHMS
  , encode_DmyIMS_p
  , encode_YmdHMS
  , encode_YmdIMS_p
  , parser_DmyHMS
  , parser_DmyHMS_lenient
  , parser_YmdHMS
  , parser_YmdHMS_lenient
  , parser_YmdHMS_opt_S
  , parser_YmdHMS_opt_S_lenient
  , parser_DmyHMS_opt_S
  , parser_DmyHMS_opt_S_lenient
  , parser_MdyHMS
  , parser_MdyHMS_lenient
  , parser_MdyHMS_opt_S
  , parser_MdyHMS_opt_S_lenient
  , parser_lenient
  , decode_DmyHMS
  , decode_DmyHMS_lenient
  , decode_MdyHMS
  , decode_MdyHMS_lenient
  , decode_MdyHMS_opt_S
  , decode_MdyHMS_opt_S_lenient
  , decode_YmdHMS
  , decode_YmdHMS_lenient
  , decode_YmdHMS_opt_S
  , decode_YmdHMS_opt_S_lenient
  , decode_DmyHMS_opt_S
  , decode_DmyHMS_opt_S_lenient
  , decode_lenient
    -- *** UTF-8 ByteString
  , encodeUtf8_YmdHMS
  , encodeUtf8_YmdIMS_p
  , builderUtf8_YmdHMS
  , builderUtf8_YmdIMSp
  , builderUtf8_YmdIMS_p
  , builderUtf8W3C
  , decodeUtf8_YmdHMS
  , decodeUtf8_YmdHMS_opt_S
  , parserUtf8_YmdHMS
  , parserUtf8_YmdHMS_opt_S
  , zeptoUtf8_YmdHMS
    -- *** UTF-8 Bytes
  , boundedBuilderUtf8BytesIso8601Zoneless
  , decodeUtf8BytesIso8601Zoneless
    -- *** Short Text
  , decodeShortTextIso8601Zoneless
  , encodeShortTextIso8601Zulu
  , encodeShortTextIso8601Zoneless
    -- ** Offset Datetime
    -- *** Text
  , encode_YmdHMSz
  , encode_DmyHMSz
  , builder_YmdHMSz
  , builder_DmyHMSz
  , parser_YmdHMSz
  , parser_DmyHMSz
  , builder_YmdIMS_p_z
  , builder_DmyIMS_p_z
  , builderW3Cz
    -- *** UTF-8 ByteString
  , builderUtf8_YmdHMSz
  , parserUtf8_YmdHMSz
  , builderUtf8_YmdIMS_p_z
  , builderUtf8W3Cz
    -- *** UTF-8 Bytes
  , parserUtf8BytesIso8601
  , boundedBuilderUtf8BytesIso8601
  , decodeUtf8BytesIso8601
    -- *** ShortText
  , decodeShortTextIso8601
  , encodeShortTextIso8601
    -- ** Offset
    -- *** Text
  , encodeOffset
  , builderOffset
  , decodeOffset
  , parserOffset
    -- *** UTF-8 ByteString
  , encodeOffsetUtf8
  , builderOffsetUtf8
  , decodeOffsetUtf8
  , parserOffsetUtf8
    -- ** Timespan
    -- *** Text
  , encodeTimespan
  , builderTimespan
    -- *** UTF-8 ByteString
  , encodeTimespanUtf8
  , builderTimespanUtf8
    -- ** TimeInterval
  , within
  , timeIntervalToTimespan
  , whole
  , singleton
  , lowerBound
  , upperBound
  , width
  , timeIntervalBuilder
  , (...)
    -- * Types
  , Day(..)
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
  , TimeInterval(..)
  , TimeParts(..)
  ) where

import Control.Applicative
import Control.DeepSeq (NFData(..), deepseq)
import Control.Exception (evaluate)
import Control.Monad
import Data.Aeson (FromJSON,ToJSON,FromJSONKey,ToJSONKey)
import Data.Attoparsec.Text (Parser)
import Data.Bool (bool)
import Data.Bytes (Bytes)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.Foldable
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Primitive
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Vector (Vector)
import Data.Word (Word64, Word8)
import Foreign.Storable
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Torsor
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AEE
import qualified Data.Aeson.Types as AET
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Parser as BVP
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Semigroup as SG
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read as Text
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Generic.Mutable as MGVector
import qualified Data.Vector.Primitive as PVector
import qualified Data.Vector.Unboxed as UVector

#ifdef mingw32_HOST_OS
import System.Win32.Time (SYSTEMTIME(..))
import qualified System.Win32.Time as W32
#else
import Chronos.Internal.CTimespec (getPosixNanoseconds)
#endif

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
#endif

-- $setup
-- >>> import Test.QuickCheck hiding (within)
-- >>> import Test.QuickCheck.Gen
-- >>> import Data.Maybe (isJust)
-- >>> :set -XStandaloneDeriving
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> :set -XScopedTypeVariables
--
-- >>> deriving instance Arbitrary Time
-- >>> :{
--   instance Arbitrary TimeInterval where
--     arbitrary = do
--       t0 <- arbitrary
--       t1 <- suchThat arbitrary (>= t0)
--       pure (TimeInterval t0 t1)
--   instance Arbitrary TimeOfDay where
--     arbitrary = TimeOfDay
--       <$> choose (0,23)
--       <*> choose (0,59)
--       <*> choose (0, 60000000000 - 1)
--   instance Arbitrary Date where
--     arbitrary = Date
--       <$> fmap Year (choose (1800,2100))
--       <*> fmap Month (choose (0,11))
--       <*> fmap DayOfMonth (choose (1,28))
--   instance Arbitrary Datetime where
--     arbitrary = Datetime <$> arbitrary <*> arbitrary
--   instance Arbitrary OffsetDatetime where
--     arbitrary = OffsetDatetime <$> arbitrary <*> arbitrary
--   instance Arbitrary DatetimeFormat where
--     arbitrary = DatetimeFormat
--       <$> arbitrary
--       <*> elements [ Nothing, Just '/', Just ':', Just '-']
--       <*> arbitrary
--   instance Arbitrary OffsetFormat where
--     arbitrary = arbitraryBoundedEnum
--     shrink = genericShrink
--   instance Arbitrary Offset where
--     arbitrary = fmap Offset (choose ((-24) * 60, 24 * 60))
--   instance Arbitrary SubsecondPrecision where
--     arbitrary = frequency
--       [ (1, pure SubsecondPrecisionAuto)
--       , (1, SubsecondPrecisionFixed <$> choose (0,9))
--       ]
--   instance Arbitrary Day where
--     arbitrary = fmap Day (choose (0,50000))
-- :}
--

-- | A 'Timespan' representing a single second.
second :: Timespan
second = Timespan 1000000000

-- | A 'Timespan' representing a single minute.
minute :: Timespan
minute = Timespan 60000000000

-- | A 'Timespan' representing a single hour.
hour :: Timespan
hour = Timespan 3600000000000

-- | A 'Timespan' representing a single day.
day :: Timespan
day = Timespan 86400000000000

-- | A 'Timespan' representing a single week.
week :: Timespan
week = Timespan 604800000000000

-- | Convert 'Time' to 'Datetime'.
--
--   prop> \(t :: Time) -> (datetimeToTime (timeToDatetime t)) == t
timeToDatetime :: Time -> Datetime
timeToDatetime = utcTimeToDatetime . toUtc

-- | Convert 'Datetime' to 'Time'.
--
--   prop> \(d :: Datetime) -> timeToDatetime (datetimeToTime d) == d
datetimeToTime :: Datetime -> Time
datetimeToTime = fromUtc . datetimeToUtcTime

-- | Convert 'Datetime' to 'DayOfWeek'
datetimeToDayOfWeek :: Datetime -> DayOfWeek
datetimeToDayOfWeek (Datetime (Date year month date) _) =
  let k = getDayOfMonth date
      m = ((getMonth month + 10) `mod` 12) + 1
      y = adjustedYear `mod` 100
      c = adjustedYear `div` 100
      adjustedYear = if m >= 11 then getYear year - 1 else getYear year
  in DayOfWeek $ (k + (floor $ ((2.6 :: Double) * fromIntegral m) - 0.2) - (2*c) + y + (y `div` 4) + (c `div` 4)) `mod` 7

-- | Convert 'Time' to 'OffsetDatetime' by providing an 'Offset'.
timeToOffsetDatetime :: Offset -> Time -> OffsetDatetime
timeToOffsetDatetime offset = utcTimeToOffsetDatetime offset . toUtc

-- | Convert 'OffsetDatetime' to 'Time'.
offsetDatetimeToTime :: OffsetDatetime -> Time
offsetDatetimeToTime = fromUtc . offsetDatetimeToUtcTime

-- | Convert 'Time' to 'Day'. This function is lossy; consequently, it
--   does not roundtrip with 'dayToTimeMidnight'.
timeToDayTruncate :: Time -> Day
timeToDayTruncate (Time i) = Day (fromIntegral (div i 86400000000000) + 40587)

-- | Convert midnight of the given 'Day' to 'Time'.
dayToTimeMidnight :: Day -> Time
dayToTimeMidnight (Day d) = Time (fromIntegral (d - 40587) * 86400000000000)

-- | Convert 'Day' to a 'Date'.
--
--   prop> \(d :: Day) -> dateToDay (dayToDate d) == d
dayToDate :: Day -> Date
dayToDate theDay = Date year month dayOfMonth
  where
  OrdinalDate year yd = dayToOrdinalDate theDay
  MonthDate month dayOfMonth = dayOfYearToMonthDay (isLeapYear year) yd

-- | Convert a 'Date' to a 'Day'.
--
--   prop> \(d :: Date) -> dayToDate (dateToDay d) == d
dateToDay :: Date -> Day
dateToDay (Date y m d) = ordinalDateToDay $ OrdinalDate y
  (monthDateToDayOfYear (isLeapYear y) (MonthDate m d))

-- | Construct a 'Datetime' from year, month, day, hour, minute, second:
--
--   >>> datetimeFromYmdhms 2014 2 26 17 58 52
--   Datetime {datetimeDate = Date {dateYear = Year {getYear = 2014}, dateMonth = Month {getMonth = 1}, dateDay = DayOfMonth {getDayOfMonth = 26}}, datetimeTime = TimeOfDay {timeOfDayHour = 17, timeOfDayMinute = 58, timeOfDayNanoseconds = 52000000000}}
datetimeFromYmdhms ::
     Int -- ^ Year
  -> Int -- ^ Month
  -> Int -- ^ Day
  -> Int -- ^ Hour
  -> Int -- ^ Minute
  -> Int -- ^ Second
  -> Datetime
datetimeFromYmdhms y m d h m' s = Datetime
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
    else 0

-- | Construct a 'Time' from year, month, day, hour, minute, second:
--
--   >>> timeFromYmdhms 2014 2 26 17 58 52
--   Time {getTime = 1393437532000000000}
timeFromYmdhms ::
     Int -- ^ Year
  -> Int -- ^ Month
  -> Int -- ^ Day
  -> Int -- ^ Hour
  -> Int -- ^ Minute
  -> Int -- ^ Second
  -> Time
timeFromYmdhms y m d h m' s = datetimeToTime (datetimeFromYmdhms y m d h m' s)

-- | Gets the current 'Day'. This does not take the user\'s
--   time zone into account.
today :: IO Day
today = fmap timeToDayTruncate now

-- | Gets the 'Day' of tomorrow.
tomorrow :: IO Day
tomorrow = fmap (add 1 . timeToDayTruncate) now

-- | Gets the 'Day' of yesterday.
yesterday :: IO Day
yesterday = fmap (add (-1) . timeToDayTruncate) now

-- | Get the current time from the system clock.
now :: IO Time
#ifdef mingw32_HOST_OS
now = do
  SYSTEMTIME{..} <- W32.getSystemTime
  let date = Date
        { dateYear  = Year       (fromIntegral wYear)
        , dateMonth = Month      (fromIntegral wMonth - 1)
        , dateDay   = DayOfMonth (fromIntegral wDay)
        }
  let secNano = (fromIntegral wSecond :: Int64) * 1000000000
      msNano  = (fromIntegral wMilliseconds :: Int64) * 1000000
      nano    = secNano + msNano
  let time = TimeOfDay
        { timeOfDayHour        = fromIntegral wHour
        , timeOfDayMinute      = fromIntegral wMinute
        , timeOfDayNanoseconds = fromIntegral nano
        }
  let dt = Datetime date time
  pure $ datetimeToTime dt
#else
now = fmap Time getPosixNanoseconds
#endif

-- | Convert from 'Time' to 'DayOfWeek'.
timeToDayOfWeek :: Time -> DayOfWeek
timeToDayOfWeek (Time time) = DayOfWeek $
  (fromIntegral @Int64 @Int ((time `div` 86400000000000) + 4) `mod` 7)

-- | Get the current 'DayOfWeek' from the system clock.
todayDayOfWeek :: IO DayOfWeek
todayDayOfWeek = timeToDayOfWeek <$> now

-- | Get the yesterday\'s 'DayOfWeek' from the system clock.
yesterdayDayOfWeek :: IO DayOfWeek
yesterdayDayOfWeek =
  timeToDayOfWeek . (add (Timespan (-86400000000000))) <$> now

-- | Get the tomorrow\'s 'DayOfWeek' from the system clock.
tomorrowDayOfWeek :: IO DayOfWeek
tomorrowDayOfWeek =
  timeToDayOfWeek . (add (Timespan (86400000000000))) <$> now

-- | The Unix epoch, that is 1970-01-01 00:00:00.
epoch :: Time
epoch = Time 0

-- | Measures the time it takes to run an action and evaluate
--   its result to WHNF. This measurement uses a monotonic clock
--   instead of the standard system clock.
stopwatch :: IO a -> IO (Timespan, a)
stopwatch action = do
  start <- getMonotonicTimeNSec
  a <- action >>= evaluate
  end <- getMonotonicTimeNSec
  pure ((Timespan (fromIntegral (end - start))), a)

-- | Measures the time it takes to run an action. The result
--   is discarded. This measurement uses a monotonic clock
--   instead of the standard system clock.
stopwatch_ :: IO a -> IO Timespan
stopwatch_ action = do
  start <- getMonotonicTimeNSec
  _ <- action
  end <- getMonotonicTimeNSec
  pure (Timespan (fromIntegral (end - start)))

-- UtcTime. Used internally only.
data UtcTime = UtcTime
  {-# UNPACK #-} !Day -- day
  {-# UNPACK #-} !Int64 -- nanoseconds

toUtc :: Time -> UtcTime
toUtc (Time i) = let (d,t) = divMod i (getTimespan day)
 in UtcTime (add (fromIntegral d) epochDay) (fromIntegral t)

fromUtc :: UtcTime -> Time
fromUtc (UtcTime d ns') = Time $ getTimespan $ plus
  (scale (intToInt64 (difference d epochDay)) day)
  (if ns > day then day else ns)
  where ns = Timespan ns'

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

epochDay :: Day
epochDay = Day 40587

dayLengthInt64 :: Int64
dayLengthInt64 = getTimespan day

nanosecondsInMinute :: Int64
nanosecondsInMinute = 60000000000

-- | All UTC time offsets. See <https://en.wikipedia.org/wiki/List_of_UTC_time_offsets List of UTC time offsets>.
observedOffsets :: Vector Offset
observedOffsets = Vector.fromList $ map Offset
  [ -1200
  , -1100
  , -1000
  , -930
  , -900
  , -800
  , -700
  , -600
  , -500
  , -400
  , -330
  , -300
  , -230
  , -200
  , -100
  , 0
  , 100
  , 200
  , 300
  , 330
  , 400
  , 430
  , 500
  , 530
  , 545
  , 600
  , 630
  , 700
  , 800
  , 845
  , 900
  , 930
  , 1000
  , 1030
  , 1100
  , 1200
  , 1245
  , 1300
  , 1345
  , 1400
  ]

-- | The first argument in the resulting tuple in a day
--   adjustment. It should be either -1, 0, or 1, as no
--   offset should ever exceed 24 hours.
offsetTimeOfDay :: Offset -> TimeOfDay -> (Int, TimeOfDay)
offsetTimeOfDay (Offset offset) (TimeOfDay h m s) =
  (dayAdjustment,TimeOfDay h'' m'' s)
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

-- datetimeToOffsetDatetime :: Offset -> Datetime -> OffsetDatetime
-- datetimeToOffsetDatetime offset

utcTimeToOffsetDatetime :: Offset -> UtcTime -> OffsetDatetime
utcTimeToOffsetDatetime offset (UtcTime (Day d) nanoseconds) =
  let (!dayAdjustment,!tod) = offsetTimeOfDay offset (nanosecondsSinceMidnightToTimeOfDay nanoseconds)
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
  let (!dayAdjustment,!tod) = offsetTimeOfDay (Offset $ negate off) timeOfDay
      !(Day !theDay) = dateToDay date
   in UtcTime
        (Day (theDay + dayAdjustment))
        (timeOfDayToNanosecondsSinceMidnight tod)

-- | Convert a 'MonthDate' to a 'DayOfYear'.
monthDateToDayOfYear ::
     Bool -- ^ Is it a leap year?
  -> MonthDate
  -> DayOfYear
monthDateToDayOfYear isLeap (MonthDate month@(Month m) (DayOfMonth dayOfMonth)) =
  DayOfYear ((div (367 * (fromIntegral m + 1) - 362) 12) + k + day')
  where
  day' = fromIntegral $ clip 1 (daysInMonth isLeap month) dayOfMonth
  k = if month < Month 2 then 0 else if isLeap then -1 else -2

-- | Convert an 'OrdinalDate' to a 'Day'.
ordinalDateToDay :: OrdinalDate -> Day
ordinalDateToDay (OrdinalDate year@(Year y') theDay) = Day mjd where
  y = y' - 1
  mjd = (fromIntegral . getDayOfYear $
           (clip (DayOfYear 1) (if isLeapYear year then DayOfYear 366 else DayOfYear 365) theDay)
        )
      + (365 * y)
      + (div y 4) - (div y 100)
      + (div y 400) - 678576

-- | Is the 'Year' a leap year?
--
--   >>> isLeapYear (Year 1996)
--   True
--
--   >>> isLeapYear (Year 2019)
--   False
isLeapYear :: Year -> Bool
isLeapYear (Year year) = (mod year 4 == 0) && ((mod year 400 == 0) || not (mod year 100 == 0))

-- | Convert a 'DayOfYear' to a 'MonthDate'.
dayOfYearToMonthDay ::
     Bool -- ^ Is it a leap year?
  -> DayOfYear
  -> MonthDate
dayOfYearToMonthDay isLeap dayOfYear =
  let (!doyUpperBound,!monthTable,!dayTable) =
        if isLeap
          then (DayOfYear 366, leapYearDayOfYearMonthTable, leapYearDayOfYearDayOfMonthTable)
          else (DayOfYear 365, normalYearDayOfYearMonthTable, normalYearDayOfYearDayOfMonthTable)
      DayOfYear clippedDay = clip (DayOfYear 1) doyUpperBound dayOfYear
      clippedDayInt = fromIntegral clippedDay :: Int
      month = UVector.unsafeIndex monthTable clippedDayInt
      theDay = UVector.unsafeIndex dayTable clippedDayInt
   in MonthDate month theDay

-- | Convert a 'Day' to an 'OrdinalDate'.
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

{- $format

The formats provided is this module are language-agnostic.
To find meridiem formats and month formats, look in a
language-specific module.

-}

-- | The W3C 'DatetimeFormat'.
--
--   >>> encode_YmdHMS SubsecondPrecisionAuto w3c (timeToDatetime (timeFromYmdhms 2014 2 26 17 58 52))
--   "2014-02-26T17:58:52"
--
--  prop> \(s :: SubsecondPrecision) (dt :: Datetime) -> isJust (decode_YmdHMS w3c (encode_YmdHMS s w3c dt))
w3c :: DatetimeFormat
w3c = DatetimeFormat (Just '-') (Just 'T') (Just ':')

-- | A 'DatetimeFormat' that separates the members of
--   the 'Date' by slashes.
--
--   >>> encode_YmdHMS SubsecondPrecisionAuto slash (timeToDatetime (timeFromYmdhms 2014 2 26 17 58 52))
--   "2014/02/26 17:58:52"
--
--   prop> \(s :: SubsecondPrecision) (dt :: Datetime) -> isJust (decode_YmdHMS slash (encode_YmdHMS s slash dt))
slash :: DatetimeFormat
slash = DatetimeFormat (Just '/') (Just ' ') (Just ':')

-- | A 'DatetimeFormat' that separates the members of
--   the 'Date' by hyphens.
--
--   >>> encode_YmdHMS SubsecondPrecisionAuto hyphen (timeToDatetime (timeFromYmdhms 2014 2 26 17 58 52))
--   "2014-02-26 17:58:52"
--
--   prop> \(s :: SubsecondPrecision) (dt :: Datetime) -> isJust (decode_YmdHMS hyphen (encode_YmdHMS s hyphen dt))
hyphen :: DatetimeFormat
hyphen = DatetimeFormat (Just '-') (Just ' ') (Just ':')

-- | A 'DatetimeFormat' with no separators, except for a
--   `T` between the 'Date' and 'Time'.
--
--   >>> encode_YmdHMS SubsecondPrecisionAuto compact (timeToDatetime (timeFromYmdhms 2014 2 26 17 58 52))
--   "20140226T175852"
--
--   prop> \(s :: SubsecondPrecision) (dt :: Datetime) -> isJust (decode_YmdHMS compact (encode_YmdHMS s compact dt))
compact :: DatetimeFormat
compact = DatetimeFormat Nothing (Just 'T') Nothing

-- | Return the number of days in a given month.
daysInMonth ::
     Bool -- ^ Is this a leap year?
  -> Month -- ^ Month of year
  -> Int
daysInMonth isLeap m = if isLeap
  then caseMonth leapYearMonthLength m
  else caseMonth normalYearMonthLength m

leapYearMonthLength :: MonthMatch Int
leapYearMonthLength = buildMonthMatch 31 29 31 30 31 30 31 31 30 31 30 31

normalYearMonthLength :: MonthMatch Int
normalYearMonthLength = buildMonthMatch 31 28 31 30 31 30 31 31 30 31 30 31

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

-- | Build a 'MonthMatch' from twelve (12) values.
buildMonthMatch :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> MonthMatch a
buildMonthMatch a b c d e f g h i j k l =
  MonthMatch (Vector.fromListN 12 [a,b,c,d,e,f,g,h,i,j,k,l])

-- | Match a 'Month' against a 'MonthMatch'.
caseMonth :: MonthMatch a -> Month -> a
caseMonth (MonthMatch v) (Month ix) = Vector.unsafeIndex v ix

-- | Build an 'UnboxedMonthMatch' from twelve (12) values.
buildUnboxedMonthMatch :: UVector.Unbox a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> UnboxedMonthMatch a
buildUnboxedMonthMatch a b c d e f g h i j k l =
  UnboxedMonthMatch (UVector.fromListN 12 [a,b,c,d,e,f,g,h,i,j,k,l])

-- | Match a 'Month' against an 'UnboxedMonthMatch'.
caseUnboxedMonth :: UVector.Unbox a => UnboxedMonthMatch a -> Month -> a
caseUnboxedMonth (UnboxedMonthMatch v) (Month ix) = UVector.unsafeIndex v ix

-- | Build a 'DayOfWeekMatch' from seven (7) values.
buildDayOfWeekMatch :: a -> a -> a -> a -> a -> a -> a -> DayOfWeekMatch a
buildDayOfWeekMatch a b c d e f g =
  DayOfWeekMatch (Vector.fromListN 7 [a,b,c,d,e,f,g])

-- | Match a 'DayOfWeek' against a 'DayOfWeekMatch'.
caseDayOfWeek :: DayOfWeekMatch a -> DayOfWeek -> a
caseDayOfWeek (DayOfWeekMatch v) (DayOfWeek ix) = Vector.unsafeIndex v ix
-- | Given a 'Date' and a separator, construct a 'Text' 'TB.Builder'
--   corresponding to Year\/Month\/Day encoding.
builder_Ymd :: Maybe Char -> Date -> TB.Builder
builder_Ymd msep (Date y m d) = case msep of
  Nothing ->
       yearToZeroPaddedDigit y
    <> monthToZeroPaddedDigit m
    <> zeroPadDayOfMonth d
  Just sep -> let sepBuilder = TB.singleton sep in
       yearToZeroPaddedDigit y
    <> sepBuilder
    <> monthToZeroPaddedDigit m
    <> sepBuilder
    <> zeroPadDayOfMonth d

-- | Given a 'Date' and a separator, construct a 'Text' 'TB.Builder'
--   corresponding to a Day\/Month\/Year encoding.
builder_Dmy :: Maybe Char -> Date -> TB.Builder
builder_Dmy msep (Date y m d) = case msep of
  Nothing ->
       zeroPadDayOfMonth d
    <> monthToZeroPaddedDigit m
    <> yearToZeroPaddedDigit y
  Just sep -> let sepBuilder = TB.singleton sep in
       zeroPadDayOfMonth d
    <> sepBuilder
    <> monthToZeroPaddedDigit m
    <> sepBuilder
    <> yearToZeroPaddedDigit y

-- | Parse a Year\/Month\/Day-encoded 'Date' that uses the
--   given separator.
parser_Ymd :: Maybe Char -> Parser Date
parser_Ymd msep = do
  y <- parseFixedDigits 4
  traverse_ AT.char msep
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a Year\/Month\/Day-encoded 'Date' that either has no separators or
--   uses any non-numeric character for each separator.
parser_Ymd_lenient :: Parser Date
parser_Ymd_lenient = do
  y <- parseFixedDigits 4
  sep1 <- optional parserLenientSeparator
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  sep2 <- optional parserLenientSeparator
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  case (sep1, sep2) of
    (Nothing, Just _) -> fail "Separators must all exist or not"
    (Just _, Nothing) -> fail "Separators must all exist or not"
    _ -> pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a Month\/Day\/Year-encoded 'Date' that uses the
--   given separator.
parser_Mdy :: Maybe Char -> Parser Date
parser_Mdy msep = do
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  traverse_ AT.char msep
  y <- parseFixedDigits 4
  pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a Month\/Day\/Year-encoded 'Date' that either has no separators or
-- uses any non-numeric character for each separator.
parser_Mdy_lenient :: Parser Date
parser_Mdy_lenient = do
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  sep1 <- optional parserLenientSeparator
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  sep2 <- optional parserLenientSeparator
  y <- parseFixedDigits 4
  case (sep1, sep2) of
    (Nothing, Just _) -> fail "Separators must all exist or not"
    (Just _, Nothing) -> fail "Separators must all exist or not"
    _ -> pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a Day\/Month\/Year-encoded 'Date' that uses the
--   given separator.
parser_Dmy :: Maybe Char -> Parser Date
parser_Dmy msep = do
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  traverse_ AT.char msep
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AT.char msep
  y <- parseFixedDigits 4
  pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a Day\/Month\/Year-encoded 'Date' that either has no separators or
--   uses any non-numeric character for each separator.
parser_Dmy_lenient :: Parser Date
parser_Dmy_lenient = do
  d <- parseFixedDigits 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  sep1 <- optional parserLenientSeparator
  m <- parseFixedDigits 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  sep2 <- optional parserLenientSeparator
  y <- parseFixedDigits 4
  case (sep1, sep2) of
    (Nothing, Just _) -> fail "Separators must all exist or not"
    (Just _, Nothing) -> fail "Separators must all exist or not"
    _ -> pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Given a 'Date' and a separator, construct a 'ByteString' 'BB.Builder'
--   corresponding to a Day\/Month\/Year encoding.
builderUtf8_Ymd :: Maybe Char -> Date -> BB.Builder
builderUtf8_Ymd msep (Date y m d) = case msep of
  Nothing ->
       yearToZeroPaddedDigitBS y
    <> monthToZeroPaddedDigitBS m
    <> zeroPadDayOfMonthBS d
  Just sep -> let sepBuilder = BB.char7 sep in
       yearToZeroPaddedDigitBS y
    <> sepBuilder
    <> monthToZeroPaddedDigitBS m
    <> sepBuilder
    <> zeroPadDayOfMonthBS d

-- | Parse a Year\/Month\/Day-encoded 'Date' that uses the
--   given separator.
parserUtf8_Ymd :: Maybe Char -> AB.Parser Date
parserUtf8_Ymd msep = do
  y <- parseFixedDigitsIntBS 4
  traverse_ AB.char msep
  m <- parseFixedDigitsIntBS 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ AB.char msep
  d <- parseFixedDigitsIntBS 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Given a 'SubsecondPrecision' and a separator, construct a
--   'Text' 'TB.Builder' corresponding to an Hour\/Minute\/Second
--   encoding.
builder_HMS :: SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_HMS sp msep (TimeOfDay h m ns) =
     indexTwoDigitTextBuilder h
  <> internalBuilder_NS sp msep m ns

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a separator,
--   construct a 'Text' 'TB.Builder' according to an IMS encoding.
--
--   This differs from 'builder_IMSp' in that their is a space
--   between the seconds and locale.
builder_IMS_p :: MeridiemLocale Text -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_IMS_p meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep m ns
  <> " "
  <> internalBuilder_p meridiemLocale h

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a separator,
--   construct a 'Text' 'TB.Builder' according to an IMS encoding.
builder_IMSp :: MeridiemLocale Text -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> TB.Builder
builder_IMSp meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilder_I h
  <> internalBuilder_NS sp msep m ns
  <> internalBuilder_p meridiemLocale h

internalBuilder_I :: Int -> TB.Builder
internalBuilder_I h =
  indexTwoDigitTextBuilder $ if h > 12
    then h - 12
    else if h == 0
      then 12
      else h

internalBuilder_p :: MeridiemLocale Text -> Int -> TB.Builder
internalBuilder_p (MeridiemLocale am pm) h = if h > 11
  then TB.fromText pm
  else TB.fromText am

-- | Parse an Hour\/Minute\/Second-encoded 'TimeOfDay' that uses
--   the given separator.
parser_HMS :: Maybe Char -> Parser TimeOfDay
parser_HMS msep = do
  h <- parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AT.char msep
  m <- parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ AT.char msep
  ns <- parseSecondsAndNanoseconds
  pure (TimeOfDay h m ns)

parserLenientSeparator :: Parser ()
parserLenientSeparator = AT.satisfy (not . isDigit) *> pure ()

-- | Parse an Hour\/Minute\/Second-encoded 'TimeOfDay' that either has no
--   separators or uses any given non-numeric character for each separator.
parser_HMS_lenient :: Parser TimeOfDay
parser_HMS_lenient = do
  h <- parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  parserLenientSeparator
  m <- parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  parserLenientSeparator
  ns <- parseSecondsAndNanoseconds
  pure (TimeOfDay h m ns)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_HMS_opt_S :: Maybe Char -> Parser TimeOfDay
parser_HMS_opt_S msep = do
  h <- parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AT.char msep
  m <- parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- AT.peekChar
  case mc of
    Nothing -> pure (TimeOfDay h m 0)
    Just c -> case msep of
      Just sep -> if c == sep
        then do
          _ <- AT.anyChar -- should be the separator
          ns <- parseSecondsAndNanoseconds
          pure (TimeOfDay h m ns)
        else pure (TimeOfDay h m 0)
      -- if there is no separator, we will try to parse the
      -- remaining part as seconds. We commit to trying to
      -- parse as seconds if we see any number as the next
      -- character.
      Nothing -> if isDigit c
        then do
          ns <- parseSecondsAndNanoseconds
          pure (TimeOfDay h m ns)
        else pure (TimeOfDay h m 0)

-- | Parses text that is formatted as either of the following with either no
-- separators or any non-numeric characters for each separator:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_HMS_opt_S_lenient :: Parser TimeOfDay
parser_HMS_opt_S_lenient = do
  h <- parseFixedDigits 2
  when (h > 23) (fail "hour must be between 0 and 23")
  parserLenientSeparator
  m <- parseFixedDigits 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- AT.peekChar
  case mc of
    Nothing -> pure (TimeOfDay h m 0)
    Just c | not (isDigit c) -> do
      _ <- AT.anyChar -- should be the separator
      ns <- parseSecondsAndNanoseconds
      pure (TimeOfDay h m ns)
    Just _ -> do
      ns <- parseSecondsAndNanoseconds
      pure (TimeOfDay h m ns)

parseSecondsAndNanoseconds :: Parser Int64
parseSecondsAndNanoseconds = do
  s' <- parseFixedDigits 2
  let s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- AT.char '.'
         numberOfZeroes <- countZeroes
         AT.peekChar >>= \case
           Just c | c >= '0' && c <= '9' -> do
             x <- AT.decimal
             let totalDigits = countDigits x + numberOfZeroes
                 result = if totalDigits == 9
                   then x
                   else if totalDigits < 9
                     then x * raiseTenTo (9 - totalDigits)
                     else quot x (raiseTenTo (totalDigits - 9))
             pure (fromIntegral result)
           _ -> pure 0
    ) <|> pure 0
  pure (s * 1000000000 + nanoseconds)

countZeroes :: AT.Parser Int
countZeroes = go 0 where
  go !i = do
    m <- AT.peekChar
    case m of
      Nothing -> pure i
      Just c -> if c == '0'
        then AT.anyChar *> go (i + 1)
        else pure i

nanosecondsBuilder :: Int64 -> TB.Builder
nanosecondsBuilder w
  | w == 0 = mempty
  | w > 99999999 = "." <> TB.decimal w
  | w > 9999999 = ".0" <> TB.decimal w
  | w > 999999 = ".00" <> TB.decimal w
  | w > 99999 = ".000" <> TB.decimal w
  | w > 9999 = ".0000" <> TB.decimal w
  | w > 999 = ".00000" <> TB.decimal w
  | w > 99 = ".000000" <> TB.decimal w
  | w > 9 = ".0000000" <> TB.decimal w
  | otherwise = ".00000000" <> TB.decimal w

microsecondsBuilder :: Int64 -> TB.Builder
microsecondsBuilder w
  | w == 0 = mempty
  | w > 99999 = "." <> TB.decimal w
  | w > 9999 = ".0" <> TB.decimal w
  | w > 999 = ".00" <> TB.decimal w
  | w > 99 = ".000" <> TB.decimal w
  | w > 9 = ".0000" <> TB.decimal w
  | otherwise = ".00000" <> TB.decimal w

millisecondsBuilder :: Int64 -> TB.Builder
millisecondsBuilder w
  | w == 0 = mempty
  | w > 99 = "." <> TB.decimal w
  | w > 9 = ".0" <> TB.decimal w
  | otherwise = ".00" <> TB.decimal w

prettyNanosecondsBuilder :: SubsecondPrecision -> Int64 -> TB.Builder
prettyNanosecondsBuilder sp nano = case sp of
  SubsecondPrecisionAuto
    | milliRem == 0 -> millisecondsBuilder milli
    | microRem == 0 -> microsecondsBuilder micro
    | otherwise -> nanosecondsBuilder nano
  SubsecondPrecisionFixed d -> if d == 0
    then mempty
    else
      let newSubsecondPart = quot nano (raiseTenTo (9 - d))
       in "."
          <> TB.fromText (Text.replicate (d - countDigits newSubsecondPart) "0")
          <> TB.decimal newSubsecondPart
  where
  (milli,milliRem) = quotRem nano 1000000
  (micro,microRem) = quotRem nano 1000

-- | Encode a 'Timespan' as 'Text' using the given 'SubsecondPrecision'.
encodeTimespan :: SubsecondPrecision -> Timespan -> Text
encodeTimespan sp =
  LT.toStrict . TB.toLazyText . builderTimespan sp

-- | Construct a 'Text' 'TB.Builder' corresponding to an encoding
--   of the given 'Timespan' using the given 'SubsecondPrecision'.
builderTimespan :: SubsecondPrecision -> Timespan -> TB.Builder
builderTimespan sp (Timespan ns) =
  TB.decimal sInt64 <> prettyNanosecondsBuilder sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000

internalBuilder_NS :: SubsecondPrecision -> Maybe Char -> Int -> Int64 -> TB.Builder
internalBuilder_NS sp msep m ns = case msep of
  Nothing -> indexTwoDigitTextBuilder m
          <> indexTwoDigitTextBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  Just sep -> let sepBuilder = TB.singleton sep in
             sepBuilder
          <> indexTwoDigitTextBuilder m
          <> sepBuilder
          <> indexTwoDigitTextBuilder s
          <> prettyNanosecondsBuilder sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000
  !s = fromIntegral sInt64

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct a
--   'Text' 'TB.Builder' corresponding to a
--   Day\/Month\/Year,Hour\/Minute\/Second encoding of the given 'Datetime'.
builder_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> builder_Dmy mdateSep date
            <> builder_HMS sp mtimeSep time
    Just sep -> builder_Dmy mdateSep date
             <> TB.singleton sep
             <> builder_HMS sp mtimeSep time

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision',
--   and a 'DatetimeFormat', construct a 'Text' 'TB.Builder'
--   corresponding to a Day\/Month\/Year,IMS encoding of the given
--   'Datetime'. This differs from 'builder_DmyIMSp' in that
--   it adds a space between the locale and seconds.
builder_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Dmy mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision',
--   and a 'DatetimeFormat', construct a 'Text' 'TB.Builder'
--   corresponding to a Day\/Month\/Year,IMS encoding of the given
--   'Datetime'.
builder_DmyIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_DmyIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Dmy mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

-- | Given a 'SubsecondPrecision' and 'DatetimeFormat', construct
--   'Text' that corresponds to a Day\/Month\/Year,Hour\/Minute\/Second
--   encoding of the given 'Datetime'.
encode_DmyHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyHMS sp format =
  LT.toStrict . TB.toLazyText . builder_DmyHMS sp format

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct 'Text' that corresponds to a
--   Day\/Month\/Year,IMS encoding of the given 'Datetime'. This
--   inserts a space between the locale and seconds.
encode_DmyIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_DmyIMS_p a sp b = LT.toStrict . TB.toLazyText . builder_DmyIMS_p a sp b

-- | Given a 'SubsecondPrecision' and 'DatetimeFormat', construct
--   'Text' that corresponds to a Year\/Month\/Day,Hour\/Minute\/Second
--   encoding of the given 'Datetime'.
encode_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdHMS sp format =
  LT.toStrict . TB.toLazyText . builder_YmdHMS sp format

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct 'Text' that corresponds to a
--   Year\/Month\/Day,IMS encoding of the given 'Datetime'. This
--   inserts a space between the locale and seconds.
encode_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> Text
encode_YmdIMS_p a sp b = LT.toStrict . TB.toLazyText . builder_YmdIMS_p a sp b

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct
--   a 'Text' 'TB.Builder' corresponding to a
--   Year\/Month\/Day,Hour\/Minute\/Second encoding of the given 'Datetime'.
builder_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> builder_Ymd mdateSep date
            <> builder_HMS sp mtimeSep time
    Just sep -> builder_Ymd mdateSep date
             <> TB.singleton sep
             <> builder_HMS sp mtimeSep time

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct a 'Text' 'TB.Builder' that
--   corresponds to a Year\/Month\/Day,IMS encoding of the
--   given 'Datetime'. This inserts a space between the locale
--   and seconds.
builder_YmdIMS_p :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Ymd mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct a 'Text' 'TB.Builder' that
--   corresponds to a Year\/Month\/Day,IMS encoding of the
--   given 'Datetime'.
builder_YmdIMSp :: MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> Datetime -> TB.Builder
builder_YmdIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builder_Ymd mdateSep date
  <> maybe mempty TB.singleton msep
  <> builder_IMS_p locale sp mtimeSep time

-- | Construct a 'Text' 'TB.Builder' corresponding to the W3C
--   encoding of the given 'Datetime'.
--
--   Deprecated. This is just a poorly named alias for 'builderIso8601'.
builderW3C :: Datetime -> TB.Builder
builderW3C = builderIso8601

-- | Construct a 'Text' 'TB.Builder' corresponding to the ISO-8601
--   encoding of the given 'Datetime'.
builderIso8601 :: Datetime -> TB.Builder
builderIso8601 = builder_YmdHMS SubsecondPrecisionAuto w3c

-- | Construct 'Text' corresponding to the ISO-8601
--   encoding of the given 'Datetime'.
encodeIso8601 :: Datetime -> Text
encodeIso8601 = LT.toStrict . TB.toLazyText . builderIso8601

-- | Decode a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime'
--   from 'Text' that was encoded with the given 'DatetimeFormat'.
decode_YmdHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS format =
  either (const Nothing) Just . AT.parseOnly (parser_YmdHMS format)

-- | Decode a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator.
decode_YmdHMS_lenient :: Text -> Maybe Datetime
decode_YmdHMS_lenient =
  either (const Nothing) Just . AT.parseOnly parser_YmdHMS_lenient

-- | Parse a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime'
--   that was encoded with the given 'DatetimeFormat'.
parser_DmyHMS :: DatetimeFormat -> Parser Datetime
parser_DmyHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Dmy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS mtimeSep
  pure (Datetime date time)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_DmyHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_DmyHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Dmy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS_opt_S mtimeSep
  pure (Datetime date time)

-- | Parse a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator, such as:
--
-- 01-05-2017T23:13:05
-- 01-05-2017 23:13:05
-- 01/05/2017 23:13:05
-- 01y01/2018x23;50&29
parser_DmyHMS_lenient :: Parser Datetime
parser_DmyHMS_lenient = do
  mdate <- optional $ parser_Dmy Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS Nothing
    Nothing -> Datetime <$> parser_Dmy_lenient <* parserLenientSeparator <*> parser_HMS_lenient

-- | Parse a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_DmyHMS_opt_S_lenient :: Parser Datetime
parser_DmyHMS_opt_S_lenient = do
  mdate <- optional $ parser_Dmy Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS_opt_S Nothing
    Nothing -> Datetime <$> parser_Dmy_lenient <* parserLenientSeparator <*> parser_HMS_opt_S_lenient

-- | Decodes Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
-- 'Text' that is encoded with either no separators or any non-numeric
-- characters as separators, such as:
--
-- 2017-01-05T23:13:05
-- 2017-01-05 23:13:05
-- 2017/01/05 23:13:05
-- 2018x01y01/23;50&29
decode_DmyHMS_lenient :: Text -> Maybe Datetime
decode_DmyHMS_lenient = either (const Nothing) Just . AT.parseOnly parser_DmyHMS_lenient

-- | Decode a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime'
--   from 'Text' that was encoded with the given 'DatetimeFormat'.
decode_DmyHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS format =
  either (const Nothing) Just . AT.parseOnly (parser_DmyHMS format)

-- | Decode a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with with the given 'DatetimeFormat' and with
--   either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
decode_DmyHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_DmyHMS_opt_S format =
  either (const Nothing) Just . AT.parseOnly (parser_DmyHMS_opt_S format)

-- | Decode a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
decode_DmyHMS_opt_S_lenient :: Text -> Maybe Datetime
decode_DmyHMS_opt_S_lenient =
  either (const Nothing) Just . AT.parseOnly parser_DmyHMS_opt_S_lenient


-- | Parses a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime'
--   that was encoded using the given 'DatetimeFormat'.
parser_MdyHMS :: DatetimeFormat -> Parser Datetime
parser_MdyHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Mdy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS mtimeSep
  pure (Datetime date time)

-- | Parses a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' that was
--   encoded with either no separators or any non-numeric character for each
--   separator.
parser_MdyHMS_lenient :: Parser Datetime
parser_MdyHMS_lenient = do
  mdate <- optional $ parser_Mdy Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS Nothing
    Nothing -> Datetime <$> parser_Mdy_lenient <* parserLenientSeparator <*> parser_HMS_lenient

-- | Parse a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with with the given 'DatetimeFormat' and with
--   either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero.
parser_MdyHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_MdyHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Mdy mdateSep
  traverse_ AT.char msep
  time <- parser_HMS_opt_S mtimeSep
  pure (Datetime date time)

-- | Parse a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero.
parser_MdyHMS_opt_S_lenient :: Parser Datetime
parser_MdyHMS_opt_S_lenient = do
  mdate <- optional $ parser_Mdy Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS_opt_S Nothing
    Nothing -> Datetime <$> parser_Mdy_lenient <* parserLenientSeparator <*> parser_HMS_opt_S_lenient

-- | Decode a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime'
--   from 'Text' that was encoded with the given 'DatetimeFormat'.
decode_MdyHMS :: DatetimeFormat -> Text -> Maybe Datetime
decode_MdyHMS format =
  either (const Nothing) Just . AT.parseOnly (parser_MdyHMS format)

-- | Decode a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' with either no separators or any non-numeric character for each
--   separator.
decode_MdyHMS_lenient :: Text -> Maybe Datetime
decode_MdyHMS_lenient =
  either (const Nothing) Just . AT.parseOnly parser_MdyHMS_lenient

-- | Decode a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with the given 'DatetimeFormat' and with either of
--   the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero.
decode_MdyHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_MdyHMS_opt_S format =
  either (const Nothing) Just . AT.parseOnly (parser_MdyHMS_opt_S format)

-- | Parse a Month\/Day\/Year,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' with either no separators or any non-numeric character for each
--   separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero.
decode_MdyHMS_opt_S_lenient :: Text -> Maybe Datetime
decode_MdyHMS_opt_S_lenient =
  either (const Nothing) Just . AT.parseOnly parser_MdyHMS_opt_S_lenient

-- | Parses a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime'
--   that was encoded using the given 'DatetimeFormat'.
parser_YmdHMS :: DatetimeFormat -> Parser Datetime
parser_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Ymd mdateSep
  traverse_ AT.char msep
  time <- parser_HMS mtimeSep
  pure (Datetime date time)

-- | Parses a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime' that was
--   encoded with either no separators or any non-numeric character for each
--   separator.
parser_YmdHMS_lenient :: Parser Datetime
parser_YmdHMS_lenient = do
  mdate <- optional $ parser_Ymd Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS Nothing
    Nothing -> Datetime <$> parser_Ymd_lenient <* parserLenientSeparator <*> parser_HMS_lenient

-- | Parses a Year\/Month\/Date,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with the given 'DatetimeFormat' and with either of
--   the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_YmdHMS_opt_S :: DatetimeFormat -> Parser Datetime
parser_YmdHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parser_Ymd mdateSep
  traverse_ AT.char msep
  time <- parser_HMS_opt_S mtimeSep
  pure (Datetime date time)

-- | Parses a Year\/Month\/Date,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parser_YmdHMS_opt_S_lenient :: Parser Datetime
parser_YmdHMS_opt_S_lenient = do
  mdate <- optional $ parser_Ymd Nothing
  case mdate of
    Just date -> Datetime date <$> parser_HMS_opt_S Nothing
    Nothing -> Datetime <$> parser_Ymd_lenient <* parserLenientSeparator <*> parser_HMS_opt_S_lenient

-- | Decode a Year\/Month\/Date,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with the given 'DatetimeFormat' and with either of
--   the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
decode_YmdHMS_opt_S :: DatetimeFormat -> Text -> Maybe Datetime
decode_YmdHMS_opt_S format =
  either (const Nothing) Just . AT.parseOnly (parser_YmdHMS_opt_S format)

-- | Decode a Year\/Month\/Date,Hour\/Minute\/Second-encoded 'Datetime' from
--   'Text' that was encoded with either no separators or any non-numeric
--   character for each separator and with either of the following time formats:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
decode_YmdHMS_opt_S_lenient :: Text -> Maybe Datetime
decode_YmdHMS_opt_S_lenient =
  either (const Nothing) Just . AT.parseOnly parser_YmdHMS_opt_S_lenient

-- | Parses a 'Datetime' from 'Text' that was encoded with any of the following
-- formats and with either no separators or any non-numeric character for each
-- separator.
--
-- * @%Y-%M-%D %H:%M@
-- * @%Y-%M-%D %H:%M:%S@
-- * @%D-%M-%Y %H:%M@
-- * @%D-%M-%Y %H:%M:%S@
-- * @%M-%D-%Y %H:%M@
-- * @%M-%D-%Y %H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is not provided,
-- it is assumed to be zero. Note that this is the least performant parser due
-- to backtracking
parser_lenient :: Parser Datetime
parser_lenient = parser_YmdHMS_opt_S_lenient <|> parser_DmyHMS_opt_S_lenient <|> parser_MdyHMS_opt_S_lenient

-- | Parses text that was encoded in DMY, YMD, or MDY format with optional
-- seconds and any non-numeric character as separators.
decode_lenient :: Text -> Maybe Datetime
decode_lenient =
  either (const Nothing) Just . AT.parseOnly parser_lenient
---------------
-- ByteString stuff
---------------

-- | Given a 'SubsecondPrecision' and a separator, construct a 'ByteString' 'BB.Builder' corresponding to an Hour\/Minute\/Second encoding of the given 'TimeOfDay'.
builderUtf8_HMS :: SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_HMS sp msep (TimeOfDay h m ns) =
     indexTwoDigitByteStringBuilder h
  <> internalBuilderUtf8_NS sp msep m ns

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a separator, construct a 'ByteString' 'BB.Builder' corresponding to an IMS encoding of the given 'TimeOfDay'. This differs from 'builderUtf8_IMSp' in that
-- there is a space between the seconds and locale.
builderUtf8_IMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_IMS_p meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilderUtf8_I h
  <> internalBuilderUtf8_NS sp msep m ns
  <> " "
  <> internalBuilderUtf8_p meridiemLocale h

internalBuilderUtf8_I :: Int -> BB.Builder
internalBuilderUtf8_I h =
  indexTwoDigitByteStringBuilder $ if h > 12
    then h - 12
    else if h == 0
      then 12
      else h

internalBuilderUtf8_p :: MeridiemLocale ByteString -> Int -> BB.Builder
internalBuilderUtf8_p (MeridiemLocale am pm) h = if h > 11
  then BB.byteString pm
  else BB.byteString am

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a separator, construct a 'ByteString' 'BB.Builder' corresponding to an IMS encoding of the given 'TimeOfDay'.
builderUtf8_IMSp :: MeridiemLocale ByteString -> SubsecondPrecision -> Maybe Char -> TimeOfDay -> BB.Builder
builderUtf8_IMSp meridiemLocale sp msep (TimeOfDay h m ns) =
     internalBuilderUtf8_I h
  <> internalBuilderUtf8_NS sp msep m ns
  <> internalBuilderUtf8_p meridiemLocale h

-- | Parse an Hour\/Minute\/Second-encoded 'TimeOfDay' that uses
--   the given separator.
parserUtf8_HMS :: Maybe Char -> AB.Parser TimeOfDay
parserUtf8_HMS msep = do
  h <- parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AB.char msep
  m <- parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ AB.char msep
  ns <- parseSecondsAndNanosecondsUtf8
  pure (TimeOfDay h m ns)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parserUtf8_HMS_opt_S :: Maybe Char -> AB.Parser TimeOfDay
parserUtf8_HMS_opt_S msep = do
  h <- parseFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  traverse_ AB.char msep
  m <- parseFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  mc <- AB.peekChar
  case mc of
    Nothing -> pure (TimeOfDay h m 0)
    Just c -> case msep of
      Just sep -> if c == sep
        then do
          _ <- AB.anyChar -- should be the separator
          ns <- parseSecondsAndNanosecondsUtf8
          pure (TimeOfDay h m ns)
        else pure (TimeOfDay h m 0)
      -- if there is no separator, we will try to parse the
      -- remaining part as seconds. We commit to trying to
      -- parse as seconds if we see any number as the next
      -- character.
      Nothing -> if isDigit c
        then do
          ns <- parseSecondsAndNanosecondsUtf8
          pure (TimeOfDay h m ns)
        else pure (TimeOfDay h m 0)

parseSecondsAndNanosecondsUtf8 :: AB.Parser Int64
parseSecondsAndNanosecondsUtf8 = do
  s' <- parseFixedDigitsIntBS 2
  let !s = fromIntegral s' :: Int64
  -- TODO: whoops, this should probably be gt 59, not 60
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- AB.char '.'
         numberOfZeroes <- countZeroesUtf8
         AB.peekChar >>= \case
           Just c | c >= '0' && c <= '9' -> do
             x <- AB.decimal
             let totalDigits = countDigits x + numberOfZeroes
                 result = if totalDigits == 9
                   then x
                   else if totalDigits < 9
                     then x * raiseTenTo (9 - totalDigits)
                     else quot x (raiseTenTo (totalDigits - 9))
             pure (fromIntegral result)
           _ -> pure 0
    ) <|> pure 0
  pure (s * 1000000000 + nanoseconds)

countZeroesUtf8 :: AB.Parser Int
countZeroesUtf8 = go 0 where
  go !i = do
    m <- AB.peekChar
    case m of
      Nothing -> pure i
      Just c -> if c == '0'
        then AB.anyChar *> go (i + 1)
        else pure i

nanosecondsBuilderUtf8 :: Int64 -> BB.Builder
nanosecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99999999 = "." <> int64Builder w
  | w > 9999999 = ".0" <> int64Builder w
  | w > 999999 = ".00" <> int64Builder w
  | w > 99999 = ".000" <> int64Builder w
  | w > 9999 = ".0000" <> int64Builder w
  | w > 999 = ".00000" <> int64Builder w
  | w > 99 = ".000000" <> int64Builder w
  | w > 9 = ".0000000" <> int64Builder w
  | otherwise = ".00000000" <> int64Builder w

microsecondsBuilderUtf8 :: Int64 -> BB.Builder
microsecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99999 = "." <> int64Builder w
  | w > 9999 = ".0" <> int64Builder w
  | w > 999 = ".00" <> int64Builder w
  | w > 99 = ".000" <> int64Builder w
  | w > 9 = ".0000" <> int64Builder w
  | otherwise = ".00000" <> int64Builder w

millisecondsBuilderUtf8 :: Int64 -> BB.Builder
millisecondsBuilderUtf8 w
  | w == 0 = mempty
  | w > 99 = "." <> int64Builder w
  | w > 9 = ".0" <> int64Builder w
  | otherwise = ".00" <> int64Builder w

prettyNanosecondsBuilderUtf8 :: SubsecondPrecision -> Int64 -> BB.Builder
prettyNanosecondsBuilderUtf8 sp nano = case sp of
  SubsecondPrecisionAuto
    | milliRem == 0 -> millisecondsBuilderUtf8 milli
    | microRem == 0 -> microsecondsBuilderUtf8 micro
    | otherwise -> nanosecondsBuilderUtf8 nano
  SubsecondPrecisionFixed d -> if d == 0
    then mempty
    else
      let newSubsecondPart = quot nano (raiseTenTo (9 - d))
       in BB.char7 '.'
          <> BB.byteString (BC.replicate (d - countDigits newSubsecondPart) '0')
          <> int64Builder newSubsecondPart
  where
  (milli,milliRem) = quotRem nano 1000000
  (micro,microRem) = quotRem nano 1000

-- | Given a 'SubsecondPrecision', construct a 'ByteString' corresponding
--   to an encoding of the given 'Timespan'.
encodeTimespanUtf8 :: SubsecondPrecision -> Timespan -> ByteString
encodeTimespanUtf8 sp =
  LB.toStrict . BB.toLazyByteString . builderTimespanUtf8 sp

-- | Given a 'SubsecondPrecision', construct a 'ByteString' 'BB.Builder'
--   corresponding to an encoding of the given 'Timespan'.
builderTimespanUtf8 :: SubsecondPrecision -> Timespan -> BB.Builder
builderTimespanUtf8 sp (Timespan ns) =
  int64Builder sInt64 <> prettyNanosecondsBuilderUtf8 sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000

int64Builder :: Int64 -> BB.Builder
int64Builder = BB.integerDec . fromIntegral

internalBuilderUtf8_NS :: SubsecondPrecision -> Maybe Char -> Int -> Int64 -> BB.Builder
internalBuilderUtf8_NS sp msep m ns = case msep of
  Nothing -> indexTwoDigitByteStringBuilder m
          <> indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilderUtf8 sp nsRemainder
  Just sep -> let sepBuilder = BB.char7 sep in
             sepBuilder
          <> indexTwoDigitByteStringBuilder m
          <> sepBuilder
          <> indexTwoDigitByteStringBuilder s
          <> prettyNanosecondsBuilderUtf8 sp nsRemainder
  where
  (!sInt64,!nsRemainder) = quotRem ns 1000000000
  !s = fromIntegral sInt64

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct
--   a 'ByteString' corresponding to a Year\/Month\/Day,Hour\/Minute\/Second
--   encoding of the given 'Datetime'.
encodeUtf8_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> ByteString
encodeUtf8_YmdHMS sp format =
  LB.toStrict . BB.toLazyByteString . builderUtf8_YmdHMS sp format

-- | Given a 'MeridiemLocale', a 'SubsecondPrecision', and a 'DatetimeFormat',
--   construct a 'ByteString' corresponding to a Year\/Month\/Day,IMS encoding
--   of the given 'Datetime'. This inserts a space between the locale and
--   seconds.
encodeUtf8_YmdIMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> ByteString
encodeUtf8_YmdIMS_p a sp b = LB.toStrict . BB.toLazyByteString . builderUtf8_YmdIMS_p a sp b

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct
--   a 'ByteString' 'BB.Builder' corresponding to a
--   Year\/Month\/Day,Hour\/Minute\/Second encoding of the
--   given 'Datetime'.
builderUtf8_YmdHMS :: SubsecondPrecision -> DatetimeFormat -> Datetime -> BB.Builder
builderUtf8_YmdHMS sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
  case msep of
    Nothing -> builderUtf8_Ymd mdateSep date
            <> builderUtf8_HMS sp mtimeSep time
    Just sep -> builderUtf8_Ymd mdateSep date
             <> BB.char7 sep
             <> builderUtf8_HMS sp mtimeSep time

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct
--   a 'ByteString' 'BB.Builder' corresponding to a
--   Year\/Month\/Day,IMS encoding of the given 'Datetime'. This inserts
--   a space between the locale and seconds.
builderUtf8_YmdIMS_p :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> BB.Builder
builderUtf8_YmdIMS_p locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builderUtf8_Ymd mdateSep date
  <> maybe mempty BB.char7 msep
  <> builderUtf8_IMS_p locale sp mtimeSep time

-- | Given a 'SubsecondPrecision' and a 'DatetimeFormat', construct
--   a 'ByteString' 'BB.Builder' corresponding to a
--   Year\/Month\/Day,IMS encoding of the given 'Datetime'.
builderUtf8_YmdIMSp :: MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> Datetime -> BB.Builder
builderUtf8_YmdIMSp locale sp (DatetimeFormat mdateSep msep mtimeSep) (Datetime date time) =
     builderUtf8_Ymd mdateSep date
  <> maybe mempty BB.char7 msep
  <> builderUtf8_IMS_p locale sp mtimeSep time

-- | Construct a 'ByteString' 'BB.Builder' corresponding to
--   a W3C encoding of the given 'Datetime'.
builderUtf8W3C :: Datetime -> BB.Builder
builderUtf8W3C = builderUtf8_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

-- | Decode a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime' from
--   a 'ByteString'.
decodeUtf8_YmdHMS :: DatetimeFormat -> ByteString -> Maybe Datetime
decodeUtf8_YmdHMS format =
  either (const Nothing) Just . AB.parseOnly (parserUtf8_YmdHMS format)

-- | Parse a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'Datetime' that was
--   encoded using the given 'DatetimeFormat'.
parserUtf8_YmdHMS :: DatetimeFormat -> AB.Parser Datetime
parserUtf8_YmdHMS (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parserUtf8_Ymd mdateSep
  traverse_ AB.char msep
  time <- parserUtf8_HMS mtimeSep
  pure (Datetime date time)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
parserUtf8_YmdHMS_opt_S :: DatetimeFormat -> AB.Parser Datetime
parserUtf8_YmdHMS_opt_S (DatetimeFormat mdateSep msep mtimeSep) = do
  date <- parserUtf8_Ymd mdateSep
  traverse_ AB.char msep
  time <- parserUtf8_HMS_opt_S mtimeSep
  pure (Datetime date time)

-- | Parses text that is formatted as either of the following:
--
-- * @%H:%M@
-- * @%H:%M:%S@
--
-- That is, the seconds and subseconds part is optional. If it is
-- not provided, it is assumed to be zero. This format shows up
-- in Google Chrome\'s @datetime-local@ inputs.
decodeUtf8_YmdHMS_opt_S :: DatetimeFormat -> ByteString -> Maybe Datetime
decodeUtf8_YmdHMS_opt_S format =
  either (const Nothing) Just . AB.parseOnly (parserUtf8_YmdHMS_opt_S format)

-- | Given an 'OffsetFormat', a 'SubsecondPrecision', and
--   a 'DatetimeFormat', construct a 'Text' 'TB.Builder'
--   corresponding to a Year\/Month\/Day,Hour\/Minute\/Second encoding
--   of the given 'OffsetDatetime'.
builder_YmdHMSz :: OffsetFormat -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> TB.Builder
builder_YmdHMSz offsetFormat sp datetimeFormat (OffsetDatetime datetime offset) =
     builder_YmdHMS sp datetimeFormat datetime
  <> builderOffset offsetFormat offset

-- | Parse a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'OffsetDatetime'
--   that was encoded using the given 'OffsetFormat'
--   and 'DatetimeFormat'.
parser_YmdHMSz :: OffsetFormat -> DatetimeFormat -> Parser OffsetDatetime
parser_YmdHMSz offsetFormat datetimeFormat = OffsetDatetime
  <$> parser_YmdHMS datetimeFormat
  <*> parserOffset offsetFormat

-- | Given an 'OffsetFormat', a 'MeridiemLocale', a
--   'SubsecondPrecision', and 'DatetimeFormat', construct a
--   'Text' 'TB.Builder' corresponding to a Year\/Month\/Day,IMS-encoding
--   of the given 'OffsetDatetime'.
builder_YmdIMS_p_z :: OffsetFormat -> MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> TB.Builder
builder_YmdIMS_p_z offsetFormat meridiemLocale sp datetimeFormat (OffsetDatetime datetime offset) =
     builder_YmdIMS_p meridiemLocale sp datetimeFormat datetime
  <> " "
  <> builderOffset offsetFormat offset

-- | Given an 'OffsetFormat', a 'SubsecondPrecision',
--   and a 'DatetimeFormat', construct 'Text' corresponding to
--   the Year\/Month\/Day,Hour\/Minute\/Second-encoding of
--   the given 'OffsetDatetime'.
encode_YmdHMSz :: OffsetFormat -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> Text
encode_YmdHMSz offsetFormat sp datetimeFormat =
    LT.toStrict . TB.toLazyText . builder_YmdHMSz offsetFormat sp datetimeFormat

-- | Given an 'OffsetFormat', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct a 'Text' 'TB.Builder' corresponding
--   to the Day\/Month\/Year,Hour\/Minute\/Second-encoding of
--   the given 'OffsetDatetime'.
builder_DmyHMSz :: OffsetFormat -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> TB.Builder
builder_DmyHMSz offsetFormat sp datetimeFormat (OffsetDatetime datetime offset) =
     builder_DmyHMS sp datetimeFormat datetime
  <> builderOffset offsetFormat offset

-- | Parse a Day\/Month\/Year,Hour\/Minute\/Second-encoded 'OffsetDatetime'
--   that was encoded using the given 'OffsetFormat'
--   and 'DatetimeFormat'.
parser_DmyHMSz :: OffsetFormat -> DatetimeFormat -> AT.Parser OffsetDatetime
parser_DmyHMSz offsetFormat datetimeFormat = OffsetDatetime
  <$> parser_DmyHMS datetimeFormat
  <*> parserOffset offsetFormat

-- | Given an 'OffsetFormat', a 'MeridiemLocale', a
--   'SubsecondPrecision', and a 'DatetimeFormat', construct a 'Text'
--   'TB.Builder' corresponding to the Day\/Month\/Year,IMS encoding
--   of the given 'OffsetDatetime'.
builder_DmyIMS_p_z :: OffsetFormat -> MeridiemLocale Text -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> TB.Builder
builder_DmyIMS_p_z offsetFormat meridiemLocale sp datetimeFormat (OffsetDatetime datetime offset) =
      builder_DmyIMS_p meridiemLocale sp datetimeFormat datetime
   <> " "
   <> builderOffset offsetFormat offset

-- | Given an 'OffsetFormat', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct 'Text' corresponding to the
--   Day\/Month\/Year,Hour\/Minute\/Second encoding of the given
--   'OffsetDatetime'.
encode_DmyHMSz :: OffsetFormat -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> Text
encode_DmyHMSz offsetFormat sp datetimeFormat =
    LT.toStrict . TB.toLazyText . builder_DmyHMSz offsetFormat sp datetimeFormat

-- | Construct a 'Text' 'TB.Builder' corresponding to the w3c-formatting
--   of the given 'OffsetDatetime'.
builderW3Cz :: OffsetDatetime -> TB.Builder
builderW3Cz = builder_YmdHMSz
  OffsetFormatColonOn
  SubsecondPrecisionAuto
  (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

-- | Encode an 'Offset' to 'Text' using the given 'OffsetFormat'.
encodeOffset :: OffsetFormat -> Offset -> Text
encodeOffset fmt = LT.toStrict . TB.toLazyText . builderOffset fmt

-- | Construct a 'TB.Builder' corresponding to the given 'Offset'
--   encoded using the given 'OffsetFormat'.
builderOffset :: OffsetFormat -> Offset -> TB.Builder
builderOffset x = case x of
  OffsetFormatColonOff -> builderOffset_z
  OffsetFormatColonOn -> builderOffset_z1
  OffsetFormatSecondsPrecision -> builderOffset_z2
  OffsetFormatColonAuto -> builderOffset_z3

-- | Decode an 'Offset' from 'Text' that was encoded
--   using the given 'OffsetFormat'.
decodeOffset :: OffsetFormat -> Text -> Maybe Offset
decodeOffset fmt =
  either (const Nothing) Just . AT.parseOnly (parserOffset fmt <* AT.endOfInput)

-- | Parse an 'Offset' that was encoded using the given 'OffsetFormat'.
parserOffset :: OffsetFormat -> Parser Offset
parserOffset x = case x of
  OffsetFormatColonOff -> parserOffset_z
  OffsetFormatColonOn -> parserOffset_z1
  OffsetFormatSecondsPrecision -> parserOffset_z2
  OffsetFormatColonAuto -> parserOffset_z3

-- | True means positive, false means negative
parseSignedness :: Parser Bool
parseSignedness = do
  c <- AT.anyChar
  if c == '-'
    then pure False
    else if c == '+'
      then pure True
      else fail "while parsing offset, expected [+] or [-]"

parserOffset_z :: Parser Offset
parserOffset_z = do
  pos <- parseSignedness
  h <- parseFixedDigits 2
  m <- parseFixedDigits 2
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

parserOffset_z1 :: Parser Offset
parserOffset_z1 = do
  pos <- parseSignedness
  h <- parseFixedDigits 2
  _ <- AT.char ':'
  m <- parseFixedDigits 2
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

parserOffset_z2 :: AT.Parser Offset
parserOffset_z2 = do
  pos <- parseSignedness
  h <- parseFixedDigits 2
  _ <- AT.char ':'
  m <- parseFixedDigits 2
  _ <- AT.string ":00"
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

-- | This is generous in what it accepts. If you give
--   something like +04:00 as the offset, it will be
--   allowed, even though it could be shorter.
parserOffset_z3 :: AT.Parser Offset
parserOffset_z3 = do
  pos <- parseSignedness
  h <- parseFixedDigits 2
  mc <- AT.peekChar
  case mc of
    Just ':' -> do
      _ <- AT.anyChar -- should be a colon
      m <- parseFixedDigits 2
      let !res = h * 60 + m
      pure . Offset $ if pos
        then res
        else negate res
    _ -> pure . Offset $ if pos
      then h * 60
      else h * (-60)

builderOffset_z :: Offset -> TB.Builder
builderOffset_z (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitTextBuilder a
      <> indexTwoDigitTextBuilder b

builderOffset_z1 :: Offset -> TB.Builder
builderOffset_z1 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitTextBuilder a
      <> ":"
      <> indexTwoDigitTextBuilder b

builderOffset_z2 :: Offset -> TB.Builder
builderOffset_z2 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitTextBuilder a
      <> ":"
      <> indexTwoDigitTextBuilder b
      <> ":00"

builderOffset_z3 :: Offset -> TB.Builder
builderOffset_z3 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in if b == 0
        then prefix
          <> indexTwoDigitTextBuilder a
        else prefix
          <> indexTwoDigitTextBuilder a
          <> ":"
          <> indexTwoDigitTextBuilder b

-- | Given an 'OffsetFormat', a 'SubsecondPrecision', and a
--   'DatetimeFormat', construct a 'ByteString' 'BB.Builder'
--   corresponding to the Year\/Month\/Day,Hour\/Minute\/Second
--   encoding of the given 'OffsetDatetime'.
builderUtf8_YmdHMSz :: OffsetFormat -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> BB.Builder
builderUtf8_YmdHMSz offsetFormat sp datetimeFormat (OffsetDatetime datetime offset) =
     builderUtf8_YmdHMS sp datetimeFormat datetime
  <> builderOffsetUtf8 offsetFormat offset

-- | Parse a Year\/Month\/Day,Hour\/Minute\/Second-encoded 'OffsetDatetime'
--   that was encoded using the given 'OffsetFormat' and
--   'DatetimeFormat'.
parserUtf8_YmdHMSz :: OffsetFormat -> DatetimeFormat -> AB.Parser OffsetDatetime
parserUtf8_YmdHMSz offsetFormat datetimeFormat = OffsetDatetime
  <$> parserUtf8_YmdHMS datetimeFormat
  <*> parserOffsetUtf8 offsetFormat

-- | Given an 'OffsetFormat', a 'MeridiemLocale, a 'SubsecondPrecision',
--   and a 'DatetimeFormat', construct a 'ByteString' 'BB.Builder'
--   corresponding to a Year\/Month\/Day,IMS-encoded 'OffsetDatetime'.
builderUtf8_YmdIMS_p_z :: OffsetFormat -> MeridiemLocale ByteString -> SubsecondPrecision -> DatetimeFormat -> OffsetDatetime -> BB.Builder
builderUtf8_YmdIMS_p_z offsetFormat meridiemLocale sp datetimeFormat (OffsetDatetime datetime offset) =
     builderUtf8_YmdIMS_p meridiemLocale sp datetimeFormat datetime
  <> " "
  <> builderOffsetUtf8 offsetFormat offset

-- | Construct a 'ByteString' 'BB.Builder' corresponding to the W3C
--   encoding of the given 'Datetime'.
builderUtf8W3Cz :: OffsetDatetime -> BB.Builder
builderUtf8W3Cz = builderUtf8_YmdHMSz
  OffsetFormatColonOn
  SubsecondPrecisionAuto
  (DatetimeFormat (Just '-') (Just 'T') (Just ':'))

-- | Encode an 'Offset' as a 'ByteString' using the given 'OffsetFormat'.
encodeOffsetUtf8 :: OffsetFormat -> Offset -> ByteString
encodeOffsetUtf8 fmt = LB.toStrict . BB.toLazyByteString . builderOffsetUtf8 fmt

-- | Decode an 'Offset' from a 'ByteString' that was encoded using the given
--   'OffsetFormat'.
decodeOffsetUtf8 :: OffsetFormat -> ByteString -> Maybe Offset
decodeOffsetUtf8 fmt =
  either (const Nothing) Just . AB.parseOnly (parserOffsetUtf8 fmt)

-- | Construct a 'ByteString' 'BB.Builder' corresponding to the
--   encoding of an 'Offset' using the given 'OffsetFormat'.
builderOffsetUtf8 :: OffsetFormat -> Offset -> BB.Builder
builderOffsetUtf8 x = case x of
  OffsetFormatColonOff -> builderOffsetUtf8_z
  OffsetFormatColonOn -> builderOffsetUtf8_z1
  OffsetFormatSecondsPrecision -> builderOffsetUtf8_z2
  OffsetFormatColonAuto -> builderOffsetUtf8_z3

-- | Parse an 'Offset' that was encoded using the given
--   'OffsetFormat'.
parserOffsetUtf8 :: OffsetFormat -> AB.Parser Offset
parserOffsetUtf8 x = case x of
  OffsetFormatColonOff -> parserOffsetUtf8_z
  OffsetFormatColonOn -> parserOffsetUtf8_z1
  OffsetFormatSecondsPrecision -> parserOffsetUtf8_z2
  OffsetFormatColonAuto -> parserOffsetUtf8_z3

-- | True means positive, false means negative
parseSignednessUtf8 :: AB.Parser Bool
parseSignednessUtf8 = do
  c <- AB.anyChar
  if c == '-'
    then pure False
    else if c == '+'
      then pure True
      else fail "while parsing offset, expected [+] or [-]"

parserOffsetUtf8_z :: AB.Parser Offset
parserOffsetUtf8_z = do
  pos <- parseSignednessUtf8
  h <- parseFixedDigitsIntBS 2
  m <- parseFixedDigitsIntBS 2
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

parserOffsetUtf8_z1 :: AB.Parser Offset
parserOffsetUtf8_z1 = do
  pos <- parseSignednessUtf8
  h <- parseFixedDigitsIntBS 2
  _ <- AB.char ':'
  m <- parseFixedDigitsIntBS 2
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

parserOffsetUtf8_z2 :: AB.Parser Offset
parserOffsetUtf8_z2 = do
  pos <- parseSignednessUtf8
  h <- parseFixedDigitsIntBS 2
  _ <- AB.char ':'
  m <- parseFixedDigitsIntBS 2
  _ <- AB.string ":00"
  let !res = h * 60 + m
  pure . Offset $ if pos
    then res
    else negate res

-- | This is generous in what it accepts. If you give
--   something like +04:00 as the offset, it will be
--   allowed, even though it could be shorter.
parserOffsetUtf8_z3 :: AB.Parser Offset
parserOffsetUtf8_z3 = do
  pos <- parseSignednessUtf8
  h <- parseFixedDigitsIntBS 2
  mc <- AB.peekChar
  case mc of
    Just ':' -> do
      _ <- AB.anyChar -- should be a colon
      m <- parseFixedDigitsIntBS 2
      let !res = h * 60 + m
      pure . Offset $ if pos
        then res
        else negate res
    _ -> pure . Offset $ if pos
      then h * 60
      else h * (-60)

builderOffsetUtf8_z :: Offset -> BB.Builder
builderOffsetUtf8_z (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitByteStringBuilder a
      <> indexTwoDigitByteStringBuilder b

builderOffsetUtf8_z1 :: Offset -> BB.Builder
builderOffsetUtf8_z1 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitByteStringBuilder a
      <> ":"
      <> indexTwoDigitByteStringBuilder b

builderOffsetUtf8_z2 :: Offset -> BB.Builder
builderOffsetUtf8_z2 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in prefix
      <> indexTwoDigitByteStringBuilder a
      <> ":"
      <> indexTwoDigitByteStringBuilder b
      <> ":00"

builderOffsetUtf8_z3 :: Offset -> BB.Builder
builderOffsetUtf8_z3 (Offset i) =
  let (!a,!b) = divMod (abs i) 60
      !prefix = if signum i == (-1) then "-" else "+"
   in if b == 0
        then prefix
          <> indexTwoDigitByteStringBuilder a
        else prefix
          <> indexTwoDigitByteStringBuilder a
          <> ":"
          <> indexTwoDigitByteStringBuilder b

-- Zepto parsers

-- | Parse a 'Datetime' that was encoded using the
--   given 'DatetimeFormat'.
zeptoUtf8_YmdHMS :: DatetimeFormat -> Z.Parser Datetime
zeptoUtf8_YmdHMS (DatetimeFormat mdateSep msep' mtimeSep) = do
  date <- zeptoUtf8_Ymd mdateSep
  let msep = BC.singleton <$> msep'
  traverse_ Z.string msep
  time <- zeptoUtf8_HMS mtimeSep
  pure (Datetime date time)

zeptoCountZeroes :: Z.Parser Int
zeptoCountZeroes = do
  bs <- Z.takeWhile (0x30 ==)
  pure $! BC.length bs

-- | Parse a 'Date' that was encoded using
--   the given separator.
zeptoUtf8_Ymd :: Maybe Char -> Z.Parser Date
zeptoUtf8_Ymd msep' = do
  y <- zeptoFixedDigitsIntBS 4
  let msep = BC.singleton <$> msep'
  traverse_ Z.string msep
  m <- zeptoFixedDigitsIntBS 2
  when (m < 1 || m > 12) (fail "month must be between 1 and 12")
  traverse_ Z.string msep
  d <- zeptoFixedDigitsIntBS 2
  when (d < 1 || d > 31) (fail "day must be between 1 and 31")
  pure (Date (Year y) (Month $ m - 1) (DayOfMonth d))

-- | Parse a 'TimeOfDay' that was encoded using
--   the given separator.
zeptoUtf8_HMS :: Maybe Char -> Z.Parser TimeOfDay
zeptoUtf8_HMS msep' = do
  h <- zeptoFixedDigitsIntBS 2
  when (h > 23) (fail "hour must be between 0 and 23")
  let msep = BC.singleton <$> msep'
  traverse_ Z.string msep
  m <- zeptoFixedDigitsIntBS 2
  when (m > 59) (fail "minute must be between 0 and 59")
  traverse_ Z.string msep
  ns <- zeptoSecondsAndNanosecondsUtf8
  pure (TimeOfDay h m ns)

zeptoFixedDigitsIntBS :: Int -> Z.Parser Int
zeptoFixedDigitsIntBS n = do
  t <- Z.take n
  case BC.readInt t of
    Nothing -> fail "datetime decoding could not parse integral bytestring (a)"
    Just (i,r) -> if BC.null r
      then pure i
      else fail "datetime decoding could not parse integral bytestring (b)"

zeptoSecondsAndNanosecondsUtf8 :: Z.Parser Int64
zeptoSecondsAndNanosecondsUtf8 = do
  s' <- zeptoFixedDigitsIntBS 2
  let s = fromIntegral s' :: Int64
  when (s > 60) (fail "seconds must be between 0 and 60")
  nanoseconds <-
    ( do _ <- Z.string "."
         numberOfZeroes <- zeptoCountZeroes
         x <- zdecimal
         let totalDigits = countDigits x + numberOfZeroes
             result = if totalDigits == 9
               then x
               else if totalDigits < 9
                 then x * raiseTenTo (9 - totalDigits)
                 else quot x (raiseTenTo (totalDigits - 9))
         pure (fromIntegral result)
    ) <|> pure 0
  pure (s * 1000000000 + nanoseconds)

zdecimal :: Z.Parser Int64
zdecimal = do
  digits <- Z.takeWhile wordIsDigit
  case BC.readInt digits of
    Nothing -> fail "somehow this didn't work"
    Just (i,_) -> pure $! fromIntegral i

wordIsDigit :: Word8 -> Bool
wordIsDigit a = 0x30 <= a && a <= 0x39

-- | The 'Month' of January.
january :: Month
january = Month 0

-- | The 'Month' of February.
february :: Month
february = Month 1

-- | The 'Month' of March.
march :: Month
march = Month 2

-- | The 'Month' of April.
april :: Month
april = Month 3

-- | The 'Month' of May.
may :: Month
may = Month 4

-- | The 'Month' of June.
june :: Month
june = Month 5

-- | The 'Month' of July.
july :: Month
july = Month 6

-- | The 'Month' of August.
august :: Month
august = Month 7

-- | The 'Month' of September.
september :: Month
september = Month 8

-- | The 'Month' of October.
october :: Month
october = Month 9

-- | The 'Month' of November.
november :: Month
november = Month 10

-- | The 'Month' of December.
december :: Month
december = Month 11

-- | The 'DayOfWeek' Sunday.
sunday :: DayOfWeek
sunday = DayOfWeek 0

-- | The 'DayOfWeek' Monday.
monday :: DayOfWeek
monday = DayOfWeek 1

-- | The 'DayOfWeek' Tuesday.
tuesday :: DayOfWeek
tuesday = DayOfWeek 2

-- | The 'DayOfWeek' Wednesday.
wednesday :: DayOfWeek
wednesday = DayOfWeek 3

-- | The 'DayOfWeek' Thursday.
thursday :: DayOfWeek
thursday = DayOfWeek 4

-- | The 'DayOfWeek' Friday.
friday :: DayOfWeek
friday = DayOfWeek 5

-- | The 'DayOfWeek' Saturday.
saturday :: DayOfWeek
saturday = DayOfWeek 6

countDigits :: (Integral a) => a -> Int
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

parseFixedDigits :: Int -> AT.Parser Int
parseFixedDigits n = do
  t <- AT.take n
  case Text.decimal t of
    Left err -> fail err
    Right (i,r) -> if Text.null r
      then pure i
      else fail "datetime decoding could not parse integral text"

parseFixedDigitsIntBS :: Int -> AB.Parser Int
parseFixedDigitsIntBS n = do
  t <- AB.take n
  case BC.readInt t of
    Nothing -> fail "datetime decoding could not parse integral bytestring (a)"
    Just (i,r) -> if BC.null r
      then pure i
      else fail "datetime decoding could not parse integral bytestring (b)"

-- Only provide positive numbers to this function.
indexTwoDigitTextBuilder :: Int -> TB.Builder
indexTwoDigitTextBuilder i = if i < 100
  then Vector.unsafeIndex twoDigitTextBuilder (fromIntegral i)
  else TB.decimal i

-- | Only provide positive numbers to this function.
indexTwoDigitByteStringBuilder :: Int -> BB.Builder
indexTwoDigitByteStringBuilder i = if i < 100
  then Vector.unsafeIndex twoDigitByteStringBuilder (fromIntegral i)
  else BB.intDec i

twoDigitByteStringBuilder :: Vector BB.Builder
twoDigitByteStringBuilder = Vector.fromList
  $ map (BB.byteString . BC.pack) twoDigitStrings
{-# NOINLINE twoDigitByteStringBuilder #-}

twoDigitTextBuilder :: Vector TB.Builder
twoDigitTextBuilder = Vector.fromList
  $ map (TB.fromText . Text.pack) twoDigitStrings
{-# NOINLINE twoDigitTextBuilder #-}

twoDigitStrings :: [String]
twoDigitStrings =
  [ "00","01","02","03","04","05","06","07","08","09"
  , "10","11","12","13","14","15","16","17","18","19"
  , "20","21","22","23","24","25","26","27","28","29"
  , "30","31","32","33","34","35","36","37","38","39"
  , "40","41","42","43","44","45","46","47","48","49"
  , "50","51","52","53","54","55","56","57","58","59"
  , "60","61","62","63","64","65","66","67","68","69"
  , "70","71","72","73","74","75","76","77","78","79"
  , "80","81","82","83","84","85","86","87","88","89"
  , "90","91","92","93","94","95","96","97","98","99"
  ]

raiseTenTo :: Int -> Int64
raiseTenTo i = if i > 15
  then 10 ^ i
  else UVector.unsafeIndex tenRaisedToSmallPowers i

tenRaisedToSmallPowers :: UVector.Vector Int64
tenRaisedToSmallPowers = UVector.fromList $ map (10 ^) [0 :: Int ..15]

yearToZeroPaddedDigit :: Year -> TB.Builder
yearToZeroPaddedDigit (Year x)
  | x < 10 = "000" <> TB.decimal x
  | x < 100 = "00" <> TB.decimal x
  | x < 1000 = "0" <> TB.decimal x
  | otherwise = TB.decimal x

monthToZeroPaddedDigit :: Month -> TB.Builder
monthToZeroPaddedDigit (Month x) =
  indexTwoDigitTextBuilder (x + 1)

zeroPadDayOfMonth :: DayOfMonth -> TB.Builder
zeroPadDayOfMonth (DayOfMonth d) = indexTwoDigitTextBuilder d

yearToZeroPaddedDigitBS :: Year -> BB.Builder
yearToZeroPaddedDigitBS (Year x)
  | x < 10 = "000" <> BB.intDec x
  | x < 100 = "00" <> BB.intDec x
  | x < 1000 = "0" <> BB.intDec x
  | otherwise = BB.intDec x

monthToZeroPaddedDigitBS :: Month -> BB.Builder
monthToZeroPaddedDigitBS (Month x) =
  indexTwoDigitByteStringBuilder (x + 1)

zeroPadDayOfMonthBS :: DayOfMonth -> BB.Builder
zeroPadDayOfMonthBS (DayOfMonth d) = indexTwoDigitByteStringBuilder d

-- | Is the given 'Time' within the 'TimeInterval'?
within :: Time -> TimeInterval -> Bool
t `within` (TimeInterval t0 t1) = t >= t0 && t <= t1

-- | Convert a 'TimeInterval' to a 'Timespan'. This is equivalent to 'width'.
timeIntervalToTimespan :: TimeInterval -> Timespan
timeIntervalToTimespan = width

-- | The 'TimeInterval' that covers the entire range of 'Time's that Chronos supports.
--
--   prop> \(t :: Time) -> within t whole
whole :: TimeInterval
whole = TimeInterval minBound maxBound

-- | The singleton (degenerate) 'TimeInterval'.
singleton :: Time -> TimeInterval
singleton x = TimeInterval x x

-- | Get the lower bound of the 'TimeInterval'.
lowerBound :: TimeInterval -> Time
lowerBound (TimeInterval t0 _) = t0

-- | Get the upper bound of the 'TimeInterval'.
upperBound :: TimeInterval -> Time
upperBound (TimeInterval _ t1) = t1

-- | The width of the 'TimeInterval'. This is equivalent to 'timeIntervalToTimespan'.
width :: TimeInterval -> Timespan
width (TimeInterval x y) = difference y x

-- | A smart constructor for 'TimeInterval'. In general, you should prefer using this
--   over the 'TimeInterval' constructor, since it maintains the invariant that
--   @'lowerBound' interval '<=' 'upperBound' interval@.
timeIntervalBuilder :: Time -> Time -> TimeInterval
timeIntervalBuilder x y = case compare x y of
  GT -> TimeInterval y x
  _ -> TimeInterval x y

infix 3 ...

-- | An infix 'timeIntervalBuilder'.
(...) :: Time -> Time -> TimeInterval
(...) = timeIntervalBuilder

-- | A day represented as the modified Julian date, the number of days
--   since midnight on November 17, 1858.
newtype Day = Day { getDay :: Int }
  deriving (Show,Read,Eq,Ord,Hashable,Enum,ToJSON,FromJSON,Storable,Prim,NFData)

instance Torsor Day Int where
  add i (Day d) = Day (d + i)
  difference (Day a) (Day b) = a - b

-- | The day of the week.
newtype DayOfWeek = DayOfWeek { getDayOfWeek :: Int }
  deriving (Show,Read,Eq,Ord,Hashable,NFData)

-- | The day of the month.
newtype DayOfMonth = DayOfMonth { getDayOfMonth :: Int }
  deriving (Show,Read,Eq,Ord,Prim,Enum,NFData)

-- | The day of the year.
newtype DayOfYear = DayOfYear { getDayOfYear :: Int }
  deriving (Show,Read,Eq,Ord,Prim,NFData)

-- | The month of the year.
newtype Month = Month { getMonth :: Int }
  deriving (Show,Read,Eq,Ord,Prim,NFData)

instance Enum Month where
  fromEnum = getMonth
  toEnum = Month
  succ (Month x) = if x < 11
    then Month (x + 1)
    else error "Enum.succ{Month}: tried to take succ of December"
  pred (Month x) = if x > 0
    then Month (x - 1)
    else error "Enum.pred{Month}: tried to take pred of January"
  enumFrom x = enumFromTo x (Month 11)

-- | 'Month' starts at 0 and ends at 11 (January to December)
instance Bounded Month where
  minBound = Month 0
  maxBound = Month 11

-- | The number of years elapsed since the beginning
--   of the Common Era.
newtype Year = Year { getYear :: Int }
  deriving (Show,Read,Eq,Ord, NFData)

-- | A <https://en.wikipedia.org/wiki/UTC_offset UTC offset>.
newtype Offset = Offset { getOffset :: Int }
  deriving (Show,Read,Eq,Ord,Enum,NFData)

-- | POSIX time with nanosecond resolution.
newtype Time = Time { getTime :: Int64 }
  deriving (FromJSON,ToJSON,Hashable,Eq,Ord,Show,Read,Storable,Prim,Bounded, NFData)

-- | Match a 'DayOfWeek'. By `match`, we mean that a 'DayOfWeekMatch'
--   is a mapping from the integer value of a 'DayOfWeek' to some value
--   of type @a@. You should construct a 'DayOfWeekMatch' with
--   'buildDayOfWeekMatch', and match it using 'caseDayOfWeek'.
newtype DayOfWeekMatch a = DayOfWeekMatch { getDayOfWeekMatch :: Vector a }
  deriving (NFData)

-- | Match a 'Month'. By `match`, we mean that a 'MonthMatch' is
--   a mapping from the integer value of a 'Month' to some value of
--   type @a@. You should construct a 'MonthMatch' with
--   'buildMonthMatch', and match it using 'caseMonth'.
newtype MonthMatch a = MonthMatch { getMonthMatch :: Vector a }
  deriving (NFData)

-- | Like 'MonthMatch', but the matched value can have an instance of
--   'UVector.Unbox'.
newtype UnboxedMonthMatch a = UnboxedMonthMatch { getUnboxedMonthMatch :: UVector.Vector a }
  deriving (NFData)

-- | A timespan. This is represented internally as a number
--   of nanoseconds.
newtype Timespan = Timespan { getTimespan :: Int64 }
  deriving (Show,Read,Eq,Ord,ToJSON,FromJSON,Additive,NFData)

instance Semigroup Timespan where
  (Timespan a) <> (Timespan b) = Timespan (a + b)

instance Monoid Timespan where
  mempty = Timespan 0
  mappend = (SG.<>)

instance Torsor Time Timespan where
  add (Timespan ts) (Time t) = Time (t + ts)
  difference (Time t) (Time s) = Timespan (t - s)

instance Scaling Timespan Int64 where
  scale i (Timespan ts) = Timespan (i * ts)

instance Torsor Offset Int where
  add i (Offset x) = Offset (x + i)
  difference (Offset x) (Offset y) = x - y

-- | The precision used when encoding seconds to a human-readable format.
data SubsecondPrecision
  = SubsecondPrecisionAuto -- ^ Rounds to second, millisecond, microsecond, or nanosecond
  | SubsecondPrecisionFixed {-# UNPACK #-} !Int -- ^ Specify number of places after decimal
  deriving (Eq, Ord, Show, Read)

instance NFData SubsecondPrecision where
  rnf (SubsecondPrecisionAuto) = ()
  rnf (SubsecondPrecisionFixed a) = a `deepseq` ()


-- | A date as represented by the Gregorian calendar.
data Date = Date
  { dateYear  :: {-# UNPACK #-} !Year
  , dateMonth :: {-# UNPACK #-} !Month
  , dateDay   :: {-# UNPACK #-} !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

instance NFData Date where
  rnf (Date y m d) = y `deepseq` m `deepseq` d `deepseq` ()

-- | An 'OrdinalDate' is a 'Year' and the number of days elapsed
--   since the 'Year' began.
data OrdinalDate = OrdinalDate
  { ordinalDateYear :: {-# UNPACK #-} !Year
  , ordinalDateDayOfYear :: {-# UNPACK #-} !DayOfYear
  } deriving (Show,Read,Eq,Ord)

instance NFData OrdinalDate where
  rnf (OrdinalDate y d) = y `deepseq` d `deepseq` ()

-- | A month and the day of the month. This does not actually represent
--   a specific date, since this recurs every year.
data MonthDate = MonthDate
  { monthDateMonth :: {-# UNPACK #-} !Month
  , monthDateDay :: {-# UNPACK #-} !DayOfMonth
  } deriving (Show,Read,Eq,Ord)

instance NFData MonthDate where
  rnf (MonthDate m d) = m `deepseq` d `deepseq` ()

-- | A 'Date' as represented by the Gregorian calendar
--   and a 'TimeOfDay'.
--   While the 'ToJSON' instance encodes with a hyphen separator, the
--   'FromJSON' instance allows any non-digit character to act as
--   separator, using the lenient parser.
data Datetime = Datetime
  { datetimeDate :: {-# UNPACK #-} !Date
  , datetimeTime :: {-# UNPACK #-} !TimeOfDay
  } deriving (Show,Read,Eq,Ord)

instance NFData Datetime where
  rnf (Datetime d t) = d `deepseq` t `deepseq` ()

-- | A 'Datetime' with a time zone 'Offset'.
data OffsetDatetime = OffsetDatetime
  { offsetDatetimeDatetime :: {-# UNPACK #-} !Datetime
  , offsetDatetimeOffset :: {-# UNPACK #-} !Offset
  } deriving (Show,Read,Eq,Ord)

instance NFData OffsetDatetime where
  rnf (OffsetDatetime dt o) = dt `deepseq` o `deepseq` ()

-- | A time of day with nanosecond resolution.
data TimeOfDay = TimeOfDay
  { timeOfDayHour :: {-# UNPACK #-} !Int
  , timeOfDayMinute :: {-# UNPACK #-} !Int
  , timeOfDayNanoseconds :: {-# UNPACK #-} !Int64
  } deriving (Show,Read,Eq,Ord)

instance NFData TimeOfDay where
  rnf (TimeOfDay h m s) = h `deepseq` m `deepseq` s `deepseq` ()

-- | The format of a 'Datetime'. In particular
--   this provides separators for parts of the 'Datetime'
--   and nothing else.
data DatetimeFormat = DatetimeFormat
  { datetimeFormatDateSeparator :: !(Maybe Char)
    -- ^ Separator in the date
  , datetimeFormatSeparator :: !(Maybe Char)
    -- ^ Separator between date and time
  , datetimeFormatTimeSeparator :: !(Maybe Char)
    -- ^ Separator in the time
  } deriving (Show,Read,Eq,Ord)

instance NFData DatetimeFormat where
  rnf (DatetimeFormat s1 s2 s3) = s1 `deepseq` s2 `deepseq` s3 `deepseq` ()

-- | Formatting settings for a timezone offset.
data OffsetFormat
  = OffsetFormatColonOff -- ^ @%z@ (e.g., -0400)
  | OffsetFormatColonOn -- ^ @%:z@ (e.g., -04:00)
  | OffsetFormatSecondsPrecision -- ^ @%::z@ (e.g., -04:00:00)
  | OffsetFormatColonAuto -- ^ @%:::z@ (e.g., -04, +05:30)
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData OffsetFormat where
  rnf !_ = ()

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

instance NFData a => NFData (DatetimeLocale a) where
  rnf (DatetimeLocale d1 d2 m1 m2) =
    d1 `deepseq` d2 `deepseq` m1 `deepseq` m2 `deepseq` ()

-- | A TimeInterval represents a start and end time.
--   It can sometimes be more ergonomic than the 'Torsor' API when
--   you only care about whether or not a 'Time' is within a certain range.
--
--   To construct a 'TimeInterval', it is best to use 'timeIntervalBuilder',
--   which maintains the invariant that @'lowerBound' interval '<=' 'upperBound' interval@
--   (all functions that act on 'TimeInterval's assume this invariant).
data TimeInterval = TimeInterval {-# UNPACK #-} !Time {-# UNPACK #-} !Time
    deriving (Read,Show,Eq,Ord,Bounded)

instance NFData TimeInterval where
  rnf (TimeInterval t1 t2) = t1 `deepseq` t2 `deepseq` ()

-- | Locale-specific formatting for AM and PM.
data MeridiemLocale a = MeridiemLocale
  { meridiemLocaleAm :: !a
  , meridiemLocalePm :: !a
  } deriving (Read,Show,Eq,Ord)

instance NFData a => NFData (MeridiemLocale a) where
  rnf (MeridiemLocale am pm) = am `deepseq` pm `deepseq` ()

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

------------------------
-- The Torsor and Enum instances for Date and OrdinalDate
-- are both bad. This only causes problems for dates
-- at least a million years in the future. Some of this
-- badness is caused by pragmatism, and some of it is caused by
-- my own laziness.
--
-- The badness that comes from pragmatism:
--   - Int technically is not a good delta for Date. Date
--     has too many inhabitants. If we subtract the lowest
--     Date from the highest Date, we get something too
--     big to fit in a machine integer.
--   - There is no good way to write fromEnum or toEnum for
--     Date. Again, Date has more inhabitants than Int, so
--     it simply cannot be done.
-- The badness that comes from laziness:
--   - Technically, we should still be able to add deltas to
--     Dates that do not fit in machine integers. We should
--     also be able to correctly subtract Dates to cannot
--     fit in machine integers.
--   - For similar reasons, the Enum functions succ, pred,
--     enumFromThen, enumFromThenTo, etc. could all have
--     better definitions than the default ones currently
--     used.
-- If, for some reason, anyone ever wants to fix the badness
-- that comes from laziness, all
-- you really have to do is define a version of dateToDay,
-- dayToDate, ordinalDateToDay, and dayToOrdinalDate
-- that uses something bigger instead of Day. Maybe something like
-- (Int,Word) or (Int,Word,Word). I'm not exactly sure how
-- big it would need to be to work correctly. Then you could
-- handle deltas of two very far off days correctly, provided
-- that the two days weren't also super far from each other.
--
------------------------
instance Torsor Date Int where
  add i d = dayToDate (add i (dateToDay d))
  difference a b = difference (dateToDay a) (dateToDay b)

instance Torsor OrdinalDate Int where
  add i d = dayToOrdinalDate (add i (ordinalDateToDay d))
  difference a b = difference (ordinalDateToDay a) (ordinalDateToDay b)

instance Enum Date where
  fromEnum d = fromEnum (dateToDay d)
  toEnum i = dayToDate (toEnum i)

instance Enum OrdinalDate where
  fromEnum d = fromEnum (ordinalDateToDay d)
  toEnum i = dayToOrdinalDate (toEnum i)

instance ToJSON Datetime where
  toJSON = AE.String . encode_YmdHMS SubsecondPrecisionAuto hyphen
  toEncoding x = AEE.unsafeToEncoding (BB.char7 '"' SG.<> builderUtf8_YmdHMS SubsecondPrecisionAuto hyphen x SG.<> BB.char7 '"')

instance FromJSON Datetime where
  parseJSON =
    AE.withText "Datetime" aesonParserDatetime

aesonParserDatetime :: Text -> AET.Parser Datetime
aesonParserDatetime =
  either (const (fail "could not parse Datetime")) pure . AT.parseOnly (parser_lenient <* AT.endOfInput)

instance ToJSON Offset where
  toJSON = AE.String . encodeOffset OffsetFormatColonOn
  toEncoding x = AEE.unsafeToEncoding (BB.char7 '"' SG.<> builderOffsetUtf8 OffsetFormatColonOn x SG.<> BB.char7 '"')

instance FromJSON Offset where
  parseJSON = AE.withText "Offset" aesonParserOffset

instance ToJSONKey Offset where
  toJSONKey = AE.ToJSONKeyText
    (keyFromText . encodeOffset OffsetFormatColonOn)
    (\x -> AEE.unsafeToEncoding (BB.char7 '"' SG.<> builderOffsetUtf8 OffsetFormatColonOn x SG.<> BB.char7 '"'))
    where
#if MIN_VERSION_aeson(2,0,0)
      keyFromText = AK.fromText
#else
      keyFromText = id
#endif

instance FromJSONKey Offset where
  fromJSONKey = AE.FromJSONKeyTextParser aesonParserOffset

aesonParserOffset :: Text -> AET.Parser Offset
aesonParserOffset t = case decodeOffset OffsetFormatColonOn t of
  Nothing -> fail "could not parse Offset"
  Just x -> pure x

-- | Holds all of the parts encoded by a 'Time'.
--   Can be used for formatting if what is presently in the API
--   does not suffice.
data TimeParts = TimeParts
  { timePartsDay :: !Int -- ^ days 0-31
  , timePartsMonth :: !Int -- ^ months 0-11
  , timePartsYear :: !Int
  , timePartsHour :: !Int -- ^ hours 0-23
  , timePartsMinute :: !Int -- ^ minutes 0-59
  , timePartsSecond :: !Int -- ^ seconds 0-59
  , timePartsSubsecond :: !Int -- ^ fraction of a second with nanosecond resolution
  , timePartsOffset :: !Int
  }
  deriving (Eq, Read, Show)

instance NFData TimeParts where
  rnf (TimeParts d mo y h m s ss o) =
    d `deepseq` mo `deepseq` y `deepseq` h `deepseq` m `deepseq` s `deepseq` ss `deepseq` o `deepseq` ()

-- | Deconstruct a 'Time' into its 'TimeParts'.
timeParts :: Offset -> Time -> TimeParts
timeParts o0 t0 =
  let
    OffsetDatetime (Datetime dt t) o = timeToOffsetDatetime o0 t0
    Date y mo d = dt
    TimeOfDay h mi s = t
    (wholeSeconds, subsecond) = divMod s 100000000
  in TimeParts
    { timePartsDay = fromIntegral (getDayOfMonth d)
    , timePartsMonth = fromIntegral (getMonth mo)
    , timePartsYear = fromIntegral (getYear y)
    , timePartsHour = h
    , timePartsMinute = mi
    , timePartsSecond = fromIntegral wholeSeconds
    , timePartsSubsecond = fromIntegral subsecond
    , timePartsOffset = getOffset o
    }

-- | Decode an ISO-8601-encode datetime. The encoded time must not by suffixed
-- by an offset. Any offset (e.g. @-05:00@, @+00:00@, @Z@) will cause a decode
-- failure.
decodeShortTextIso8601Zoneless :: ShortText -> Maybe Chronos.Datetime
decodeShortTextIso8601Zoneless !t = decodeUtf8BytesIso8601Zoneless
  (Bytes.fromShortByteString (TS.toShortByteString t))

-- | Decode an ISO-8601-encode datetime. The encoded time must include an offset
-- (e.g. @-05:00@, @+00:00@, @Z@).
decodeShortTextIso8601 :: ShortText -> Maybe Chronos.OffsetDatetime
decodeShortTextIso8601 !t = decodeUtf8BytesIso8601
  (Bytes.fromShortByteString (TS.toShortByteString t))

decodeUtf8BytesIso8601Zoneless :: Bytes -> Maybe Chronos.Datetime
decodeUtf8BytesIso8601Zoneless !b =
  BVP.parseBytesMaybe (parserUtf8BytesIso8601Zoneless <* BVP.endOfInput ()) b

decodeUtf8BytesIso8601 :: Bytes -> Maybe Chronos.OffsetDatetime
decodeUtf8BytesIso8601 !b =
  BVP.parseBytesMaybe (parserUtf8BytesIso8601 <* BVP.endOfInput ()) b

parserUtf8BytesIso8601Zoneless :: BVP.Parser () s Chronos.Datetime
{-# noinline parserUtf8BytesIso8601Zoneless #-}
parserUtf8BytesIso8601Zoneless = do
  year <- Latin.decWord ()
  Latin.char () '-'
  month' <- Latin.decWord ()
  let !month = month' - 1
  when (month >= 12) (BVP.fail ())
  Latin.char () '-'
  dayWord <- Latin.decWord ()
  when (dayWord > 31) (BVP.fail ())
  let !date = Chronos.Date
        (Chronos.Year (fromIntegral year))
        (Chronos.Month (fromIntegral month))
        (Chronos.DayOfMonth (fromIntegral dayWord))
  Latin.char () 'T'
  hourWord <- Latin.decWord8 ()
  when (hourWord > 23) (BVP.fail ())
  Latin.char () ':'
  minuteWord <- Latin.decWord8 ()
  when (minuteWord > 59) (BVP.fail ())
  Latin.char () ':'
  sec <- Latin.decWord8 ()
  when (sec > 59) (BVP.fail ())
  !nanos <- Latin.trySatisfy (=='.') >>= \case
    True -> do
      (n,w) <- BVP.measure (Latin.decWord64 ())
      when (n > 9) (BVP.fail ())
      let go !acc !b = case b of
            0 -> acc
            _ -> go (acc * 10) (b - 1)
          !ns = go w (9 - n)
      pure ns
    False -> pure 0
  let !td = Chronos.TimeOfDay
        (fromIntegral hourWord)
        (fromIntegral minuteWord)
        (fromIntegral @Word64 @Int64 (fromIntegral sec * 1000000000 + nanos))
  pure $! Chronos.Datetime date td

-- | Consume an ISO-8601-encoded datetime with offset. This will consume any of
-- the following:
--
-- > 2021-12-05T23:01:09Z
-- > 2021-12-05T23:01:09.000Z
-- > 2021-12-05T23:01:09.123456789Z
-- > 2021-12-05T23:01:09+05:00
-- > 2021-12-05T23:01:09.357-11:00
parserUtf8BytesIso8601 :: BVP.Parser () s Chronos.OffsetDatetime
{-# noinline parserUtf8BytesIso8601 #-}
parserUtf8BytesIso8601 = do
  dt <- parserUtf8BytesIso8601Zoneless
  off <- Latin.any () >>= \case
    'Z' -> pure 0
    '+' -> parserBytesOffset
    '-' -> do
      !off <- parserBytesOffset
      pure (negate off)
    _ -> BVP.fail ()
  pure $! Chronos.OffsetDatetime dt (Chronos.Offset off)

-- Should consume exactly five characters: HH:MM. However, the implementation
-- is more generous.
parserBytesOffset :: BVP.Parser () s Int
parserBytesOffset = do
  h <- Latin.decWord8 ()
  Latin.char () ':'
  m <- Latin.decWord8 ()
  let !r = ((fromIntegral @Word8 @Int h) * 60) + fromIntegral @Word8 @Int m
  pure r

encodeShortTextIso8601Zulu :: Datetime -> ShortText
{-# noinline encodeShortTextIso8601Zulu #-}
encodeShortTextIso8601Zulu !dt =
  let !(ByteArray x) = Bounded.run Nat.constant
        ( boundedBuilderUtf8BytesIso8601Zoneless dt
        `Bounded.append`
        Bounded.ascii 'Z'
        )
   in TS.fromShortByteStringUnsafe (SBS.SBS x)

encodeShortTextIso8601Zoneless :: Datetime -> ShortText
{-# noinline encodeShortTextIso8601Zoneless #-}
encodeShortTextIso8601Zoneless !dt =
  let !(ByteArray x) = Bounded.run Nat.constant
        (boundedBuilderUtf8BytesIso8601Zoneless dt)
   in TS.fromShortByteStringUnsafe (SBS.SBS x)

encodeShortTextIso8601 :: OffsetDatetime -> ShortText
{-# noinline encodeShortTextIso8601 #-}
encodeShortTextIso8601 offdt =
  let !(ByteArray x) = Bounded.run Nat.constant
        (boundedBuilderUtf8BytesIso8601 offdt)
   in TS.fromShortByteStringUnsafe (SBS.SBS x)

boundedBuilderUtf8BytesIso8601 :: OffsetDatetime -> Bounded.Builder 50
boundedBuilderUtf8BytesIso8601 (OffsetDatetime dt off) =
  ( boundedBuilderUtf8BytesIso8601Zoneless dt
    `Bounded.append`
    boundedBuilderOffset off
  )

-- | Encode a datetime with ISO-8601. The result does not include any
-- indication of a time zone. If the subsecond part is zero, it is suppressed.
-- Examples of output:
--
-- > 2021-01-05T23:00:51
-- > 2021-01-05T23:00:52.123000000
-- > 2021-01-05T23:00:53.674094347
boundedBuilderUtf8BytesIso8601Zoneless :: Datetime -> Bounded.Builder 44
boundedBuilderUtf8BytesIso8601Zoneless (Datetime (Date (Year y) (Month mth) (DayOfMonth d)) (TimeOfDay h mt sns)) =
    let (s,ns) = quotRem sns 1_000_000_000 in
    Bounded.wordDec (fromIntegral y)
    `Bounded.append`
    Bounded.ascii '-'
    `Bounded.append`
    Bounded.wordPaddedDec2 (fromIntegral (mth + 1))
    `Bounded.append`
    Bounded.ascii '-'
    `Bounded.append`
    Bounded.wordPaddedDec2 (fromIntegral d)
    `Bounded.append`
    Bounded.ascii 'T'
    `Bounded.append`
    Bounded.wordPaddedDec2 (fromIntegral h)
    `Bounded.append`
    Bounded.ascii ':'
    `Bounded.append`
    Bounded.wordPaddedDec2 (fromIntegral mt)
    `Bounded.append`
    Bounded.ascii ':'
    `Bounded.append`
    Bounded.wordPaddedDec2 (fromIntegral s)
    `Bounded.append`
    (case ns of
      0 -> Bounded.weaken @0 @10 Lte.constant Bounded.empty
      _ ->
        Bounded.ascii '.'
        `Bounded.append`
        Bounded.wordPaddedDec9 (fromIntegral ns)
    )

boundedBuilderOffset :: Offset -> Bounded.Builder 6
boundedBuilderOffset (Offset mins) = case mins of
  0 -> Bounded.weaken @1 @6 Lte.constant (Bounded.ascii 'Z')
  _ ->
    let !absMins = fromIntegral @Int @Word (abs mins)
        !absHrs = quot absMins 60
        !absMinutes = rem absMins 60
     in Bounded.ascii (bool '-' '+' (mins > 0))
        `Bounded.append`
        Bounded.wordPaddedDec2 absHrs
        `Bounded.append`
        Bounded.ascii ':'
        `Bounded.append`
        Bounded.wordPaddedDec2 absMinutes
