{-# LANGUAGE OverloadedStrings #-}

{- | This module provides some english locale helpers.
    It is very incomplete. Please send a pull request
    to https://github.com/byteverse/chronos if you need
    additions to this API.
-}
module Chronos.Locale.English
  ( lower
  , upper
  , lowerDots
  , upperDots
  , abbreviated
  , unabbreviated
  ) where

import Chronos (buildMonthMatch)
import Chronos.Types
import Data.Text (Text)

{- $setup

>>> :set -XOverloadedStrings
>>> import Chronos (january, august, december, october, caseMonth)
-}

-- | Lowercase "am"/"pm".
lower :: MeridiemLocale Text
lower = MeridiemLocale "am" "pm"

-- | Uppercase "AM"/"PM".
upper :: MeridiemLocale Text
upper = MeridiemLocale "AM" "PM"

-- | Lowercase "a.m."/"p.m."
lowerDots :: MeridiemLocale Text
lowerDots = MeridiemLocale "a.m." "p.m."

-- | Uppercase "A.M."/"P.M."
upperDots :: MeridiemLocale Text
upperDots = MeridiemLocale "A.M." "P.M."

{- | Unabbreviated 'Month's of the year.

  >>> caseMonth unabbreviated january
  "January"

  >>> caseMonth unabbreviated december
  "December"
-}
unabbreviated :: MonthMatch Text
unabbreviated =
  buildMonthMatch
    "January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December"

{- | Abbreviated 'Month's of the year.

  >>> caseMonth abbreviated october
  "Oct"

  >>> caseMonth abbreviated august
  "Aug"
-}
abbreviated :: MonthMatch Text
abbreviated =
  buildMonthMatch
    "Jan"
    "Feb"
    "Mar"
    "Apr"
    "May"
    "Jun"
    "Jul"
    "Aug"
    "Sep"
    "Oct"
    "Nov"
    "Dec"
