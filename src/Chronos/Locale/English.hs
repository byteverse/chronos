{-# LANGUAGE OverloadedStrings #-}

module Chronos.Locale.English
  ( lower
  , upper
  , lowerDots
  , upperDots
  , abbreviated
  , unabbreviated
  ) where

import           Chronos       (buildMonthMatch)
import           Chronos.Types
import           Data.Text     (Text)

lower :: MeridiemLocale Text
lower = MeridiemLocale "am" "pm"

upper :: MeridiemLocale Text
upper = MeridiemLocale "AM" "PM"

lowerDots :: MeridiemLocale Text
lowerDots = MeridiemLocale "a.m." "p.m."

upperDots :: MeridiemLocale Text
upperDots = MeridiemLocale "A.M." "P.M."

unabbreviated :: MonthMatch Text
unabbreviated = buildMonthMatch
  "January" "February" "March" "April"
  "May" "June" "July" "August"
  "September" "October" "November" "December"

abbreviated :: MonthMatch Text
abbreviated = buildMonthMatch
  "Jan" "Feb" "Mar" "Apr" "May" "Jun"
  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

