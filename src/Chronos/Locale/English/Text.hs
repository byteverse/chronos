{-# LANGUAGE OverloadedStrings #-}

module Chronos.Locale.English.Text where

import Chronos.Types
import qualified Chronos.Month as Month
import Data.Text (Text)

meridiemLower :: MeridiemLocale Text
meridiemLower = MeridiemLocale "am" "pm"

meridiemUpper :: MeridiemLocale Text
meridiemUpper = MeridiemLocale "AM" "PM"

meridiemLowerDotted :: MeridiemLocale Text
meridiemLowerDotted = MeridiemLocale "a.m." "p.m."

meridiemUpperDotted :: MeridiemLocale Text
meridiemUpperDotted = MeridiemLocale "A.M." "P.M."

monthFull :: MonthMatch Text
monthFull = Month.match
  "January" "February" "March" "April"
  "May" "June" "July" "August"
  "September" "October" "November" "December"

monthAbbreviated :: MonthMatch Text
monthAbbreviated = Month.match
  "Jan" "Feb" "Mar" "Apr" "May" "Jun"
  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

