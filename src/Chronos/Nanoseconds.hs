module Chronos.Nanoseconds
  ( -- * Arithmetic
    add
  , scale
  , negate
    -- * From Duration
    -- $fromduration
  , fromSeconds
  , fromMinutes
  , fromHours
  , fromDays
  , fromWeeks
  ) where

import Prelude hiding (negate)
import qualified Prelude as Prelude
import Data.Int
import Chronos.Types

add :: Nanoseconds -> Nanoseconds -> Nanoseconds
add (Nanoseconds a) (Nanoseconds b) = Nanoseconds (a + b)

negate :: Nanoseconds -> Nanoseconds
negate (Nanoseconds a) = Nanoseconds (Prelude.negate a)

scale :: Int64 -> Nanoseconds -> Nanoseconds
scale i (Nanoseconds x) = Nanoseconds (i * x)

{- $fromduration

These functions are at times convenient, but on a fundamental level,
all of them except for 'fromSeconds' are incorrect. If we account for
leap seconds, we must acknowledge that not all minutes contain 60 seconds.
Some contain 61 seconds, and in the future some may contain 59 seconds.

-}

fromSeconds :: Int64 -> Nanoseconds
fromSeconds = Nanoseconds . (1000000000 *)

fromMinutes :: Int64 -> Nanoseconds
fromMinutes = Nanoseconds . (60000000000 *)

fromHours :: Int64 -> Nanoseconds
fromHours = Nanoseconds . (3600000000000 *)

fromDays :: Int64 -> Nanoseconds
fromDays = Nanoseconds . (86400000000000 *)

fromWeeks :: Int64 -> Nanoseconds
fromWeeks = Nanoseconds . (604800000000000 *)

