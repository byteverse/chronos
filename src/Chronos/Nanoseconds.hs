module Chronos.Nanoseconds where

import Data.Int
import Chronos.Types

add :: Nanoseconds -> Nanoseconds -> Nanoseconds
add (Nanoseconds a) (Nanoseconds b) = Nanoseconds (a + b)

scale :: Int64 -> Nanoseconds -> Nanoseconds
scale i (Nanoseconds x) = Nanoseconds (i * x)

