module Chronos.Match where

import Chronos.Types
import qualified Data.Vector as Vector

buildDayOfWeekMatch :: a -> a -> a -> a -> a -> a -> a -> DayOfWeekMatch a
buildDayOfWeekMatch a b c d e f g =
  DayOfWeekMatch (Vector.fromList [a,b,c,d,e,f,g])

buildMonthMatch :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> MonthMatch a
buildMonthMatch a b c d e f g h i j k l =
  MonthMatch (Vector.fromList [a,b,c,d,e,f,g,h,i,j,k,l])



