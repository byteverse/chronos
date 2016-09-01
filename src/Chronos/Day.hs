module Chronos.Day where

import Chronos.Types
import Data.Int

add :: Int32 -> Day -> Day
add a (Day b) = Day (a + b)

diff :: Day -> Day -> Int32
diff (Day a) (Day b) = a - b

