module Chronos.Day where

import Chronos.Types
import Data.Int

add :: Int -> Day -> Day
add a (Day b) = Day (a + b)

diff :: Day -> Day -> Int
diff (Day a) (Day b) = a - b

