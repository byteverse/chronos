module Chronos.Tai where

import Chronos.Types

epoch :: TaiTime
epoch = TaiTime 0

add :: Nanoseconds -> TaiTime -> TaiTime
add (Nanoseconds a) (TaiTime b) = TaiTime (a + b)

diff :: TaiTime -> TaiTime -> Nanoseconds
diff (TaiTime a) (TaiTime b) = Nanoseconds (a - b)

