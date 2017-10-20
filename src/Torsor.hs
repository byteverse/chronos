{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Torsor
  ( Additive(..)
  , Torsor(..)
  , Scaling(..)
  ) where

import Data.Int
import Data.Word

class Additive v where
  zero :: v
  invert :: v -> v
  plus :: v -> v -> v
  minus :: v -> v -> v

class Additive v => Torsor p v | p -> v where
  add :: v -> p -> p
  difference :: p -> p -> v

class (Additive v, Additive s) => Scaling v s | v -> s where
  scale :: s -> v -> v

instance Additive Int where
  zero = 0
  invert = negate
  plus = (+)
  minus = (-)

instance Torsor Int Int where
  add = (+)
  difference = (-)

instance Scaling Int Int where
  scale = (*)

instance Additive Word where
  zero = 0
  invert = negate
  plus = (+)
  minus = (-)

instance Torsor Word Word where
  add = (+)
  difference = (-)

instance Scaling Word Word where
  scale = (*)

instance Additive Double where
  zero = 0
  invert = negate
  plus = (+)
  minus = (-)

instance Torsor Double Double where
  add = (+)
  difference = (-)

instance Scaling Double Double where
  scale = (*)

instance Additive Int64 where
  zero = 0
  invert = negate
  plus = (+)
  minus = (-)

instance Torsor Int64 Int64 where
  add = (+)
  difference = (-)

instance Scaling Int64 Int64 where
  scale = (*)

instance Additive Word64 where
  zero = 0
  invert = negate
  plus = (+)
  minus = (-)

instance Torsor Word64 Word64 where
  add = (+)
  difference = (-)

instance Scaling Word64 Word64 where
  scale = (*)
