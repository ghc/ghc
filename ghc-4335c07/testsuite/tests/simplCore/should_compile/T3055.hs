
module Foo where

import Data.Bits

-- This should be simplified away to -28
int :: Int
int = negate ((((((15 + 18 - 11) * 3) `quot` 2) `rem` 19) `shiftL` 3) `shiftR` 2)

