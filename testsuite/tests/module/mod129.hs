-- !!! hiding class members (but not class.)
module M where

import Prelude hiding ( (<), (>))

x :: Ord a => a -> a
x = undefined

