-- !!! hiding class members.
module M where

import Prelude hiding ( (<), (>))

x :: Int -> Int
x = (<) 2
