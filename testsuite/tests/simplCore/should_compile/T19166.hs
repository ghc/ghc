module T19166 where

import Data.Bits

foo :: Int -> Int
foo x = case x .&. 0x3 of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> 4 -- unreachable branch but not removed
    _ -> undefined -- required for the PM checker but unreachable too
