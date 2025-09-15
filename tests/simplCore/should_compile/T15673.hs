module T14573 where

import Data.Bits (shift)

badOne :: [Int] -> Integer     -- replace Integer by Int and all is good!
badOne is = sum $ zipWith (\n _->shift 1 n) [0..] is
