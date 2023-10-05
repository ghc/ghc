module Main where

import Data.Int
import System.Environment

bitcount x = if x > 0 
    then let (d,m) = divMod x 2 in  bitcount d + m
    else 0

main = print $ sum  $ map bitcount 
       [ 0 :: Int64 .. 2^20 - 1 ]



