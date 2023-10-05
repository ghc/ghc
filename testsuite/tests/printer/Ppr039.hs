{-# LANGUAGE MultiWayIf #-}
module MultiWayIf where

foo = if | test1 -> e1
         | test2 witharg -> e2
         | otherwise -> def

bar = if { | test1 -> if { | test2 -> e1
                           | test3 -> e2 }
           | test4 -> e3
         }

-- taken from GHC's test suite
x  = 10
x1 = if | x < 10 -> "< 10" | otherwise -> ""
x2 = if | x < 10 -> "< 10"
        | otherwise -> ""
x3 = if | x < 10 -> "< 10"
        | otherwise -> ""
x4 = if | True -> "yes"
x5 = if | True -> if | False -> 1 | True -> 2

x6 = if | x < 10 -> if | True -> "yes"
                       | False -> "no"
        | otherwise -> "maybe"

x7 = (if | True -> 0)

-- issue #98
spam = if | () <- () -> ()
