module Sum where

{-- {-# SPECIALISE f :: Int -> Int #-}
-- {-# CONTRACT f :: x:{y | y > 0} -> {r | r == x + 1} #-}
-- {-# CONTRACT f :: x:{y | y > 0} -> {y | y > 0} -> {r | r == x + 1} #-}
-- {-# CONTRACT f :: any -> {y | y > 0}  #-}
-- {-# CONTRACT f :: {y | y > 0} -> _ #-}
{-# CONTRACT inc :: {y | y > 0} -> {r | r > 1} #-}
inc :: Int -> Int
inc x = x + 1

-}
-- {-# CONTRACT sum2 :: x:{y | y > 0} -> {y | y > x} -> {r | r > 0} #-}
-- {-# CONTRACT sum2 :: x:{y | y > 0} -> {y | y > x} -> _ #-}
sum2 :: Int -> Int -> Int
sum2 x y = x + y

{-
t1  = inc 5
t2a = sum2 (inc 5) 2
t2b = sum2 (inc 5) 6
-}
