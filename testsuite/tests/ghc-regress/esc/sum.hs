module Sum where

-- {-# SPECIALISE f :: Int -> Int #-}
-- {-# CONTRACT f :: x:{y | y > 0} -> {r | r == x + 1} #-}
-- {-# CONTRACT f :: x:{y | y > 0} -> {y | y > 0} -> {r | r == x + 1} #-}
-- {-# CONTRACT f :: any -> {y | y > 0}  #-}
-- {-# CONTRACT f :: {y | y > 0} -> any #-}
{-# CONTRACT f :: x:{y | y > 0} -> {r | r > 0} #-}
f :: Int -> Int
f x = x + 1

-- {-# CONTRACT f2 :: x:{y | y > 0} -> {y | y > x} -> {r | r > 0} #-}
{-# CONTRACT f2 :: x:{y | y > 0} -> {y | y > x} -> any #-}
f2 :: Int -> Int -> Int
f2 x y = x + y

t1  = f 5
t2a = f2 (f 5) 2
t2b = f2 (f 5) 6
