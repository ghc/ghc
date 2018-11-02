module T9441a where

f1 :: Integer -> Integer
f1 1 = 1
f1 n = n * f1 (n - 1)

f2 :: Integer -> Integer
f2 1 = 1
f2 m = m * f2 (m - 1)
