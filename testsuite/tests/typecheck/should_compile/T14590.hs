module T14590 where

f1, f2, f3, f4 :: Int -> Int -> Int
f1 x y = (x `_`)  y
f2 x y = (x `_a`) y
f3 x y = (`_`  x) y
f4 x y = (`_a` x) y
