
module T7776 where

f :: Int -> Int -> Int
f x y = let a ~# b = a + b
        in x ~# y
