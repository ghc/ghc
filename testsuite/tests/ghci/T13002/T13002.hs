module T13002 where

test :: Int -> Int
test n = length ([0..(10^n)] :: [Int])
