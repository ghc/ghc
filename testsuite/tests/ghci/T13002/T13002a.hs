module T13002a where

testa :: Int -> Int
testa n = length ([0..(10^n)] :: [Int])
