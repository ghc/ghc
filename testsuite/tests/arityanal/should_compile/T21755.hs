module T21755 where

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

f :: Int -> (Int -> Int) -> Int -> Int
f k z =
    if even (mySum [0..k])
      then \n -> n + 1
      else \n -> z n
