module T19157 where

mySum :: [Int] -> Int
mySum lst = go 0 lst
  where
    go :: Int -> [Int] -> Int
    go sum [] = sum
    go sum (s : ss) = go (sum + s) ss
