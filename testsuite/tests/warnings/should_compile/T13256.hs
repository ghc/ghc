module T13256 where

v :: Int
v = (\x -> case (x :: Int) of 100000000000000000000000000000000 -> 0) 8 :: Int
