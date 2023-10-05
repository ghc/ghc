module ShouldSucceed where

f (x:xs) = if (x == (fromInteger 2)) then xs else []
