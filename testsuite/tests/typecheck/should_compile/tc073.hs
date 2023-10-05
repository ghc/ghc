
module ShouldSucceed where

f [] = []
f (x:xs) = x : (f xs)
