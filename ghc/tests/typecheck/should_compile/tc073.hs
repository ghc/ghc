
module ShouldSucc where

f [] = []
f (x:xs) = x : (f xs)
