
-- Data types with no constructors

module ShouldCompile where

data S
data T a

f :: [T a] -> Int
f xs = length xs


