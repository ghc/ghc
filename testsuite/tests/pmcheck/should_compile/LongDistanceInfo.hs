module Lib where

data T = A | B | C

f :: T -> Int
f A = 1
f x = case x of
  A -> 2
  B -> 3
  C -> 4
