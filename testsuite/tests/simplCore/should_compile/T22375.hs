module T22375 where

data X = A | B | C | D | E
  deriving Eq

f :: X -> Int -> Int
f x v
  | x == A = 1 + v
  | x == B = 2 + v
  | x == C = 3 + v
  | x == D = 4 + v
  | otherwise = 5 + v
