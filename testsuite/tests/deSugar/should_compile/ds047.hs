-- !!! Nullary rec-pats for constructors that hasn't got any labelled
-- !!! fields is legal Haskell, and requires extra care in the desugarer.
module ShouldCompile where

data X = X Int [Int]

f :: X -> Int
f (X _ []) = 0
f X{}      = 1
