module ShouldFail where

-- !!! Ambiguous constraint in type signature

f :: Eq a => Int -> Int
f x = x
