-- !!! no type variable on a context
-- !!! reported by Sigbjorn Finne

module ShouldFail where

moby :: Num => Int -> a -> Int
moby x y = x+y
