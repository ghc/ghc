module ShouldFail where

-- !!! this is not legal Haskell 98, but GHC parses it
f = f where g = g where
 h = h
