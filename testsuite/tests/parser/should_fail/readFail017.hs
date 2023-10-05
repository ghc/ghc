module ShouldFail where

-- GHC < 5.01 used to get the line number wrong.
f (f f) = f

g = g
