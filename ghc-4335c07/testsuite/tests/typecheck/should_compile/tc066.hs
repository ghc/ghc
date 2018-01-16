module ShouldSucceed where

data Pair a b = MkPair a b
f x = [ a | (MkPair c a) <- x ]
