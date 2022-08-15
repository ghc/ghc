module T22042a where

data A = A | AA deriving Show
data B = B | AB deriving Show
data C = C | AC deriving Show

data SC = SC !A !B !C

foo :: SC -> String
foo (SC a b c) = show a ++ show b ++ show c
