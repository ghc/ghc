-- | 't' and 't2' should have a strictness signature for arity 2 here.
module Test where

newtype T = MkT (Int -> Int -> Int)

t :: T
t = MkT (\a b -> a + b)

t2 :: T
t2 = MkT (+)
