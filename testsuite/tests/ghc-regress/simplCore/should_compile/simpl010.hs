
{-# LANGUAGE ExistentialQuantification #-}

-- This tests an interaction between GADTs and join points
-- The case-of-case transformation can pretty easily result
-- in a type mis-match, because the join point does not see
-- the refinement from the case branch

module ShouldCompile( h ) where

data T a = forall b. T b [a] | T2

f :: a -> T a -> [a]
f x (T _ a) = a ++ a ++ a ++ [x]
f x T2      = [x]

h :: a -> Bool -> T a -> T a -> [a]
h x b p q = f x (case b of { True -> p; False -> q })
	