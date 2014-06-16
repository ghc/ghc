{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}
{-# OPTIONS -dcore-lint #-}

{- 	 This one killed GHC 5.02

The problem is that in rather obscure cases (involving functional
dependencies) it is possible to get an AbsBinds [] [] (no tyvars, no
dicts) which nevertheless has some "dictionary bindings".  These come
out of the typechecker in non-dependency order, so we need to Rec them
just in case.   Otherwise we get a CoreLint out-of-scope error.

Reported by Armin Groesslinger

-}

module ShouldCompile
where

data X a = X a

class Y a b | a -> b where
    y :: a -> X b

instance Y [[a]] a where
    y ((x:_):_) = X x

g :: Num a => [X a] -> [X a]
g xs = h xs
    where
    h ys = ys ++ map (k (y [[0]])) xs

k :: X a -> X a -> X a
k _ _ = y ([] ++ [[]] ++ [])
