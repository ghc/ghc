{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}
-- UndecidableInstances now needed because the Coverage Condition fails

-- !!! Functional dependencies
-- This one gave another fail in tcReadMutVar

module M1 where

class HasFoo a foo | a -> foo where
    foo :: a -> foo
instance HasFoo Int Int where
    foo = id

instance HasFoo a b => HasFoo [a] b where
    foo = foo . head

test:: [[Int]] -> Int
test = foo
