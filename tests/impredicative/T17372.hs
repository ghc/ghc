{-# LANGUAGE ImpredicativeTypes, ConstraintKinds, GADTs, AllowAmbiguousTypes #-}

module T17372 where

-- This typechecks
x1 = () :: ((Show a => Num a => Int) ~ ((Show a, Num a) => Int)) => ()

-- -> replace `Num a` with `(Eq a, Ord a)`

-- This doesn't typecheck
-- Couldn't match type ‘Ord a0 => Int’ with ‘Int’
x2 = () :: ((Show a => (Eq a, Ord a) => Int) ~ ((Show a, (Eq a, Ord a)) => Int)) => ()

type A a = (Eq a, Ord a)

-- This typechecks
x3 = () :: (Eq a, Ord a) ~ A a => ()

-- This doesn't typecheck
-- Couldn't match type ‘()’ with ‘Ord a0 -> ()’
x4 = () :: (A a => ()) ~ ((Eq a, Ord a) => ()) => ()

-- -> replace `Num a` with `A a` instead

-- This typechecks
x5 = () :: ((Show a => A a => Int) ~ ((Show a, A a) => Int)) => ()

-- Let's make a type synonym out of the entire constraint
type C c a = ((Show a => c => Int) ~ ((Show a, c) => Int))

-- Now all of these typecheck:
x6 = () :: C (Num a) a => ()
x7 = () :: C (Eq a, Ord a) a => ()
x8 = () :: C (A a) a => ()

-- This doesn't typecheck
x9 = () :: ((() => () => Int) ~ (((), ()) => Int)) => ()

-- Let's turn this constraint into a different type synonym
type B a b = ((a => b => Int) ~ ((a, b) => Int))

-- These both typecheck now:
x10 = () :: B (Show a) (Num a) => ()
x11 = () :: B () () => ()
