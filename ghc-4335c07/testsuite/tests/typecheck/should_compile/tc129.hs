{-# LANGUAGE ImplicitParams #-}

-- !!! Test inheritance of implicit parameters.
-- GHC 5.00.2 fails this test

-- The thing is to do with whether an implicit parameter
-- constraint can be "inherited".  See notes in TcSimplify.lhs

module ShouldCompile where

data R = R {f :: Int}

foo :: (?x :: Int) => R -> R
foo r = r {f = ?x}

baz :: (?x :: Int) => Int
baz = (?x +1) :: Int

