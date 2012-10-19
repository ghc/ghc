{-# LANGUAGE ImplicitParams, RankNTypes #-}

-- This program failed to typecheck in an early version of
-- GHC with impredicative polymorphism, but it was fixed by
-- doing pre-subsumption in the subsumption check.
-- Trac bug #821

module ShouldCompile where

type PPDoc = (?env :: Int) => Char

f :: Char -> PPDoc
f  = succ 
