{-# LANGUAGE RankNTypes #-}

-- This program tickled a bug in 5.02.2's forall-lifting

module ShouldCompile where

class Class x where
 combinator' :: (forall y. Class y => y -> y) -> x -> x

combinator :: (forall y. Class y => y -> y)
           -> (forall x. Class x => x -> x)
combinator f = combinator' f
