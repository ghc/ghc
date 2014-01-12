{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- This fails, because the type in the pattern doesn't exactly match
-- the context type.  We don't do subsumption in patterns any more.

-- GHC 7.0: now we do again

module Foo where

foo :: (forall c. c -> c) -> [Char]
foo (f :: forall a. [a] -> [a]) = f undefined

