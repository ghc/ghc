{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances #-}
-- UndecidableInstances because 'b' appears in the context but not the head

module ShouldCompile where

-- This accepted by Hugs, but not by GHC 4.08.1
-- Reported by Thomas Hallgren Nov 00

class P a
class R a b | b->a

instance (P a,R a b) => P [b]

{- GHC 4.08.1 doesn't seem to allow variables in the context that
don't appear after the =>, but which are still ok since they are
determined by the functional dependenices.  -}


