{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts, DatatypeContexts #-}

module ShouldCompile where

import Data.Foldable
import Data.Traversable

data Trivial a = Trivial
   deriving (Functor,Foldable,Traversable)

-- lots of different things
data Strange a b c
    = T1 a b c
    | T2 c c c
    | T3 [a] [b] [c]         -- lists
    | T4 [[a]] [[b]] [[c]]   -- nested lists
    | T5 (c,(b,b),(c,c))     -- tuples
    | T6 ([c],Strange a b c) -- tycons
  deriving (Functor,Foldable,Traversable)

data NotPrimitivelyRecursive a
    = S1 (NotPrimitivelyRecursive (a,a))
    | S2 a
  deriving (Functor,Foldable,Traversable)

data Eq a => StupidConstraint a b = Stupid a b
  deriving (Functor,Foldable,Traversable)

-- requires Foldable/Traversable constraint on f and g
data Compose f g a = Compose (f (g a))
  deriving (Functor,Foldable,Traversable)
