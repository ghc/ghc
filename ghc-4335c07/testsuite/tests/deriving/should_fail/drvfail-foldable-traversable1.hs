{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module ShouldFail where

import Data.Foldable
import Data.Traversable

-- Derive Traversable without Functor
data Trivial1 a = Trivial1 a
   deriving (Foldable,Traversable)

-- Derive Traversable without Foldable
data Trivial2 a = Trivial2 a
   deriving (Functor,Traversable)

-- Foldable with function type
data Infinite a = Infinite (Int -> a)
   deriving (Functor,Foldable)

-- Foldable with function type
data Cont r a = Cont ((a -> r) -> r)
   deriving (Functor,Traversable)
