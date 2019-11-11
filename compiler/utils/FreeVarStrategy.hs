module FreeVarStrategy (FreeVarStrategy(..)) where

import {-# SOURCE #-} TyCoRep (CoercionHole)

import Var

import Data.Foldable
import Data.Monoid (Monoid)

class Monoid m => FreeVarStrategy m where
  -- | Introduce a free 'CohercionHole'.
  coholeFV  :: CoercionHole -> m
  -- | Introduce a free variable (and any free variables of its type/kind).
  unitFV   :: Var -> m
  bindVar :: Var -> m -> m
  foldMapFVs :: Foldable f => (a -> m) -> f a -> m
  foldMapFVs f = foldMap f
