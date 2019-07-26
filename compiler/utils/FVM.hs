module FVM (FVM(..)) where

import {-# SOURCE #-} TyCoRep (CoercionHole)

import Var
import VarSet

import Data.Monoid (Monoid)

class Monoid m => FVM m where
  coholeFV  :: CoercionHole -> m
  unitFV   :: Var -> m
  tycoVarsFV :: DVarSet -> m
  bindVar :: Var -> m -> m
