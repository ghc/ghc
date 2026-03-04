module GHC.Core.FVs where

import {-# SOURCE #-} GHC.Core (CoreRule)
import GHC.Types.Var.Set (VarSet)

ruleFreeVars :: CoreRule -> VarSet
