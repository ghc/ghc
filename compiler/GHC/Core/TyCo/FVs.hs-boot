module GHC.Core.TyCo.FVs where

import GHC.Prelude ( Bool, Int )
import GHC.Types.Var.Set( TyCoVarSet )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, Coercion, UnivCoDeps )

noFreeVarsOfType    :: Type -> Bool
tyCoVarsOfType      :: Type -> TyCoVarSet
univCoDepsCoercions :: UnivCoDeps -> [Coercion]
univCoDepsSize      :: UnivCoDeps -> Int
