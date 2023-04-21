module GHC.Core.TyCo.FVs where

import GHC.Prelude ( Bool )
import GHC.Types.Var.Set( TyCoVarSet )
import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )

noFreeVarsOfType :: Type -> Bool
tyCoVarsOfType   :: Type -> TyCoVarSet