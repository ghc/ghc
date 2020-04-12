module GHC.CoreToIface where

import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, TyLit, Coercion )
import {-# SOURCE #-} GHC.Iface.Type( IfaceType, IfaceTyCon, IfaceForAllBndr
                                    , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import GHC.Types.Var ( TyCoVarBinder )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Core.TyCon ( TyCon )
import GHC.Types.Var.Set( VarSet )

-- For GHC.Core.TyCo.Rep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyCoVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
