module GHC.CoreToIface where

import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, TyLit, Coercion )
import {-# SOURCE #-} GHC.Iface.Type( IfaceType, IfaceTyCon, IfaceBndr
                                    , IfaceCoercion, IfaceTyLit, IfaceAppArgs
                                    , IfaceForAllTyFlag )
import GHC.Types.Var ( VarBndr, ForAllTyBinder )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Core.TyCon ( TyCon )
import GHC.Types.Var.Set( VarSet )

-- For GHC.Core.TyCo.Rep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllTyBndrs :: [ForAllTyBinder] -> [VarBndr IfaceBndr IfaceForAllTyFlag]
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
