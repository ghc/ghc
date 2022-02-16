module GHC.CoreToIface where

import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type, TyLit, Coercion )
import {-# SOURCE #-} GHC.Iface.Type( IfaceType, IfaceTyCon, IfaceBndr
                                    , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import {-# SOURCE #-} GHC.Iface.Type.Ppr
  ( IfaceTypePpr, IfaceBndrPpr
  , IfaceCoercionPpr, IfaceAppArgsPpr )
import GHC.Types.Var ( VarBndr, TyCoVar )
import GHC.Types.Var.Env ( TidyEnv )
import GHC.Core.TyCon ( TyCon )
import GHC.Types.Var.Set( VarSet )

-- For GHC.Core.TyCo.Rep
toIfaceTypeX :: VarSet -> Type -> IfaceTypePpr
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: (VarBndr TyCoVar flag) -> (VarBndr IfaceBndrPpr flag)
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgsPpr
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercionPpr
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgsPpr
