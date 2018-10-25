module ToIface where

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} IfaceType( IfaceType, IfaceTyCon, IfaceForAllBndr
                               , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import Var ( TyVarBinder )
import TyCon ( TyCon )
import VarSet( VarSet )

-- For TyCoRep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
