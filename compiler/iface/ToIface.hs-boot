module ToIface where

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} IfaceType
import Var ( TyVar, TyVarBinder )
import TyCon ( TyCon )
import VarSet( VarSet )

-- For TyCoRep
toIfaceType :: Type -> IfaceType
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
