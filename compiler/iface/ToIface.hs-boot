module ToIface where

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} IfaceType
import Var ( TyVar, TyVarBinder )
import TyCon ( TyCon )

-- For TyCoRep
toIfaceType :: Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
