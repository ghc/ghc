module ToIface where

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} IfaceType( IfaceType, IfaceTyCon, IfaceForAllBndr
                               , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import Var ( TyCoVarBinder )
import VarEnv ( TidyEnv )
import TyCon ( TyCon )
import VarSet( VarSet )

-- For TyCoRep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyCoVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
