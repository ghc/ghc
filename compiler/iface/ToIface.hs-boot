module ToIface where

import {-# SOURCE #-} TyCoRep ( Type, TyLit, Coercion )
import {-# SOURCE #-} IfaceType( IfaceType, IfaceTyCon, IfaceBndr
                               , IfaceCoercion, IfaceTyLit, IfaceAppArgs )
import Var ( VarBndr, TyCoVar )
import VarEnv ( TidyEnv )
import TyCon ( TyCon )
import VarSet( VarSet )

-- For TyCoRep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: (VarBndr TyCoVar flag) -> (VarBndr IfaceBndr flag)
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceAppArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
tidyToIfaceTcArgs :: TidyEnv -> TyCon -> [Type] -> IfaceAppArgs
