{-# LANGUAGE FlexibleContexts #-}

module Type where
import TyCon
import Var ( TyVar )
import {-# SOURCE #-} TyCoRep( Type, Kind )
import Util

isPredTy     :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
piResultTy :: Type -> Type -> Type

typeKind :: Type -> Kind
eqType :: Type -> Type -> Bool

coreViewOneStarKind :: Type -> Maybe Type

partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])

coreView :: Type -> Maybe Type

tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
