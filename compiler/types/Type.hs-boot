{-# LANGUAGE FlexibleContexts #-}

module Type where
import TyCon
import Var ( TyVar )
import {-# SOURCE #-} TyCoRep( Type, Coercion, Kind )
import Util

isPredTy     :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
mkCastTy :: Type -> Coercion -> Type
piResultTy :: Type -> Type -> Type

typeKind :: Type -> Kind
eqType :: Type -> Type -> Bool

partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type

tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
