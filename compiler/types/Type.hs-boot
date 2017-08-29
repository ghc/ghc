{-# LANGUAGE FlexibleContexts #-}

module Type where
import TyCon
import Var ( TyCoVar )
import {-# SOURCE #-} TyCoRep( Type, Coercion )
import Util

isPredTy     :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
mkCastTy :: Type -> Coercion -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

eqType :: Type -> Type -> Bool

partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type

tyCoVarsOfTypesWellScoped :: [Type] -> [TyCoVar]
tyCoVarsOfTypeWellScoped :: Type -> [TyCoVar]
toposortTyVars :: [TyCoVar] -> [TyCoVar]
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
