{-# LANGUAGE FlexibleContexts #-}

module Type where

import GhcPrelude
import {-# SOURCE #-} TyCon
import {-# SOURCE #-} TyCoRep( Type, Coercion )
import Util

isPredTy     :: HasDebugCallStack => Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy    :: Type -> Type -> Type
mkCastTy   :: Type -> Coercion -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

eqType :: Type -> Type -> Bool

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type
isRuntimeRepTy :: Type -> Bool
isLiftedTypeKind :: Type -> Bool

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
