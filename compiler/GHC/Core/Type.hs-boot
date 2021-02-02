{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GHC.Prelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc

isPredTy     :: HasDebugCallStack => Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy    :: Type -> Type -> Type
mkCastTy   :: Type -> Coercion -> Type
mkTyConTy  :: TyCon -> Type
mkTyConApp :: TyCon -> [Type] -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type
isRuntimeRepTy :: Type -> Bool
isMultiplicityTy :: Type -> Bool
isLiftedTypeKind :: Type -> Bool
tYPE :: Type -> Type

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
tyConAppTyCon_maybe :: Type -> Maybe TyCon

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
