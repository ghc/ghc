{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GHC.Prelude
import GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc

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
