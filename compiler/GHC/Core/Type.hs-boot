{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GHC.Prelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc
import GHC.Types.Var( AnonArgFlag )
import GHC.Types.Basic( TypeOrConstraint )

isPredTy     :: HasDebugCallStack => Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy    :: Type -> Type -> Type
mkCastTy   :: Type -> Coercion -> Type
mkTyConTy  :: TyCon -> Type
mkTyConApp :: TyCon -> [Type] -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

typeKind :: HasDebugCallStack => Type -> Type
typeTypeOrConstraint :: HasDebugCallStack => Type -> TypeOrConstraint

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type
isRuntimeRepTy :: Type -> Bool
isLevityTy :: Type -> Bool
isMultiplicityTy :: Type -> Bool
isLiftedTypeKind :: Type -> Bool

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
tyConAppTyCon_maybe :: Type -> Maybe TyCon

getLevity :: HasDebugCallStack => Type -> Type

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])

chooseAnonArgFlag :: HasDebugCallStack => Type -> Type -> AnonArgFlag