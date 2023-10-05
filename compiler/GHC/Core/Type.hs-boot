{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GHC.Prelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc
import GHC.Types.Var( FunTyFlag, TyVar )
import GHC.Types.Basic( TypeOrConstraint )

isPredTy     :: HasDebugCallStack => Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy    :: Type -> Type -> Type
mkCastTy   :: Type -> Coercion -> Type
mkTyConApp :: TyCon -> [Type] -> Type
mkCoercionTy :: Coercion -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

typeKind :: HasDebugCallStack => Type -> Type
typeTypeOrConstraint :: HasDebugCallStack => Type -> TypeOrConstraint

coreView       :: Type -> Maybe Type
rewriterView   :: Type -> Maybe Type
isRuntimeRepTy :: Type -> Bool
isLevityTy :: Type -> Bool
isMultiplicityTy :: Type -> Bool
isLiftedTypeKind :: Type -> Bool

splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
tyConAppTyCon_maybe :: Type -> Maybe TyCon
getTyVar_maybe      :: Type -> Maybe TyVar

getLevity :: HasDebugCallStack => Type -> Type

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])

chooseFunTyFlag :: HasDebugCallStack => Type -> Type -> FunTyFlag
