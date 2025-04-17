{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Type where

import GHC.Prelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc
import GHC.Types.Var( FunTyFlag, TyVar )
import GHC.Types.Basic( TypeOrConstraint )


coreView         :: Type -> Maybe Type
rewriterView     :: Type -> Maybe Type
chooseFunTyFlag  :: HasDebugCallStack => Type -> Type -> FunTyFlag
typeKind         :: HasDebugCallStack => Type -> Type
isCoercionTy     :: Type -> Bool
mkAppTy          :: Type -> Type -> Type
mkCastTy         :: Type -> Coercion -> Type
mkTyConApp       :: TyCon -> [Type] -> Type
getLevity        :: HasDebugCallStack => Type -> Type
getTyVar_maybe   :: Type -> Maybe TyVar
isLiftedTypeKind :: Type -> Bool
isManyTy         :: Type -> Bool

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
typeTypeOrConstraint    :: HasDebugCallStack => Type -> TypeOrConstraint
