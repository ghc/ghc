module GHC.Core.Type where

import GHC.Prelude
import {-# SOURCE #-} GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.TyCo.Rep( Type, Coercion )
import GHC.Utils.Misc
import GHC.Types.Var( FunTyFlag, TyVar, Id )
import GHC.Types.Basic( TypeOrConstraint )


coreView         :: Type -> Maybe Type
rewriterView     :: Type -> Maybe Type
chooseFunTyFlag  :: HasDebugCallStack => Type -> Type -> FunTyFlag
typeKind         :: HasDebugCallStack => Type -> Type
isCoercionTy     :: Type -> Bool
mkCastTy         :: Type -> Coercion -> Type
getLevity        :: HasDebugCallStack => Type -> Type
getTyVar_maybe   :: Type -> Maybe TyVar
isLiftedTypeKind :: Type -> Bool

partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
typeTypeOrConstraint    :: HasDebugCallStack => Type -> TypeOrConstraint

coreFullView :: Type -> Type
isNativeIndexFunctionId :: Id -> Bool
nativeIndexFunctionTys_maybe :: Id -> Maybe (Type, Type)

-- | Check the consumed arrow kind for proven nominal matchability.
isMatchableFunTy :: Type -> Bool

-- | Build an arrow from the actual argument and result sorts.
mkFunctionType :: HasDebugCallStack => Type -> Type -> Type -> Type -> Type
