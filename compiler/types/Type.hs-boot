module Type where
import TyCon
import {-# SOURCE #-} TyCoRep( Type, Kind )

isPredTy :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
piResultTy :: Type -> Type -> Type

typeKind :: Type -> Kind
eqType :: Type -> Type -> Bool

coreViewOneStarKind :: Type -> Maybe Type

partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])
