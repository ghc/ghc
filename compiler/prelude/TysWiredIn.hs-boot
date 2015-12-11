module TysWiredIn where

import TyCon
import {-# SOURCE #-} TyCoRep    (Type, Kind)


listTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type

levityTy, unliftedDataConTy :: Type

liftedTypeKind :: Kind
