module TysWiredIn where

import {-# SOURCE #-} TyCon      (TyCon)
import {-# SOURCE #-} TypeRep    (Type)


eqTyCon, coercibleTyCon, instanceOfTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
