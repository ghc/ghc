module TysWiredIn where

import {-# SOURCE #-} TyCon      (TyCon)
import {-# SOURCE #-} TypeRep    (Type)


eqTyCon, listTyCon, coercibleTyCon :: TyCon
typeNatKind, typeSymbolKind :: Type
mkBoxedTupleTy :: [Type] -> Type
