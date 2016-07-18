module RepType where

import BasicTypes (Arity, RepArity)
import Outputable (Outputable)
import TyCoRep (Type)

data RepType
  = MultiRep [SlotTy]
  | UnaryRep UnaryType

data SlotTy
type UnaryType = Type

repType :: Type -> RepType
typeRepArity :: Arity -> Type -> RepArity

instance Outputable RepType
