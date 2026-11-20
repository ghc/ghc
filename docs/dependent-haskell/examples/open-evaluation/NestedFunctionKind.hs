{-# LANGUAGE ExplicitNamespaces, GADTs, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NestedFunctionKind (type (~>), Demote) where

import Data.Kind (Type)
import GHC.Exts (M)

-- | Represent the domain and result of a type-level function.
data TyFun :: Type -> Type -> Type

-- | The kind of the defunctionalization symbols for this domain and result.
type a ~> b = TyFun a b %M -> Type

-- | Interpret a function code as an arrow.
type family Demote (k :: Type) :: Type
type instance Demote Type = Type
type instance Demote Bool = Bool
type instance Demote (a ~> b) = DemoteX a -> DemoteX b

-- | The choice of arrow in the result can differ from the choice in its kind.
type family DemoteX (a :: k) :: Demote k
type instance DemoteX (a :: Type) = Demote a
