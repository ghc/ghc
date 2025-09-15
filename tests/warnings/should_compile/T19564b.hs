{-# OPTIONS_GHC -Wmissing-kind-signatures #-}
{-# LANGUAGE GADTs, PolyKinds, TypeFamilies #-}
-- with kind signatures: no warnings
module T19564b where

import Data.Kind (Type, Constraint)

-- type family
type Id :: Type -> Type
type family Id x where
    Id Int = Int

-- class definition
type Alt :: (Type -> Type) -> Constraint
class Functor f => Alt f where
    (<!>) :: f a -> f a -> f a

-- type alias
type Arr :: Type -> Type -> Type
type Arr a b = a -> b

type B :: Type
type B = Bool

-- Haskell98 data
type YesNo :: Type
data YesNo = Yes | No

type V2 :: Type -> Type
data V2 a = V2 a a

-- GADT
type Free :: (Type -> Type) -> (Type -> Type)
data Free f a where
    Pure :: a -> Free f a
    Ap   :: f b -> Free f (b -> a) -> Free f a

-- data family
type D1 :: Type -> Type
data family D1 a

-- associated type family
type C :: Type -> Constraint
class C a where
    -- is defaulted, doesn't need annotation
    type AT a b
