{-# OPTIONS_GHC -Wmissing-kind-signatures #-}
{-# LANGUAGE GADTs, PolyKinds, TypeFamilies #-}
-- with cusks but without -XCUSK, warnings
module T19564c where

import Data.Kind (Type, Constraint)

-- type family
type family Id (x :: Type) :: Type where
    Id Int = Int

-- class definition
class Functor f => Alt (f :: Type -> Type) where
    (<!>) :: f a -> f a -> f a

-- type alias
type Arr (a :: Type) (b :: Type) = a -> b :: Type
type B = Bool :: Type

-- Haskell98 data
data YesNo = Yes | No
data V2 (a :: Type) = V2 a a

-- GADT
data Free (f :: Type -> Type) (a :: Type) where
    Pure :: a -> Free f a
    Ap   :: f b -> Free f (b -> a) -> Free f a

-- data family
data family D1 (a :: Type)

-- associated type family
class C (a :: Type) where
    type AT a b
