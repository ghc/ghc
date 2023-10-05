{-# OPTIONS_GHC -Wmissing-poly-kind-signatures #-}
{-# LANGUAGE GADTs, PolyKinds, TypeFamilies #-}
-- without standalone kind signatures or cusks: warnings
module T22826 where

import Data.Kind (Type)

-- type family
type family Id x where
    Id Int = Int

-- class definition
class Functor f => Alt f where
    (<!>) :: f a -> f a -> f a

-- polykinded class
class EqP f where
    eqp :: f a -> f b -> Bool

-- type alias
type Arr a b = a -> b
type B = Bool

-- Haskell98 data
data YesNo = Yes | No
data V2 a = V2 a a

-- GADT
data Free f a where
    Pure :: a -> Free f a
    Ap   :: f b -> Free f (b -> a) -> Free f a

-- data family
data family D1 a

-- polykinded data
data Proxy a = Proxy

-- associated type family
class C a where
    type AT a b

-- polykinded type with partial kind spec
-- not warned: PolyKinds don't add variables here
data D (k :: Type) a (b :: k) where
  D :: [a] -> D k a b

-- polykinded type without kind signature, which is polymorphic,
-- but PolyKinds won't change it.
data E a k b = MkE a (VProxy k b)

type VProxy :: forall k -> k -> Type
data VProxy k a = MkVP
