{-# OPTIONS_GHC -Wmissing-kind-signatures #-}
{-# LANGUAGE GADTs, PolyKinds, TypeFamilies #-}
-- without standalone kind signatures or cusks: warnings
module T19564a where

-- type family
type family Id x where
    Id Int = Int

-- class definition
class Functor f => Alt f where
    (<!>) :: f a -> f a -> f a

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

-- associated type family
class C a where
    type AT a b
