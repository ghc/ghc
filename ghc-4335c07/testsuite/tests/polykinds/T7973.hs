{-# LANGUAGE DataKinds, PolyKinds, KindSignatures #-}
{-# LANGUAGE ExistentialQuantification, UndecidableInstances, TypeFamilies #-}

module Test where

-- Kind-level proxies.
data {-kind-} K (a :: *) = KP

-- A type with 1 kind-polymorphic type argument.
data T (n :: k)

-- A type with 1 kind argument.
data F (kp :: K k)

-- A class with 1 kind argument.
class (kp ~ KP) => C (kp :: K k) where
  f :: T (a :: k) -> F kp

-- A type with 1 kind argument.
-- Contains an existentially quantified type-variable of this kind.
data SomeT (kp :: K k) = forall (n :: k). Mk (T n)

-- Show `SomeT` by converting it to `F`, using `C`.
instance (C kp, Show (F kp)) => Show (SomeT kp) where
  show (Mk x) = show (f x)
