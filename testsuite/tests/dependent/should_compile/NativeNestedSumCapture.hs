{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

module NativeNestedSumCapture where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:))

data N = Z | S N

data Vec a (n :: N) where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

type Add :: N -> N -> N
type family Add n m where
  Add Z m = m
  Add (S n) m = S (Add n m)

type Keep a (n :: N) (m :: N) (v :: Vec a (Add n m)) =
  some (left :: Vec a n) |
    (some (right :: Vec a m) | (Proxy left, Proxy right, v :~: v))

type Shadow :: forall (a :: Type) -> forall (n :: N) -> forall (m :: N)
            -> Vec a (Add n m) -> Type
type Shadow a (n :: N) (m :: N) (v :: Vec a (Add n m)) =
  some (n :: Vec a n) |
    (some (x :: Proxy n) | (Proxy n, Proxy x, v :~: v))

type NestedShadow (n :: N) (v :: Vec Bool (Add n n)) =
  some (n :: Vec Bool n) |
    (some (n :: Proxy n) |
      (some (x :: Proxy n) | (Proxy n, Proxy x, v :~: v)))
