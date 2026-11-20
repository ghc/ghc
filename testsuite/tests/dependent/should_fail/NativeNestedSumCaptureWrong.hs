{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies, UndecidableInstances #-}

module NativeNestedSumCaptureWrong where

import Data.Kind (Type)

data N = Z | S N

data Vec a (n :: N) where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

type Add :: N -> N -> N
type family Add n m where
  Add Z m = m
  Add (S n) m = S (Add n m)

type AtLength :: forall (n :: N) -> Vec Bool n -> Type
type AtLength n v = ()

type Wrong (n :: N) (m :: N) (v :: Vec Bool (Add n m)) =
  some (left :: Vec Bool n) |
    (some (right :: Vec Bool m) | AtLength (Add m n) v)
