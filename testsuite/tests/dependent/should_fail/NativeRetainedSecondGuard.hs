{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, StandaloneKindSignatures #-}
module NativeRetainedSecondGuard where

import Data.Kind (Type)
data Nat = Z | S Nat
data Vec (n :: Nat) where
    Nil :: Vec Z
    Cons :: Bool -> Vec n -> Vec (S n)
type Seen :: forall (n :: Nat) -> Vec n -> Type
data Seen n xs = Seen

missing :: foreach (n :: Nat) -> foreach (xs :: Vec n) -> Seen n xs
missing Z _ = Seen
missing (S _) (Cons x _)
  | x = Seen
