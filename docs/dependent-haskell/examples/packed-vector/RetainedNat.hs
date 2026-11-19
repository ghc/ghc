{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
-- | Eliminate a package at its stored natural index.
module RetainedNat (Retained(..), withRetained, constantPackage) where

import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import PackedVecLibrary (Nat)

-- | Store a runtime natural with a payload from the selected family.
data Retained (p :: Nat -> Type) where
    Retain :: foreach (n :: Nat) -> p n -> Retained p

-- | Give the consumer the stored witness and its matching payload.
withRetained :: Retained p -> (foreach (n :: Nat) -> p n -> r) -> r
withRetained (Retain n value) consume = consume n value

-- | Store a payload whose type ignores the witness.
constantPackage :: Nat -> a -> Retained (Const a)
constantPackage k value = Retain k (Const value)
