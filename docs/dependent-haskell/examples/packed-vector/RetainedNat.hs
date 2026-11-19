{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, RankNTypes, TypeFamilies #-}
-- | Eliminate a package at its stored natural index.
module RetainedNat (Retained(..), withRetained, constantPackage, Witness, project) where

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

-- | The stored index of a package over any admitted payload family.
type family Witness (q :: Retained p) :: Nat where
    Witness ('Retain n value) = n

-- | Return the payload at the particular witness stored by the caller.
project :: foreach (q :: Retained p) -> p (Witness q)
project (Retain _ value) = value
