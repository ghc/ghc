{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module DirectIndexedVector (SomeVec(SomeVec)) where

import PackedVecLibrary (Nat, Vec)

-- | Store a runtime length and a vector at that length.
data SomeVec a (n :: Nat) where
    SomeVec :: foreach (n :: Nat) -> Vec a n -> SomeVec a n
