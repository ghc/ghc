{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module DirectIndexedVector (SomeVec(SomeVec), SuccVec(SuccVec)) where

import PackedVecLibrary (Nat(S), Vec)

-- | Store a runtime length and a vector at that length.
data SomeVec a (n :: Nat) where
    SomeVec :: foreach (n :: Nat) -> Vec a n -> SomeVec a n

-- | Store a vector with a public index one greater than its length.
data SuccVec a (size :: Nat) where
    SuccVec :: foreach (n :: Nat) -> Vec a n -> SuccVec a ('S n)
