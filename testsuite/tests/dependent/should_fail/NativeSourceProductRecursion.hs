{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourceProductRecursion where

import PackedVecLibrary (Nat(..), plus)

-- The recursive call reconstructs its argument instead of passing its field.
badProduct :: Nat -> Nat -> Nat
badProduct Z     m = Z
badProduct (S n) m = plus m (badProduct (S n) m)

type Nondecreasing = badProduct ('S 'Z) ('S 'Z)
