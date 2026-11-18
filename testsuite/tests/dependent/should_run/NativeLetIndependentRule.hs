{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeOperators #-}
module Main where

import PackedVecLibrary

-- | Read the first cell without checking the product's value.
headOfProduct :: foreach (r :: Nat) -> foreach (c :: Nat) -> Char
headOfProduct r c =
    let total, area :: Nat
        total = S area
        area = r * c
    in safeHead (replicateNative total 's')

-- | The product equation can reduce, but neither caller needs that equality.
knownTail :: foreach (c :: Nat) -> Char
knownTail c =
    let total, area :: Nat
        total = S area
        area = (S Z) * c
    in safeHead (replicateNative total 'k')

unusedTail :: foreach (c :: Nat) -> Char
unusedTail c =
    let area :: Nat
        area = (S Z) * c
    in const 'u' (replicateNative area 'x')

-- | Read a vector whose index exposes a successor.
safeHead :: Vec a (1 + n) -> a
safeHead (VCons x _) = x

{-# RULES "change independent multiplication" forall (n :: Nat) (m :: Nat). n * m = Z #-}

{-# RULES "change independent times" forall n m. times n m = Z #-}

main :: IO ()
main = print (headOfProduct (S Z) (S Z), headOfProduct undefined (S Z), knownTail undefined, unusedTail undefined)
