{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, UndecidableInstances #-}
module NativeNestedPackagePatProvider where

import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

type HiddenVec a = exists (m :: Nat). Vec a m

data NestedResult a where
    NestedResult :: (some (n :: Nat) |
        (Vec a n, (some (m :: Nat) | (Vec a n, Vec a m, HiddenVec a)))) -> NestedResult a

seal :: Nested a -> NestedResult a
seal (Nested (Pair n (xs, Pair m (ys, zs)))) =
    NestedResult (Pair n (xs, Pair m (ys, zs, zs)))

data Inputs a = Inputs (NestedResult a) (HiddenVec a)

sealInputs :: Nested a -> Inputs a
sealInputs input@(Nested (Pair _ (xs, _))) = Inputs (seal input) xs

data Coupled a (n :: Nat) = Coupled (Vec a n) (Vec a n)
data Sealed a = Sealed (exists (n :: Nat). Coupled a n)

emptyInput :: Nested Bool
emptyInput = makeNested (S Z) Z (error "skipped primary vector") (VCons False VNil) VNil

fullInput :: Nested Bool
fullInput = makeNested (S Z) (S Z) (VCons False VNil) (VCons False VNil) (VCons True VNil)
