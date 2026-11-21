{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}

module ZipRows (Min, zipMinimum, zipRows) where

import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), sourceLength)
import ZipLengthSource (zipLength)

-- | Pair the elements up to the end of the shorter list.
zipRows :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> Vec (a, b) (zipLength xs ys)
zipRows xs ys = case xs of
    [] -> VNil
    x : xt -> case ys of
        [] -> VNil
        y : yt -> VCons (x, y) (zipRows xt yt)

-- | The smaller of two natural numbers, inspecting the first number first.
type family Min (n :: Nat) (m :: Nat) :: Nat where
    Min Z m = Z
    Min (S n) Z = Z
    Min (S n) (S m) = S (Min n m)

-- | Prove the output length by induction on the paired cells.
zipMinimum :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b]) ->
    zipLength xs ys :~: Min (sourceLength xs) (sourceLength ys)
zipMinimum xs ys = case xs of
    [] -> Refl
    _ : xt -> case ys of
        [] -> Refl
        _ : yt -> case zipMinimum @a @b xt yt of Refl -> Refl
