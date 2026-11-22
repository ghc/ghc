{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}

module ZipRows (Min, countWithHeaderStep, zipMinimum, zipRows) where

import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), sourceLength)
import ZipLengthSource (countWithHeader, zipLength)

-- | Pair the elements up to the end of the shorter list.
zipRows :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> Vec (a, b) (zipLength xs ys)
zipRows [] _ = VNil
zipRows (_ : _) [] = VNil
zipRows (x : xs) (y : ys) = VCons (x, y) (zipRows xs ys)

-- | The smaller of two natural numbers, inspecting the first number first.
type family Min (n :: Nat) (m :: Nat) :: Nat where
    Min Z m = Z
    Min (S n) Z = Z
    Min (S n) (S m) = S (Min n m)

-- | Prove the output length by induction on the paired cells.
zipMinimum :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b]) ->
    zipLength xs ys :~: Min (sourceLength xs) (sourceLength ys)
zipMinimum [] _ = Refl
zipMinimum (_ : _) [] = Refl
zipMinimum (_ : xs) (_ : ys) = case zipMinimum xs ys of Refl -> Refl

-- | Prove the count for one record and the same header list.
countWithHeaderStep :: forall a b. foreach (h :: a) -> foreach (hs :: [a])
                    -> foreach (r :: b) -> foreach (rs :: [b])
                    -> countWithHeader (h ': hs) (r ': rs) :~: S (countWithHeader (h ': hs) rs)
countWithHeaderStep _ _ _ _ = Refl
