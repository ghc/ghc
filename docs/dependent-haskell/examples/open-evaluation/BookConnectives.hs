{-# LANGUAGE ExperimentalDependentTypes, GADTs, TypeOperators #-}
-- | Isomorphisms for commutativity of sums and products in PLFH.
module BookConnectives (sumComm, prodComm) where

import BookIsomorphism (Iso(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Exchange the two alternatives.
swapSum :: Either a b -> Either b a
swapSum (Left x)  = Right x
swapSum (Right y) = Left y

-- | Exchanging the alternatives twice returns the original sum.
swapSumInv :: foreach (s :: Either a b) -> swapSum (swapSum s) :~: s
swapSumInv (Left _)  = Refl
swapSumInv (Right _) = Refl

-- | Disjunction is commutative.
sumComm :: Iso (Either a b) (Either b a)
sumComm = MkIso swapSum swapSum swapSumInv swapSumInv

-- | Exchange the fields of a pair.
swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)

-- | Exchanging the fields twice returns the original pair.
swapPairInv :: foreach (p :: (a, b)) -> swapPair (swapPair p) :~: p
swapPairInv p = case p of
    (,) _ _ -> Refl

-- | Conjunction is commutative.
prodComm :: Iso (a, b) (b, a)
prodComm = MkIso swapPair swapPair swapPairInv swapPairInv
