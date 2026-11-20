{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators, UndecidableInstances #-}

-- | Partition a vector and preserve its total length.
module ConservedPartition (Split, partitionVec, Branch, branchContents) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Nat(..), Vec(..), Add, addSucc, natToInt, toList)

-- | Store both output lengths, their vectors, and their sum.
type Split a (total :: Nat) =
    some (kept :: Nat) | some (dropped :: Nat) |
        (Vec a kept, Vec a dropped, Add kept dropped :~: total)

-- | Partition a finite vector in input order within each output.
partitionVec :: (a -> Bool) -> Vec a n -> Split a n
partitionVec _ VNil = Pair Z (Pair Z (VNil, VNil, Refl))
partitionVec p (VCons x xs) =
    case partitionVec p xs of
        Pair kept (Pair dropped (ys, zs, Refl))
            | p x -> Pair (S kept) (Pair dropped (VCons x ys, zs, Refl))
            | otherwise -> case addSucc kept dropped of
                Refl -> Pair kept (Pair (S dropped) (ys, VCons x zs, Refl))

-- | Use one partition at its retained length.
type Branch a r = foreach (n :: Nat) -> Vec a n -> r

-- | Read the length and contents of one partition.
branchContents :: Branch a (Int, [a])
branchContents n xs = (natToInt n, toList xs)
