{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes, RequiredTypeArguments #-}

-- | Construct and open imported dependent pairs in ordinary Haskell.
module ListVectorClient (fromList, contents, regroup, regroupObservation, choosePartition) where

import ConservedPartition (Branch, Split, partitionVec)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Nat, Vec, appendVec, natToInt, sourceLength, sourceVector, toList)
import StoredVector (ListVector)

-- | The same list supplies the length and the vector.
fromList :: [a] -> ListVector a
fromList xs = Pair (sourceLength xs) (sourceVector xs)

-- | Read either field independently.
contents :: ListVector a -> (Int, [a])
contents (Pair n xs) = (natToInt n, toList xs)

-- | Put the elements that pass the predicate first, at the same length.
regroup :: (a -> Bool) -> Vec a n -> Vec a n
regroup p xs = case partitionVec p xs of
    Pair _ (Pair _ (kept, dropped, Refl)) -> appendVec kept dropped

-- | Observe the original length and the regrouped elements.
regroupObservation :: (a -> Bool) -> [a] -> (Int, [a])
regroupObservation p xs = case fromList xs of
    Pair n v -> (natToInt n, toList (regroup p v))

-- | Give the callback the length and elements of the selected partition.
choosePartition :: forall a r. forall (total :: Nat) -> Bool -> Split a total -> Branch a r -> r
choosePartition _ selectKept (Pair kept (Pair dropped (xs, ys, _))) consume
    | selectKept = consume kept xs
    | otherwise  = consume dropped ys
