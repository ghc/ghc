{-# LANGUAGE BangPatterns, DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes, RequiredTypeArguments, ScopedTypeVariables #-}

-- | Construct and open imported dependent pairs in ordinary Haskell.
module ListVectorClient
    ( fromList, contents, familyResultLength, regroup, regroupObservation, choosePartition,
      checkedSplitObservation, checkedSplitIO
    ) where

import ConservedPartition (Branch, Split, partitionVec)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair), first)
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, appendVec, natToInt, sourceLength, sourceVector,
                         splitAtVecChecked, toList)
import StoredVector (ListVector, VectorResult)

-- | The same list supplies the length and the vector.
fromList :: [a] -> ListVector a
fromList xs = Pair (sourceLength xs) (sourceVector xs)

-- | Read either field independently.
contents :: ListVector a -> (Int, [a])
contents (Pair n xs) = (natToInt n, toList xs)

-- | Read the length of a family result without demanding its vector.
familyResultLength :: Nat -> Int
familyResultLength n = natToInt (first (Pair n (error "unused vector") :: VectorResult True Char))

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

-- | Check an independent cut and report the suffix length and both parts.
checkedSplitObservation :: Nat -> [a] -> Maybe (Int, [a], [a])
checkedSplitObservation cut input = case fromList input of
    Pair total cells -> case splitAtVecChecked cut total cells of
        Nothing -> Nothing
        Just (Pair suffix (left, right, _)) ->
            Just (natToInt suffix, toList left, toList right)

-- | Observe a checked split with no suffix and a second vector at the original length.
checkedSplitIO :: forall a total. IO (Maybe (CheckedSplit a ('S 'Z) total))
               -> Vec a total -> IO (Maybe ([a], [a]))
checkedSplitIO obtain later = do
    split <- obtain
    pure (split >>= \pair -> consume pair later)
  where
    consume :: CheckedSplit a ('S 'Z) total -> Vec a total -> Maybe ([a], [a])
    consume (Pair Z !(left, right, Refl)) (second :: Vec a ('S 'Z)) =
        Just (toList (appendVec left right), toList second)
    consume _ _ = Nothing
