{-# LANGUAGE BangPatterns, DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module NamedSplitClient (observeNamedSplitIO) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (CheckedSplit, Nat(..), Vec, natToInt, toList)

-- | Use a checked split's suffix in the type of the second vector.
observeNamedSplitIO :: forall a total. IO (Maybe (CheckedSplit a ('S 'Z) total))
                    -> Vec a total -> IO (Maybe (Int, [a], [a]))
observeNamedSplitIO obtain later = do
    split <- obtain
    pure (fmap (`consume` later) split)
  where
    consume :: CheckedSplit a ('S 'Z) total -> Vec a total -> (Int, [a], [a])
    consume (Pair suffix !(left, right :: Vec a n, Refl)) (second :: Vec a ('S n)) =
        (natToInt suffix, toList left ++ toList @a @n right, toList @a @('S n) second)
