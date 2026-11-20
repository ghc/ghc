{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}

module VectorSplitObservation (reassembleTwo) where

import PackedVecLibrary (Nat (..), Vec, appendVec, splitAtVec)

-- | Split a two-cell vector after its first cell and append the two parts.
reassembleTwo :: Vec Bool ('S ('S 'Z)) -> Vec Bool ('S ('S 'Z))
reassembleTwo input =
    case splitAtVec (S Z) input of
        (left, right) -> appendVec left right
