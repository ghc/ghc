{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

-- | Store vectors of different lengths behind one record type.
module StoredVector (Batch(..), singleton, cells) where

import GHC.Sigma (Sigma(Pair))
import GHC.Sigma.Projection (second)
import PackedVecLibrary (Nat(..), Vec(..), toList)

-- | The field is strict in the pair. Its vector and length remain lazy.
data Batch a = Batch { stored :: !(some (n :: Nat) | Vec a n) }

-- | Store one cell with its runtime length.
singleton :: a -> Batch a
singleton x = Batch (Pair (S Z) (VCons x VNil))

-- | Read the vector without inspecting its stored length.
cells :: Batch a -> [a]
cells batch = let pair = stored batch in toList (second pair)
