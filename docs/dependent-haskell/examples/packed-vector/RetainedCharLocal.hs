{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}
-- | Define the two projections in the same declaration group.
module RetainedCharLocal (localLength, localCells) where

import GHC.Exts (Char(..))
import PackedVecLibrary (Nat(..), Vec(..))
import RetainedCharVector (CharVector(..))

-- | Read the witness without demanding the vector.
localLength :: CharVector -> Nat
localLength (CharVector n _) = n

-- | Return the vector at the length selected by the local worker.
localCells :: foreach (r :: CharVector) -> Vec Char (localLength r)
localCells (CharVector _ xs) = xs
