{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}
module RetainedCharProjection (storedCells) where

import PackedVecLibrary (Vec)
import RetainedCharLength (storedLength)
import RetainedCharVector (CharVector(..))

-- | Return the vector with the length stored in this package.
storedCells :: foreach (r :: CharVector) -> Vec Char (storedLength r)
storedCells (CharVector _ xs) = xs
