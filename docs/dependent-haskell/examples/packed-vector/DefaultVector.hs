{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module DefaultVector (DefaultElement(..)) where

import PackedVecLibrary (Nat, Vec, replicateNative)

-- | A type whose default value can fill a vector of a requested length.
class DefaultElement a where
  -- | Supply the value to put in each cell.
  defaultElement :: a
  -- | Construct a vector with the requested length.
  filled :: foreach (n :: Nat) -> Vec a n
  filled n = replicateNative n defaultElement
