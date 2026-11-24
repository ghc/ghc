{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
-- | Store a vector with its runtime length.
module ParamColumns (Columns(..)) where

import PackedVecLibrary (Nat, Vec)

-- | The field type uses the element parameter and this record's length.
data Columns a = Columns
  { storedLength :: Nat
  , cells :: Vec a storedLength
  }
