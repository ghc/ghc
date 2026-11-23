{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
-- | Named vectors that share one stored length.
module NamedSiblingColumns (NamedColumns(..)) where

import PackedVecLibrary (Nat, Vec)

-- | Each vector uses the stored length of this record.
data NamedColumns = NamedColumns
  { storedLength :: Nat
  , namedLetters :: Vec Char storedLength
  , namedFlags :: Vec Bool storedLength
  }
