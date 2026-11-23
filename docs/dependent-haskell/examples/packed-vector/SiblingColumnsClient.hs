{-# LANGUAGE NoExperimentalDependentTypes #-}
module SiblingColumnsClient (readPairs) where

import PackedVecLibrary (toList, zipVec)
import SiblingColumns (Columns, flags, letters)

-- | Read the two vectors at the shared stored length.
readPairs :: Columns -> [(Char, Bool)]
readPairs p = toList (zipVec (letters p) (flags p))
