{-# LANGUAGE NoExperimentalDependentTypes #-}

-- | Use generated dependent record selectors in ordinary Haskell.
module NamedSiblingColumnsClient (readPairs) where

import NamedSiblingColumns (NamedColumns, namedFlags, namedLetters)
import PackedVecLibrary (toList, zipVec)

-- | Pair each character with the flag at the same position.
readPairs :: NamedColumns -> [(Char, Bool)]
readPairs p = toList (zipVec (namedLetters p) (namedFlags p))
