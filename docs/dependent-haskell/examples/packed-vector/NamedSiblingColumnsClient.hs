{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

-- | Use generated dependent record selectors in ordinary Haskell.
module NamedSiblingColumnsClient (readPairs, copyFlagsAtSameLength) where

import Data.Type.Equality ((:~:)(Refl))
import NamedSiblingColumns (NamedColumns, namedFlags, namedLetters, storedLength)
import PackedVecLibrary (sameNat, toList, zipVec)

-- | Pair each character with the flag at the same position.
readPairs :: NamedColumns -> [(Char, Bool)]
readPairs p = toList (zipVec (namedLetters p) (namedFlags p))

-- | Keep the original letters and copy the flags if the stored lengths agree.
copyFlagsAtSameLength :: NamedColumns -> NamedColumns -> Maybe NamedColumns
copyFlagsAtSameLength p q =
    case sameNat (storedLength p) (storedLength q) of
        Just Refl -> Just (p { storedLength = storedLength q, namedFlags = namedFlags q })
        Nothing -> Nothing
