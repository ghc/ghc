{-# language MagicHash #-}

import GHC.Integer.GMP.Internals
import GHC.Natural

main = print $ map isValidNatural [0, 1, maxWord, maxWord + 1, invalid]
  where
    maxWord = fromIntegral (maxBound :: Word)
    invalid = NatJ# oneBigNat -- 1 would fit into the NatS# constructor.
