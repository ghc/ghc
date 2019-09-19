{-# language MagicHash #-}

import GHC.Num.Natural
import GHC.Num.BigNat
import GHC.Exts

main = print $ map naturalCheck [0, 1, maxWord, maxWord + 1, invalid]
  where
    maxWord = fromIntegral (maxBound :: Word)
    invalid = NB (bigNatOne void#) -- 1 would fit into the NS constructor.
