{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word

big :: Word
big = maxBound

carry :: Word
carry = case big of
  W# w -> case plusWord2# w w of
    (# hi, lo #) -> W# hi

main = print carry
