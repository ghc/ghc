{-# LANGUAGE MagicHash #-}
module UShrSubWord (ushrW8) where

import GHC.Exts
import GHC.Word

ushrW8 :: Word8 -> Int -> Word8
ushrW8 x n = x `shiftR` n
  where shiftR (W8# w) (I# i) = W8# (wordToWord8# (word8ToWord# w `uncheckedShiftRL#` i))
