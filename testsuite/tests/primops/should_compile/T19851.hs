{-# LANGUAGE MagicHash #-}
module T19851 where

import GHC.Prim
import GHC.Word


w8 = W8# (uncheckedShiftLWord8# (wordToWord8# 100##) (-4#))
w16 = W16# (uncheckedShiftLWord16# (wordToWord16# 100##) (-4#))
w32 = W32# (uncheckedShiftLWord32# (wordToWord32# 100##) (-4#))

