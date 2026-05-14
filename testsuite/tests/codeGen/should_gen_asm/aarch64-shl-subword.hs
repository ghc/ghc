{-# LANGUAGE MagicHash #-}
module ShlSubWord (shlW8) where

import GHC.Exts
import GHC.Word

shlW8 :: Word8 -> Word8
shlW8 (W8# w) = W8# (uncheckedShiftLWord8# w 4#)
