{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Prim
import GHC.Int

-- Shift should be larger than the word size (e.g. 64 on 64-bit) for this test.
main = print (I# (uncheckedIShiftL# 1# 1000#))
