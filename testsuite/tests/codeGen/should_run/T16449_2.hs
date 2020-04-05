{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Prim
import GHC.Int

-- Test that large unchecked shifts, which constitute undefined behavior, do
-- not crash the compiler and instead evaluate to 0.
-- See Note [Guarding against silly shifts] in GHC.Core.Opt.ConstantFold.

-- Shift should be larger than the word size (e.g. 64 on 64-bit) for this test.
main = print (I# (uncheckedIShiftL# 1# 1000#))
