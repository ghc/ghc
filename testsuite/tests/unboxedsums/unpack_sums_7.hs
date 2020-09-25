-- NB: Compiling this module throws an exception involving Weak# at the end of compilation.
-- This is unrelated to unpacked sums but we need to include the error in the expected output for the test to pass.

module UnpackedSums7 where

data T = MkT {-# UNPACK #-} !MI

data MI = NoI | JI Int

t = MkT (JI 5)
