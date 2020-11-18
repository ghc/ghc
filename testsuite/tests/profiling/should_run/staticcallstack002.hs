{-# LANGUAGE UnboxedTuples #-}
module Main where

import GHC.Stack.CCS

-- Unboxed data constructors don't have info tables so there is
-- a special case to not generate distinct info tables for unboxed
-- constructors.
main = do
  print . tail =<< whereFrom (undefined (# #))
  print . tail =<< whereFrom (undefined (# () #))
  print . tail =<< whereFrom (undefined (# (), () #))
  print . tail =<< whereFrom (undefined (# | () #))

