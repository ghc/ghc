{-# LANGUAGE UnboxedTuples #-}
module Main where

import GHC.InfoProv

-- Unboxed data constructors don't have info tables so there is
-- a special case to not generate distinct info tables for unboxed
-- constructors.
main = do
  print =<< whereFrom (undefined (# #))
  print =<< whereFrom (undefined (# () #))
  print =<< whereFrom (undefined (# (), () #))
  print =<< whereFrom (undefined (# | () #))
