{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
module ShouldCompile where

import GHC.Types

data T where
  C1 :: Int %1 -> T
  C2 :: Int %(m :: Multiplicity) -> T
  C3 :: Int -> T
