{-# LANGUAGE Safe #-}
module SH_Overlap2_B (
    C(..)
  ) where

class C a where
  f :: a -> String

