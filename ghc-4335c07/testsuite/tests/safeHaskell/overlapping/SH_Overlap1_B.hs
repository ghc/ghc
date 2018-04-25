{-# LANGUAGE Safe #-}
module SH_Overlap1_B (
    C(..)
  ) where

class C a where
  f :: a -> String

