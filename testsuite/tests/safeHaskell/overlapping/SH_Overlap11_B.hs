{-# OPTIONS_GHC -fwarn-unsafe #-}
module SH_Overlap11_B (
    C(..)
  ) where

class C a where
  f :: a -> String

