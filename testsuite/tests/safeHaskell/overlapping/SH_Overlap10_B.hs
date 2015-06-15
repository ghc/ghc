{-# OPTIONS_GHC -fwarn-unsafe #-}
module SH_Overlap10_B (
    C(..)
  ) where

class C a where
  f :: a -> String

