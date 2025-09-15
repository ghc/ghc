{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
module SH_Overlap2_B (
    C(..)
  ) where

class C a where
  f :: a -> String

