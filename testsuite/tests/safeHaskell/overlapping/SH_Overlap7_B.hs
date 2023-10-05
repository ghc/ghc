{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE Safe #-}
module SH_Overlap7_B (
    C(..)
  ) where

class C a where
  f :: a -> String

