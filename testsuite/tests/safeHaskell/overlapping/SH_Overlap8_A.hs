{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap8_A (
    C(..)
  ) where

class C a where
  f :: a -> String

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

