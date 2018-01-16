{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap3_A (
    C(..)
  ) where

import SH_Overlap3_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

