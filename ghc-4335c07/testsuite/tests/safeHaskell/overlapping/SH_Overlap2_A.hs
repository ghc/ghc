{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap2_A (
    C(..)
  ) where

import SH_Overlap2_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

