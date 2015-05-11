{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap1_A (
    C(..)
  ) where

import SH_Overlap1_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

