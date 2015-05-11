{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap4_A (
    C(..)
  ) where

import SH_Overlap4_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

