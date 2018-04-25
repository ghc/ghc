{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap5_A (
    C(..)
  ) where

import SH_Overlap5_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

