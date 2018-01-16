{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap9_A (
    C(..)
  ) where

import SH_Overlap9_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

