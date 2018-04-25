{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap10_A (
    C(..)
  ) where

import SH_Overlap10_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

