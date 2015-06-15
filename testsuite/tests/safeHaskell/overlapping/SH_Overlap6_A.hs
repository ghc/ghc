{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap6_A (
    C(..)
  ) where

import SH_Overlap6_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

