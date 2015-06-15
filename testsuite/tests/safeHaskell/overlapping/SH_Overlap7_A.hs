{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
module SH_Overlap7_A (
    C(..)
  ) where

import SH_Overlap7_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

