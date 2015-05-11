{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}
module SH_Overlap11_A (
    C(..)
  ) where

import SH_Overlap11_B

instance
  {-# OVERLAPS #-}
  C [Int] where
    f _ = "[Int]"

