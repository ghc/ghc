{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as `SH_Overlap6`, but now we are inferring safety. Should be inferred
-- unsafe due to overlapping instances at call site `f`.
--
-- Testing that we are given correct reason.
module SH_Overlap11 where

import SH_Overlap11_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

