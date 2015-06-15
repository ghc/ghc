{-# OPTIONS_GHC -fwarn-safe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as `SH_Overlap6`, but now we are inferring safety. Should be inferred
-- unsafe due to overlapping instances at call site `f`.
module SH_Overlap9 where

import SH_Overlap9_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

