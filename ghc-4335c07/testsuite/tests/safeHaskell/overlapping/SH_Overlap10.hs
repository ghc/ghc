{-# OPTIONS_GHC -fwarn-unsafe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as `SH_Overlap6`, but now we are inferring safety. Safe since
-- overlapped instance declares itself overlappable.
module SH_Overlap10 where

import SH_Overlap10_A

instance
  {-# OVERLAPS #-}
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

