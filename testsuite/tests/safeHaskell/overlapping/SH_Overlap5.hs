{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Compilation should fail as we have overlapping instances that don't obey
-- our heuristics.
module SH_Overlap5 where

import safe SH_Overlap5_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

