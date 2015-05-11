{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as `SH_Overlap5` but dependencies are now inferred-safe, not
-- explicitly marked. Compilation should still fail.
module SH_Overlap6 where

import safe SH_Overlap6_A

instance C [a] where
  f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

