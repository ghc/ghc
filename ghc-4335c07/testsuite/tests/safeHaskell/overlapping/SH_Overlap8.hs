{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Overlapping instances, but with a single parameter type-class and no
-- orphans. So `SH_Overlap8` decided to explicitly depend on `SH_Overlap8_A`
-- since that's where the type-class `C` with function `f` is defined.
--
-- Question: Safe or Unsafe? Safe
module SH_Overlap8 where

import safe SH_Overlap8_A

instance C [a] where
  f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

