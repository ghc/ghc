{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as SH_Overlap1, but module where overlap occurs (SH_Overlap3) is
-- marked `Unsafe`. Compilation should succeed (symmetry with inferring safety).
module SH_Overlap3 where

import SH_Overlap3_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

