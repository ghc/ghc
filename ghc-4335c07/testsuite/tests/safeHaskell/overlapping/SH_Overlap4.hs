{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as SH_Overlap3, however, SH_Overlap4_A is imported as `safe`.
--
-- Question: Should compilation now fail? N. At first it seems a nice idea to
-- tie the overlap check to safe imports. However, instances are a global
-- entity and can be imported by multiple import paths. How should safe imports
-- interact with this? Especially when considering transitive situations...
--
-- Simplest is to just enforce the overlap check in Safe and Trustworthy
-- modules, but not in Unsafe ones.
module SH_Overlap4 where

import safe SH_Overlap4_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

