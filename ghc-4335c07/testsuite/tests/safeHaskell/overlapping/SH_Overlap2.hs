{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Same as SH_Overlap1, but SH_Overlap2_A is not imported as 'safe'.
--
-- Question: Should the OI-check be enforced? Y, see reasoning in
-- `SH_Overlap4.hs` for why the Safe Haskell overlapping instance check should
-- be tied to Safe Haskell mode only, and not to safe imports.
module SH_Overlap2 where

import SH_Overlap2_A

instance
  C [a] where
    f _ = "[a]"

test :: String
test = f ([1,2,3,4] :: [Int])

