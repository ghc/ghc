{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module should actually fail to compile since we have the instances C
-- [Int] from the -XSafe module SafeInfered05_A overlapping as the most
-- specific instance the other instance C [a] from this module. This is in
-- violation of our single-origin-policy.
--
-- Right now though, the above actually compiles fine but *this is a bug*.
-- Compiling module SafeInfered05_A with -XSafe has the right affect of causing
-- the compilation of module SafeInfered05 to then subsequently fail. So we
-- have a discrepancy between a safe-inferred module and a -XSafe module, which
-- there should not be.
--
-- It does raise a question of if this bug should be fixed. Right now we've
-- designed Safe Haskell to be completely opt-in, even with safe-inference.
-- Fixing this of course changes this, causing safe-inference to alter the
-- compilation success of some cases. How common it is to have overlapping
-- declarations without -XOverlappingInstances specified needs to be tested.
--
module SafeInfered05 where

import safe SafeInfered05_A

instance C [a] where
  f _ = "[a]"

test2 :: String
test2 = f ([1,2,3,4] :: [Int])

