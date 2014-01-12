{-# OPTIONS_GHC -fno-package-trust #-}
-- | Basic test to see if Safe flags compiles
-- test should fail as there shouldn't be a no-package-trust flag, only a
-- package-trust flag!
module SafeFlags19 where

f :: Int
f = 1

