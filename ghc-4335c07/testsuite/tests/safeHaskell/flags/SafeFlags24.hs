{-# OPTIONS_GHC -fwarn-safe #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is infered safe
-- In this test the warning _shouldn't_ fire
module SafeFlags23 where

import System.IO.Unsafe

f :: Int
f = 1

