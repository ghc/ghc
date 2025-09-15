{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is inferred unsafe
-- In this test the warning _should_ fire
module SafeFlags22 where

import System.IO.Unsafe

f :: Int
f = 1

