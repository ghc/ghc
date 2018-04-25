{-# OPTIONS_GHC -fwarn-unsafe -Werror #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is infered unsafe
-- In this test the warning _should_ fire and cause a compile fail
module SafeFlags22 where

import System.IO.Unsafe

f :: Int
f = 1

