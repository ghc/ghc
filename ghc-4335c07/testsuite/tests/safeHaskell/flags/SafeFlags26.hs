{-# OPTIONS_GHC -fwarn-safe -Werror #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is infered safe
-- In this test the warning _should_ fire and cause a compile fail
module SafeFlags26 where

f :: Int
f = 1

