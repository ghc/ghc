{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -fwarn-safe #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is inferred safe
-- In this test the warning _should_ fire
module SafeFlags25 where

f :: Int
f = 1

