{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}
-- | Basic test to see if Safe warning flags compile
-- Warn if module is inferred unsafe
-- In this test the warning _shouldn't_ fire
module SafeFlags21 where

f :: Int
f = 1

