
-- This one crashed the 6.6 release candidate

module ShouldCompile where

-- A specialise pragma with no type signature
-- fac :: Num a => a -> a
fac n = fac (n + 1)
{-# SPECIALISE fac :: Int -> Int #-}
