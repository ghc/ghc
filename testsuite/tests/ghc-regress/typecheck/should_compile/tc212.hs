{-# OPTIONS -fglasgow-exts #-}

-- This one crashed the 6.6 release candidate

module ShouldCompile where

-- A specialise pragma with no type signature
fac n = fac n
{-# SPECIALISE fac :: Int -> Int #-}
