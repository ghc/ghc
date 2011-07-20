-- !!! Check that type signatures and pragmas that 
-- !!! don't have a "parent" are correctly reported

module ShouldFail where

-- Top level test
f :: Int -> Int
{-# INLINE f #-}

-- Nested test
h :: Int -> Int	-- This one is ok
h x = x
    where
      g :: Int -> Int	-- Bogus

