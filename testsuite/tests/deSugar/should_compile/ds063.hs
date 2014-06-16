{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module ShouldCompile where

f :: Int -> Int
f ((+1) -> 1) = 5
f _           = 3

-- Should not give an overlapping-patterns or non-exhaustive-patterns error
-- See Trac #2395
