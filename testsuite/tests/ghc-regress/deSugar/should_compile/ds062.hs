{-# OPTIONS_GHC -Wall #-}

module ShouldCompile where

f :: String -> Int
f x | null x    = 1
    | otherwise = 2

-- Should not give a non-exhaustive-patterns error
-- See Trac #1759

