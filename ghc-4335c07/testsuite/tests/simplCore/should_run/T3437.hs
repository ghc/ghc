{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

-- Trac #3437
-- When we do SpecConstr on 'go', we want the specialised
-- function to *still* be strict in k.  Otherwise we get
-- a bad space leak!

-- The test is run with +RTS -M10m to limit the amount of heap
-- It should run in constant space, but if the function isn't 
-- strict enough it'll run out of heap

module Main where

go :: [Int] -> [Int] -> [Int]
go (0:xs) !k = k
go (n:xs) !k = go (n-1 : xs) (k ++ k)

main = print (go [100000000] [])
