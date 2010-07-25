-- Check overlap in n+k patterns

{-# LANGUAGE NPlusKPatterns #-}

module Foo where

g :: Int -> Int
g (x+1) = x
g y     = y	
g _     = 0	-- Overlapped

h :: Int -> Int
h (x+1) = x
h _     = 0	-- Not overlapped
