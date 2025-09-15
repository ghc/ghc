{-# OPTIONS_GHC  -fmax-simplifier-iterations=20 #-}
-- This module made the Simplifier iterate for ever

module T24582(woo) where


foo :: String -> Int -> a
{-# NOINLINE foo #-}
foo s _ = error s

f :: (Int->Int) -> Int
{-# NOINLINE f #-}
f g = g 3

x :: Int -> a
x = foo "urk"

woo = f x
