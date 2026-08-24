{-# LANGUAGE Strict, MonoLocalBinds #-}
{-# OPTIONS_GHC -Wredundant-bang-patterns #-}
module T27323b where

f :: Int -> Int
f x = let (a, b) = (x, x) in a + b
