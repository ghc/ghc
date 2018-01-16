{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T2204 where

f :: String -> Int
f "01" = 0

g :: Int -> Int
g 0 = 0
