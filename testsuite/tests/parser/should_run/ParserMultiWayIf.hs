{-# LANGUAGE MultiWayIf #-}

module Main where

x  = 10
x1 = if | x < 10 -> "< 10" | otherwise -> ""
x2 = if | x < 10 -> "< 10"
        | otherwise -> ""
x3 = if | x < 10 -> "< 10"
   | otherwise -> ""
x4 = if | True -> "yes"
x5 = if | True -> if | False -> 1 | True -> 2

main = print $ x5 == 2

