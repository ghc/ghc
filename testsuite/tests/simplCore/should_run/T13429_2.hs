-- This one come from lehins, between comment:22 and 23 of #13429
module Main where

import T13429_2a as Array

arr2 :: Array D Int Int -> Array D Int Int
arr2 arr = Array.map (*2) arr

main :: IO ()
main = print $ arr2 $ makeArray 1600 id
