-- Test n+k patterns

{-# LANGUAGE NPlusKPatterns #-}

module Main where

f (n+1) = n

g :: Int -> Int
g (n+4) = n

main = print (f 3)	>>
       print (g 9)
