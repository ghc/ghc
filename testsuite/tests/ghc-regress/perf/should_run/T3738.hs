
module Main where

import T3738a

{-# INLINE bar #-}
bar :: Int -> [Int]
bar x = map (+ 2) (foo x)

main = print (bar 2 !! 10000)
