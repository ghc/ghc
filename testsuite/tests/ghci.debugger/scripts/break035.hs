module Main where

import System.Timeout

f :: Int -> Int
f x = x + 1

main :: IO ()
main = timeout 100000 (print (f 1)) >>= print
