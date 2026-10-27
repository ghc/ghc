module Main where

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g y = y * 2

main :: IO ()
main = print (f 3)
