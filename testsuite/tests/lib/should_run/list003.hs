-- !!! Test that length doesn't give a stack overflow

module Main (main) where

main :: IO ()
main = print $ length $ filter odd [0 .. 9999999]

