module Main where

import Data.List (iterate')

main :: IO ()
main = print $ lookup (2^20) $ iterate' (\(!k,!v) -> (k + 1, v + 2)) (0 :: Int, 0 :: Int)
