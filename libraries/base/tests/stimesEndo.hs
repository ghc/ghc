module Main where

import Data.Semigroup

adder :: Int -> Endo Int
adder n = stimes n (Endo (+ 1))

main :: IO ()
main = print $ map (\n -> appEndo (adder n) 0) [0 .. 5]
