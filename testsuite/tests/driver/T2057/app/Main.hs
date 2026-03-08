module Main where

import B

main :: IO ()
main = case g MkT of
  MkT -> print ()
