module Main where

import A
import B

data C = C A
data D = D B

main :: IO ()
main = putStrLn ""
