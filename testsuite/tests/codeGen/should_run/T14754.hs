module Main where

import Debug.Trace

main :: IO ()
main = print (alg 3 1)

alg :: Word -> Word -> Word
alg a b
  | traceShow (a, b) False = undefined
  | c < b = alg b c
  | c > b = alg c b
  | otherwise = c
  where
    c = a - b
