{-# OPTIONS_GHC -O #-}
module Main where

import Numeric.Natural

i :: Int
i = -10

main :: IO ()
main = let n :: Natural
           n = fromIntegral i
        in print n
