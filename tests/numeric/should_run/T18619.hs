module Main where

import Data.Bits

main :: IO ()
main = do
  print $ Data.Bits.shiftL (1 :: Integer) ((-1) :: Int)
