-- !!! Testing that Show for Addr is OK..
module Main(main) where

import Addr

main :: IO ()
main = do
  print (intToAddr maxBound)
  print (intToAddr minBound)

