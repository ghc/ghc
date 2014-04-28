module Main (main) where

import GHC.Conc

main = do
  setAllocationCounter (10*1024)
  enableAllocationLimit
  print (length [1..])

