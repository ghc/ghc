module Main (main) where

import GHC.Conc
import Control.Concurrent
import Control.Exception

main = do
  setAllocationCounter (10*1024)
  enableAllocationLimit

  -- alloc limit overflow while masked: should successfully print the
  -- result, and then immediately raise the exception
  r <- mask_ $ try $ print (length [1..100000])

  print (r :: Either SomeException ())
