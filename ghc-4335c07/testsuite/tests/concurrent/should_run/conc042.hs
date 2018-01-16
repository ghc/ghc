module Main where

import GHC.Conc

-- Create a new TVar and check that it contains the expected value
main = do
  putStr "Before\n"
  t <- atomically ( newTVar 42 )
  r <- atomically ( readTVar t )
  putStr ("After " ++ (show r) ++ "\n")
         
