module Main where

import GHC.Conc

-- Create a new TVar, update it and check that it contains the expected value after the 
-- transaction
main = do
  putStr "Before\n"
  t <- atomically ( newTVar 42 )
  atomically ( writeTVar t 17 )
  r <- atomically ( readTVar t )
  putStr ("After " ++ (show r) ++ "\n")
         
