module Main where

import GHC.Conc

-- Create a new TVar, update it and check that it contains the expected value within
-- the transaction
main = do
  putStr "Before\n"
  t <- atomically ( newTVar 42 )
  r <- atomically ( do writeTVar t 17 
                       readTVar t)
  putStr ("After " ++ (show r) ++ "\n")
         
