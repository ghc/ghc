module Main where

import GHC.Conc

-- Create a new TVar and never use it
main = do
  putStr "Before\n"
  t <- atomically ( newTVar 42 )
  putStr "After\n"
