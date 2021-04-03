module Main where

import GHC.Stack.CCS

data D = D Int deriving Show

ff = id (D 5)
{-# NOINLINE ff #-}
{-# NOINLINE qq #-}

qq x = D x

caf = D 5

main = do
  print =<< whereFrom (D 5)
  print =<< whereFrom caf
  print =<< whereFrom (id (D 5))
