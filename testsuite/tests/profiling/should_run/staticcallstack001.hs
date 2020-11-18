module Main where

import GHC.Stack.CCS

data D = D Int deriving Show

ff = id (D 5)
{-# NOINLINE ff #-}
{-# NOINLINE qq #-}

qq x = D x

caf = D 5

main = do
  print . tail =<< whereFrom (D 5)
  print . tail =<< whereFrom caf
  print . tail =<< whereFrom (id (D 5))

