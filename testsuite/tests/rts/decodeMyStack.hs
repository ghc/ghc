module Main where

import GHC.Stack.CloneStack

main = do
  stack <- cloneMyStack
  ptrs <- decode stack
  print ptrs
