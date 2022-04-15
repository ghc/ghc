module Main where

import GHC.Stack.CloneStack
import GHC.Exts.DecodeStack

main :: IO ()
main = do
  stack <- cloneMyStack
  res <- decodeStack stack
  print res
  return ()
