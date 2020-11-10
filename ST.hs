module Main where

import GHC.Exts.Heap
import System.Mem

main = foo 100

loop 0 = foo 0
loop n = print ("ITERATION", n) >> foo n >> loop (n-1)

{-# NOINLINE foo #-}
foo 0 = () <$ getStack
foo n = print "x" >> foo (n - 1) >> print "x"

getStack = do
  fs <- getCurrentStackData
  print fs
  getLine
  print (length fs)
  getLine
