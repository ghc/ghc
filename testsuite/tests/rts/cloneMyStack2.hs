{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Stack.CloneStack

main = foo 100

{-# NOINLINE foo #-}
foo 0 = () <$ getStack
foo n = print "x" >> foo (n - 1) >> print "x"

-- This shouldn't segfault
getStack = do
  !s <- cloneMyStack
  return ()
