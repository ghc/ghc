-- Test for ticket #7936:
-- https://ghc.haskell.org/trac/ghc/ticket/7936
--
-- Used to fail with:
--
-- $ cabal test T7936 --test-options="+RTS -M1M -RTS"
-- T7936: Heap exhausted;

module Main where

import System.Random (newStdGen)
import Control.Monad (replicateM_)

main = replicateM_ 100000 newStdGen
