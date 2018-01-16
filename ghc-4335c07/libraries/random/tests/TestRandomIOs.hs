-- Test for ticket #4218 (TestRandomIOs):
-- https://ghc.haskell.org/trac/ghc/ticket/4218
--
-- Used to fail with:
--
-- $ cabal test TestRandomIOs --test-options="+RTS -M1M -RTS"
-- TestRandomIOs: Heap exhausted;

module Main where

import Control.Monad (replicateM)
import System.Random (randomIO)

-- Build a list of 5000 random ints in memory (IO Monad is strict), and print
-- the last one.
-- Should use less than 1Mb of heap space, or we are generating a list of
-- unevaluated thunks.
main = do
    rs <- replicateM 5000 randomIO :: IO [Int]
    print $ last rs
