-- Test for ticket #4218 (TestRandomRs):
-- https://ghc.haskell.org/trac/ghc/ticket/4218
--
-- Fixed together with ticket #8704
-- https://ghc.haskell.org/trac/ghc/ticket/8704
-- Commit 4695ffa366f659940369f05e419a4f2249c3a776
--
-- Used to fail with:
--
-- $ cabal test TestRandomRs --test-options="+RTS -M1M -RTS"
-- TestRandomRs: Heap exhausted;

module TestRandomRs where

import Control.Monad (liftM)
import System.Random (randomRs, getStdGen)

-- Return the five-thousandth random number:
-- Should run in constant space (< 1Mb heap).
main :: IO ()
main = do
    n <- (last . take 5000 . randomRs (0, 1000000)) `liftM` getStdGen
    print (n::Integer)
