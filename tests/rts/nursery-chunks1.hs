-- Test for a bug that provoked the following assertion failure:
-- nursery-chunks1: internal error: ASSERTION FAILED: file rts/sm/Sanity.c, line 903
module Main (main) where

import Control.Monad
import System.Environment
import GHC.Conc

main = do
  [n,m] <- fmap read <$> getArgs
  forM_ [1..n] $ \n' ->
    when (sum [1.. m::Integer] > 0) $ setNumCapabilities (fromIntegral n')
