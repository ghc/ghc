module Main(main) where

-- A program posted by Paul Keir on haskell-cafe (19/12/2008).  Shows
-- up bad behaviour in the parallel GC.

import Control.Concurrent
import GHC.Conc

heavytask :: MVar Integer -> Integer -> IO ()
heavytask m n = putMVar m $! (fibs !! 70000)
  where
    fibs = n : (n+1) : zipWith (+) fibs (tail fibs)

-- so now fibs is not globally shared but is used per-heavytask
-- it is also evaluated by heavy task rather than just putting a thunk
-- into the MVar

main = do ms <- sequence $ replicate 8 newEmptyMVar
          sequence_
            [ forkOnIO (fromIntegral n) (heavytask m n)
            | (m, n) <- zip ms [0..] ]
          ms' <- mapM takeMVar ms
          mapM_ print ms'
