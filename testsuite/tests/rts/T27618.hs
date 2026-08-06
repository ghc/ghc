{-# LANGUAGE NumericUnderscores #-}

-- Test for a major GC segfault triggered by threadLabel# returning NULL in
-- a GC pointer field on unlabeled threads. Run with -N4 -A32k.

module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import GHC.Conc.Sync (threadLabel)
import System.Mem (performMajorGC)

{-# NOINLINE queryLabel #-}
queryLabel :: ThreadId -> IO (Maybe String)
queryLabel = threadLabel

main :: IO ()
main = do
  stop <- newIORef False
  _ <- forkIO $ forever performMajorGC
  targets <- replicateM 8 $ forkIO $ forever (threadDelay 1_000_000)
  dones <- forM [1 :: Int .. 8] $ \_ -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      let loop = do
            s <- readIORef stop
            unless s $ do
              forM_ targets $ \t -> do
                r <- queryLabel t
                r `seq` pure ()
              loop
      loop
      putMVar done ()
    pure done
  threadDelay 2_000_000
  writeIORef stop True
  mapM_ takeMVar dones
  putStrLn "done"
