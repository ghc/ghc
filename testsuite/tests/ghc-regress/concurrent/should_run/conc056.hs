-- Exposed a bug in 6.4.1, fixed in rev. 1.16 of ghc/rts/Exception.cmm

import Network
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception

inc :: TVar Int -> STM ()
inc tv = do
  v <- readTVar tv
  writeTVar tv (v + 1)

bad :: MVar () -> IO ()
bad m = do { connectTo "0.0.0.0" (Service "http"); return () }
	 `finally` putMVar m ()

main :: IO ()
main = do
  tv <- atomically (newTVar 0)
  m <- newEmptyMVar
  forkOS (sequence_ $ repeat $ atomically (inc tv))
  forkOS (bad m)
  takeMVar m
  threadDelay 100000 -- allow time for the exception to be printed

