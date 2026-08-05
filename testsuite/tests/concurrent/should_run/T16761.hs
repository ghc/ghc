-- Test that Eq ThreadId is based on thread identity (eq_thread),
-- not on the numeric thread id, which may wrap around (#16761).
module Main (main) where

import Control.Concurrent
import System.Mem (performGC)

main :: IO ()
main = do
  t0 <- myThreadId
  print (t0 == t0)

  mv <- newEmptyMVar
  _ <- forkIO (myThreadId >>= putMVar mv)
  tChild <- takeMVar mv
  print (t0 == tChild)
  print (tChild == tChild)

  -- Equality must be stable even after the GC moves the TSOs.
  performGC
  print (t0 == t0)

  -- Ord must agree with Eq.
  print (compare t0 tChild /= EQ)
  print (compare t0 t0 == EQ)
