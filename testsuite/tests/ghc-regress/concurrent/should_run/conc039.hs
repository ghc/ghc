{-# OPTIONS -fffi #-}

import Foreign
import System.Mem
import Control.Concurrent

foreign export ccall "performGC_" performGC' :: IO ()
performGC' = do yield; performGC

foreign import ccall "performGC_" f :: IO ()

main = do
  forkIO f
  yield

-- This tests for a bug in the garbage collector, whereby a main
-- thread that has completed may be GC'd before its return value is
-- propagated back to the caller of rts_evalIO().
--
-- The sequence we hope to create is:
--	- main thread (1) forks off thread (2)
--	- thread (2) invokes new main thread (3) via a 'safe' ccall
--	- thread (3) yields to thread (1)
--	- thread (1) completes, but cannot return yet because (3)
--	  is the current main thread (unless we
--	  are in SMP or RTS_SUPPORTS_THREADS mode)
--	- thread (3) invokes a GC
--	- thread (1) is GC'd, unless we're careful!
