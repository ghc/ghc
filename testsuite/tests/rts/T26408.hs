import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

-- | Test to make sure that deadlock detection works even when there are other
-- unrelated threads that are blocked on I\/O or timeouts.
-- Historically however this did affect things in the non-threaded RTS which
-- would only do deadlock detection if there were no runnable threads /and/
-- no pending I\/O. See <https://gitlab.haskell.org/ghc/ghc/-/issues/26408>
main :: IO ()
main = do
  
  -- Set up two threads that are deadlocked on each other
  aDone <- newTVarIO False
  bDone <- newTVarIO False
  let blockingThread theirDone ourDone =
        atomically $ do
          done <- readTVar theirDone
          guard done
          writeTVar ourDone True
  _ <- forkIO (blockingThread bDone aDone)
  _ <- forkIO (blockingThread aDone bDone)

  -- Set up another thread that is blocked on a long timeout.
  --
  -- We use a timeout rather than I/O as it's more portable, whereas I/O waits
  -- are different between posix and windows I/O managers.
  --
  -- One gotcha is that when the timeout completes then the deadlock will be
  -- detected again (since the bug is about I/O or timeouts masking deadlock
  -- detection). So for a reliable test the timeout used here must be longer
  -- than the test framework's own timeout. So we use maxBound, and we adjust
  -- the test framework's timeout to be short (see run_timeout_multiplier).
  _ <- forkIO (threadDelay maxBound)

  -- Wait on the deadlocked threads to terminate. We now expect that the threads
  -- that are deadlocked are detected as such and an exception is raised.
  -- Note that if this fails, the test itself will effectively deadlock and
  -- will rely on the test framework's timeout.
  atomically $ do
    status <- mapM readTVar [aDone, bDone]
    guard (or status)
