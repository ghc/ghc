import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

-- | Test to make sure that we can detect a /partial deadlock/ that is /not/
-- also a /system deadlock/: that is we can detect a set of deadlocked threads
-- even when there are other unrelated threads that are waiting on I\/O or
-- timeouts.
--
-- Historically this did not work in the non-threaded RTS which would only do
-- deadlock detection if there was a system deadlock: no runnable threads /and/
-- no pending I\/O or timers.
--
-- See Note [Deadlock detection]
--
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

  -- Wait on the two deadlocked threads to terminate. This is now a partial
  -- deadlock but not a system deadlock (the main thread and the first two
  -- threads are deadlocked, but the one waiting on the timeout is ok).
  --
  -- We now expect that the threads that are deadlocked are detected as such
  -- and an exception is thrown to some or all of them.
  --
  -- Note that if this fails, the test itself will effectively deadlock and
  -- will rely on the test framework's timeout.
  atomically $ do
    status <- mapM readTVar [aDone, bDone]
    guard (or status)
