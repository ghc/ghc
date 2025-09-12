import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.Mem.Experimental (performDeadlockDetection)

-- | Test to make sure that we can detect a partial deadlock when idle GC is
-- turned off, and we manually invoke performDeadlockDetection.
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

  -- Arrange for another thread to wait a bit (longer than the default 300ms
  -- idle GC timeout, just to be sure) and then manually invoke deadlock
  -- detection.
  _ <- forkIO (threadDelay 500_000 >> performDeadlockDetection)

  -- And another thread to ensure that we don't have a system deadlock once
  -- the thread above terminates. Otheriwse we might get false test passes
  -- even if performDeadlockDetection were a no-op because when the thread
  -- above terminates then (if not for this one) all threads would be
  -- deadlocked, and system deadlock detection would kick in and detect the
  -- deadlock instead (defeating the point of the test).
  _ <- forkIO (threadDelay maxBound)

  -- Wait on the two deadlocked threads to terminate. This is now a partial
  -- deadlock but not a system deadlock (the main thread and the first two
  -- threads are deadlocked, but the one waiting on the timeout is ok).
  --
  -- When the manually invoked deadlock detection runs (above) we expect that
  -- the threads that are deadlocked are detected as such, and that an
  -- exception will be thrown to all of them.
  --
  -- Note that if this fails, the test itself will effectively deadlock and
  -- will rely on the test framework's timeout.
  atomically $ do
    status <- mapM readTVar [aDone, bDone]
    guard (or status)
