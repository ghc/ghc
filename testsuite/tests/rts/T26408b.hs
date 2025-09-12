import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import System.Posix.Signals

-- | Test to make sure that we can detect a /system deadlock/ promptly which
-- means without relying on idle GC. To check this we have to turn off idle GC,
-- (+RTS -I0) which is what normally triggers deadlock detection.
--
-- This is only expected to work in the non-threaded RTS. In principle this
-- test could also work with the threaded RTS when using only a single
-- capability, if the I\/O managers can support it. Currently that is not the
-- case.
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

  -- Wait on the two deadlocked threads to terminate. This is now a system
  -- deadlock, involving all three threads (the main thread and the other two).
  --
  -- We now expect that the threads that are deadlocked are detected as such
  -- and an exception is thrown to some or all of them.
  --
  -- Note that if this fails, the test itself will effectively deadlock and
  -- will rely on the test framework's timeout.
  atomically $ do
    status <- mapM readTVar [aDone, bDone]
    guard (or status)
