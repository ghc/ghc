import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import System.Posix.Signals
import GHC.Stats.Experimental

-- | Test to make sure that we do not falsely detect a deadlock when there is
-- nothing to do except wait on signals. This is to make sure that our prompt
-- system deadlock detection is not too eager.
--
-- We arrange a partial deadlock that can be broken if a signal arrives and
-- the signal handler can unblock things.
--
-- This should /not/ result in any deadlock being detected, and no other odd
-- behaviour, like getting caught in a loop and doing too many idle GCs. So we
-- count and report the number of major GCs.
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

  installHandler userDefinedSignal1
                 (Catch $ do
                    atomically $ writeTVar aDone True
                              >> writeTVar bDone True
                    putStrLn "caught SIGUSR1")
                 Nothing

  _ <- forkIO (blockingThread bDone aDone)
  _ <- forkIO (blockingThread aDone bDone)
  yield

  -- Wait on the deadlocked threads to terminate. We now expect that the threads
  -- that are deadlocked are detected as such and an exception is raised.
  -- Note that if this fails, the test itself will effectively deadlock and
  -- will rely on the test framework's timeout.
  putStrLn "waiting on deadlocked threads..."
  atomically $ do
    status <- mapM readTVar [aDone, bDone]
    guard (or status)

  -- We expect exactly 1 major GC: the one triggered for deadlock detection
  -- where we find no deadlock. We should then sleep without doing more GC.
  -- An easy mistake would be looping: detecting probable deadlock, do idle GC.
  -- See notifyIdleGcDeadlock().
  stats <- getRTSStats
  putStrLn ("terminated normally, major GCs = " ++ show (major_gcs stats))
  -- This test _could_ be sensitive to changes in default heap sizes etc. If
  -- this number ends up as 2 for example, then adjust the RTS opts (in the
  -- makefile) so that the heap is big enough that normally no major GC would
  -- be needed.
