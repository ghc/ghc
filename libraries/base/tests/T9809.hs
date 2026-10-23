-- #9809: when many threads registered timeouts at the same time, the
-- TimerManager published unevaluated timeout queues with atomicModifyIORef'.
-- The threads then built long chains of thunks and blocked on each other's
-- black holes, and the heap grew to hundreds of megabytes. all.T runs this
-- test with a heap limit that the old code exceeded.

import Control.Concurrent
import Control.Monad
import Data.IORef
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, registerDelay, retry, writeTVar)

main :: IO ()
main = do
    let n = 100000
    ref <- newIORef []
    registered <- newTVarIO (0 :: Int)
    replicateM_ n . forkIO $ do
        tv <- registerDelay 10
        atomicModifyIORef' ref (\tvs -> (tv : tvs, ()))
        atomically $ readTVar registered >>= writeTVar registered . (+ 1)
    atomically $ do
        r <- readTVar registered
        when (r /= n) retry
    readIORef ref >>= mapM_ waitFor

-- | Block until a delay has fired, so that the test also checks that the
-- TimerManager fires every timeout.
waitFor :: TVar Bool -> IO ()
waitFor tv = atomically $ readTVar tv >>= \fired -> unless fired retry
