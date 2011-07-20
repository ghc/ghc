import Control.Concurrent
import Control.Exception
import Data.Array
import System.Random
import System.Environment
import Control.Monad
import GHC.Conc

-- A fiendish throwTo test.  A bunch of threads take random MVars from
-- a shared array; if the MVar has Nothing in it, replace it with Just
-- of the current thread's ThreadId.  If the MVar has another ThreadId
-- in it, then killThread that thread, and replace it with the current
-- thread's ThreadId.  We keep going until only one thread is left
-- standing.
--
-- On multiple CPUs this should give throwTo a good workout.
--
main = do
  [m, t] <- fmap (fmap read) getArgs
  ms <- replicateM m $ newMVar Nothing
  let arr = listArray (1,m) ms
  dead <- newTVarIO 0
  ts <- replicateM t $ forkIO (thread m arr `onException` 
                                      (atomically $ do d <- readTVar dead
                                                       writeTVar dead $! d+1))
  atomically $ do
    d <- readTVar dead
    when (d < t-1) $ retry

thread m arr = do
  x <- randomIO
  id <- myThreadId
  modifyMVar_ (arr ! ((x `mod` m) + 1)) $ \b ->
    case b of
      Nothing    -> return (Just id)
      Just other -> do when (other /= id) $ killThread other
                       return (Just id)
  thread m arr
