-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.JobControl
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A job control concurrency abstraction
-----------------------------------------------------------------------------
module Distribution.Client.JobControl (
    JobControl,
    newSerialJobControl,
    newParallelJobControl,
    spawnJob,
    collectJob,
    remainingJobs,
    cancelJobs,

    JobLimit,
    newJobLimit,
    withJobLimit,

    Lock,
    newLock,
    criticalSection
  ) where

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, bracket_, throwIO, try)
import Distribution.Client.Compat.Semaphore


-- | A simple concurrency abstraction. Jobs can be spawned and can complete
-- in any order. This allows both serial and parallel implementations.
--
data JobControl m a = JobControl {
       -- | Add a new job to the pool of jobs
       spawnJob    :: m a -> m (),

       -- | Wait until one job is complete
       collectJob  :: m a,

       -- | Returns True if there are any outstanding jobs
       -- (ie spawned but yet to be collected)
       remainingJobs :: m Bool,

       -- | Try to cancel any outstanding but not-yet-started jobs.
       -- Call 'remainingJobs' after this to find out if any jobs are left
       -- (ie could not be cancelled).
       cancelJobs  :: m ()
     }


-- | Make a 'JobControl' that executes all jobs serially and in order.
-- It only executes jobs on demand when they are collected, not eagerly.
--
-- Cancelling will cancel /all/ jobs that have not been collected yet.
--
newSerialJobControl :: IO (JobControl IO a)
newSerialJobControl = do
    qVar <- newTChanIO
    return JobControl {
      spawnJob      = spawn     qVar,
      collectJob    = collect   qVar,
      remainingJobs = remaining qVar,
      cancelJobs    = cancel    qVar
    }
  where
    spawn :: TChan (IO a) -> IO a -> IO ()
    spawn qVar job = atomically $ writeTChan qVar job

    collect :: TChan (IO a) -> IO a
    collect qVar =
      join $ atomically $ readTChan qVar

    remaining :: TChan (IO a) -> IO Bool
    remaining qVar  = fmap not $ atomically $ isEmptyTChan qVar

    cancel :: TChan (IO a) -> IO ()
    cancel qVar = do
      _ <- atomically $ readAllTChan qVar
      return ()

-- | Make a 'JobControl' that eagerly executes jobs in parallel, with a given
-- maximum degree of parallelism.
--
-- Cancelling will cancel jobs that have not yet begun executing, but jobs
-- that have already been executed or are currently executing cannot be
-- cancelled.
--
newParallelJobControl :: Int -> IO (JobControl IO a)
newParallelJobControl n | n < 1 || n > 1000 =
  error $ "newParallelJobControl: not a sensible number of jobs: " ++ show n
newParallelJobControl maxJobLimit = do
    inqVar   <- newTChanIO
    outqVar  <- newTChanIO
    countVar <- newTVarIO 0
    replicateM_ maxJobLimit $
      forkIO $
        worker inqVar outqVar
    return JobControl {
      spawnJob      = spawn   inqVar  countVar,
      collectJob    = collect outqVar countVar,
      remainingJobs = remaining       countVar,
      cancelJobs    = cancel  inqVar  countVar
    }
  where
    worker ::  TChan (IO a) -> TChan (Either SomeException a) -> IO ()
    worker inqVar outqVar =
      forever $ do
        job <- atomically $ readTChan inqVar
        res <- try job
        atomically $ writeTChan outqVar res

    spawn :: TChan (IO a) -> TVar Int -> IO a -> IO ()
    spawn inqVar countVar job =
      atomically $ do
        modifyTVar' countVar (+1)
        writeTChan inqVar job

    collect :: TChan (Either SomeException a) -> TVar Int -> IO a
    collect outqVar countVar = do
      res <- atomically $ do
        modifyTVar' countVar (subtract 1)
        readTChan outqVar
      either throwIO return res

    remaining :: TVar Int -> IO Bool
    remaining countVar = fmap (/=0) $ atomically $ readTVar countVar

    cancel :: TChan (IO a) -> TVar Int -> IO ()
    cancel inqVar countVar =
      atomically $ do
        xs <- readAllTChan inqVar
        modifyTVar' countVar (subtract (length xs))

readAllTChan :: TChan a -> STM [a]
readAllTChan qvar = go []
  where
    go xs = do
      mx <- tryReadTChan qvar
      case mx of
        Nothing -> return (reverse xs)
        Just x  -> go (x:xs)

-------------------------
-- Job limits and locks
--

data JobLimit = JobLimit QSem

newJobLimit :: Int -> IO JobLimit
newJobLimit n =
  fmap JobLimit (newQSem n)

withJobLimit :: JobLimit -> IO a -> IO a
withJobLimit (JobLimit sem) =
  bracket_ (waitQSem sem) (signalQSem sem)

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

criticalSection :: Lock -> IO a -> IO a
criticalSection (Lock lck) act = bracket_ (takeMVar lck) (putMVar lck ()) act
