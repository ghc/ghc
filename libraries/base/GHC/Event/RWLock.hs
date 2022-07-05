{- | Write-biased multi-reader lock
 adapted from https://github.com/Yuras/qase/blob/master/lib/RWLock.hs

 https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock
-}

{-# language NoImplicitPrelude, RecordWildCards #-}

module GHC.Event.RWLock
( RWLock
, new
, rlock
, runlock
, wlock
, wunlock
, AlreadyUnlocked(..)
, withReadLock
, withWriteLock
)
where

import GHC.Base
import GHC.Conc.Sync (TVar, atomically, writeTVar, newTVarIO, readTVar, retry, throwSTM)
import GHC.Enum
import GHC.Exception.Type (Exception)
import GHC.IO (mask, uninterruptibleMask_, onException, bracket)
import GHC.Show

data WriterState
  = NoWriter
  | OneWriter
  | WaitingWriter

-- | Read-write lock
-- This lock is write-biased. This means that once a writer is waiting no
-- reads-locks will be granted until that write-lock is acquired and released.
-- It is not fair for multiple writers:
--  * A second writer waiting on the lock is not expected to proceed before
--    waiting readers.
--  * There is no attempt to give the second and later waiting writers  the lock
--    in the order of their acquires.
data RWLock = RWLock
  { readersVar :: TVar Int
  , writerVar :: TVar WriterState
  }

-- | Create new lock
new :: IO RWLock
new = do
  readersVar <- newTVarIO 0
  writerVar <- newTVarIO NoWriter
  return RWLock{..}

-- | Lock for read
rlock :: RWLock -> IO ()
rlock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    NoWriter -> readTVar readersVar >>= writeTVar readersVar . succ
    _ -> retry

-- | Unlock for read
runlock :: RWLock -> IO ()
runlock RWLock{..} = atomically $ do
  n <- readTVar readersVar
  if (n <= 0) then throwSTM AlreadyUnlocked  else pure ()
  writeTVar readersVar (pred n)

-- | Lock for write
wlock :: RWLock -> IO ()
wlock lock = mask $ \restore -> do
  requestLock lock
  restore (waitLock lock) `onException`
    uninterruptibleMask_ (unrequestLock lock)

-- | Unlock for write
wunlock :: RWLock -> IO ()
wunlock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    OneWriter ->
      writeTVar writerVar NoWriter
    _ -> throwSTM AlreadyUnlocked

-- | Wait until we are the waiting writer.
requestLock :: RWLock -> IO ()
requestLock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    NoWriter -> writeTVar writerVar WaitingWriter
    _ -> retry

-- | Relinquish our position as waiting writer.
-- Precondition: We are the waiting writer
unrequestLock :: RWLock -> IO ()
unrequestLock RWLock{..} = atomically $ writeTVar writerVar NoWriter

-- | Wait for the writer lock
-- Precondition: We are the waiting writer
waitLock :: RWLock -> IO ()
waitLock RWLock{..} = atomically $ do
  n <- readTVar readersVar
  if n == 0 then writeTVar writerVar OneWriter else retry

-- | A bracket for readers
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock rwl action = bracket (wlock rwl) (const $ wunlock rwl) (const action)

-- | A bracket for writers
withReadLock :: RWLock -> IO a -> IO a
withReadLock rwl action = bracket (rlock rwl) (const $ runlock rwl) (const action)

-- | Unlock requested without prior lock
data AlreadyUnlocked = AlreadyUnlocked
  deriving (Show, Eq)

instance Exception AlreadyUnlocked
