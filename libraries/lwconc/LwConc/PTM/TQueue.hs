{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TQueue
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- A 'TQueue' is like a 'TChan', with two important differences:
--
--  * it has faster throughput than both 'TChan' and 'Chan' (although
--    the costs are amortised, so the cost of individual operations
--    can vary a lot).
--
--  * it does /not/ provide equivalents of the 'dupTChan' and
--    'cloneTChan' operations.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
--
-----------------------------------------------------------------------------

module LwConc.PTM.TQueue (
        -- * TQueue
	TQueue,
	newTQueue,
	newTQueueIO,
	readTQueue,
	-- tryReadTQueue,
	peekTQueue,
	-- tryPeekTQueue,
	writeTQueue,
        unGetTQueue,
        isEmptyTQueue,
  ) where

import LwConc.Substrate
import Data.Typeable (Typeable)

-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
data TQueue a = TQueue {-# UNPACK #-} !(PVar [a])
                       {-# UNPACK #-} !(PVar [a])
  deriving Typeable

instance Eq (TQueue a) where
  TQueue a _ == TQueue b _ = a == b

-- |Build and returns a new instance of 'TQueue'
newTQueue :: PTM (TQueue a)
newTQueue = do
  read  <- newPVar []
  write <- newPVar []
  return (TQueue read write)

-- |@IO@ version of 'newTQueue'.  This is useful for creating top-level
-- 'TQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTQueueIO :: IO (TQueue a)
newTQueueIO = do
  read  <- newPVarIO []
  write <- newPVarIO []
  return (TQueue read write)

-- |Write a value to a 'TQueue'.
writeTQueue :: TQueue a -> a -> PTM ()
writeTQueue (TQueue _read write) a = do
  listend <- readPVar write
  writePVar write (a:listend)

-- |Read the next value from the 'TQueue'.
readTQueue :: TQueue a -> PTM a
readTQueue (TQueue read write) = do
  xs <- readPVar read
  case xs of
    (x:xs') -> do writePVar read xs'
                  return x
    [] -> do ys <- readPVar write
             case ys of
               [] -> retry
               _  -> case reverse ys of
                       [] -> error "readTQueue"
                       (z:zs) -> do writePVar write []
                                    writePVar read zs
                                    return z

-- | A version of 'readTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
-- tryReadTQueue :: TQueue a -> PTM (Maybe a)
-- tryReadTQueue c = fmap Just (readTQueue c) `orElse` return Nothing

-- | Get the next value from the @TQueue@ without removing it,
-- retrying if the channel is empty.
peekTQueue :: TQueue a -> PTM a
peekTQueue c = do
  x <- readTQueue c
  unGetTQueue c x
  return x

-- | A version of 'peekTQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
-- tryPeekTQueue :: TQueue a -> PTM (Maybe a)
-- tryPeekTQueue c = do
--   m <- tryReadTQueue c
--  case m of
--    Nothing -> return Nothing
--    Just x  -> do
--      unGetTQueue c x
--      return m

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTQueue :: TQueue a -> a -> PTM ()
unGetTQueue (TQueue read _write) a = do
  xs <- readPVar read
  writePVar read (a:xs)

-- |Returns 'True' if the supplied 'TQueue' is empty.
isEmptyTQueue :: TQueue a -> PTM Bool
isEmptyTQueue (TQueue read write) = do
  xs <- readPVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readPVar write
             case ys of
               [] -> return True
               _  -> return False
