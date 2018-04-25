{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TBQueue
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- 'TBQueue' is a bounded version of 'TQueue'. The queue has a maximum
-- capacity set when it is created.  If the queue already contains the
-- maximum number of elements, then 'writeTBQueue' blocks until an
-- element is removed from the queue.
--
-- The implementation is based on the traditional purely-functional
-- queue representation that uses two lists to obtain amortised /O(1)/
-- enqueue and dequeue operations.
--
-- @since 2.4
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TBQueue (
        -- * TBQueue
        TBQueue,
        newTBQueue,
        newTBQueueIO,
        readTBQueue,
        tryReadTBQueue,
        peekTBQueue,
        tryPeekTBQueue,
        writeTBQueue,
        unGetTBQueue,
        isEmptyTBQueue,
        isFullTBQueue,
  ) where

import Data.Typeable
import GHC.Conc

#define _UPK_(x) {-# UNPACK #-} !(x)

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
--
-- @since 2.4
data TBQueue a
   = TBQueue _UPK_(TVar Int)  -- CR: read capacity
             _UPK_(TVar [a])  -- R:  elements waiting to be read
             _UPK_(TVar Int)  -- CW: write capacity
             _UPK_(TVar [a])  -- W:  elements written (head is most recent)
  deriving Typeable

instance Eq (TBQueue a) where
  TBQueue a _ _ _ == TBQueue b _ _ _ = a == b

-- Total channel capacity remaining is CR + CW. Reads only need to
-- access CR, writes usually need to access only CW but sometimes need
-- CR.  So in the common case we avoid contention between CR and CW.
--
--   - when removing an element from R:
--     CR := CR + 1
--
--   - when adding an element to W:
--     if CW is non-zero
--         then CW := CW - 1
--         then if CR is non-zero
--                 then CW := CR - 1; CR := 0
--                 else **FULL**

-- |Build and returns a new instance of 'TBQueue'
newTBQueue :: Int   -- ^ maximum number of elements the queue can hold
           -> STM (TBQueue a)
newTBQueue size = do
  read  <- newTVar []
  write <- newTVar []
  rsize <- newTVar 0
  wsize <- newTVar size
  return (TBQueue rsize read wsize write)

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = do
  read  <- newTVarIO []
  write <- newTVarIO []
  rsize <- newTVarIO 0
  wsize <- newTVarIO size
  return (TBQueue rsize read wsize write)

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue rsize _read wsize write) a = do
  w <- readTVar wsize
  if (w /= 0)
     then do writeTVar wsize $! w - 1
     else do
          r <- readTVar rsize
          if (r /= 0)
             then do writeTVar rsize 0
                     writeTVar wsize $! r - 1
             else retry
  listend <- readTVar write
  writeTVar write (a:listend)

-- |Read the next value from the 'TBQueue'.
readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue rsize read _wsize write) = do
  xs <- readTVar read
  r <- readTVar rsize
  writeTVar rsize $! r + 1
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return x
    [] -> do
      ys <- readTVar write
      case ys of
        [] -> retry
        _  -> do
          let (z:zs) = reverse ys -- NB. lazy: we want the transaction to be
                                  -- short, otherwise it will conflict
          writeTVar write []
          writeTVar read zs
          return z

-- | A version of 'readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTBQueue :: TBQueue a -> STM (Maybe a)
tryReadTBQueue c = fmap Just (readTBQueue c) `orElse` return Nothing

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM a
peekTBQueue c = do
  x <- readTBQueue c
  unGetTBQueue c x
  return x

-- | A version of 'peekTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTBQueue :: TBQueue a -> STM (Maybe a)
tryPeekTBQueue c = do
  m <- tryReadTBQueue c
  case m of
    Nothing -> return Nothing
    Just x  -> do
      unGetTBQueue c x
      return m

-- |Put a data item back onto a channel, where it will be the next item read.
-- Blocks if the queue is full.
unGetTBQueue :: TBQueue a -> a -> STM ()
unGetTBQueue (TBQueue rsize read wsize _write) a = do
  r <- readTVar rsize
  if (r > 0)
     then do writeTVar rsize $! r - 1
     else do
          w <- readTVar wsize
          if (w > 0)
             then writeTVar wsize $! w - 1
             else retry
  xs <- readTVar read
  writeTVar read (a:xs)

-- |Returns 'True' if the supplied 'TBQueue' is empty.
isEmptyTBQueue :: TBQueue a -> STM Bool
isEmptyTBQueue (TBQueue _rsize read _wsize write) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

-- |Returns 'True' if the supplied 'TBQueue' is full.
--
-- @since 2.4.3
isFullTBQueue :: TBQueue a -> STM Bool
isFullTBQueue (TBQueue rsize _read wsize _write) = do
  w <- readTVar wsize
  if (w > 0)
     then return False
     else do
         r <- readTVar rsize
         if (r > 0)
            then return False
            else return True
