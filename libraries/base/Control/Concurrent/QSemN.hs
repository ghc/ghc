{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.QSemN
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- Quantity semaphores in which each thread may wait for an arbitrary
-- \"amount\".
--
-----------------------------------------------------------------------------

module Control.Concurrent.QSemN
        (  -- * General Quantity Semaphores
          QSemN,        -- abstract
          newQSemN,     -- :: Int   -> IO QSemN
          waitQSemN,    -- :: QSemN -> Int -> IO ()
          signalQSemN   -- :: QSemN -> Int -> IO ()
      ) where

import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar
                          , tryPutMVar, isEmptyMVar)
import Control.Exception
import Control.Monad (when)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | 'QSemN' is a quantity semaphore in which the resource is acquired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSemN` calls.
--
-- The pattern
--
-- >   bracket_ (waitQSemN n) (signalQSemN n) (...)
--
-- is safe; it never loses any of the resource.
--
data QSemN = QSemN !(IORef (Int, [(Int, MVar ())], [(Int, MVar ())]))

-- The semaphore state (i, xs, ys):
--
--   i is the current resource value
--
--   (xs,ys) is the queue of blocked threads, where the queue is
--           given by xs ++ reverse ys.  We can enqueue new blocked threads
--           by consing onto ys, and dequeue by removing from the head of xs.
--
-- A blocked thread is represented by an empty (MVar ()).  To unblock
-- the thread, we put () into the MVar.
--
-- A thread can dequeue itself by also putting () into the MVar, which
-- it must do if it receives an exception while blocked in waitQSemN.
-- This means that when unblocking a thread in signalQSemN we must
-- first check whether the MVar is already full.

-- |Build a new 'QSemN' with a supplied initial quantity.
--  The initial quantity must be at least 0.
newQSemN :: Int -> IO QSemN
newQSemN initial
  | initial < 0 = fail "newQSemN: Initial quantity must be non-negative"
  | otherwise   = do
      sem <- newIORef (initial, [], [])
      return (QSemN sem)

-- An unboxed version of Maybe (MVar a)
data MaybeMV a = JustMV !(MVar a) | NothingMV

-- |Wait for the specified quantity to become available
waitQSemN :: QSemN -> Int -> IO ()
-- We need to mask here. Once we've enqueued our MVar, we need
-- to be sure to wait for it. Otherwise, we could lose our
-- allocated resource.
waitQSemN qs@(QSemN m) sz = mask_ $ do
    -- unsafePerformIO and not unsafeDupablePerformIO. We must
    -- be sure to wait on the same MVar that gets enqueued.
  mmvar <- atomicModifyIORef m $ \ (i,b1,b2) -> unsafePerformIO $ do
    let z = i-sz
    if z < 0
      then do
        b <- newEmptyMVar
        return ((i, b1, (sz,b):b2), JustMV b)
      else return ((z, b1, b2), NothingMV)

  -- Note: this case match actually allocates the MVar if necessary.
  case mmvar of
    NothingMV -> return ()
    JustMV b -> wait b
  where
    wait :: MVar () -> IO ()
    wait b =
      takeMVar b `onException` do
        already_filled <- not <$> tryPutMVar b ()
        when already_filled $ signalQSemN qs sz

-- |Signal that a given quantity is now available from the 'QSemN'.
signalQSemN :: QSemN -> Int -> IO ()
-- We don't need to mask here because we should *already* be masked
-- here (e.g., by bracket). Indeed, if we're not already masked,
-- it's too late to do so.
--
-- What if the unsafePerformIO thunk is forced in another thread,
-- and receives an asynchronous exception? That shouldn't be a
-- problem: when we force it ourselves, presumably masked, we
-- will resume its execution.
signalQSemN (QSemN m) sz0 = do
    -- unsafePerformIO and not unsafeDupablePerformIO. We must not
    -- wake up more threads than we're supposed to.
  unit <- atomicModifyIORef m $ \(i,a1,a2) ->
            unsafePerformIO (loop (sz0 + i) a1 a2)

  -- Forcing this will actually wake the necessary threads.
  evaluate unit
 where
   loop 0  bs b2 = return ((0,  bs, b2), ())
   loop sz [] [] = return ((sz, [], []), ())
   loop sz [] b2 = loop sz (reverse b2) []
   loop sz ((j,b):bs) b2
     | j > sz = do
       r <- isEmptyMVar b
       if r then return ((sz, (j,b):bs, b2), ())
            else loop sz bs b2
     | otherwise = do
       r <- tryPutMVar b ()
       if r then loop (sz-j) bs b2
            else loop sz bs b2
