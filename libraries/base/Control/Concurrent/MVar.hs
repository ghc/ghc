{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, UnboxedTuples, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- An @'MVar' t@ is mutable location that is either empty or contains a
-- value of type @t@.  It has two fundamental operations: 'putMVar'
-- which fills an 'MVar' if it is empty and blocks otherwise, and
-- 'takeMVar' which empties an 'MVar' if it is full and blocks
-- otherwise.  They can be used in multiple different ways:
--
--   1. As synchronized mutable variables,
--
--   2. As channels, with 'takeMVar' and 'putMVar' as receive and send, and
--
--   3. As a binary semaphore @'MVar' ()@, with 'takeMVar' and 'putMVar' as
--      wait and signal.
--
-- They were introduced in the paper "Concurrent Haskell" by Simon
-- Peyton Jones, Andrew Gordon and Sigbjorn Finne, though some details
-- of their implementation have since then changed (in particular, a
-- put on a full MVar used to error, but now merely blocks.)
--
-- * Applicability
--
-- 'MVar's offer more flexibility than 'IORef's, but less flexibility
-- than 'STM'.  They are appropriate for building synchronization
-- primitives and performing simple interthread communication; however
-- they are very simple and susceptible to race conditions, deadlocks or
-- uncaught exceptions.  Do not use them if you need perform larger
-- atomic operations such as reading from multiple variables: use 'STM'
-- instead.
--
-- In particular, the "bigger" functions in this module ('readMVar',
-- 'swapMVar', 'withMVar', 'modifyMVar_' and 'modifyMVar') are simply
-- the composition of a 'takeMVar' followed by a 'putMVar' with
-- exception safety.
-- These only have atomicity guarantees if all other threads
-- perform a 'takeMVar' before a 'putMVar' as well;  otherwise, they may
-- block.
--
-- * Fairness
--
-- No thread can be blocked indefinitely on an 'MVar' unless another
-- thread holds that 'MVar' indefinitely.  One usual implementation of
-- this fairness guarantee is that threads blocked on an 'MVar' are
-- served in a first-in-first-out fashion, but this is not guaranteed
-- in the semantics.
--
-- * Gotchas
--
-- Like many other Haskell data structures, 'MVar's are lazy.  This
-- means that if you place an expensive unevaluated thunk inside an
-- 'MVar', it will be evaluated by the thread that consumes it, not the
-- thread that produced it.  Be sure to 'evaluate' values to be placed
-- in an 'MVar' to the appropriate normal form, or utilize a strict
-- MVar provided by the strict-concurrency package.
--
-- * Ordering
--
-- 'MVar' operations are always observed to take place in the order
-- they are written in the program, regardless of the memory model of
-- the underlying machine.  This is in contrast to 'IORef' operations
-- which may appear out-of-order to another thread in some cases.
--
-- * Example
--
-- Consider the following concurrent data structure, a skip channel.
-- This is a channel for an intermittent source of high bandwidth
-- information (for example, mouse movement events.)  Writing to the
-- channel never blocks, and reading from the channel only returns the
-- most recent value, or blocks if there are no new values.  Multiple
-- readers are supported with a @dupSkipChan@ operation.
--
-- A skip channel is a pair of 'MVar's. The first 'MVar' contains the
-- current value, and a list of semaphores that need to be notified
-- when it changes. The second 'MVar' is a semaphore for this particular
-- reader: it is full if there is a value in the channel that this
-- reader has not read yet, and empty otherwise.
--
-- @
--     data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())
--
--     newSkipChan :: IO (SkipChan a)
--     newSkipChan = do
--         sem <- newEmptyMVar
--         main <- newMVar (undefined, [sem])
--         return (SkipChan main sem)
--
--     putSkipChan :: SkipChan a -> a -> IO ()
--     putSkipChan (SkipChan main _) v = do
--         (_, sems) <- takeMVar main
--         putMVar main (v, [])
--         mapM_ (\sem -> putMVar sem ()) sems
--
--     getSkipChan :: SkipChan a -> IO a
--     getSkipChan (SkipChan main sem) = do
--         takeMVar sem
--         (v, sems) <- takeMVar main
--         putMVar main (v, sem:sems)
--         return v
--
--     dupSkipChan :: SkipChan a -> IO (SkipChan a)
--     dupSkipChan (SkipChan main _) = do
--         sem <- newEmptyMVar
--         (v, sems) <- takeMVar main
--         putMVar main (v, sem:sems)
--         return (SkipChan main sem)
-- @
--
-- This example was adapted from the original Concurrent Haskell paper.
-- For more examples of 'MVar's being used to build higher-level
-- synchronization primitives, see 'Control.Concurrent.Chan' and
-- 'Control.Concurrent.QSem'.
--
-----------------------------------------------------------------------------

module Control.Concurrent.MVar
        (
          -- * @MVar@s
          MVar
        , newEmptyMVar
        , newMVar
        , takeMVar
        , putMVar
        , readMVar
        , swapMVar
        , tryTakeMVar
        , tryPutMVar
        , isEmptyMVar
        , withMVar
        , modifyMVar_
        , modifyMVar
        , modifyMVarMasked_
        , modifyMVarMasked
#ifndef __HUGS__
        , atomicReadMVar
        , tryAtomicReadMVar
        , mkWeakMVar
        , addMVarFinalizer
#endif
    ) where

#ifdef __HUGS__
import Hugs.ConcBase ( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
                  tryTakeMVar, tryPutMVar, isEmptyMVar,
                )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.MVar ( MVar(..), newEmptyMVar, newMVar, takeMVar, putMVar,
                  tryTakeMVar, tryPutMVar, isEmptyMVar, atomicReadMVar
                )
import qualified GHC.MVar
import GHC.Weak
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#else
import Prelude
#endif

import Control.Exception.Base

{-|
  This is a combination of 'takeMVar' and 'putMVar'; ie. it takes the value
  from the 'MVar', puts it back, and also returns it.  This function
  is atomic only if there are no other producers (i.e. threads calling
  'putMVar') for this 'MVar'.  Note: a 'tryTakeMVar' may temporarily
  see the 'MVar' as empty while a read is occurring.
-}
readMVar :: MVar a -> IO a
readMVar m =
  mask_ $ do
    a <- takeMVar m
    putMVar m a
    return a

{-|
  Take a value from an 'MVar', put a new value into the 'MVar' and
  return the value taken. This function is atomic only if there are
  no other producers for this 'MVar'.
-}
swapMVar :: MVar a -> a -> IO a
swapMVar mvar new =
  mask_ $ do
    old <- takeMVar mvar
    putMVar mvar new
    return old

{-|
  'withMVar' is an exception-safe wrapper for operating on the contents
  of an 'MVar'.  This operation is exception-safe: it will replace the
  original contents of the 'MVar' if an exception is raised (see
  "Control.Exception").  However, it is only atomic if there are no
  other producers for this 'MVar'.
-}
{-# INLINE withMVar #-}
-- inlining has been reported to have dramatic effects; see
-- http://www.haskell.org//pipermail/haskell/2006-May/017907.html
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io =
  mask $ \restore -> do
    a <- takeMVar m
    b <- restore (io a) `onException` putMVar m a
    putMVar m a
    return b

{-|
  An exception-safe wrapper for modifying the contents of an 'MVar'.
  Like 'withMVar', 'modifyMVar' will replace the original contents of
  the 'MVar' if an exception is raised during the operation.  This
  function is only atomic if there are no other producers for this
  'MVar'.
-}
{-# INLINE modifyMVar_ #-}
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io =
  mask $ \restore -> do
    a  <- takeMVar m
    a' <- restore (io a) `onException` putMVar m a
    putMVar m a'

{-|
  A slight variation on 'modifyMVar_' that allows a value to be
  returned (@b@) in addition to the modified value of the 'MVar'.
-}
{-# INLINE modifyMVar #-}
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io =
  mask $ \restore -> do
    a      <- takeMVar m
    (a',b) <- restore (io a) `onException` putMVar m a
    putMVar m a'
    return b

{-|
  Like 'modifyMVar_', but the @IO@ action in the second argument is executed with
  asynchronous exceptions masked.
-}
{-# INLINE modifyMVarMasked_ #-}
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ m io =
  mask_ $ do
    a  <- takeMVar m
    a' <- io a `onException` putMVar m a
    putMVar m a'

{-|
  Like 'modifyMVar', but the @IO@ action in the second argument is executed with
  asynchronous exceptions masked.
-}
{-# INLINE modifyMVarMasked #-}
modifyMVarMasked :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVarMasked m io =
  mask_ $ do
    a      <- takeMVar m
    (a',b) <- io a `onException` putMVar m a
    putMVar m a'
    return b

{-# DEPRECATED addMVarFinalizer "use mkWeakMVar instead" #-} -- deprecated in 7.6
addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer = GHC.MVar.addMVarFinalizer

-- | Make a 'Weak' pointer to an 'MVar', using the second argument as
-- a finalizer to run when 'MVar' is garbage-collected
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar m@(MVar m#) f = IO $ \s ->
  case mkWeak# m# m f s of (# s1, w #) -> (# s1, Weak w #)
