{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Concurrent.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- An @'MVar' t@ is a mutable location that is either empty or contains a
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
-- They were introduced in the paper
-- ["Concurrent Haskell"](https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/concurrent-haskell.pdf)
-- by Simon Peyton Jones, Andrew Gordon and Sigbjorn Finne, though
-- some details of their implementation have since then changed (in
-- particular, a put on a full 'MVar' used to error, but now merely
-- blocks.)
--
-- === Applicability
--
-- 'MVar's offer more flexibility than 'Data.IORef.IORef's, but less flexibility
-- than 'GHC.Conc.STM'.  They are appropriate for building synchronization
-- primitives and performing simple inter-thread communication; however
-- they are very simple and susceptible to race conditions, deadlocks or
-- uncaught exceptions.  Do not use them if you need to perform larger
-- atomic operations such as reading from multiple variables: use 'GHC.Conc.STM'
-- instead.
--
-- In particular, the "bigger" functions in this module ('swapMVar',
-- 'withMVar', 'modifyMVar_' and 'modifyMVar') are simply
-- the composition of a 'takeMVar' followed by a 'putMVar' with
-- exception safety.
-- These have atomicity guarantees only if all other threads
-- perform a 'takeMVar' before a 'putMVar' as well;  otherwise, they may
-- block.
--
-- === Fairness
--
-- No thread can be blocked indefinitely on an 'MVar' unless another
-- thread holds that 'MVar' indefinitely.  One usual implementation of
-- this fairness guarantee is that threads blocked on an 'MVar' are
-- served in a first-in-first-out fashion (this is what GHC does),
-- but this is not guaranteed in the semantics.
--
-- === Gotchas
--
-- Like many other Haskell data structures, 'MVar's are lazy.  This
-- means that if you place an expensive unevaluated thunk inside an
-- 'MVar', it will be evaluated by the thread that consumes it, not the
-- thread that produced it.  Be sure to 'evaluate' values to be placed
-- in an 'MVar' to the appropriate normal form, or utilize a strict
-- @MVar@ provided by the [strict-concurrency](https://hackage.haskell.org/package/strict-concurrency) package.
--
-- === Ordering
--
-- 'MVar' operations are always observed to take place in the order
-- they are written in the program, regardless of the memory model of
-- the underlying machine.  This is in contrast to 'Data.IORef.IORef' operations
-- which may appear out-of-order to another thread in some cases.
--
-- === Example
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
-- data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())
--
-- newSkipChan :: IO (SkipChan a)
-- newSkipChan = do
--     sem <- newEmptyMVar
--     main <- newMVar (undefined, [sem])
--     return (SkipChan main sem)
--
-- putSkipChan :: SkipChan a -> a -> IO ()
-- putSkipChan (SkipChan main _) v = do
--     (_, sems) <- takeMVar main
--     putMVar main (v, [])
--     mapM_ (\\sem -> putMVar sem ()) sems
--
-- getSkipChan :: SkipChan a -> IO a
-- getSkipChan (SkipChan main sem) = do
--     takeMVar sem
--     (v, sems) <- takeMVar main
--     putMVar main (v, sem : sems)
--     return v
--
-- dupSkipChan :: SkipChan a -> IO (SkipChan a)
-- dupSkipChan (SkipChan main _) = do
--     sem <- newEmptyMVar
--     (v, sems) <- takeMVar main
--     putMVar main (v, sem : sems)
--     return (SkipChan main sem)
-- @
--
-- This example was adapted from the original Concurrent Haskell paper.
-- For more examples of 'MVar's being used to build higher-level
-- synchronization primitives, see 'Control.Concurrent.Chan' and
-- 'Control.Concurrent.QSem'.
--

module Control.Concurrent.MVar
    (-- *  @MVar@s
     MVar,
     newEmptyMVar,
     newMVar,
     takeMVar,
     putMVar,
     readMVar,
     swapMVar,
     tryTakeMVar,
     tryPutMVar,
     isEmptyMVar,
     withMVar,
     withMVarMasked,
     modifyMVar_,
     modifyMVar,
     modifyMVarMasked_,
     modifyMVarMasked,
     tryReadMVar,
     mkWeakMVar,
     addMVarFinalizer
     ) where

import GHC.Internal.Control.Concurrent.MVar
