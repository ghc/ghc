{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

-- | Synchronising variables whose contents are always in weak head normal form
-- (WHNF).
--
-- This module is a thin wrapper around "Control.Concurrent.MVar": each
-- operation has the same semantics as its counterpart there, except that the
-- value put in the variable is forced to WHNF first.
-- "Control.Concurrent.MVar" is the reference documentation for these
-- operations — for the fairness and ordering guarantees, for the atomicity
-- caveats of the \"bigger\" functions, and for worked examples; only the
-- differences are repeated here.
--
-- Note that WHNF is not deep: putting @Just <thunk>@ in an
-- @'MVar' (Maybe a)@ still puts a thunk inside the @Just@.

module Control.Concurrent.MVar.Strict
  ( MVar

    -- * Operations
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , tryReadMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
  , mkWeakMVar
  ) where

import Data.Functor
import GHC.Base
import GHC.Weak (Weak)
import qualified Control.Concurrent.MVar as Lazy

-- | A synchronising variable, wrapping @MVar@ from "Control.Concurrent.MVar".
-- It can be thought of as a box, which may be empty or full.
--
-- The value it holds is always in WHNF: every operation of this module forces
-- the value before it is put in the box.
newtype MVar a = MVar (Lazy.MVar a)
  deriving stock (Eq)

-- | Create an 'MVar' which is initially empty.
--
-- See @newEmptyMVar@ in "Control.Concurrent.MVar" for the full semantics.
newEmptyMVar :: IO (MVar a)
newEmptyMVar = MVar <$> Lazy.newEmptyMVar

-- | Create an 'MVar' which contains the supplied value.
--
-- See @newMVar@ in "Control.Concurrent.MVar" for the full semantics.
--
-- The initial value is forced to WHNF before it is stored.
newMVar :: a -> IO (MVar a)
newMVar a = fmap MVar . Lazy.newMVar =<< (pure $! a)

-- | Return the contents of the 'MVar', leaving it empty. Blocks while the
-- 'MVar' is empty.
--
-- See @takeMVar@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for the single-wakeup and FIFO guarantees.
takeMVar :: MVar a -> IO a
takeMVar (MVar var) = Lazy.takeMVar var

-- | Atomically read the contents of an 'MVar'. Blocks while the 'MVar' is
-- empty.
--
-- See @readMVar@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for the multiple-wakeup guarantee.
readMVar :: MVar a -> IO a
readMVar (MVar var) = Lazy.readMVar var

-- | Put a value into an 'MVar'. Blocks while the 'MVar' is full.
--
-- See @putMVar@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for the single-wakeup and FIFO guarantees.
--
-- The value is forced to WHNF before it is put.
putMVar :: MVar a -> a -> IO ()
putMVar (MVar var) a = Lazy.putMVar var =<< (pure $! a)

-- | A non-blocking version of 'takeMVar'.
--
-- See @tryTakeMVar@ in "Control.Concurrent.MVar" for the full semantics.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar var) = Lazy.tryTakeMVar var

-- | A non-blocking version of 'putMVar'.
--
-- See @tryPutMVar@ in "Control.Concurrent.MVar" for the full semantics.
--
-- The value is forced to WHNF before it is put. Note that it is forced whether
-- or not the 'MVar' turns out to be empty.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar var) a = Lazy.tryPutMVar var =<< (pure $! a)

-- | Take a value from an 'MVar', put a new value into the 'MVar' and return the
-- value taken.
--
-- See @swapMVar@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for its atomicity caveat.
--
-- The new value is forced to WHNF before it is put.
swapMVar :: MVar a -> a -> IO a
swapMVar (MVar var) a = Lazy.swapMVar var =<< (pure $! a)

-- | A non-blocking version of 'readMVar'.
--
-- See @tryReadMVar@ in "Control.Concurrent.MVar" for the full semantics.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar var) = Lazy.tryReadMVar var

-- | Check whether a given 'MVar' is empty.
--
-- See @isEmptyMVar@ in "Control.Concurrent.MVar" for the full semantics. The
-- result is only a snapshot, so prefer 'tryTakeMVar' where possible.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar var) = Lazy.isEmptyMVar var

-- | An exception-safe wrapper for operating on the contents of an 'MVar'.
--
-- See @withMVar@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for its atomicity caveat.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar (MVar var) action = Lazy.withMVar var action
{-# INLINE withMVar #-}

-- | Like 'withMVar', but the @IO@ action in the second argument is executed
-- with asynchronous exceptions masked.
--
-- See @withMVarMasked@ in "Control.Concurrent.MVar" for the full semantics.
withMVarMasked :: MVar a -> (a -> IO b) -> IO b
withMVarMasked (MVar var) action = Lazy.withMVarMasked var action
{-# INLINE withMVarMasked #-}

-- | An exception-safe wrapper for modifying the contents of an 'MVar'.
--
-- See @modifyMVar_@ in "Control.Concurrent.MVar" for the full semantics, in
-- particular for its atomicity caveat.
--
-- The new value is forced to WHNF before it is put back.
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ (MVar var) action = Lazy.modifyMVar_ var $ \a0 -> do
  a <- action a0
  pure $! a
{-# INLINE modifyMVar_ #-}

-- | A slight variation on 'modifyMVar_' that allows a value to be returned
-- (@b@) in addition to the modified value of the 'MVar'.
--
-- See @modifyMVar@ in "Control.Concurrent.MVar" for the full semantics.
--
-- The new value is forced to WHNF before it is put back. The returned value is
-- not forced.
modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (MVar var) action = Lazy.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> (pure $! a)
{-# INLINE modifyMVar #-}

-- | Like 'modifyMVar_', but the @IO@ action in the second argument is executed
-- with asynchronous exceptions masked.
--
-- See @modifyMVarMasked_@ in "Control.Concurrent.MVar" for the full semantics.
--
-- The new value is forced to WHNF before it is put back.
modifyMVarMasked_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ (MVar var) action = Lazy.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  pure $! a
{-# INLINE modifyMVarMasked_ #-}

-- | Like 'modifyMVar', but the @IO@ action in the second argument is executed
-- with asynchronous exceptions masked.
--
-- See @modifyMVarMasked@ in "Control.Concurrent.MVar" for the full semantics.
--
-- The new value is forced to WHNF before it is put back. The returned value is
-- not forced.
modifyMVarMasked :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVarMasked (MVar var) action = Lazy.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> (pure $! a)
{-# INLINE modifyMVarMasked #-}

-- | Make a 'Weak' pointer to an 'MVar', using the second argument as a
-- finalizer to run when the 'MVar' is garbage-collected.
--
-- See @mkWeakMVar@ in "Control.Concurrent.MVar" for the full semantics.
mkWeakMVar :: MVar a -> IO () -> IO (Weak (MVar a))
mkWeakMVar (MVar var) finalizer = coerce <$> Lazy.mkWeakMVar var finalizer
