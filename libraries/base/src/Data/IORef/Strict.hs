{-# LANGUAGE DerivingStrategies #-}

-- | Mutable references in the IO monad whose contents are always in weak head
-- normal form (WHNF).
--
-- This module is a thin wrapper around "Data.IORef": each operation has the
-- same semantics as its counterpart there, except that the value put in the
-- reference is forced to WHNF first. "Data.IORef" is the reference
-- documentation for these operations; only the differences are repeated here.
--
-- Note that WHNF is not deep: storing @Just <thunk>@ in an
-- @'IORef' (Maybe a)@ still stores a thunk inside the @Just@.

module Data.IORef.Strict
  ( IORef

    -- * Operations
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , atomicModifyIORef
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import GHC.Base
import GHC.Weak (Weak)
import qualified Data.IORef as Lazy

-- | A mutable reference in the IO monad, wrapping @IORef@ from "Data.IORef".
--
-- The value it holds is always in WHNF: every operation of this module forces
-- the value before it is written.
newtype IORef a = IORef (Lazy.IORef a)
  deriving stock (Eq)

-- | Build a new 'IORef'.
--
-- See @newIORef@ in "Data.IORef" for the full semantics.
--
-- The initial value is forced to WHNF before it is stored.
newIORef :: a -> IO (IORef a)
newIORef a = fmap IORef . Lazy.newIORef =<< (pure $! a)

-- | Read the value of an 'IORef'.
--
-- See @readIORef@ in "Data.IORef" for the full semantics, in particular for the
-- memory model that applies to reads and writes.
readIORef :: IORef a -> IO a
readIORef (IORef var) = Lazy.readIORef var

-- | Write a new value into an 'IORef'.
--
-- See @writeIORef@ in "Data.IORef" for the full semantics. This function does
-- not create a memory barrier; use 'atomicWriteIORef' when that matters.
--
-- The new value is forced to WHNF before it is stored.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) a = Lazy.writeIORef var =<< (pure $! a)

-- | Mutate the contents of an 'IORef', combining 'readIORef' and 'writeIORef'.
--
-- See @modifyIORef@ in "Data.IORef" for the full semantics. This is not an
-- atomic update; use 'atomicModifyIORef' in a multithreaded environment.
--
-- The new value is forced to WHNF before it is stored.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef var) f = Lazy.modifyIORef' var f

-- | Atomically modify the contents of an 'IORef'.
--
-- See @atomicModifyIORef@ in "Data.IORef" for the full semantics, in particular
-- for the atomicity and memory-barrier guarantees.
--
-- Both the new value and the returned value are forced to WHNF.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef (IORef var) f = Lazy.atomicModifyIORef' var f

-- | Variant of 'writeIORef' that imposes a reordering barrier.
--
-- See @atomicWriteIORef@ in "Data.IORef" for the full semantics.
--
-- The new value is forced to WHNF before it is stored.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef (IORef var) a = Lazy.atomicWriteIORef var =<< (pure $! a)

-- | Make a 'Weak' pointer to an 'IORef', using the second argument as a
-- finalizer to run when the 'IORef' is garbage-collected.
--
-- See @mkWeakIORef@ in "Data.IORef" for the full semantics.
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef (IORef var) finalizer = fmap coerce (Lazy.mkWeakIORef var finalizer)
