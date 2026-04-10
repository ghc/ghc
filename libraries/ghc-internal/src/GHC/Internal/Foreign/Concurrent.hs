{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.Concurrent
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- FFI datatypes and operations that use or require concurrency (GHC only).
--
-----------------------------------------------------------------------------

module GHC.Internal.Foreign.Concurrent
  (
        -- * Concurrency-based 'ForeignPtr' operations

        -- | These functions generalize their namesakes in the portable
        -- "Foreign.ForeignPtr" module by allowing arbitrary 'IO' actions
        -- as finalizers.  These finalizers necessarily run in a separate
        -- thread, cf. /Destructors, Finalizers and Synchronization/,
        -- by Hans Boehm, /POPL/, 2003.

        newForeignPtr,
        addForeignPtrFinalizer,
  ) where

import GHC.Internal.IO         ( IO )
import GHC.Internal.Ptr        ( Ptr )
import GHC.Internal.ForeignPtr ( ForeignPtr )
import qualified GHC.Internal.ForeignPtr

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
--
-- ^Turns a plain memory reference into a foreign object by
-- associating a finalizer - given by the monadic operation - with the
-- reference.
--
-- When finalization is triggered by GC, the storage manager will start the
-- finalizer, in a separate thread, some time after the last reference to the
-- @ForeignPtr@ is dropped.  There is __no guarantee of promptness__, and
-- in fact there is no guarantee that the finalizer will eventually
-- run at all for GC-triggered finalization.
--
-- When finalization is triggered by explicitly calling @finalizeForeignPtr@,
-- the finalizer will run immediately on the current Haskell thread.
--
-- Note that references from a finalizer do not necessarily prevent
-- another object from being finalized.  If A's finalizer refers to B
-- (perhaps using 'GHC.Internal.Foreign.ForeignPtr.touchForeignPtr', then the only
-- guarantee is that B's finalizer will never be started before A's.  If both
-- A and B are unreachable, then both finalizers will start together.  See
-- 'GHC.Internal.Foreign.ForeignPtr.touchForeignPtr' for more on finalizer ordering.
--
newForeignPtr = GHC.Internal.ForeignPtr.newConcForeignPtr

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
-- ^This function adds a finalizer to the given 'ForeignPtr'.  The
-- finalizer will run /before/ all other finalizers for the same
-- object which have already been registered.
--
-- This is a variant of 'GHC.Internal.Foreign.ForeignPtr.addForeignPtrFinalizer',
-- where the finalizer is an arbitrary 'IO' action.  When it is
-- invoked, the finalizer will run in a new thread.
--
-- NB. Be very careful with these finalizers.  One common trap is that
-- if a finalizer references another finalized value, it does not
-- prevent that value from being finalized.  In particular, 'System.IO.Handle's
-- are finalized objects, so a finalizer should not refer to a
-- 'System.IO.Handle' (including 'GHC.Internal.System.IO.stdout', 'GHC.Internal.System.IO.stdin', or
-- 'GHC.Internal.System.IO.stderr').
--
addForeignPtrFinalizer = GHC.Internal.ForeignPtr.addForeignPtrConcFinalizer

