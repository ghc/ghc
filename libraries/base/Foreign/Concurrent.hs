{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Concurrent
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

module Foreign.Concurrent
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

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase	( IO )
import GHC.Ptr		( Ptr )
import GHC.ForeignPtr	( ForeignPtr )
import qualified GHC.ForeignPtr
#endif

#ifdef __GLASGOW_HASKELL__
newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
-- ^Turns a plain memory reference into a foreign object by associating
-- a finalizer - given by the monadic operation - with the reference.
-- The finalizer will be executed after the last reference to the
-- foreign object is dropped.  Note that there is no guarantee on how
-- soon the finalizer is executed after the last reference was dropped;
-- this depends on the details of the Haskell storage manager.  The only
-- guarantee is that the finalizer runs before the program terminates.
newForeignPtr = GHC.ForeignPtr.newConcForeignPtr

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
-- ^This function adds a finalizer to the given 'ForeignPtr'.
-- The finalizer will run after the last reference to the foreign object
-- is dropped, but /before/ all previously registered finalizers for the
-- same object.
addForeignPtrFinalizer = GHC.ForeignPtr.addForeignPtrConcFinalizer
#endif
