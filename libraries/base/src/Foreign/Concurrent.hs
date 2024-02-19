{-# LANGUAGE Safe #-}

-- |
--
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

module Foreign.Concurrent
    (-- *  Concurrency-based 'ForeignPtr' operations
     -- |  These functions generalize their namesakes in the portable
     -- "Foreign.ForeignPtr" module by allowing arbitrary 'IO' actions
     -- as finalizers.  These finalizers necessarily run in a separate
     -- thread, cf. /Destructors, Finalizers and Synchronization/,
     -- by Hans Boehm, /POPL/, 2003.
     newForeignPtr,
     addForeignPtrFinalizer
     ) where

import GHC.Internal.Foreign.Concurrent
