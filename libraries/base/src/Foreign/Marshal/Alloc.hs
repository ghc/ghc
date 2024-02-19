{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Marshal.Alloc
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The module "Foreign.Marshal.Alloc" provides operations to allocate and
-- deallocate blocks of raw memory (i.e., unstructured chunks of memory
-- outside of the area maintained by the Haskell storage manager).  These
-- memory blocks are commonly used to pass compound data structures to
-- foreign functions or to provide space in which compound result values
-- are obtained from foreign functions.
--
-- If any of the allocation functions fails, an exception is thrown.
-- In some cases, memory exhaustion may mean the process is terminated.
-- If 'free' or 'reallocBytes' is applied to a memory area
-- that has been allocated with 'alloca' or 'allocaBytes', the
-- behaviour is undefined.  Any further access to memory areas allocated with
-- 'alloca' or 'allocaBytes', after the computation that was passed to
-- the allocation function has terminated, leads to undefined behaviour.  Any
-- further access to the memory area referenced by a pointer passed to
-- 'realloc', 'reallocBytes', or 'free' entails undefined
-- behaviour.
--
-- All storage allocated by functions that allocate based on a /size in bytes/
-- must be sufficiently aligned for any of the basic foreign types
-- that fits into the newly allocated storage. All storage allocated by
-- functions that allocate based on a specific type must be sufficiently
-- aligned for that type. Array allocation routines need to obey the same
-- alignment constraints for each array element.
--
-- The underlying implementation is wrapping the @<stdlib.h>@
-- @malloc@, @realloc@, and @free@.
-- In other words it should be safe to allocate using C-@malloc@,
-- and free memory with 'free' from this module.
--

module Foreign.Marshal.Alloc
    (-- *  Memory allocation
     -- **  Local allocation
     alloca,
     allocaBytes,
     allocaBytesAligned,
     -- **  Dynamic allocation
     malloc,
     mallocBytes,
     calloc,
     callocBytes,
     realloc,
     reallocBytes,
     free,
     finalizerFree
     ) where

import GHC.Internal.Foreign.Marshal.Alloc