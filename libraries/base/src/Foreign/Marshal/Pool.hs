{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Marshal.Pool
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains support for pooled memory management. Under this scheme,
-- (re-)allocations belong to a given pool, and everything in a pool is
-- deallocated when the pool itself is deallocated. This is useful when
-- 'Foreign.Marshal.Alloc.alloca' with its implicit allocation and deallocation
-- is not flexible enough, but explicit uses of 'Foreign.Marshal.Alloc.malloc'
-- and 'free' are too awkward.
--

module Foreign.Marshal.Pool
    (-- *  Pool management
     Pool,
     newPool,
     freePool,
     withPool,
     -- *  (Re-)Allocation within a pool
     pooledMalloc,
     pooledMallocBytes,
     pooledRealloc,
     pooledReallocBytes,
     pooledMallocArray,
     pooledMallocArray0,
     pooledReallocArray,
     pooledReallocArray0,
     -- *  Combined allocation and marshalling
     pooledNew,
     pooledNewArray,
     pooledNewArray0
     ) where

import GHC.Internal.Foreign.Marshal.Pool