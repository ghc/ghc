{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
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
--------------------------------------------------------------------------------

module Foreign.Marshal.Pool (
   -- * Pool management
   Pool,
   newPool,
   freePool,
   withPool,

   -- * (Re-)Allocation within a pool
   pooledMalloc,
   pooledMallocBytes,

   pooledRealloc,
   pooledReallocBytes,

   pooledMallocArray,
   pooledMallocArray0,

   pooledReallocArray,
   pooledReallocArray0,

   -- * Combined allocation and marshalling
   pooledNew,
   pooledNewArray,
   pooledNewArray0

) where

import GHC.Base
import GHC.Exception         ( throw )
import GHC.IO                ( mask, catchAny )
import GHC.Num
import GHC.List              ( length )
import GHC.IO.Exception      ( ioError, userError )
import GHC.Real              ( fromIntegral )

import Foreign.Marshal.Array ( pokeArray, pokeArray0 )
import Foreign.Marshal.Utils ( copyBytes )
import Foreign.Ptr           ( Ptr, castPtr, plusPtr, nullPtr )
import Foreign.Storable      ( Storable(sizeOf, poke, pokeByteOff, peekByteOff) )
import Foreign.C.Types       ( CSize(..) )

--------------------------------------------------------------------------------

-- To avoid non-H2010 stuff like existentially quantified data constructors, we
-- simply use pointers to () below. Not very nice, but...

-- | A memory pool.

newtype Pool = Pool (Ptr Arena)

-- | Allocate a fresh memory pool.

newPool :: IO Pool
newPool = liftM Pool _newArena

-- | Deallocate a memory pool and everything which has been allocated in the
-- pool itself.

freePool :: Pool -> IO ()
freePool (Pool arena) = _arenaFree arena

-- | Execute an action with a fresh memory pool, which gets automatically
-- deallocated (including its contents) after the action has finished.

withPool :: (Pool -> IO b) -> IO b
withPool act =   -- ATTENTION: cut-n-paste from Control.Exception below!
   mask (\restore -> do
      pool <- newPool
      val <- catchAny
                (restore (act pool))
                (\e -> do freePool pool; throw e)
      freePool pool
      return val)

--------------------------------------------------------------------------------

-- | Allocate space for storable type in the given pool. The size of the area
-- allocated is determined by the 'sizeOf' method from the instance of
-- 'Storable' for the appropriate type.

pooledMalloc :: forall a . Storable a => Pool -> IO (Ptr a)
pooledMalloc pool = pooledMallocBytes pool (sizeOf (undefined :: a))

-- | Allocate the given number of bytes of storage in the pool.

pooledMallocBytes :: Pool -> Int -> IO (Ptr a)
pooledMallocBytes (Pool arena) size = do
    ptr <- _arenaAlloc arena (fromIntegral (size + headerSize))
    poke (castPtr ptr :: Ptr Int) size
    return (plusPtr ptr headerSize)

-- | Adjust the storage area for an element in the pool to the given size of
-- the required type.

pooledRealloc :: forall a . Storable a => Pool -> Ptr a -> IO (Ptr a)
pooledRealloc pool ptr = pooledReallocBytes pool ptr (sizeOf (undefined :: a))

-- | Adjust the storage area for an element in the pool to the given size.

pooledReallocBytes :: Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocBytes (Pool arena) ptr size =
    let reallocate prevSize | prevSize == size = return ptr
        reallocate prevSize | prevSize >  size = do
            pokeByteOff ptr (-1*headerSize) size
            return ptr
        -- Note that a possible optimization when ptr was added last: only do a
        -- copy if there is not enough space on the arena block
        reallocate prevSize = do -- prevSize <  size
            newPtr <- pooledMallocBytes (Pool arena) size
            copyBytes newPtr ptr prevSize
            return newPtr
     in do isInArena <- _inArena arena (castPtr ptr)
           if isInArena
              then peekByteOff ptr (-1*headerSize) >>= reallocate
              else ioError $ userError "pointer not in pool"

-- | Allocate storage for the given number of elements of a storable type in the
-- pool.

pooledMallocArray :: forall a . Storable a => Pool -> Int -> IO (Ptr a)
pooledMallocArray pool size =
    pooledMallocBytes pool (size * sizeOf (undefined :: a))

-- | Allocate storage for the given number of elements of a storable type in the
-- pool, but leave room for an extra element to signal the end of the array.

pooledMallocArray0 :: Storable a => Pool -> Int -> IO (Ptr a)
pooledMallocArray0 pool size =
   pooledMallocArray pool (size + 1)

-- | Adjust the size of an array in the given pool.

pooledReallocArray :: forall a . Storable a => Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocArray pool ptr size =
    pooledReallocBytes pool ptr (size * sizeOf (undefined :: a))

-- | Adjust the size of an array with an end marker in the given pool.

pooledReallocArray0 :: Storable a => Pool -> Ptr a -> Int -> IO (Ptr a)
pooledReallocArray0 pool ptr size =
   pooledReallocArray pool ptr (size + 1)

--------------------------------------------------------------------------------

-- | Allocate storage for a value in the given pool and marshal the value into
-- this storage.

pooledNew :: Storable a => Pool -> a -> IO (Ptr a)
pooledNew pool val = do
   ptr <- pooledMalloc pool
   poke ptr val
   return ptr

-- | Allocate consecutive storage for a list of values in the given pool and
-- marshal these values into it.

pooledNewArray :: Storable a => Pool -> [a] -> IO (Ptr a)
pooledNewArray pool vals = do
   ptr <- pooledMallocArray pool (length vals)
   pokeArray ptr vals
   return ptr

-- | Allocate consecutive storage for a list of values in the given pool and
-- marshal these values into it, terminating the end with the given marker.

pooledNewArray0 :: Storable a => Pool -> a -> [a] -> IO (Ptr a)
pooledNewArray0 pool marker vals = do
   ptr <- pooledMallocArray0 pool (length vals)
   pokeArray0 marker ptr vals
   return ptr

------------------------------------------------------

type Arena = ()

headerSize :: Int
headerSize = sizeOf (undefined :: Int)

foreign import ccall unsafe "newArena" _newArena :: IO  (Ptr Arena)
foreign import ccall unsafe "arenaAlloc" _arenaAlloc :: Ptr Arena -> CSize -> IO (Ptr ())
foreign import ccall unsafe "arenaFree" _arenaFree :: Ptr Arena -> IO ()
foreign import ccall unsafe "inArena" _inArena :: Ptr Arena -> Ptr () -> IO Bool

