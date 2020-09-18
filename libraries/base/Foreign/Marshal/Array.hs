{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Array
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support: routines allocating, storing, and retrieving Haskell
-- lists that are represented as arrays in the foreign language
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Array (
  -- * Marshalling arrays

  -- ** Allocation
  --
  mallocArray,
  mallocArray0,

  allocaArray,
  allocaArray0,

  reallocArray,
  reallocArray0,

  callocArray,
  callocArray0,

  -- ** Marshalling
  --
  peekArray,
  peekArray0,

  pokeArray,
  pokeArray0,

  -- ** Combined allocation and marshalling
  --
  newArray,
  newArray0,

  withArray,
  withArray0,

  withArrayLen,
  withArrayLen0,

  -- ** Copying

  -- | (argument order: destination, source)
  copyArray,
  moveArray,

  -- ** Finding the length
  --
  lengthArray0,

  -- ** Indexing
  --
  advancePtr,
) where

import Foreign.Ptr      (Ptr, plusPtr)
import Foreign.Storable (Storable(alignment,sizeOf,peekElemOff,pokeElemOff))
import Foreign.Marshal.Alloc (mallocBytes, callocBytes, allocaBytesAligned, reallocBytes)
import Foreign.Marshal.Utils (copyBytes, moveBytes)

import GHC.Num
import GHC.List
import GHC.Base

-- allocation
-- ----------

-- |Allocate storage for the given number of elements of a storable type
-- (like 'Foreign.Marshal.Alloc.malloc', but for multiple elements).
--
mallocArray :: forall a . Storable a => Int -> IO (Ptr a)
mallocArray  size = mallocBytes (size * sizeOf (undefined :: a))

-- |Like 'mallocArray', but add an extra position to hold a special
-- termination element.
--
mallocArray0      :: Storable a => Int -> IO (Ptr a)
mallocArray0 size  = mallocArray (size + 1)

-- |Like 'mallocArray', but allocated memory is filled with bytes of value zero.
--
callocArray :: forall a . Storable a => Int -> IO (Ptr a)
callocArray size = callocBytes (size * sizeOf (undefined :: a))

-- |Like 'callocArray0', but allocated memory is filled with bytes of value
-- zero.
--
callocArray0 :: Storable a => Int -> IO (Ptr a)
callocArray0 size  = callocArray (size + 1)

-- |Temporarily allocate space for the given number of elements
-- (like 'Foreign.Marshal.Alloc.alloca', but for multiple elements).
--
allocaArray :: forall a b . Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray size = allocaBytesAligned (size * sizeOf (undefined :: a))
                                      (alignment (undefined :: a))

-- |Like 'allocaArray', but add an extra position to hold a special
-- termination element.
--
allocaArray0      :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 size  = allocaArray (size + 1)
{-# INLINE allocaArray0 #-}
  -- needed to get allocaArray to inline into withCString, for unknown
  -- reasons --SDM 23/4/2010, see #4004 for benchmark

-- |Adjust the size of an array
--
reallocArray :: forall a . Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray ptr size = reallocBytes ptr (size * sizeOf (undefined :: a))

-- |Adjust the size of an array including an extra position for the end marker.
--
reallocArray0          :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 ptr size  = reallocArray ptr (size + 1)


-- marshalling
-- -----------

-- |Convert an array of given length into a Haskell list.  The implementation
-- is tail-recursive and so uses constant stack space.
--
peekArray          :: Storable a => Int -> Ptr a -> IO [a]
peekArray size ptr | size <= 0 = return []
                 | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

-- |Convert an array terminated by the given end marker into a Haskell list
--
peekArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker ptr  = do
  size <- lengthArray0 marker ptr
  peekArray size ptr

-- |Write the list elements consecutive into memory
--
pokeArray :: Storable a => Ptr a -> [a] -> IO ()
pokeArray ptr vals0 = go vals0 0#
  where go [] _          = return ()
        go (val:vals) n# = do pokeElemOff ptr (I# n#) val; go vals (n# +# 1#)

-- |Write the list elements consecutive into memory and terminate them with the
-- given marker element
--
pokeArray0 :: Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker ptr vals0 = go vals0 0#
  where go [] n#         = pokeElemOff ptr (I# n#) marker
        go (val:vals) n# = do pokeElemOff ptr (I# n#) val; go vals (n# +# 1#)

-- combined allocation and marshalling
-- -----------------------------------

-- |Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values
-- (like 'Foreign.Marshal.Utils.new', but for multiple elements).
--
newArray      :: Storable a => [a] -> IO (Ptr a)
newArray vals  = do
  ptr <- mallocArray (length vals)
  pokeArray ptr vals
  return ptr

-- |Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end marker
--
newArray0             :: Storable a => a -> [a] -> IO (Ptr a)
newArray0 marker vals  = do
  ptr <- mallocArray0 (length vals)
  pokeArray0 marker ptr vals
  return ptr

-- |Temporarily store a list of storable values in memory
-- (like 'Foreign.Marshal.Utils.with', but for multiple elements).
--
withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray vals = withArrayLen vals . const

-- |Like 'withArray', but the action gets the number of values
-- as an additional parameter
--
withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen vals f  =
  allocaArray len $ \ptr -> do
      pokeArray ptr vals
      f len ptr
  where
    len = length vals

-- |Like 'withArray', but a terminator indicates where the array ends
--
withArray0 :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker vals = withArrayLen0 marker vals . const

-- |Like 'withArrayLen', but a terminator indicates where the array ends
--
withArrayLen0 :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 marker vals f  =
  allocaArray0 len $ \ptr -> do
      pokeArray0 marker ptr vals
      f len ptr
  where
    len = length vals

-- copying (argument order: destination, source)
-- -------

-- |Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may /not/ overlap
--
copyArray :: forall a . Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray dest src size = copyBytes dest src (size * sizeOf (undefined :: a))

-- |Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas /may/ overlap
--
moveArray :: forall a . Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray  dest src size = moveBytes dest src (size * sizeOf (undefined :: a))


-- finding the length
-- ------------------

-- |Return the number of elements in an array, excluding the terminator
--
lengthArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 marker ptr  = loop 0
  where
    loop i = do
        val <- peekElemOff ptr i
        if val == marker then return i else loop (i+1)


-- indexing
-- --------

-- |Advance a pointer into an array by the given number of elements
--
advancePtr :: forall a . Storable a => Ptr a -> Int -> Ptr a
advancePtr ptr i = ptr `plusPtr` (i * sizeOf (undefined :: a))
