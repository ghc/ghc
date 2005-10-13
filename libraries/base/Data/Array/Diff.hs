-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Diff
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.IArray)
--
-- Functional arrays with constant-time update.
--
-----------------------------------------------------------------------------

module Data.Array.Diff (

    -- * Diff array types

    -- | Diff arrays have an immutable interface, but rely on internal
    -- updates in place to provide fast functional update operator
    -- '//'.
    --
    -- When the '//' operator is applied to a diff array, its contents
    -- are physically updated in place. The old array silently changes
    -- its representation without changing the visible behavior:
    -- it stores a link to the new current array along with the
    -- difference to be applied to get the old contents.
    --
    -- So if a diff array is used in a single-threaded style,
    -- i.e. after '//' application the old version is no longer used,
    -- @a'!'i@ takes O(1) time and @a '//' d@ takes O(@length d@).
    -- Accessing elements of older versions gradually becomes slower.
    --
    -- Updating an array which is not current makes a physical copy.
    -- The resulting array is unlinked from the old family. So you
    -- can obtain a version which is guaranteed to be current and
    -- thus have fast element access by @a '//' []@.

    -- Possible improvement for the future (not implemented now):
    -- make it possible to say "I will make an update now, but when
    -- I later return to the old version, I want it to mutate back
    -- instead of being copied".

    IOToDiffArray, -- data IOToDiffArray
                   --     (a :: * -> * -> *) -- internal mutable array
                   --     (i :: *)           -- indices
                   --     (e :: *)           -- elements

    -- | Type synonyms for the two most important IO array types.

    -- Two most important diff array types are fully polymorphic
    -- lazy boxed DiffArray:
    DiffArray,     -- = IOToDiffArray IOArray
    -- ...and strict unboxed DiffUArray, working only for elements
    -- of primitive types but more compact and usually faster:
    DiffUArray,    -- = IOToDiffArray IOUArray

    -- * Overloaded immutable array interface
    
    -- | Module "Data.Array.IArray" provides the interface of diff arrays.
    -- They are instances of class 'IArray'.
    module Data.Array.IArray,

    -- * Low-level interface

    -- | These are really internal functions, but you will need them
    -- to make further 'IArray' instances of various diff array types
    -- (for either more 'MArray' types or more unboxed element types).
    newDiffArray, readDiffArray, replaceDiffArray
    )
    where

------------------------------------------------------------------------
-- Imports.

import Prelude

import Data.Ix
import Data.Array.Base
import Data.Array.IArray
import Data.Array.IO

import Foreign.Ptr        ( Ptr, FunPtr )
import Foreign.StablePtr  ( StablePtr )
import Data.Int           ( Int8,  Int16,  Int32,  Int64 )
import Data.Word          ( Word, Word8, Word16, Word32, Word64 )

import System.IO.Unsafe	  ( unsafePerformIO )
import Control.Exception  ( evaluate )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar, readMVar )

------------------------------------------------------------------------
-- Diff array types.

-- | An arbitrary 'MArray' type living in the 'IO' monad can be converted
-- to a diff array.

newtype IOToDiffArray a i e =
    DiffArray {varDiffArray :: MVar (DiffArrayData a i e)}

-- Internal representation: either a mutable array, or a link to
-- another diff array patched with a list of index+element pairs.
data DiffArrayData a i e = Current (a i e)
                         | Diff (IOToDiffArray a i e) [(Int, e)]

-- | Fully polymorphic lazy boxed diff array.
type DiffArray  = IOToDiffArray IOArray

-- | Strict unboxed diff array, working only for elements
-- of primitive types but more compact and usually faster than 'DiffArray'.
type DiffUArray = IOToDiffArray IOUArray

-- Having 'MArray a e IO' in instance context would require
-- -fallow-undecidable-instances, so each instance is separate here.

------------------------------------------------------------------------
-- Showing DiffArrays

instance (Ix ix, Show ix, Show e) => Show (DiffArray ix e) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Char) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Word) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Float) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Double) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int8) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int16) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int32) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Int64) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Word8) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Word16) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Word32) where
  showsPrec = showsIArray

instance (Ix ix, Show ix) => Show (DiffUArray ix Word64) where
  showsPrec = showsIArray

------------------------------------------------------------------------
-- Boring instances.

instance HasBounds a => HasBounds (IOToDiffArray a) where
    bounds a = unsafePerformIO $ boundsDiffArray a

instance IArray (IOToDiffArray IOArray) e where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray1` ies

instance IArray (IOToDiffArray IOUArray) Char where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Int where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Word where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) (Ptr a) where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) (FunPtr a) where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Float where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Double where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) (StablePtr a) where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Int8 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Int16 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Int32 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Int64 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Word8 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Word16 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Word32 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies

instance IArray (IOToDiffArray IOUArray) Word64 where
    unsafeArray   lu ies = unsafePerformIO $ newDiffArray lu ies
    unsafeAt      a i    = unsafePerformIO $ a `readDiffArray` i
    unsafeReplace a ies  = unsafePerformIO $ a `replaceDiffArray2` ies



------------------------------------------------------------------------
-- The important stuff.

newDiffArray :: (MArray a e IO, Ix i)
             => (i,i)
             -> [(Int, e)]
             -> IO (IOToDiffArray a i e)
newDiffArray (l,u) ies = do
    a <- newArray_ (l,u)
    sequence_ [unsafeWrite a i e | (i, e) <- ies]
    var <- newMVar (Current a)
    return (DiffArray var)

readDiffArray :: (MArray a e IO, Ix i)
              => IOToDiffArray a i e
              -> Int
              -> IO e
a `readDiffArray` i = do
    d <- readMVar (varDiffArray a)
    case d of
        Current a'  -> unsafeRead a' i
        Diff a' ies -> maybe (readDiffArray a' i) return (lookup i ies)

replaceDiffArray :: (MArray a e IO, Ix i)
                => IOToDiffArray a i e
                -> [(Int, e)]
                -> IO (IOToDiffArray a i e)
a `replaceDiffArray` ies = do
    d <- takeMVar (varDiffArray a)
    case d of
        Current a' -> case ies of
            [] -> do
                -- We don't do the copy when there is nothing to change
                -- and this is the current version. But see below.
                putMVar (varDiffArray a) d
                return a
            _:_ -> do
                diff <- sequence [do e <- unsafeRead a' i; return (i, e)
                                  | (i, _) <- ies]
                sequence_ [unsafeWrite a' i e | (i, e) <- ies]
                var' <- newMVar (Current a')
                putMVar (varDiffArray a) (Diff (DiffArray var') diff)
                return (DiffArray var')
        Diff _ _ -> do
            -- We still do the copy when there is nothing to change
            -- but this is not the current version. So you can use
            -- 'a // []' to make sure that the resulting array has
            -- fast element access.
            putMVar (varDiffArray a) d
            a' <- thawDiffArray a
                -- thawDiffArray gives a fresh array which we can
                -- safely mutate.
            sequence_ [unsafeWrite a' i e | (i, e) <- ies]
            var' <- newMVar (Current a')
            return (DiffArray var')

-- The elements of the diff list might recursively reference the
-- array, so we must seq them before taking the MVar to avoid
-- deadlock.
replaceDiffArray1 :: (MArray a e IO, Ix i)
                => IOToDiffArray a i e
                -> [(Int, e)]
                -> IO (IOToDiffArray a i e)
a `replaceDiffArray1` ies = do
    mapM_ (evaluate . fst) ies
    a `replaceDiffArray` ies

-- If the array contains unboxed elements, then the elements of the
-- diff list may also recursively reference the array from inside
-- replaceDiffArray, so we must seq them too.
replaceDiffArray2 :: (MArray a e IO, Ix i)
                => IOToDiffArray a i e
                -> [(Int, e)]
                -> IO (IOToDiffArray a i e)
a `replaceDiffArray2` ies = do
    mapM_ (\(a,b) -> do evaluate a; evaluate b) ies
    a `replaceDiffArray` ies


boundsDiffArray :: (HasBounds a, Ix ix)
                => IOToDiffArray a ix e
                -> IO (ix,ix)
boundsDiffArray a = do
    d <- readMVar (varDiffArray a)
    case d of
        Current a' -> return (bounds a')
        Diff a' _  -> boundsDiffArray a'

freezeDiffArray :: (MArray a e IO, Ix ix)
                => a ix e
                -> IO (IOToDiffArray a ix e)
freezeDiffArray a = case bounds a of
  (l,u) -> do
    a' <- newArray_ (l,u)
    sequence_ [unsafeRead a i >>= unsafeWrite a' i | i <- [0 .. rangeSize (l,u) - 1]]
    var <- newMVar (Current a')
    return (DiffArray var)

{-# RULES
"freeze/DiffArray" freeze = freezeDiffArray
    #-}

-- unsafeFreezeDiffArray is really unsafe. Better don't use the old
-- array at all after freezing. The contents of the source array will
-- be changed when '//' is applied to the resulting array.

unsafeFreezeDiffArray :: (MArray a e IO, Ix ix)
                      => a ix e
                      -> IO (IOToDiffArray a ix e)
unsafeFreezeDiffArray a = do
    var <- newMVar (Current a)
    return (DiffArray var)

{-# RULES
"unsafeFreeze/DiffArray" unsafeFreeze = unsafeFreezeDiffArray
    #-}

thawDiffArray :: (MArray a e IO, Ix ix)
              => IOToDiffArray a ix e
              -> IO (a ix e)
thawDiffArray a = do
    d <- readMVar (varDiffArray a)
    case d of
        Current a' -> case bounds a' of
	  (l,u) -> do
            a'' <- newArray_ (l,u)
            sequence_ [unsafeRead a' i >>= unsafeWrite a'' i | i <- [0 .. rangeSize (l,u) - 1]]
            return a''
        Diff a' ies -> do
            a'' <- thawDiffArray a'
            sequence_ [unsafeWrite a'' i e | (i, e) <- ies]
            return a''

{-# RULES
"thaw/DiffArray" thaw = thawDiffArray
    #-}

-- unsafeThawDiffArray is really unsafe. Better don't use the old
-- array at all after thawing. The contents of the resulting array
-- will be changed when '//' is applied to the source array.

unsafeThawDiffArray :: (MArray a e IO, Ix ix)
                    => IOToDiffArray a ix e
                    -> IO (a ix e)
unsafeThawDiffArray a = do
    d <- readMVar (varDiffArray a)
    case d of
        Current a'  -> return a'
        Diff a' ies -> do
            a'' <- unsafeThawDiffArray a'
            sequence_ [unsafeWrite a'' i e | (i, e) <- ies]
            return a''

{-# RULES
"unsafeThaw/DiffArray" unsafeThaw = unsafeThawDiffArray
    #-}
