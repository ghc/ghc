-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Storable
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- A storable array is an IO-mutable array which stores its
-- contents in a contiguous memory block living in the C
-- heap. Elements are stored according to the class 'Storable'.
-- You can obtain the pointer to the array contents to manipulate
-- elements from languages like C.
--
-- It is similar to 'Data.Array.IO.IOUArray' but slower.
-- Its advantage is that it's compatible with C.
--
-----------------------------------------------------------------------------

module Data.Array.Storable (
    
    -- * Arrays of 'Storable' things.
    StorableArray, -- data StorableArray index element
                   --     -- index type must be in class Ix
                   --     -- element type must be in class Storable
    
    -- * Overloaded mutable array interface
    -- | Module "Data.Array.MArray" provides the interface of storable arrays.
    -- They are instances of class 'MArray' (with the 'IO' monad).
    module Data.Array.MArray,
    
    -- * Accessing the pointer to the array contents
    withStorableArray, -- :: StorableArray i e -> (Ptr e -> IO a) -> IO a
    
    touchStorableArray, -- :: StorableArray i e -> IO ()

    unsafeForeignPtrToStorableArray
    )
    where

import Prelude

import Data.Array.Base
import Data.Array.MArray
import Foreign hiding (newArray)

-- |The array type
data StorableArray i e = StorableArray !i !i !(ForeignPtr e)

instance HasBounds StorableArray where
    bounds (StorableArray l u _) = (l,u)

instance Storable e => MArray StorableArray e IO where

    newArray (l,u) init = do
        fp <- mallocForeignPtrArray size
        withForeignPtr fp $ \a ->
            sequence_ [pokeElemOff a i init | i <- [0..size-1]]
        return (StorableArray l u fp)
        where
        size = rangeSize (l,u)

    newArray_ (l,u) = do
        fp <- mallocForeignPtrArray (rangeSize (l,u))
        return (StorableArray l u fp)

    unsafeRead (StorableArray _ _ fp) i =
        withForeignPtr fp $ \a -> peekElemOff a i

    unsafeWrite (StorableArray _ _ fp) i e =
        withForeignPtr fp $ \a -> pokeElemOff a i e

-- |The pointer to the array contents is obtained by 'withStorableArray'.
-- The idea is similar to 'ForeignPtr' (used internally here).
-- The pointer should be used only during execution of the 'IO' action
-- retured by the function passed as argument to 'withStorableArray'.
withStorableArray :: StorableArray i e -> (Ptr e -> IO a) -> IO a
withStorableArray (StorableArray _ _ fp) f = withForeignPtr fp f

-- |If you want to use it afterwards, ensure that you
-- 'touchStorableArray' after the last use of the pointer,
-- so the array is not freed too early.
touchStorableArray :: StorableArray i e -> IO ()
touchStorableArray (StorableArray _ _ fp) = touchForeignPtr fp

-- |Construct a 'StorableArray' from an arbitrary 'ForeignPtr'.  It is
-- the caller's responsibility to ensure that the 'ForeignPtr' points to
-- an area of memory sufficient for the specified bounds.
unsafeForeignPtrToStorableArray 
   :: ForeignPtr e -> (i,i) -> IO (StorableArray i e)
unsafeForeignPtrToStorableArray p (l,u) =
   return (StorableArray l u p)
