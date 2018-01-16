{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RoleAnnotations #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Storable.Internals
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Actual implementation of "Data.Array.Storable".
--
-- @since 0.4.0.0
-----------------------------------------------------------------------------

module Data.Array.Storable.Internals (
    StorableArray(..),
    withStorableArray,
    touchStorableArray,
    unsafeForeignPtrToStorableArray,
  ) where

import Data.Array.Base
import Data.Array.MArray
import Foreign hiding (newArray)

-- |The array type
data StorableArray i e = StorableArray !i !i Int !(ForeignPtr e)
-- Both parameters have class-based invariants. See also #9220.
type role StorableArray nominal nominal

instance Storable e => MArray StorableArray e IO where
    getBounds (StorableArray l u _ _) = return (l,u)

    getNumElements (StorableArray _l _u n _) = return n

    newArray (l,u) initialValue = do
        fp <- mallocForeignPtrArray size
        withForeignPtr fp $ \a ->
            sequence_ [pokeElemOff a i initialValue | i <- [0..size-1]]
        return (StorableArray l u size fp)
        where
        size = rangeSize (l,u)

    unsafeNewArray_ (l,u) = do
        let n = rangeSize (l,u)
        fp <- mallocForeignPtrArray n
        return (StorableArray l u n fp)

    newArray_ = unsafeNewArray_

    unsafeRead (StorableArray _ _ _ fp) i =
        withForeignPtr fp $ \a -> peekElemOff a i

    unsafeWrite (StorableArray _ _ _ fp) i e =
        withForeignPtr fp $ \a -> pokeElemOff a i e

-- |The pointer to the array contents is obtained by 'withStorableArray'.
-- The idea is similar to 'ForeignPtr' (used internally here).
-- The pointer should be used only during execution of the 'IO' action
-- retured by the function passed as argument to 'withStorableArray'.
withStorableArray :: StorableArray i e -> (Ptr e -> IO a) -> IO a
withStorableArray (StorableArray _ _ _ fp) f = withForeignPtr fp f

-- |If you want to use it afterwards, ensure that you
-- 'touchStorableArray' after the last use of the pointer,
-- so the array is not freed too early.
touchStorableArray :: StorableArray i e -> IO ()
touchStorableArray (StorableArray _ _ _ fp) = touchForeignPtr fp

-- |Construct a 'StorableArray' from an arbitrary 'ForeignPtr'.  It is
-- the caller's responsibility to ensure that the 'ForeignPtr' points to
-- an area of memory sufficient for the specified bounds.
unsafeForeignPtrToStorableArray
   :: Ix i => ForeignPtr e -> (i,i) -> IO (StorableArray i e)
unsafeForeignPtrToStorableArray p (l,u) =
   return (StorableArray l u (rangeSize (l,u)) p)

