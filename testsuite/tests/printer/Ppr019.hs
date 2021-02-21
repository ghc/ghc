{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}

{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.IO.Internal
-- Copyright   :  (c) The University of Glasgow 2001-2012
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.Base)
--
-- Mutable boxed and unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO.Internals (
    IOArray(..),         -- instance of: Eq, Typeable
    IOUArray(..),        -- instance of: Eq, Typeable
    castIOUArray,        -- :: IOUArray ix a -> IO (IOUArray ix b)
    unsafeThawIOUArray,
  ) where

import Data.Int
import Data.Word
import Data.Typeable

import Control.Monad.ST         ( RealWorld, stToIO )
import Foreign.Ptr              ( Ptr, FunPtr )
import Foreign.StablePtr        ( StablePtr )

import Data.Array.Base

import GHC.IOArray (IOArray(..))

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (IO monad)

-- | Mutable, unboxed, strict arrays in the 'IO' monad.  The type
-- arguments are as follows:
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.  Only certain element types
--    are supported: see "Data.Array.MArray" for a list of instances.
--
newtype IOUArray i e = IOUArray (STUArray RealWorld i e)
                       deriving Typeable
-- Both parameters have class-based invariants. See also #9220.
type role IOUArray nominal nominal

instance Eq (IOUArray i e) where
    IOUArray s1 == IOUArray s2  =  s1 == s2

instance MArray IOUArray Bool IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Char IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (Ptr a) IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (FunPtr a) IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Float IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Double IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (StablePtr a) IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int8 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int16 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int32 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int64 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word8 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word16 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word32 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word64 IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

-- | Casts an 'IOUArray' with one element type into one with a
-- different element type.  All the elements of the resulting array
-- are undefined (unless you know what you\'re doing...).
castIOUArray :: IOUArray ix a -> IO (IOUArray ix b)
castIOUArray (IOUArray marr) = stToIO $ do
    marr' <- castSTUArray marr
    return (IOUArray marr')

{-# INLINE unsafeThawIOUArray #-}
unsafeThawIOUArray :: UArray ix e -> IO (IOUArray ix e)
unsafeThawIOUArray arr = stToIO $ do
    marr <- unsafeThawSTUArray arr
    return (IOUArray marr)

{-# RULES
"unsafeThaw/IOUArray" unsafeThaw = unsafeThawIOUArray
    #-}

thawIOUArray :: UArray ix e -> IO (IOUArray ix e)
thawIOUArray arr = stToIO $ do
    marr <- thawSTUArray arr
    return (IOUArray marr)

{-# RULES
"thaw/IOUArray" thaw = thawIOUArray
    #-}

{-# INLINE unsafeFreezeIOUArray #-}
unsafeFreezeIOUArray :: IOUArray ix e -> IO (UArray ix e)
unsafeFreezeIOUArray (IOUArray marr) = stToIO (unsafeFreezeSTUArray marr)

{-# RULES
"unsafeFreeze/IOUArray" unsafeFreeze = unsafeFreezeIOUArray
    #-}

freezeIOUArray :: IOUArray ix e -> IO (UArray ix e)
freezeIOUArray (IOUArray marr) = stToIO (freezeSTUArray marr)

{-# RULES
"freeze/IOUArray" freeze = freezeIOUArray
    #-}
