-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Array.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: IO.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Mutable boxed/unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO (
   module Data.Array.MArray,
   IOArray,		-- instance of: Eq, Typeable
   IOUArray,		-- instance of: Eq, Typeable
   castIOUArray,	-- :: IOUArray i a -> IO (IOUArray i b)
 ) where

import Prelude

import Data.Array		( Array )
import Data.Array.MArray
import Data.Int
import Data.Word
import Data.Dynamic

import Foreign.Ptr		( Ptr, FunPtr )
import Foreign.StablePtr	( StablePtr )

#ifdef __GLASGOW_HASKELL__
-- GHC only to the end of file

import Data.Array.Base
import GHC.Arr    	( STArray, freezeSTArray, unsafeFreezeSTArray,
                          thawSTArray, unsafeThawSTArray )

import GHC.ST		( ST(..) )
import GHC.IOBase	( stToIO )

import GHC.Base

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (IO monad)

newtype IOArray i e = IOArray (STArray RealWorld i e) deriving Eq

iOArrayTc :: TyCon
iOArrayTc = mkTyCon "IOArray"

instance (Typeable a, Typeable b) => Typeable (IOArray a b) where
  typeOf a = mkAppTy iOArrayTc [typeOf ((undefined :: IOArray a b -> a) a),
				typeOf ((undefined :: IOArray a b -> b) a)]

instance HasBounds IOArray where
    {-# INLINE bounds #-}
    bounds (IOArray marr) = bounds marr

instance MArray IOArray e IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOArray marr) i e = stToIO (unsafeWrite marr i e)

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (IO monad)

newtype IOUArray i e = IOUArray (STUArray RealWorld i e) deriving Eq

iOUArrayTc :: TyCon
iOUArrayTc = mkTyCon "IOUArray"

instance (Typeable a, Typeable b) => Typeable (IOUArray a b) where
  typeOf a = mkAppTy iOUArrayTc [typeOf ((undefined :: IOUArray a b -> a) a),
				 typeOf ((undefined :: IOUArray a b -> b) a)]

instance HasBounds IOUArray where
    {-# INLINE bounds #-}
    bounds (IOUArray marr) = bounds marr

instance MArray IOUArray Bool IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Char IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (Ptr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (FunPtr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Float IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Double IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (StablePtr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int8 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int16 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int32 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int64 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word8 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word16 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word32 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word64 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

-----------------------------------------------------------------------------
-- Freezing

freezeIOArray :: Ix ix => IOArray ix e -> IO (Array ix e)
freezeIOArray (IOArray marr) = stToIO (freezeSTArray marr)

freezeIOUArray :: Ix ix => IOUArray ix e -> IO (UArray ix e)
freezeIOUArray (IOUArray marr) = stToIO (freezeSTUArray marr)

{-# RULES
"freeze/IOArray"  freeze = freezeIOArray
"freeze/IOUArray" freeze = freezeIOUArray
    #-}

{-# INLINE unsafeFreezeIOArray #-}
unsafeFreezeIOArray :: Ix ix => IOArray ix e -> IO (Array ix e)
unsafeFreezeIOArray (IOArray marr) = stToIO (unsafeFreezeSTArray marr)

{-# INLINE unsafeFreezeIOUArray #-}
unsafeFreezeIOUArray :: Ix ix => IOUArray ix e -> IO (UArray ix e)
unsafeFreezeIOUArray (IOUArray marr) = stToIO (unsafeFreezeSTUArray marr)

{-# RULES
"unsafeFreeze/IOArray"  unsafeFreeze = unsafeFreezeIOArray
"unsafeFreeze/IOUArray" unsafeFreeze = unsafeFreezeIOUArray
    #-}

-----------------------------------------------------------------------------
-- Thawing

thawIOArray :: Ix ix => Array ix e -> IO (IOArray ix e)
thawIOArray arr = stToIO $ do
    marr <- thawSTArray arr
    return (IOArray marr)

thawIOUArray :: Ix ix => UArray ix e -> IO (IOUArray ix e)
thawIOUArray arr = stToIO $ do
    marr <- thawSTUArray arr
    return (IOUArray marr)

{-# RULES
"thaw/IOArray"  thaw = thawIOArray
"thaw/IOUArray" thaw = thawIOUArray
    #-}

{-# INLINE unsafeThawIOArray #-}
unsafeThawIOArray :: Ix ix => Array ix e -> IO (IOArray ix e)
unsafeThawIOArray arr = stToIO $ do
    marr <- unsafeThawSTArray arr
    return (IOArray marr)

{-# INLINE unsafeThawIOUArray #-}
unsafeThawIOUArray :: Ix ix => UArray ix e -> IO (IOUArray ix e)
unsafeThawIOUArray arr = stToIO $ do
    marr <- unsafeThawSTUArray arr
    return (IOUArray marr)

{-# RULES
"unsafeThaw/IOArray"  unsafeThaw = unsafeThawIOArray
"unsafeThaw/IOUArray" unsafeThaw = unsafeThawIOUArray
    #-}

castSTUArray :: STUArray s ix a -> ST s (STUArray s ix b)
castSTUArray (STUArray l u marr#) = return (STUArray l u marr#)

castIOUArray :: IOUArray ix a -> IO (IOUArray ix b)
castIOUArray (IOUArray marr) = stToIO $ do
    marr' <- castSTUArray marr
    return (IOUArray marr')

#endif /* __GLASGOW_HASKELL__ */
