{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TArray
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- TArrays: transactional arrays, for use in the PTM monad
--
-----------------------------------------------------------------------------

module LwConc.PTM.TArray (
    TArray
) where

import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)
import Data.Typeable (Typeable)
#ifdef __GLASGOW_HASKELL__
import LwConc.Substrate
#else
import Control.Sequential.PTM (PTM)
#endif

-- |TArray is a transactional array, supporting the usual 'MArray'
-- interface for mutable arrays.
--
-- It is currently implemented as @Array ix (TVar e)@,
-- but it may be replaced by a more efficient implementation in the future
-- (the interface will remain the same, however).
--
newtype TArray i e = TArray (Array i (PVar e)) deriving (Eq, Typeable)

instance MArray TArray e PTM where
    getBounds (TArray a) = return (bounds a)
    newArray b e = do
        a <- rep (rangeSize b) (newPVar e)
        return $ TArray (listArray b a)
    newArray_ b = do
        a <- rep (rangeSize b) (newPVar arrEleBottom)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readPVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writePVar (unsafeAt a i) e
    getNumElements (TArray a) = return (numElements a)

-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)
