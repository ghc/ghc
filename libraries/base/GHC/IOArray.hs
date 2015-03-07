{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, RoleAnnotations #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IOArray
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The IOArray type
--
-----------------------------------------------------------------------------

module GHC.IOArray (
        IOArray(..),
        newIOArray, unsafeReadIOArray, unsafeWriteIOArray,
        readIOArray, writeIOArray,
        boundsIOArray
    ) where

import GHC.Base
import GHC.IO
import GHC.Arr

-- ---------------------------------------------------------------------------
-- | An 'IOArray' is a mutable, boxed, non-strict array in the 'IO' monad.
-- The type arguments are as follows:
--
--  * @i@: the index type of the array (should be an instance of 'Ix')
--
--  * @e@: the element type of the array.
--
--

newtype IOArray i e = IOArray (STArray RealWorld i e)

-- index type should have a nominal role due to Ix class. See also #9220.
type role IOArray nominal representational

-- explicit instance because Haddock can't figure out a derived one
instance Eq (IOArray i e) where
  IOArray x == IOArray y = x == y

-- |Build a new 'IOArray'
newIOArray :: Ix i => (i,i) -> e -> IO (IOArray i e)
{-# INLINE newIOArray #-}
newIOArray lu initial  = stToIO $ do {marr <- newSTArray lu initial; return (IOArray marr)}

-- | Read a value from an 'IOArray'
unsafeReadIOArray  :: IOArray i e -> Int -> IO e
{-# INLINE unsafeReadIOArray #-}
unsafeReadIOArray (IOArray marr) i = stToIO (unsafeReadSTArray marr i)

-- | Write a new value into an 'IOArray'
unsafeWriteIOArray :: IOArray i e -> Int -> e -> IO ()
{-# INLINE unsafeWriteIOArray #-}
unsafeWriteIOArray (IOArray marr) i e = stToIO (unsafeWriteSTArray marr i e)

-- | Read a value from an 'IOArray'
readIOArray  :: Ix i => IOArray i e -> i -> IO e
readIOArray (IOArray marr) i = stToIO (readSTArray marr i)

-- | Write a new value into an 'IOArray'
writeIOArray :: Ix i => IOArray i e -> i -> e -> IO ()
writeIOArray (IOArray marr) i e = stToIO (writeSTArray marr i e)

{-# INLINE boundsIOArray #-}
boundsIOArray :: IOArray i e -> (i,i)
boundsIOArray (IOArray marr) = boundsSTArray marr

