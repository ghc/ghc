-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IORef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Mutable references in the IO monad.
--
-----------------------------------------------------------------------------

module Data.IORef
  ( 
	-- * IORefs
	IORef,		      -- abstract, instance of: Eq, Typeable
	newIORef,	      -- :: a -> IO (IORef a)
        readIORef,	      -- :: IORef a -> IO a
        writeIORef,	      -- :: IORef a -> a -> IO ()
	modifyIORef,	      -- :: IORef a -> (a -> a) -> IO ()

#if !defined(__PARALLEL_HASKELL__) && defined(__GLASGOW_HASKELL__)
	mkWeakIORef,          -- :: IORef a -> IO () -> IO (Weak (IORef a))
#endif
	) where

import Prelude

#ifdef __HUGS__
import Hugs.IORef
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base		( mkWeak# )
import GHC.STRef
import GHC.IOBase
#if !defined(__PARALLEL_HASKELL__)
import GHC.Weak
#endif
#endif /* __GLASGOW_HASKELL__ */

#ifdef __NHC__
import NHC.IOExtras
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
#endif

#ifndef __NHC__
import Data.Dynamic
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__PARALLEL_HASKELL__)
-- |Make a 'Weak' pointer to an 'IORef'
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
#endif

-- |Mutate the contents of an 'IORef'
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = writeIORef ref . f =<< readIORef ref

#ifndef __NHC__
#include "Dynamic.h"
INSTANCE_TYPEABLE1(IORef,ioRefTc,"IORef")
#endif
