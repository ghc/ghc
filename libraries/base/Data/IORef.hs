-----------------------------------------------------------------------------
-- 
-- Module      :  Data.IORef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: IORef.hs,v 1.3 2001/12/21 15:07:21 simonmar Exp $
--
-- Mutable references in the IO monad.
--
-----------------------------------------------------------------------------

module Data.IORef
	( IORef		      -- abstract, instance of: Eq, Typeable
        , newIORef	      -- :: a -> IO (IORef a)
        , readIORef	      -- :: IORef a -> IO a
        , writeIORef	      -- :: IORef a -> a -> IO ()
	, modifyIORef	      -- :: IORef a -> (a -> a) -> IO ()

#if !defined(__PARALLEL_HASKELL__) && defined(__GLASGOW_HASKELL__)
	, mkWeakIORef           -- :: IORef a -> IO () -> IO (Weak (IORef a))
#endif
	) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Base		( mkWeak# )
import GHC.STRef
import GHC.IOBase
#if !defined(__PARALLEL_HASKELL__)
import GHC.Weak
#endif
#endif /* __GLASGOW_HASKELL__ */

#ifdef __HUGS__
import IOExts		( IORef, newIORef, writeIORef, readIORef )
import ST		( stToIO, newSTRef, readSTRef, writeSTRef )
#endif

import Data.Dynamic

#ifndef __PARALLEL_HASKELL__
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
#endif

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = writeIORef ref . f =<< readIORef ref

#include "Dynamic.h"
INSTANCE_TYPEABLE1(IORef,ioRefTc,"IORef")
