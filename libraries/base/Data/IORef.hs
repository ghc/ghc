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
	atomicModifyIORef,    -- :: IORef a -> (a -> (a,b)) -> IO b

#if !defined(__PARALLEL_HASKELL__) && defined(__GLASGOW_HASKELL__)
	mkWeakIORef,          -- :: IORef a -> IO () -> IO (Weak (IORef a))
#endif
	) where

import Prelude	-- Explicit dependency helps 'make depend' do the right thing

#ifdef __HUGS__
import Hugs.IORef
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base		( mkWeak#, atomicModifyMutVar# )
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
    , excludeFinalisers
    )
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


-- |Atomically modifies the contents of an 'IORef'.
--
-- This function is useful for using 'IORef' in a safe way in a multithreaded
-- program.  If you only have one 'IORef', then using 'atomicModifyIORef' to
-- access and modify it will prevent race conditions.
--
-- Extending the atomicity to multiple 'IORef's is problematic, so it
-- is recommended that if you need to do anything more complicated
-- then using 'Control.Concurrent.MVar.MVar' instead is a good idea.
--
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
#if defined(__GLASGOW_HASKELL__)
atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

#elif defined(__HUGS__)
atomicModifyIORef = plainModifyIORef	-- Hugs has no preemption
  where plainModifyIORef r f = do
		a <- readIORef r
		case f a of (a',b) -> writeIORef r a' >> return b
#elif defined(__NHC__)
atomicModifyIORef r f =
  excludeFinalisers $ do
    a <- readIORef r
    let (a',b) = f a
    writeIORef r a'
    return b
#endif
