{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}

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
        IORef,                -- abstract, instance of: Eq, Typeable
        newIORef,             -- :: a -> IO (IORef a)
        readIORef,            -- :: IORef a -> IO a
        writeIORef,           -- :: IORef a -> a -> IO ()
        modifyIORef,          -- :: IORef a -> (a -> a) -> IO ()
        atomicModifyIORef,    -- :: IORef a -> (a -> (a,b)) -> IO b

#if !defined(__PARALLEL_HASKELL__) && defined(__GLASGOW_HASKELL__)
        mkWeakIORef,          -- :: IORef a -> IO () -> IO (Weak (IORef a))
#endif
        -- ** Memory Model

        -- $memmodel

        ) where

#ifdef __HUGS__
import Hugs.IORef
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.STRef
import GHC.IORef hiding (atomicModifyIORef)
import qualified GHC.IORef
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
-- |Make a 'Weak' pointer to an 'IORef', using the second argument as a finalizer
-- to run when 'IORef' is garbage-collected
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
#endif

-- |Mutate the contents of an 'IORef'
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f


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
atomicModifyIORef = GHC.IORef.atomicModifyIORef

#elif defined(__HUGS__)
atomicModifyIORef = plainModifyIORef    -- Hugs has no preemption
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

{- $memmodel

  In a concurrent program, 'IORef' operations may appear out-of-order
  to another thread, depending on the memory model of the underlying
  processor architecture.  For example, on x86, loads can move ahead
  of stores, so in the following example:

>  maybePrint :: IORef Bool -> IORef Bool -> IO ()
>  maybePrint myRef yourRef = do
>    writeIORef myRef True
>    yourVal <- readIORef yourRef
>    unless yourVal $ putStrLn "critical section"
>
>  main :: IO ()
>  main = do
>    r1 <- newIORef False
>    r2 <- newIORef False
>    forkIO $ maybePrint r1 r2
>    forkIO $ maybePrint r2 r1
>    threadDelay 1000000

  it is possible that the string @"critical section"@ is printed
  twice, even though there is no interleaving of the operations of the
  two threads that allows that outcome.  The memory model of x86
  allows 'readIORef' to happen before the earlier 'writeIORef'.

  The implementation is required to ensure that reordering of memory
  operations cannot cause type-correct code to go wrong.  In
  particular, when inspecting the value read from an 'IORef', the
  memory writes that created that value must have occurred from the
  point of view of the current therad.

  'atomicModifyIORef' acts as a barrier to reordering.  Multiple
  'atomicModifyIORef' operations occur in strict program order.  An
  'atomicModifyIORef' is never observed to take place ahead of any
  earlier (in program order) 'IORef' operations, or after any later
  'IORef' operations.

-}

