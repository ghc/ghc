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
        newIORef,
        readIORef,
        writeIORef,
        modifyIORef,
        modifyIORef',
        atomicModifyIORef,
        atomicModifyIORef',
        atomicWriteIORef,

#if !defined(__PARALLEL_HASKELL__)
        mkWeakIORef,
#endif
        -- ** Memory Model

        -- $memmodel

        ) where

import GHC.Base
import GHC.STRef
import GHC.IORef hiding (atomicModifyIORef)
import qualified GHC.IORef
#if !defined(__PARALLEL_HASKELL__)
import GHC.Weak
#endif

#if !defined(__PARALLEL_HASKELL__)
-- |Make a 'Weak' pointer to an 'IORef', using the second argument as a finalizer
-- to run when 'IORef' is garbage-collected
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
#endif

-- |Mutate the contents of an 'IORef'.
--
-- Be warned that 'modifyIORef' does not apply the function strictly.  This
-- means if the program calls 'modifyIORef' many times, but seldomly uses the
-- value, thunks will pile up in memory resulting in a space leak.  This is a
-- common mistake made when using an IORef as a counter.  For example, the
-- following will likely produce a stack overflow:
--
-- >ref <- newIORef 0
-- >replicateM_ 1000000 $ modifyIORef ref (+1)
-- >readIORef ref >>= print
--
-- To avoid this problem, use 'modifyIORef'' instead.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

-- |Strict version of 'modifyIORef'
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

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
-- 'atomicModifyIORef' does not apply the function strictly.  This is important
-- to know even if all you are doing is replacing the value.  For example, this
-- will leak memory:
--
-- >ref <- newIORef '1'
-- >forever $ atomicModifyIORef ref (\_ -> ('2', ()))
--
-- Use 'atomicModifyIORef'' or 'atomicWriteIORef' to avoid this problem.
--
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef = GHC.IORef.atomicModifyIORef

-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b

-- | Variant of 'writeIORef' with the \"barrier to reordering\" property that
-- 'atomicModifyIORef' has.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()

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

