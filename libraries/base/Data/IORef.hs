{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IORef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
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
        mkWeakIORef,
        -- ** Memory Model

        -- $memmodel

        ) where

import GHC.Base
import GHC.STRef
import GHC.IORef
import GHC.Weak

-- |Make a 'Weak' pointer to an 'IORef', using the second argument as a finalizer
-- to run when 'IORef' is garbage-collected
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) (IO finalizer) = IO $ \s ->
    case mkWeak# r# r finalizer s of (# s1, w #) -> (# s1, Weak w #)

-- |Mutate the contents of an 'IORef', combining 'readIORef' and 'writeIORef'.
-- This is not an atomic update, consider using 'atomicModifyIORef' when
-- operating in a multithreaded environment.
--
-- Be warned that 'modifyIORef' does not apply the function strictly.  This
-- means if the program calls 'modifyIORef' many times, but seldom uses the
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

-- |Strict version of 'modifyIORef'.
-- This is not an atomic update, consider using 'atomicModifyIORef'' when
-- operating in a multithreaded environment.
--
-- @since 4.6.0.0
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
-- This function imposes a memory barrier, preventing reordering;
-- see "Data.IORef#memmodel" for details.
--
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef ref f = do
  (_old, ~(_new, res)) <- atomicModifyIORef2 ref f
  pure res

-- | Variant of 'writeIORef'. The prefix "atomic" relates to a fact that
-- it imposes a reordering barrier, similar to 'atomicModifyIORef'.
-- Such a write will not be reordered with other reads
-- or writes even on CPUs with weak memory model.
--
-- @since 4.6.0.0
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
  _ <- atomicSwapIORef ref a
  pure ()

{- $memmodel
  #memmodel#

  Most modern CPU achitectures (e.g. x86/64, ARM) have a memory model which allows
  threads to reorder reads with earlier writes to different locations,
  e.g. see <https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html the x86/64 architecture manual>,
  8.2.3.4 Loads May Be Reordered with Earlier Stores to Different Locations.

  Because of that, in a concurrent program, 'IORef' operations may appear out-of-order
  to another thread. In the following example:

  > import Data.IORef
  > import Control.Monad (unless)
  > import Control.Concurrent (forkIO, threadDelay)
  >
  > maybePrint :: IORef Bool -> IORef Bool -> IO ()
  > maybePrint myRef yourRef = do
  >   writeIORef myRef True
  >   yourVal <- readIORef yourRef
  >   unless yourVal $ putStrLn "critical section"
  >
  > main :: IO ()
  > main = do
  >   r1 <- newIORef False
  >   r2 <- newIORef False
  >   forkIO $ maybePrint r1 r2
  >   forkIO $ maybePrint r2 r1
  >   threadDelay 1000000

  it is possible that the string @"critical section"@ is printed
  twice, even though there is no interleaving of the operations of the
  two threads that allows that outcome.  The memory model of x86/64
  allows 'readIORef' to happen before the earlier 'writeIORef'.

  The ARM memory order model is typically even weaker than x86/64, allowing
  any reordering of reads and writes as long as they are independent
  from the point of view of the current thread.

  The implementation is required to ensure that reordering of memory
  operations cannot cause type-correct code to go wrong.  In
  particular, when inspecting the value read from an 'IORef', the
  memory writes that created that value must have occurred from the
  point of view of the current thread.

  'atomicWriteIORef', 'atomicModifyIORef' and 'atomicModifyIORef'' act
  as a barrier to reordering. Multiple calls to these functions
  occur in strict program order, never taking place ahead of any
  earlier (in program order) 'IORef' operations, or after any later
  'IORef' operations.

-}
