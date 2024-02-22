{-# LANGUAGE Safe #-}

-- |
--
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

module Data.IORef
    (-- *  IORefs
     IORef,
     newIORef,
     readIORef,
     writeIORef,
     modifyIORef,
     modifyIORef',
     atomicModifyIORef,
     atomicModifyIORef',
     atomicWriteIORef,
     mkWeakIORef,
     -- **  Memory Model
     -- $memmodel
     ) where

import GHC.Internal.Data.IORef

{- $memmodel
  #memmodel#

  Most modern CPU achitectures (e.g. x86/64, ARM) have a memory model which allows
  threads to reorder reads with earlier writes to different locations,
  e.g. see <https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html the x86/64 architecture manual>,
  8.2.3.4 Loads May Be Reordered with Earlier Stores to Different Locations.

  Because of that, in a concurrent program, 'IORef' operations may appear out-of-order
  to another thread. In the following example:

  > import GHC.Internal.Data.IORef
  > import GHC.Internal.Control.Monad (unless)
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
