-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.System.Mem
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Memory-related system things.
--
-----------------------------------------------------------------------------

{-# LANGUAGE Trustworthy #-}
-- allocation counter stuff is safe, but GHC.Internal.Conc.Sync is Unsafe

module GHC.Internal.System.Mem
       (
       -- * Garbage collection
         performGC
       , performMinorGC
       , performMajorGC
       , performBlockingMajorGC
       , performDeadlockDetection

        -- * Allocation counter and limits
        , setAllocationCounter
        , getAllocationCounter
        , enableAllocationLimit
        , disableAllocationLimit
       ) where

import GHC.Internal.Types
import GHC.Internal.Conc.Sync

-- | Triggers an immediate major garbage collection.
performGC :: IO ()
performGC = performMajorGC

-- | Triggers an immediate major garbage collection.
--
-- @since base-4.7.0.0
foreign import ccall "performMajorGC" performMajorGC :: IO ()

-- | Triggers an immediate major garbage collection, ensuring that collection
-- finishes before returning.
--
-- @since base-4.20.0.0
foreign import ccall "performBlockingMajorGC" performBlockingMajorGC :: IO ()

-- | Triggers an immediate minor garbage collection.
--
-- @since base-4.7.0.0
foreign import ccall "performGC" performMinorGC :: IO ()

-- | Triggers an immediate major garbage collection in deadlock detection mode.
-- This will detect any threads that are deadlocked and throw asynchronous
-- exceptions to them.
--
-- Normally, deadlock detection is done automatically and it is rarely useful
-- to invoke manually. However, since deadlock detection in GHC is performed
-- by idle GC, which some applications disable, then it may be useful to
-- invoke deadlock detection manually for the purpose of debugging or
-- resilliance.
--
-- @since ghc-internal-10.100.0
foreign import ccall "performDeadlockDetection" performDeadlockDetection :: IO ()
