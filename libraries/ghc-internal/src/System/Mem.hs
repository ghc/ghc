-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem
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
-- allocation counter stuff is safe, but GHC.Conc.Sync is Unsafe

module System.Mem
       (
       -- * Garbage collection
         performGC
       , performMajorGC
       , performBlockingMajorGC
       , performMinorGC

        -- * Allocation counter and limits
        , setAllocationCounter
        , getAllocationCounter
        , enableAllocationLimit
        , disableAllocationLimit
       ) where

import GHC.Base
import GHC.Conc.Sync

-- | Triggers an immediate major garbage collection.
performGC :: IO ()
performGC = performMajorGC

-- | Triggers an immediate major garbage collection.
--
-- @since 4.7.0.0
foreign import ccall "performMajorGC" performMajorGC :: IO ()

-- | Triggers an immediate major garbage collection, ensuring that collection
-- finishes before returning.
--
-- @since 4.20.0.0
foreign import ccall "performBlockingMajorGC" performBlockingMajorGC :: IO ()

-- | Triggers an immediate minor garbage collection.
--
-- @since 4.7.0.0
foreign import ccall "performGC" performMinorGC :: IO ()
