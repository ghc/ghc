{-# LANGUAGE Safe #-}

-- |
--
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

module System.Mem
    (-- *  Garbage collection
     performGC,
     performMajorGC,
     performBlockingMajorGC,
     performMinorGC,
     -- *  Allocation counter and limits
     setAllocationCounter,
     getAllocationCounter,
     enableAllocationLimit,
     disableAllocationLimit
     ) where

import GHC.Internal.System.Mem