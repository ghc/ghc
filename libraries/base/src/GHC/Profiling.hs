{-# LANGUAGE Safe #-}

-- |
-- @since 4.7.0.0

module GHC.Profiling
    (-- *  Cost Centre Profiling
     startProfTimer,
     stopProfTimer,
     -- *  Heap Profiling
     startHeapProfTimer,
     stopHeapProfTimer,
     requestHeapCensus
     ) where

import GHC.Internal.Profiling