{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Stats
    ( GCStats(..)
    , getGCStats
) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.Mem
import Data.Int

#include "Rts.h"

foreign import ccall "getGCStats"    getGCStats_    :: Ptr () -> IO ()

-- I'm probably violating a bucket of constraints here... oops.

-- | Global garbage collection and memory statistics.
data GCStats = GCStats
    { bytes_allocated :: Int64 -- ^ Total number of bytes allocated
    , num_gcs :: Int64 -- ^ Number of garbage collections performed
    , max_bytes_used :: Int64 -- ^ Maximum number of live bytes seen so far
    , num_byte_usage_samples :: Int64 -- ^ Number of byte usage samples taken
    -- | Sum of all byte usage samples, can be used with
    -- 'num_byte_usage_samples' to calculate averages with
    -- arbitrary weighting (if you are sampling this record multiple
    -- times).
    , cumulative_bytes_used :: Int64
    , bytes_copied :: Int64 -- ^ Number of bytes copied during GC
    , current_bytes_used :: Int64 -- ^ Current number of live bytes
    , current_bytes_slop :: Int64 -- ^ Current number of bytes lost to slop
    , max_bytes_slop :: Int64 -- ^ Maximum number of bytes lost to slop at any one time so far
    , peak_megabytes_allocated :: Int64 -- ^ Maximum number of megabytes allocated
    -- | CPU time spent running mutator threads.  This does not include
    -- any profiling overhead or initialization.
    , mutator_cpu_seconds :: Double
    -- | Wall clock time spent running mutator threads.  This does not
    -- include initialization.
    , mutator_wall_seconds :: Double
    , gc_cpu_seconds :: Double -- ^ CPU time spent running GC
    , gc_wall_seconds :: Double -- ^ Wall clock time spent running GC
    -- | Number of bytes copied during GC, minus space held by mutable
    -- lists held by the capabilities.  Can be used with
    -- 'par_max_bytes_copied' to determine how well parallel GC utilized
    -- all cores.
    , par_avg_bytes_copied :: Int64
    -- | Sum of number of bytes copied each GC by the most active GC
    -- thread each GC.  The ratio of 'par_avg_bytes_copied' divided by
    -- 'par_max_bytes_copied' approaches 1 for a maximally sequential
    -- run and approaches the number of threads (set by the RTS flag
    -- @-N@) for a maximally parallel run.
    , par_max_bytes_copied :: Int64
    } deriving (Show, Read)

    {-
    , g_init_cpu_seconds :: Double
    , g_init_wall_seconds :: Double
    -}

-- | Retrieves garbage collection and memory statistics as of the last
-- garbage collection.  If you would like your statistics as recent as
-- possible, first run a 'performGC' from "System.Mem".
getGCStats = allocaBytes (#size GCStats) $ \p -> do
    getGCStats_ p
    bytes_allocated <- (# peek GCStats, bytes_allocated) p
    num_gcs <- (# peek GCStats, num_gcs ) p
    num_byte_usage_samples <- (# peek GCStats, num_byte_usage_samples ) p
    max_bytes_used <- (# peek GCStats, max_bytes_used ) p
    cumulative_bytes_used <- (# peek GCStats, cumulative_bytes_used ) p
    bytes_copied <- (# peek GCStats, bytes_copied ) p
    current_bytes_used <- (# peek GCStats, current_bytes_used ) p
    current_bytes_slop <- (# peek GCStats, current_bytes_slop) p
    max_bytes_slop <- (# peek GCStats, max_bytes_slop) p
    peak_megabytes_allocated <- (# peek GCStats, peak_megabytes_allocated ) p
    {-
    init_cpu_seconds <- (# peek GCStats, init_cpu_seconds) p
    init_wall_seconds <- (# peek GCStats, init_wall_seconds) p
    -}
    mutator_cpu_seconds <- (# peek GCStats, mutator_cpu_seconds) p
    mutator_wall_seconds <- (# peek GCStats, mutator_wall_seconds) p
    gc_cpu_seconds <- (# peek GCStats, gc_cpu_seconds) p
    gc_wall_seconds <- (# peek GCStats, gc_wall_seconds) p
    par_avg_bytes_copied <- (# peek GCStats, par_avg_bytes_copied) p
    par_max_bytes_copied <- (# peek GCStats, par_max_bytes_copied) p
    return GCStats { .. }

{-

-- Nontrivial to implement: TaskStats needs arbitrarily large
-- amounts of memory, spark stats wants to use SparkCounters
-- but that needs a new rts/ header.

data TaskStats = TaskStats
    { task_mut_cpu_seconds :: Int64
    , task_mut_wall_seconds :: Int64
    , task_gc_cpu_seconds :: Int64
    , task_gc_wall_seconds :: Int64
    } deriving (Show, Read)

data SparkStats = SparkStats
    { sparks_created :: Int64
    , sparks_dud :: Int64
    , sparks_overflowed :: Int64
    , sparks_converted :: Int64
    , sparks_gcd :: Int64
    , sparks_fizzled :: Int64
    } deriving (Show, Read)

-- We also could get per-generation stats, which requires a
-- non-constant but at runtime known about of memory.

-}
