{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- | This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- This module is GHC-only and should not be considered portable.
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------
module GHC.Stats
    (
    -- * Runtime statistics
      RTSStats(..), GCDetails(..), RtsTime
    , getRTSStats
    , getRTSStatsEnabled

    -- * DEPRECATED, don't use
    , GCStats(..)
    , getGCStats
    , getGCStatsEnabled
) where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Word
import GHC.Base
import GHC.Num (Num(..))
import GHC.Real (quot, fromIntegral, (/))
import GHC.Read ( Read )
import GHC.Show ( Show )
import GHC.IO.Exception
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

#include "Rts.h"

foreign import ccall "getRTSStats" getRTSStats_ :: Ptr () -> IO ()

-- | Returns whether GC stats have been enabled (with @+RTS -T@, for example).
--
-- @since 4.9.0.0
foreign import ccall "getRTSStatsEnabled" getRTSStatsEnabled :: IO Bool

--
-- | Statistics about runtime activity since the start of the
-- program.  This is a mirror of the C @struct RTSStats@ in @RtsAPI.h@
--
-- @since 4.9.0.0
--
data RTSStats = RTSStats {
  -- -----------------------------------
  -- Cumulative stats about memory use

    -- | Total number of GCs
    gcs :: Word32
    -- | Total number of major (oldest generation) GCs
  , major_gcs :: Word32
    -- | Total bytes allocated
  , allocated_bytes :: Word64
    -- | Maximum live data (including large objects + compact regions)
  , max_live_bytes :: Word64
    -- | Maximum live data in large objects
  , max_large_objects_bytes :: Word64
    -- | Maximum live data in compact regions
  , max_compact_bytes :: Word64
    -- | Maximum slop
  , max_slop_bytes :: Word64
    -- | Maximum memory in use by the RTS
  , max_mem_in_use_bytes :: Word64
    -- | Sum of live bytes across all major GCs.  Divided by major_gcs
    -- gives the average live data over the lifetime of the program.
  , cumulative_live_bytes :: Word64
    -- | Sum of copied_bytes across all GCs
  , copied_bytes :: Word64
    -- | Sum of copied_bytes across all parallel GCs
  , par_copied_bytes :: Word64
    -- | Sum of par_max_copied_bytes across all parallel GCs
  , cumulative_par_max_copied_bytes :: Word64

  -- -----------------------------------
  -- Cumulative stats about time use
  -- (we use signed values here because due to inaccuracies in timers
  -- the values can occasionally go slightly negative)

    -- | Total CPU time used by the mutator
  , mutator_cpu_ns :: RtsTime
    -- | Total elapsed time used by the mutator
  , mutator_elapsed_ns :: RtsTime
    -- | Total CPU time used by the GC
  , gc_cpu_ns :: RtsTime
    -- | Total elapsed time used by the GC
  , gc_elapsed_ns :: RtsTime
    -- | Total CPU time (at the previous GC)
  , cpu_ns :: RtsTime
    -- | Total elapsed time (at the previous GC)
  , elapsed_ns :: RtsTime

    -- | Details about the most recent GC
  , gc :: GCDetails
  } deriving (Read, Show)

--
-- | Statistics about a single GC.  This is a mirror of the C @struct
--   GCDetails@ in @RtsAPI.h@, with the field prefixed with @gc_@ to
--   avoid collisions with 'RTSStats'.
--
data GCDetails = GCDetails {
    -- | The generation number of this GC
    gcdetails_gen :: Word32
    -- | Number of threads used in this GC
  , gcdetails_threads :: Word32
    -- | Number of bytes allocated since the previous GC
  , gcdetails_allocated_bytes :: Word64
    -- | Total amount of live data in the heap (incliudes large + compact data)
  , gcdetails_live_bytes :: Word64
    -- | Total amount of live data in large objects
  , gcdetails_large_objects_bytes :: Word64
    -- | Total amount of live data in compact regions
  , gcdetails_compact_bytes :: Word64
    -- | Total amount of slop (wasted memory)
  , gcdetails_slop_bytes :: Word64
    -- | Total amount of memory in use by the RTS
  , gcdetails_mem_in_use_bytes :: Word64
    -- | Total amount of data copied during this GC
  , gcdetails_copied_bytes :: Word64
    -- | In parallel GC, the max amount of data copied by any one thread
  , gcdetails_par_max_copied_bytes :: Word64
    -- | The time elapsed during synchronisation before GC
  , gcdetails_sync_elapsed_ns :: RtsTime
    -- | The CPU time used during GC itself
  , gcdetails_cpu_ns :: RtsTime
    -- | The time elapsed during GC itself
  , gcdetails_elapsed_ns :: RtsTime
  } deriving (Read, Show)

-- | Time values from the RTS, using a fixed resolution of nanoseconds.
type RtsTime = Int64

-- @since 4.9.0.0
--
getRTSStats :: IO RTSStats
getRTSStats = do
  statsEnabled <- getGCStatsEnabled
  unless statsEnabled .  ioError $ IOError
    Nothing
    UnsupportedOperation
    ""
    "getGCStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them."
    Nothing
    Nothing
  allocaBytes (#size RTSStats) $ \p -> do
    getRTSStats_ p
    gcs <- (# peek RTSStats, gcs) p
    major_gcs <- (# peek RTSStats, major_gcs) p
    allocated_bytes <- (# peek RTSStats, allocated_bytes) p
    max_live_bytes <- (# peek RTSStats, max_live_bytes) p
    max_large_objects_bytes <- (# peek RTSStats, max_large_objects_bytes) p
    max_compact_bytes <- (# peek RTSStats, max_compact_bytes) p
    max_slop_bytes <- (# peek RTSStats, max_slop_bytes) p
    max_mem_in_use_bytes <- (# peek RTSStats, max_mem_in_use_bytes) p
    cumulative_live_bytes <- (# peek RTSStats, cumulative_live_bytes) p
    copied_bytes <- (# peek RTSStats, copied_bytes) p
    par_copied_bytes <- (# peek RTSStats, par_copied_bytes) p
    cumulative_par_max_copied_bytes <-
      (# peek RTSStats, cumulative_par_max_copied_bytes) p
    mutator_cpu_ns <- (# peek RTSStats, mutator_cpu_ns) p
    mutator_elapsed_ns <- (# peek RTSStats, mutator_elapsed_ns) p
    gc_cpu_ns <- (# peek RTSStats, gc_cpu_ns) p
    gc_elapsed_ns <- (# peek RTSStats, gc_elapsed_ns) p
    cpu_ns <- (# peek RTSStats, cpu_ns) p
    elapsed_ns <- (# peek RTSStats, elapsed_ns) p
    let pgc = (# ptr RTSStats, gc) p
    gc <- do
      gcdetails_gen <- (# peek GCDetails, gen) pgc
      gcdetails_threads <- (# peek GCDetails, threads) pgc
      gcdetails_allocated_bytes <- (# peek GCDetails, allocated_bytes) pgc
      gcdetails_live_bytes <- (# peek GCDetails, live_bytes) pgc
      gcdetails_large_objects_bytes <-
        (# peek GCDetails, large_objects_bytes) pgc
      gcdetails_compact_bytes <- (# peek GCDetails, compact_bytes) pgc
      gcdetails_slop_bytes <- (# peek GCDetails, slop_bytes) pgc
      gcdetails_mem_in_use_bytes <- (# peek GCDetails, mem_in_use_bytes) pgc
      gcdetails_copied_bytes <- (# peek GCDetails, copied_bytes) pgc
      gcdetails_par_max_copied_bytes <-
        (# peek GCDetails, par_max_copied_bytes) pgc
      gcdetails_sync_elapsed_ns <- (# peek GCDetails, sync_elapsed_ns) pgc
      gcdetails_cpu_ns <- (# peek GCDetails, cpu_ns) pgc
      gcdetails_elapsed_ns <- (# peek GCDetails, elapsed_ns) pgc
      return GCDetails{..}
    return RTSStats{..}

-- -----------------------------------------------------------------------------
-- DEPRECATED API

-- I'm probably violating a bucket of constraints here... oops.

-- | Statistics about memory usage and the garbage collector. Apart from
-- 'currentBytesUsed' and 'currentBytesSlop' all are cumulative values since
-- the program started.
--
-- @since 4.5.0.0
{-# DEPRECATED GCStats "Use RTSStats instead.  This will be removed in GHC 8.4.1" #-}
data GCStats = GCStats
    { -- | Total number of bytes allocated
    bytesAllocated :: !Int64
    -- | Number of garbage collections performed (any generation, major and
    -- minor)
    , numGcs :: !Int64
    -- | Maximum number of live bytes seen so far
    , maxBytesUsed :: !Int64
    -- | Number of byte usage samples taken, or equivalently
    -- the number of major GCs performed.
    , numByteUsageSamples :: !Int64
    -- | Sum of all byte usage samples, can be used with
    -- 'numByteUsageSamples' to calculate averages with
    -- arbitrary weighting (if you are sampling this record multiple
    -- times).
    , cumulativeBytesUsed :: !Int64
    -- | Number of bytes copied during GC
    , bytesCopied :: !Int64
    -- | Number of live bytes at the end of the last major GC
    , currentBytesUsed :: !Int64
    -- | Current number of bytes lost to slop
    , currentBytesSlop :: !Int64
    -- | Maximum number of bytes lost to slop at any one time so far
    , maxBytesSlop :: !Int64
    -- | Maximum number of megabytes allocated
    , peakMegabytesAllocated :: !Int64
    -- | CPU time spent running mutator threads.  This does not include
    -- any profiling overhead or initialization.
    , mblocksAllocated :: !Int64 -- ^ Number of allocated megablocks
    , mutatorCpuSeconds :: !Double

    -- | Wall clock time spent running mutator threads.  This does not
    -- include initialization.
    , mutatorWallSeconds :: !Double
    -- | CPU time spent running GC
    , gcCpuSeconds :: !Double
    -- | Wall clock time spent running GC
    , gcWallSeconds :: !Double
    -- | Total CPU time elapsed since program start
    , cpuSeconds :: !Double
    -- | Total wall clock time elapsed since start
    , wallSeconds :: !Double
    -- | Number of bytes copied during GC, minus space held by mutable
    -- lists held by the capabilities.  Can be used with
    -- 'parMaxBytesCopied' to determine how well parallel GC utilized
    -- all cores.
    , parTotBytesCopied :: !Int64

    -- | Sum of number of bytes copied each GC by the most active GC
    -- thread each GC.  The ratio of 'parTotBytesCopied' divided by
    -- 'parMaxBytesCopied' approaches 1 for a maximally sequential
    -- run and approaches the number of threads (set by the RTS flag
    -- @-N@) for a maximally parallel run.
    , parMaxBytesCopied :: !Int64
    } deriving (Show, Read)

-- | Retrieves garbage collection and memory statistics as of the last
-- garbage collection.  If you would like your statistics as recent as
-- possible, first run a 'System.Mem.performGC'.
--
-- @since 4.5.0.0
{-# DEPRECATED getGCStats
    "Use getRTSStats instead.  This will be removed in GHC 8.4.1" #-}
getGCStats :: IO GCStats
getGCStats = do
  statsEnabled <- getGCStatsEnabled
  unless statsEnabled .  ioError $ IOError
    Nothing
    UnsupportedOperation
    ""
    "getGCStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them."
    Nothing
    Nothing
  allocaBytes (#size RTSStats) $ \p -> do
    getRTSStats_ p
    bytesAllocated <- (# peek RTSStats, allocated_bytes) p
    numGcs <- (# peek RTSStats, gcs ) p
    numByteUsageSamples <- (# peek RTSStats, major_gcs ) p
    maxBytesUsed <- (# peek RTSStats, max_live_bytes ) p
    cumulativeBytesUsed <- (# peek RTSStats, cumulative_live_bytes ) p
    bytesCopied <- (# peek RTSStats, copied_bytes ) p
    currentBytesUsed <- (# peek RTSStats, gc.live_bytes ) p
    currentBytesSlop <- (# peek RTSStats, gc.slop_bytes) p
    maxBytesSlop <- (# peek RTSStats, max_slop_bytes) p
    peakMegabytesAllocated <- do
      bytes <- (# peek RTSStats, max_mem_in_use_bytes ) p
      return (bytes `quot` (1024*1024))
    mblocksAllocated <- do
      bytes <- (# peek RTSStats, gc.mem_in_use_bytes) p
      return (bytes `quot` (1024*1024))
    mutatorCpuSeconds <- nsToSecs <$> (# peek RTSStats, mutator_cpu_ns) p
    mutatorWallSeconds <-
      nsToSecs <$> (# peek RTSStats, mutator_elapsed_ns) p
    gcCpuSeconds <- nsToSecs <$> (# peek RTSStats, gc_cpu_ns) p
    gcWallSeconds <- nsToSecs <$> (# peek RTSStats, gc_elapsed_ns) p
    cpuSeconds <- nsToSecs <$> (# peek RTSStats, cpu_ns) p
    wallSeconds <- nsToSecs <$> (# peek RTSStats, elapsed_ns) p
    parTotBytesCopied <- (# peek RTSStats, par_copied_bytes) p
    parMaxBytesCopied <- (# peek RTSStats, cumulative_par_max_copied_bytes) p
    return GCStats { .. }

nsToSecs :: Int64 -> Double
nsToSecs ns = fromIntegral ns / (# const TIME_RESOLUTION)

{-# DEPRECATED getGCStatsEnabled
    "use getRTSStatsEnabled instead.  This will be removed in GHC 8.4.1" #-}
getGCStatsEnabled :: IO Bool
getGCStatsEnabled = getRTSStatsEnabled
