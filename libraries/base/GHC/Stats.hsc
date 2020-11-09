{-# LANGUAGE DeriveGeneric #-}
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
) where

import Control.Monad
import Data.Int
import Data.Word
import GHC.Base
import GHC.Generics (Generic)
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
-- @since 4.10.0.0
foreign import ccall "getRTSStatsEnabled" getRTSStatsEnabled :: IO Bool

--
-- | Statistics about runtime activity since the start of the
-- program.  This is a mirror of the C @struct RTSStats@ in @RtsAPI.h@
--
-- @since 4.10.0.0
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
    -- | Maximum live data (including large objects + compact regions) in the
    -- heap. Updated after a major GC.
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
    -- | Sum of par_max_copied_bytes across all parallel GCs. Deprecated.
  , cumulative_par_max_copied_bytes :: Word64
    -- | Sum of par_balanced_copied bytes across all parallel GCs
  , cumulative_par_balanced_copied_bytes :: Word64

  -- -----------------------------------
  -- Cumulative stats about time use
  -- (we use signed values here because due to inaccuracies in timers
  -- the values can occasionally go slightly negative)

    -- | Total CPU time used by the init phase
    -- @since 4.12.0.0
  , init_cpu_ns :: RtsTime
    -- | Total elapsed time used by the init phase
    -- @since 4.12.0.0
  , init_elapsed_ns :: RtsTime
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

    -- | The CPU time used during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , nonmoving_gc_sync_cpu_ns :: RtsTime
    -- | The time elapsed during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , nonmoving_gc_sync_elapsed_ns :: RtsTime
    -- | The maximum time elapsed during the post-mark pause phase of the
    -- concurrent nonmoving GC.
  , nonmoving_gc_sync_max_elapsed_ns :: RtsTime
    -- | The CPU time used during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , nonmoving_gc_cpu_ns :: RtsTime
    -- | The time elapsed during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , nonmoving_gc_elapsed_ns :: RtsTime
    -- | The maximum time elapsed during the post-mark pause phase of the
    -- concurrent nonmoving GC.
  , nonmoving_gc_max_elapsed_ns :: RtsTime

    -- | Details about the most recent GC
  , gc :: GCDetails
  } deriving ( Read -- ^ @since 4.10.0.0
             , Show -- ^ @since 4.10.0.0
             , Generic -- ^ @since 4.15.0.0
             )

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
    -- | Total amount of live data in the heap (incliudes large + compact data).
    -- Updated after every GC. Data in uncollected generations (in minor GCs)
    -- are considered live.
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
    -- | In parallel GC, the max amount of data copied by any one thread.
    -- Deprecated.
  , gcdetails_par_max_copied_bytes :: Word64
    -- | In parallel GC, the amount of balanced data copied by all threads
  , gcdetails_par_balanced_copied_bytes :: Word64
    -- | The time elapsed during synchronisation before GC
  , gcdetails_sync_elapsed_ns :: RtsTime
    -- | The CPU time used during GC itself
  , gcdetails_cpu_ns :: RtsTime
    -- | The time elapsed during GC itself
  , gcdetails_elapsed_ns :: RtsTime

    -- | The CPU time used during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , gcdetails_nonmoving_gc_sync_cpu_ns :: RtsTime
    -- | The time elapsed during the post-mark pause phase of the concurrent
    -- nonmoving GC.
  , gcdetails_nonmoving_gc_sync_elapsed_ns :: RtsTime
  } deriving ( Read -- ^ @since 4.10.0.0
             , Show -- ^ @since 4.10.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | Time values from the RTS, using a fixed resolution of nanoseconds.
type RtsTime = Int64

-- | Get current runtime system statistics.
--
-- @since 4.10.0.0
--
getRTSStats :: IO RTSStats
getRTSStats = do
  statsEnabled <- getRTSStatsEnabled
  unless statsEnabled .  ioError $ IOError
    Nothing
    UnsupportedOperation
    ""
    "GHC.Stats.getRTSStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them."
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
    cumulative_par_balanced_copied_bytes <-
      (# peek RTSStats, cumulative_par_balanced_copied_bytes) p
    init_cpu_ns <- (# peek RTSStats, init_cpu_ns) p
    init_elapsed_ns <- (# peek RTSStats, init_elapsed_ns) p
    mutator_cpu_ns <- (# peek RTSStats, mutator_cpu_ns) p
    mutator_elapsed_ns <- (# peek RTSStats, mutator_elapsed_ns) p
    gc_cpu_ns <- (# peek RTSStats, gc_cpu_ns) p
    gc_elapsed_ns <- (# peek RTSStats, gc_elapsed_ns) p
    cpu_ns <- (# peek RTSStats, cpu_ns) p
    elapsed_ns <- (# peek RTSStats, elapsed_ns) p
    nonmoving_gc_sync_cpu_ns <- (# peek RTSStats, nonmoving_gc_sync_cpu_ns) p
    nonmoving_gc_sync_elapsed_ns <- (# peek RTSStats, nonmoving_gc_sync_elapsed_ns) p
    nonmoving_gc_sync_max_elapsed_ns <- (# peek RTSStats, nonmoving_gc_sync_max_elapsed_ns) p
    nonmoving_gc_cpu_ns <- (# peek RTSStats, nonmoving_gc_cpu_ns) p
    nonmoving_gc_elapsed_ns <- (# peek RTSStats, nonmoving_gc_elapsed_ns) p
    nonmoving_gc_max_elapsed_ns <- (# peek RTSStats, nonmoving_gc_max_elapsed_ns) p
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
      gcdetails_par_balanced_copied_bytes <-
        (# peek GCDetails, par_balanced_copied_bytes) pgc
      gcdetails_sync_elapsed_ns <- (# peek GCDetails, sync_elapsed_ns) pgc
      gcdetails_cpu_ns <- (# peek GCDetails, cpu_ns) pgc
      gcdetails_elapsed_ns <- (# peek GCDetails, elapsed_ns) pgc
      gcdetails_nonmoving_gc_sync_cpu_ns <- (# peek GCDetails, nonmoving_gc_sync_cpu_ns) pgc
      gcdetails_nonmoving_gc_sync_elapsed_ns <- (# peek GCDetails, nonmoving_gc_sync_elapsed_ns) pgc
      return GCDetails{..}
    return RTSStats{..}
