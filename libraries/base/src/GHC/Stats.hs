{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe            #-}

-- |
-- Module      :  RTS.Stats
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- /The API of this module is unstable and is tightly coupled to GHC's internals./
-- If depend on it, make sure to use a tight upper bound, e.g., @base < 4.X@ rather
-- than @base < 5@, because the interface can change rapidly without much warning.
--
-- @since 4.5.0.0
--
-- This module is a compatibility layer. It is meant to be temporary to allow
-- for the eventual deprecation of these declarations as described in [CLC
-- proposal
-- #289](https://github.com/haskell/core-libraries-committee/issues/289). These
-- declarations are now instead available from the @ghc-experimental@ package.

module GHC.Stats
    ( -- * Runtime statistics
      RTSStats(..), GCDetails(..), RtsTime
    , getRTSStats
    , getRTSStatsEnabled
    ) where


import Prelude (Bool,IO,Read,Show,(<$>))

import qualified GHC.Internal.Stats as Internal
import GHC.Generics (Generic)
import Data.Word (Word64,Word32)
import Data.Int (Int64)

-- | Time values from the RTS, using a fixed resolution of nanoseconds.
type RtsTime = Int64

--
-- | Statistics about runtime activity since the start of the
-- program.  This is a mirror of the C @struct RTSStats@ in @RtsAPI.h@
--
-- @since base-4.10.0.0
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
    -- @since base-4.12.0.0
  , init_cpu_ns :: RtsTime
    -- | Total elapsed time used by the init phase
    -- @since base-4.12.0.0
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

    -- | The total CPU time used during the post-mark pause phase of the
    -- concurrent nonmoving GC.
  , nonmoving_gc_sync_cpu_ns :: RtsTime
    -- | The total time elapsed during the post-mark pause phase of the
    -- concurrent nonmoving GC.
  , nonmoving_gc_sync_elapsed_ns :: RtsTime
    -- | The maximum elapsed length of any post-mark pause phase of the
    -- concurrent nonmoving GC.
  , nonmoving_gc_sync_max_elapsed_ns :: RtsTime
    -- | The total CPU time used by the nonmoving GC.
  , nonmoving_gc_cpu_ns :: RtsTime
    -- | The total time elapsed during which there is a nonmoving GC active.
  , nonmoving_gc_elapsed_ns :: RtsTime
    -- | The maximum time elapsed during any nonmoving GC cycle.
  , nonmoving_gc_max_elapsed_ns :: RtsTime

    -- | Details about the most recent GC
  , gc :: GCDetails
  } deriving ( Read -- ^ @since base-4.10.0.0
             , Show -- ^ @since base-4.10.0.0
             , Generic -- ^ @since base-4.15.0.0
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
    -- | Total amount of live data in the heap (includes large + compact data).
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
    -- | The amount of memory lost due to block fragmentation in bytes.
    -- Block fragmentation is the difference between the amount of blocks retained by the RTS and the blocks that are in use.
    -- This occurs when megablocks are only sparsely used, eg, when data that cannot be moved retains a megablock.
    --
    -- @since base-4.18.0.0
  , gcdetails_block_fragmentation_bytes :: Word64
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
  } deriving ( Read -- ^ @since base-4.10.0.0
             , Show -- ^ @since base-4.10.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-------------------------------- compat ----------------------------------------

internal_to_base_RTSStats :: Internal.RTSStats -> RTSStats
internal_to_base_RTSStats i@Internal.RTSStats{..} =
  let gc_details = internal_to_base_GCDetails (Internal.gc i)
  in RTSStats{gc = gc_details,..}

internal_to_base_GCDetails :: Internal.GCDetails -> GCDetails
internal_to_base_GCDetails Internal.GCDetails{..} = GCDetails{..}

-------------------------------- shims -----------------------------------------

getRTSStats :: IO RTSStats
getRTSStats = internal_to_base_RTSStats <$> Internal.getRTSStats

getRTSStatsEnabled :: IO Bool
getRTSStatsEnabled = Internal.getRTSStatsEnabled
