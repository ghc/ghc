{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  GHC.RTS.Flags
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- /The API of this module is unstable and is tightly coupled to GHC's internals./
-- If depend on it, make sure to use a tight upper bound, e.g., @base < 4.X@ rather
-- than @base < 5@, because the interface can change rapidly without much warning.
--
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime_control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
-- DYG todo

module GHC.RTS.Flags
  ( RtsTime
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , IoManagerFlag (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , ParFlags (..)
  , HpcFlags (..)
  , {-# DEPRECATED "import GHC.IO.SubSystem (IoSubSystem (..))" #-}
    IoSubSystem (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  , getParFlags
  , getHpcFlags
  ) where

import Prelude (Show,IO,Bool,Maybe,String,Int,Enum,FilePath,Double,Eq,(<$>))

import GHC.Generics (Generic)
import qualified GHC.Internal.RTS.Flags as Internal
import GHC.Internal.IO.SubSystem (IoSubSystem(..))

import Data.Word (Word32,Word)

-- | 'RtsTime' is defined as a @StgWord64@ in @stg/Types.h@
--
-- @since base-4.8.2.0
type RtsTime = Internal.RtsTime

-- | Should we produce a summary of the garbage collector statistics after the
-- program has exited?
--
-- @since base-4.8.2.0
data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving ( Show -- ^ @since base-4.8.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | Parameters of the garbage collector.
--
-- @since base-4.8.0.0
data GCFlags = GCFlags
    { statsFile             :: Maybe FilePath
    , giveStats             :: GiveGCStats
    , maxStkSize            :: Word32
    , initialStkSize        :: Word32
    , stkChunkSize          :: Word32
    , stkChunkBufferSize    :: Word32
    , maxHeapSize           :: Word32
    , minAllocAreaSize      :: Word32
    , largeAllocLim         :: Word32
    , nurseryChunkSize      :: Word32
    , minOldGenSize         :: Word32
    , heapSizeSuggestion    :: Word32
    , heapSizeSuggestionAuto :: Bool
    , oldGenFactor          :: Double
    , returnDecayFactor     :: Double
    , pcFreeHeap            :: Double
    , generations           :: Word32
    , squeezeUpdFrames      :: Bool
    , compact               :: Bool -- ^ True <=> "compact all the time"
    , compactThreshold      :: Double
    , sweep                 :: Bool
      -- ^ use "mostly mark-sweep" instead of copying for the oldest generation
    , ringBell              :: Bool
    , idleGCDelayTime       :: RtsTime
    , doIdleGC              :: Bool
    , heapBase              :: Word -- ^ address to ask the OS for memory
    , allocLimitGrace       :: Word
    , numa                  :: Bool
    , numaMask              :: Word
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Parameters concerning context switching
--
-- @since base-4.8.0.0
data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: RtsTime
    , ctxtSwitchTicks :: Int
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Miscellaneous parameters
--
-- @since base-4.8.0.0
data MiscFlags = MiscFlags
    { tickInterval          :: RtsTime
    , installSignalHandlers :: Bool
    , installSEHHandlers    :: Bool
    , generateCrashDumpFile :: Bool
    , generateStackTrace    :: Bool
    , machineReadable       :: Bool
    , disableDelayedOsMemoryReturn :: Bool
    , internalCounters      :: Bool
    , linkerAlwaysPic       :: Bool
    , linkerMemBase         :: Word
      -- ^ address to ask the OS for memory for the linker, 0 ==> off
    , ioManager             :: IoManagerFlag
    , numIoWorkerThreads    :: Word32
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- |
--
-- @since base-4.21.0.0
data IoManagerFlag =
       IoManagerFlagAuto
     | IoManagerFlagSelect        -- ^ Unix only, non-threaded RTS only
     | IoManagerFlagMIO           -- ^ cross-platform, threaded RTS only
     | IoManagerFlagWinIO         -- ^ Windows only
     | IoManagerFlagWin32Legacy   -- ^ Windows only, non-threaded RTS only
  deriving (Eq, Enum, Show)

-- | Flags to control debugging output & extra checking in various
-- subsystems.
--
-- @since base-4.8.0.0
data DebugFlags = DebugFlags
    { scheduler      :: Bool -- ^ @s@
    , interpreter    :: Bool -- ^ @i@
    , weak           :: Bool -- ^ @w@
    , gccafs         :: Bool -- ^ @G@
    , gc             :: Bool -- ^ @g@
    , nonmoving_gc   :: Bool -- ^ @n@
    , block_alloc    :: Bool -- ^ @b@
    , sanity         :: Bool -- ^ @S@
    , stable         :: Bool -- ^ @t@
    , prof           :: Bool -- ^ @p@
    , linker         :: Bool -- ^ @l@ the object linker
    , apply          :: Bool -- ^ @a@
    , stm            :: Bool -- ^ @m@
    , squeeze        :: Bool -- ^ @z@ stack squeezing & lazy blackholing
    , hpc            :: Bool -- ^ @c@ coverage
    , sparks         :: Bool -- ^ @r@
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Should the RTS produce a cost-center summary?
--
-- @since base-4.8.2.0
data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresJSON
    deriving ( Show -- ^ @since base-4.8.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | Parameters pertaining to the cost-center profiler.
--
-- @since base-4.8.0.0
data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | What sort of heap profile are we collecting?
--
-- @since base-4.8.2.0
data DoHeapProfile
    = NoHeapProfiling
    | HeapByCCS
    | HeapByMod
    | HeapByDescr
    | HeapByType
    | HeapByRetainer
    | HeapByLDV
    | HeapByClosureType
    | HeapByInfoTable
    | HeapByEra -- ^ @since base-4.20.0.0
    deriving ( Show -- ^ @since base-4.8.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | Parameters of the cost-center profiler
--
-- @since base-4.8.0.0
data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: RtsTime -- ^ time between samples
    , heapProfileIntervalTicks :: Word    -- ^ ticks between samples (derived)
    , startHeapProfileAtStartup :: Bool
    , startTimeProfileAtStartup :: Bool   -- ^ @since base-4.20.0.0
    , showCCSOnException       :: Bool
    , automaticEraIncrement    :: Bool   -- ^ @since 4.20.0.0
    , maxRetainerSetSize       :: Word
    , ccsLength                :: Word
    , modSelector              :: Maybe String
    , descrSelector            :: Maybe String
    , typeSelector             :: Maybe String
    , ccSelector               :: Maybe String
    , ccsSelector              :: Maybe String
    , retainerSelector         :: Maybe String
    , bioSelector              :: Maybe String
    , eraSelector              :: Word -- ^ @since base-4.20.0.0
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Is event tracing enabled?
--
-- @since base-4.8.2.0
data DoTrace
    = TraceNone      -- ^ no tracing
    | TraceEventLog  -- ^ send tracing events to the event log
    | TraceStderr    -- ^ send tracing events to @stderr@
    deriving ( Show -- ^ @since base-4.8.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | Parameters pertaining to event tracing
--
-- @since base-4.8.0.0
data TraceFlags = TraceFlags
    { tracing        :: DoTrace
    , timestamp      :: Bool -- ^ show timestamp in stderr output
    , traceScheduler :: Bool -- ^ trace scheduler events
    , traceGc        :: Bool -- ^ trace GC events
    , traceNonmovingGc
                     :: Bool -- ^ trace nonmoving GC heap census samples
    , sparksSampled  :: Bool -- ^ trace spark events by a sampled method
    , sparksFull     :: Bool -- ^ trace spark events 100% accurately
    , user           :: Bool -- ^ trace user events (emitted from Haskell code)
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Parameters pertaining to ticky-ticky profiler
--
-- @since base-4.8.0.0
data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-- | Parameters pertaining to parallelism
--
-- @since base-4.8.0.0
data ParFlags = ParFlags
    { nCapabilities             :: Word32
    , migrate                   :: Bool
    , maxLocalSparks            :: Word32
    , parGcEnabled              :: Bool
    , parGcGen                  :: Word32
    , parGcLoadBalancingEnabled :: Bool
    , parGcLoadBalancingGen     :: Word32
    , parGcNoSyncWithIdle       :: Word32
    , parGcThreads              :: Word32
    , setAffinity               :: Bool
    }
    deriving ( Show -- ^ @since base-4.8.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | Parameters pertaining to Haskell program coverage (HPC)
--
-- @since base-4.20.0.0
data HpcFlags = HpcFlags
    { readTixFile :: Bool
      -- ^ Controls whether a @<program>.tix@ file is read at
      -- the start of execution to initialize the RTS internal
      -- HPC datastructures.
    , writeTixFile :: Bool
      -- ^ Controls whether the @<program>.tix@ file should be
      -- written after the execution of the program.
    }
    deriving (Show -- ^ @since base-4.20.0.0
             , Generic -- ^ @since base-4.20.0.0
             )
-- | Parameters of the runtime system
--
-- @since base-4.8.0.0
data RTSFlags = RTSFlags
    { gcFlags         :: GCFlags
    , concurrentFlags :: ConcFlags
    , miscFlags       :: MiscFlags
    , debugFlags      :: DebugFlags
    , costCentreFlags :: CCFlags
    , profilingFlags  :: ProfFlags
    , traceFlags      :: TraceFlags
    , tickyFlags      :: TickyFlags
    , parFlags        :: ParFlags
    , hpcFlags        :: HpcFlags
    } deriving ( Show -- ^ @since base-4.8.0.0
               , Generic -- ^ @since base-4.15.0.0
               )

-------------------------------- compat ----------------------------------------

internal_to_base_RTSFlags :: Internal.RTSFlags -> RTSFlags
internal_to_base_RTSFlags Internal.RTSFlags{..} =
  RTSFlags{ gcFlags         = internal_to_base_GCFlags    gcFlags
          , concurrentFlags = internal_to_base_ConcFlags  concurrentFlags
          , miscFlags       = internal_to_base_MiscFlags  miscFlags
          , debugFlags      = internal_to_base_DebugFlags debugFlags
          , costCentreFlags = internal_to_base_CCFlags    costCentreFlags
          , profilingFlags  = internal_to_base_ProfFlags  profilingFlags
          , traceFlags      = internal_to_base_TraceFlags traceFlags
          , tickyFlags      = internal_to_base_TickyFlags tickyFlags
          , parFlags        = internal_to_base_ParFlags   parFlags
          , hpcFlags        = internal_to_base_HpcFlags   hpcFlags
          }

internal_to_base_GCFlags :: Internal.GCFlags -> GCFlags
internal_to_base_GCFlags i@Internal.GCFlags{..} =
  let give_stats = internal_to_base_giveStats (Internal.giveStats i)
  in GCFlags{ giveStats = give_stats, .. }
  where
    internal_to_base_giveStats :: Internal.GiveGCStats -> GiveGCStats
    internal_to_base_giveStats Internal.NoGCStats      = NoGCStats
    internal_to_base_giveStats Internal.CollectGCStats = CollectGCStats
    internal_to_base_giveStats Internal.OneLineGCStats = OneLineGCStats
    internal_to_base_giveStats Internal.SummaryGCStats = SummaryGCStats
    internal_to_base_giveStats Internal.VerboseGCStats = VerboseGCStats

internal_to_base_ParFlags :: Internal.ParFlags -> ParFlags
internal_to_base_ParFlags Internal.ParFlags{..} = ParFlags{..}

internal_to_base_HpcFlags :: Internal.HpcFlags -> HpcFlags
internal_to_base_HpcFlags Internal.HpcFlags{..} = HpcFlags{..}

internal_to_base_ConcFlags :: Internal.ConcFlags -> ConcFlags
internal_to_base_ConcFlags Internal.ConcFlags{..} = ConcFlags{..}

internal_to_base_MiscFlags :: Internal.MiscFlags -> MiscFlags
internal_to_base_MiscFlags i@Internal.MiscFlags{..} =
  let io_manager = internal_to_base_ioManager (Internal.ioManager i)
  in MiscFlags{ ioManager = io_manager, ..}
  where
    internal_to_base_ioManager :: Internal.IoManagerFlag -> IoManagerFlag
    internal_to_base_ioManager Internal.IoManagerFlagAuto        = IoManagerFlagAuto
    internal_to_base_ioManager Internal.IoManagerFlagSelect      = IoManagerFlagSelect
    internal_to_base_ioManager Internal.IoManagerFlagMIO         = IoManagerFlagMIO
    internal_to_base_ioManager Internal.IoManagerFlagWinIO       = IoManagerFlagWinIO
    internal_to_base_ioManager Internal.IoManagerFlagWin32Legacy = IoManagerFlagWin32Legacy

internal_to_base_DebugFlags :: Internal.DebugFlags -> DebugFlags
internal_to_base_DebugFlags Internal.DebugFlags{..} = DebugFlags{..}

internal_to_base_CCFlags :: Internal.CCFlags -> CCFlags
internal_to_base_CCFlags i@Internal.CCFlags{..} =
  let do_cost_centres = internal_to_base_costCentres (Internal.doCostCentres i)
  in CCFlags{ doCostCentres = do_cost_centres, ..}
  where
    internal_to_base_costCentres :: Internal.DoCostCentres -> DoCostCentres
    internal_to_base_costCentres Internal.CostCentresNone    = CostCentresNone
    internal_to_base_costCentres Internal.CostCentresSummary = CostCentresSummary
    internal_to_base_costCentres Internal.CostCentresVerbose = CostCentresVerbose
    internal_to_base_costCentres Internal.CostCentresAll     = CostCentresAll
    internal_to_base_costCentres Internal.CostCentresJSON    = CostCentresJSON

internal_to_base_ProfFlags :: Internal.ProfFlags -> ProfFlags
internal_to_base_ProfFlags i@Internal.ProfFlags{..} =
  let do_heap_profile = internal_to_base_doHeapProfile (Internal.doHeapProfile i)
  in ProfFlags{ doHeapProfile = do_heap_profile,..}
  where
    internal_to_base_doHeapProfile :: Internal.DoHeapProfile -> DoHeapProfile
    internal_to_base_doHeapProfile Internal.NoHeapProfiling   = NoHeapProfiling
    internal_to_base_doHeapProfile Internal.HeapByCCS         = HeapByCCS
    internal_to_base_doHeapProfile Internal.HeapByMod         = HeapByMod
    internal_to_base_doHeapProfile Internal.HeapByDescr       = HeapByDescr
    internal_to_base_doHeapProfile Internal.HeapByType        = HeapByType
    internal_to_base_doHeapProfile Internal.HeapByRetainer    = HeapByRetainer
    internal_to_base_doHeapProfile Internal.HeapByLDV         = HeapByLDV
    internal_to_base_doHeapProfile Internal.HeapByClosureType = HeapByClosureType
    internal_to_base_doHeapProfile Internal.HeapByInfoTable   = HeapByInfoTable
    internal_to_base_doHeapProfile Internal.HeapByEra         = HeapByEra

internal_to_base_TraceFlags :: Internal.TraceFlags -> TraceFlags
internal_to_base_TraceFlags i@Internal.TraceFlags{..} =
  let do_trace = internal_to_base_doTrace (Internal.tracing i)
  in TraceFlags{ tracing = do_trace,..}
  where
    internal_to_base_doTrace :: Internal.DoTrace -> DoTrace
    internal_to_base_doTrace Internal.TraceNone     = TraceNone
    internal_to_base_doTrace Internal.TraceEventLog = TraceEventLog
    internal_to_base_doTrace Internal.TraceStderr   = TraceStderr

internal_to_base_TickyFlags :: Internal.TickyFlags -> TickyFlags
internal_to_base_TickyFlags Internal.TickyFlags{..} = TickyFlags{..}

-------------------------------- shims -----------------------------------------

getRTSFlags :: IO RTSFlags
getRTSFlags = internal_to_base_RTSFlags <$> Internal.getRTSFlags

getGCFlags :: IO GCFlags
getGCFlags = internal_to_base_GCFlags <$> Internal.getGCFlags

getParFlags :: IO ParFlags
getParFlags = internal_to_base_ParFlags <$> Internal.getParFlags

getHpcFlags :: IO HpcFlags
getHpcFlags = internal_to_base_HpcFlags <$> Internal.getHpcFlags

getConcFlags :: IO ConcFlags
getConcFlags =  internal_to_base_ConcFlags <$> Internal.getConcFlags

{-# INLINEABLE getMiscFlags #-}
getMiscFlags :: IO MiscFlags
getMiscFlags = internal_to_base_MiscFlags <$> Internal.getMiscFlags

getDebugFlags :: IO DebugFlags
getDebugFlags = internal_to_base_DebugFlags <$> Internal.getDebugFlags

getCCFlags :: IO CCFlags
getCCFlags = internal_to_base_CCFlags <$> Internal.getCCFlags

getProfFlags :: IO ProfFlags
getProfFlags = internal_to_base_ProfFlags <$> Internal.getProfFlags

getTraceFlags :: IO TraceFlags
getTraceFlags = internal_to_base_TraceFlags <$> Internal.getTraceFlags

getTickyFlags :: IO TickyFlags
getTickyFlags = internal_to_base_TickyFlags <$> Internal.getTickyFlags
