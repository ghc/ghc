{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  GHC.Internal.RTS.Flags
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
-- @since base-4.8.0.0
--
module GHC.Internal.RTS.Flags
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

#include "Rts.h"
#include "rts/Flags.h"

import GHC.Internal.Data.Functor ((<$>))
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.C.String
import GHC.Internal.Foreign.Marshal.Utils
import GHC.Internal.Foreign.Storable
import GHC.Internal.Ptr
import GHC.Internal.Word
import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Generics (Generic)
import GHC.Internal.IO
import GHC.Internal.Real
import GHC.Internal.Show

-- | 'RtsTime' is defined as a @StgWord64@ in @stg/Types.h@
--
-- @since base-4.8.2.0
type RtsTime = Word64

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

-- | @since base-4.8.0.0
instance Enum GiveGCStats where
    fromEnum NoGCStats      = #{const NO_GC_STATS}
    fromEnum CollectGCStats = #{const COLLECT_GC_STATS}
    fromEnum OneLineGCStats = #{const ONELINE_GC_STATS}
    fromEnum SummaryGCStats = #{const SUMMARY_GC_STATS}
    fromEnum VerboseGCStats = #{const VERBOSE_GC_STATS}

    toEnum #{const NO_GC_STATS}      = NoGCStats
    toEnum #{const COLLECT_GC_STATS} = CollectGCStats
    toEnum #{const ONELINE_GC_STATS} = OneLineGCStats
    toEnum #{const SUMMARY_GC_STATS} = SummaryGCStats
    toEnum #{const VERBOSE_GC_STATS} = VerboseGCStats
    toEnum e = errorWithoutStackTrace ("invalid enum for GiveGCStats: " ++ show e)

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

-- | @since base-4.8.0.0
instance Enum DoCostCentres where
    fromEnum CostCentresNone    = #{const COST_CENTRES_NONE}
    fromEnum CostCentresSummary = #{const COST_CENTRES_SUMMARY}
    fromEnum CostCentresVerbose = #{const COST_CENTRES_VERBOSE}
    fromEnum CostCentresAll     = #{const COST_CENTRES_ALL}
    fromEnum CostCentresJSON    = #{const COST_CENTRES_JSON}

    toEnum #{const COST_CENTRES_NONE}    = CostCentresNone
    toEnum #{const COST_CENTRES_SUMMARY} = CostCentresSummary
    toEnum #{const COST_CENTRES_VERBOSE} = CostCentresVerbose
    toEnum #{const COST_CENTRES_ALL}     = CostCentresAll
    toEnum #{const COST_CENTRES_JSON}    = CostCentresJSON
    toEnum e = errorWithoutStackTrace ("invalid enum for DoCostCentres: " ++ show e)

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

-- | @since base-4.8.0.0
instance Enum DoHeapProfile where
    fromEnum NoHeapProfiling   = #{const NO_HEAP_PROFILING}
    fromEnum HeapByCCS         = #{const HEAP_BY_CCS}
    fromEnum HeapByMod         = #{const HEAP_BY_MOD}
    fromEnum HeapByDescr       = #{const HEAP_BY_DESCR}
    fromEnum HeapByType        = #{const HEAP_BY_TYPE}
    fromEnum HeapByRetainer    = #{const HEAP_BY_RETAINER}
    fromEnum HeapByLDV         = #{const HEAP_BY_LDV}
    fromEnum HeapByClosureType = #{const HEAP_BY_CLOSURE_TYPE}
    fromEnum HeapByInfoTable   = #{const HEAP_BY_INFO_TABLE}
    fromEnum HeapByEra         = #{const HEAP_BY_ERA}

    toEnum #{const NO_HEAP_PROFILING}    = NoHeapProfiling
    toEnum #{const HEAP_BY_CCS}          = HeapByCCS
    toEnum #{const HEAP_BY_MOD}          = HeapByMod
    toEnum #{const HEAP_BY_DESCR}        = HeapByDescr
    toEnum #{const HEAP_BY_TYPE}         = HeapByType
    toEnum #{const HEAP_BY_RETAINER}     = HeapByRetainer
    toEnum #{const HEAP_BY_LDV}          = HeapByLDV
    toEnum #{const HEAP_BY_CLOSURE_TYPE} = HeapByClosureType
    toEnum #{const HEAP_BY_INFO_TABLE}   = HeapByInfoTable
    toEnum #{const HEAP_BY_ERA}          = HeapByEra
    toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

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

-- | @since base-4.8.0.0
instance Enum DoTrace where
    fromEnum TraceNone     = #{const TRACE_NONE}
    fromEnum TraceEventLog = #{const TRACE_EVENTLOG}
    fromEnum TraceStderr   = #{const TRACE_STDERR}

    toEnum #{const TRACE_NONE}     = TraceNone
    toEnum #{const TRACE_EVENTLOG} = TraceEventLog
    toEnum #{const TRACE_STDERR}   = TraceStderr
    toEnum e = errorWithoutStackTrace ("invalid enum for DoTrace: " ++ show e)

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
    { nCapabilities :: Word32
    , migrate :: Bool
    , maxLocalSparks :: Word32
    , parGcEnabled :: Bool
    , parGcGen :: Word32
    , parGcLoadBalancingEnabled :: Bool
    , parGcLoadBalancingGen :: Word32
    , parGcNoSyncWithIdle :: Word32
    , parGcThreads :: Word32
    , setAffinity :: Bool
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

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr RTSFlags

getRTSFlags :: IO RTSFlags
getRTSFlags =
  RTSFlags <$> getGCFlags
           <*> getConcFlags
           <*> getMiscFlags
           <*> getDebugFlags
           <*> getCCFlags
           <*> getProfFlags
           <*> getTraceFlags
           <*> getTickyFlags
           <*> getParFlags
           <*> getHpcFlags

peekFilePath :: Ptr () -> IO (Maybe FilePath)
peekFilePath ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = return (Just "<filepath>")

-- | Read a NUL terminated string. Return Nothing in case of a NULL pointer.
peekCStringOpt :: Ptr CChar -> IO (Maybe String)
peekCStringOpt ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekCString ptr

getGCFlags :: IO GCFlags
getGCFlags = do
  let ptr = (#ptr RTS_FLAGS, GcFlags) rtsFlagsPtr
  GCFlags <$> (peekFilePath =<< #{peek GC_FLAGS, statsFile} ptr)
          <*> (toEnum . fromIntegral <$>
                (#{peek GC_FLAGS, giveStats} ptr :: IO Word32))
          <*> #{peek GC_FLAGS, maxStkSize} ptr
          <*> #{peek GC_FLAGS, initialStkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkBufferSize} ptr
          <*> #{peek GC_FLAGS, maxHeapSize} ptr
          <*> #{peek GC_FLAGS, minAllocAreaSize} ptr
          <*> #{peek GC_FLAGS, largeAllocLim} ptr
          <*> #{peek GC_FLAGS, nurseryChunkSize} ptr
          <*> #{peek GC_FLAGS, minOldGenSize} ptr
          <*> #{peek GC_FLAGS, heapSizeSuggestion} ptr
          <*> (toBool <$>
                (#{peek GC_FLAGS, heapSizeSuggestionAuto} ptr :: IO CBool))
          <*> #{peek GC_FLAGS, oldGenFactor} ptr
          <*> #{peek GC_FLAGS, returnDecayFactor} ptr
          <*> #{peek GC_FLAGS, pcFreeHeap} ptr
          <*> #{peek GC_FLAGS, generations} ptr
          <*> (toBool <$>
                (#{peek GC_FLAGS, squeezeUpdFrames} ptr :: IO CBool))
          <*> (toBool <$>
                (#{peek GC_FLAGS, compact} ptr :: IO CBool))
          <*> #{peek GC_FLAGS, compactThreshold} ptr
          <*> (toBool <$>
                (#{peek GC_FLAGS, sweep} ptr :: IO CBool))
          <*> (toBool <$>
                (#{peek GC_FLAGS, ringBell} ptr :: IO CBool))
          <*> #{peek GC_FLAGS, idleGCDelayTime} ptr
          <*> (toBool <$>
                (#{peek GC_FLAGS, doIdleGC} ptr :: IO CBool))
          <*> #{peek GC_FLAGS, heapBase} ptr
          <*> #{peek GC_FLAGS, allocLimitGrace} ptr
          <*> (toBool <$>
                (#{peek GC_FLAGS, numa} ptr :: IO CBool))
          <*> #{peek GC_FLAGS, numaMask} ptr

getParFlags :: IO ParFlags
getParFlags = do
  let ptr = (#ptr RTS_FLAGS, ParFlags) rtsFlagsPtr
  ParFlags
    <$> #{peek PAR_FLAGS, nCapabilities} ptr
    <*> (toBool <$>
          (#{peek PAR_FLAGS, migrate} ptr :: IO CBool))
    <*> #{peek PAR_FLAGS, maxLocalSparks} ptr
    <*> (toBool <$>
          (#{peek PAR_FLAGS, parGcEnabled} ptr :: IO CBool))
    <*> #{peek PAR_FLAGS, parGcGen} ptr
    <*> (toBool <$>
          (#{peek PAR_FLAGS, parGcLoadBalancingEnabled} ptr :: IO CBool))
    <*> #{peek PAR_FLAGS, parGcLoadBalancingGen} ptr
    <*> #{peek PAR_FLAGS, parGcNoSyncWithIdle} ptr
    <*> #{peek PAR_FLAGS, parGcThreads} ptr
    <*> (toBool <$>
          (#{peek PAR_FLAGS, setAffinity} ptr :: IO CBool))


getHpcFlags :: IO HpcFlags
getHpcFlags = do
  let ptr = (#ptr RTS_FLAGS, HpcFlags) rtsFlagsPtr
  HpcFlags
    <$> (toBool <$>
          (#{peek HPC_FLAGS, readTixFile} ptr :: IO CBool))
    <*> (toBool <$>
          (#{peek HPC_FLAGS, writeTixFile} ptr :: IO CBool))

getConcFlags :: IO ConcFlags
getConcFlags = do
  let ptr = (#ptr RTS_FLAGS, ConcFlags) rtsFlagsPtr
  ConcFlags <$> #{peek CONCURRENT_FLAGS, ctxtSwitchTime} ptr
            <*> #{peek CONCURRENT_FLAGS, ctxtSwitchTicks} ptr

{-# INLINEABLE getMiscFlags #-}
getMiscFlags :: IO MiscFlags
getMiscFlags = do
  let ptr = (#ptr RTS_FLAGS, MiscFlags) rtsFlagsPtr
  MiscFlags <$> #{peek MISC_FLAGS, tickInterval} ptr
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, install_signal_handlers} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, install_seh_handlers} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, generate_dump_file} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, generate_stack_trace} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, machineReadable} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, disableDelayedOsMemoryReturn} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, internalCounters} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek MISC_FLAGS, linkerAlwaysPic} ptr :: IO CBool))
            <*> #{peek MISC_FLAGS, linkerMemBase} ptr
            <*> (toEnum . fromIntegral
                 <$> (#{peek MISC_FLAGS, ioManager} ptr :: IO Word32))
            <*> (fromIntegral
                 <$> (#{peek MISC_FLAGS, numIoWorkerThreads} ptr :: IO Word32))

getDebugFlags :: IO DebugFlags
getDebugFlags = do
  let ptr = (#ptr RTS_FLAGS, DebugFlags) rtsFlagsPtr
  DebugFlags <$> (toBool <$>
                   (#{peek DEBUG_FLAGS, scheduler} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, interpreter} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, weak} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, gccafs} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, gc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, nonmoving_gc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, block_alloc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, sanity} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, stable} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, prof} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, linker} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, apply} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, stm} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, squeeze} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, hpc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek DEBUG_FLAGS, sparks} ptr :: IO CBool))

getCCFlags :: IO CCFlags
getCCFlags = do
  let ptr = (#ptr RTS_FLAGS, GcFlags) rtsFlagsPtr
  CCFlags <$> (toEnum . fromIntegral
                <$> (#{peek COST_CENTRE_FLAGS, doCostCentres} ptr :: IO Word32))
          <*> #{peek COST_CENTRE_FLAGS, profilerTicks} ptr
          <*> #{peek COST_CENTRE_FLAGS, msecsPerTick} ptr

getProfFlags :: IO ProfFlags
getProfFlags = do
  let ptr = (#ptr RTS_FLAGS, ProfFlags) rtsFlagsPtr
  ProfFlags <$> (toEnum <$> #{peek PROFILING_FLAGS, doHeapProfile} ptr)
            <*> #{peek PROFILING_FLAGS, heapProfileInterval} ptr
            <*> #{peek PROFILING_FLAGS, heapProfileIntervalTicks} ptr
            <*> (toBool <$>
                  (#{peek PROFILING_FLAGS, startHeapProfileAtStartup} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek PROFILING_FLAGS, startTimeProfileAtStartup} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek PROFILING_FLAGS, showCCSOnException} ptr :: IO CBool))
            <*> (toBool <$>
                  (#{peek PROFILING_FLAGS, incrementUserEra} ptr :: IO CBool))
            <*> #{peek PROFILING_FLAGS, maxRetainerSetSize} ptr
            <*> #{peek PROFILING_FLAGS, ccsLength} ptr
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, modSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, descrSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, typeSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccsSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, retainerSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, bioSelector} ptr)
            <*> #{peek PROFILING_FLAGS, eraSelector} ptr

getTraceFlags :: IO TraceFlags
getTraceFlags = do
  let ptr = (#ptr RTS_FLAGS, TraceFlags) rtsFlagsPtr
  TraceFlags <$> (toEnum . fromIntegral
                   <$> (#{peek TRACE_FLAGS, tracing} ptr :: IO CInt))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, timestamp} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, scheduler} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, gc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, nonmoving_gc} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, sparks_sampled} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, sparks_full} ptr :: IO CBool))
             <*> (toBool <$>
                   (#{peek TRACE_FLAGS, user} ptr :: IO CBool))

getTickyFlags :: IO TickyFlags
getTickyFlags = do
  let ptr = (#ptr RTS_FLAGS, TickyFlags) rtsFlagsPtr
  TickyFlags <$> (toBool <$>
                   (#{peek TICKY_FLAGS, showTickyStats} ptr :: IO CBool))
             <*> (peekFilePath =<< #{peek TICKY_FLAGS, tickyFile} ptr)
