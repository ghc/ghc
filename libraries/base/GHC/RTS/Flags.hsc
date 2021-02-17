{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Accessors to GHC RTS flags.
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime_control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
module GHC.RTS.Flags
  ( RtsTime
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , ParFlags (..)
  , IoSubSystem (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getIoManagerFlag
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  , getParFlags
  ) where

#include "Rts.h"
#include "rts/Flags.h"

import Data.Functor ((<$>))

import Foreign
import Foreign.C

import GHC.Base
import GHC.Enum
import GHC.Generics (Generic)
import GHC.IO
import GHC.Real
import GHC.Show

-- | 'RtsTime' is defined as a @StgWord64@ in @stg/Types.h@
--
-- @since 4.8.2.0
type RtsTime = Word64

-- | Should we produce a summary of the garbage collector statistics after the
-- program has exited?
--
-- @since 4.8.2.0
data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving ( Show -- ^ @since 4.8.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | @since 4.8.0.0
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

-- | The I/O SubSystem to use in the program.
--
-- @since 4.9.0.0
data IoSubSystem
  = IoPOSIX   -- ^ Use a POSIX I/O Sub-System
  | IoNative  -- ^ Use platform native Sub-System. For unix OSes this is the
              --   same as IoPOSIX, but on Windows this means use the Windows
              --   native APIs for I/O, including IOCP and RIO.
  deriving (Eq, Show)

-- | @since 4.9.0.0
instance Enum IoSubSystem where
    fromEnum IoPOSIX  = #{const IO_MNGR_POSIX}
    fromEnum IoNative = #{const IO_MNGR_NATIVE}

    toEnum #{const IO_MNGR_POSIX}  = IoPOSIX
    toEnum #{const IO_MNGR_NATIVE} = IoNative
    toEnum e = errorWithoutStackTrace ("invalid enum for IoSubSystem: " ++ show e)

-- | @since 4.9.0.0
instance Storable IoSubSystem where
    sizeOf = sizeOf . fromEnum
    alignment = sizeOf . fromEnum
    peek ptr = fmap toEnum $ peek (castPtr ptr)
    poke ptr v = poke (castPtr ptr) (fromEnum v)

-- | Parameters of the garbage collector.
--
-- @since 4.8.0.0
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
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Parameters concerning context switching
--
-- @since 4.8.0.0
data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: RtsTime
    , ctxtSwitchTicks :: Int
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Miscellaneous parameters
--
-- @since 4.8.0.0
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
    , ioManager             :: IoSubSystem
    , numIoWorkerThreads    :: Word32
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Flags to control debugging output & extra checking in various
-- subsystems.
--
-- @since 4.8.0.0
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
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Should the RTS produce a cost-center summary?
--
-- @since 4.8.2.0
data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresJSON
    deriving ( Show -- ^ @since 4.8.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | @since 4.8.0.0
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
-- @since 4.8.0.0
data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | What sort of heap profile are we collecting?
--
-- @since 4.8.2.0
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
    deriving ( Show -- ^ @since 4.8.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | @since 4.8.0.0
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

    toEnum #{const NO_HEAP_PROFILING}    = NoHeapProfiling
    toEnum #{const HEAP_BY_CCS}          = HeapByCCS
    toEnum #{const HEAP_BY_MOD}          = HeapByMod
    toEnum #{const HEAP_BY_DESCR}        = HeapByDescr
    toEnum #{const HEAP_BY_TYPE}         = HeapByType
    toEnum #{const HEAP_BY_RETAINER}     = HeapByRetainer
    toEnum #{const HEAP_BY_LDV}          = HeapByLDV
    toEnum #{const HEAP_BY_CLOSURE_TYPE} = HeapByClosureType
    toEnum #{const HEAP_BY_INFO_TABLE}   = HeapByInfoTable
    toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

-- | Parameters of the cost-center profiler
--
-- @since 4.8.0.0
data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: RtsTime -- ^ time between samples
    , heapProfileIntervalTicks :: Word    -- ^ ticks between samples (derived)
    , startHeapProfileAtStartup :: Bool
    , showCCSOnException       :: Bool
    , maxRetainerSetSize       :: Word
    , ccsLength                :: Word
    , modSelector              :: Maybe String
    , descrSelector            :: Maybe String
    , typeSelector             :: Maybe String
    , ccSelector               :: Maybe String
    , ccsSelector              :: Maybe String
    , retainerSelector         :: Maybe String
    , bioSelector              :: Maybe String
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Is event tracing enabled?
--
-- @since 4.8.2.0
data DoTrace
    = TraceNone      -- ^ no tracing
    | TraceEventLog  -- ^ send tracing events to the event log
    | TraceStderr    -- ^ send tracing events to @stderr@
    deriving ( Show -- ^ @since 4.8.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | @since 4.8.0.0
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
-- @since 4.8.0.0
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
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Parameters pertaining to ticky-ticky profiler
--
-- @since 4.8.0.0
data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
               )

-- | Parameters pertaining to parallelism
--
-- @since 4.8.0.0
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
    deriving ( Show -- ^ @since 4.8.0.0
             , Generic -- ^ @since 4.15.0.0
             )

-- | Parameters of the runtime system
--
-- @since 4.8.0.0
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
    } deriving ( Show -- ^ @since 4.8.0.0
               , Generic -- ^ @since 4.15.0.0
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

{- Note [The need for getIoManagerFlag]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   GHC supports both the new WINIO manager
   as well as the old MIO one. In order to
   decide which code path to take we often
   have to inspect what the user selected at
   RTS startup.

   We could use getMiscFlags but then we end up with core containing
   reads for all MiscFlags. These won't be eliminated at the core level
   even if it's obvious we will only look at the ioManager part of the
   ADT.

   We could add a INLINE pragma, but that just means whatever we inline
   into is likely to be inlined. So rather than adding a dozen pragmas
   we expose a lean way to query this particular flag. It's not satisfying
   but it works well enough and allows these checks to be inlined nicely.

-}

{-# INLINE getIoManagerFlag #-}
-- | Needed to optimize support for different IO Managers on Windows.
-- See Note [The need for getIoManagerFlag]
getIoManagerFlag :: IO IoSubSystem
getIoManagerFlag = do
      let ptr = (#ptr RTS_FLAGS, MiscFlags) rtsFlagsPtr
      mgrFlag <- (#{peek MISC_FLAGS, ioManager} ptr :: IO Word32)
      return $ (toEnum . fromIntegral) mgrFlag

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
                  (#{peek PROFILING_FLAGS, showCCSOnException} ptr :: IO CBool))
            <*> #{peek PROFILING_FLAGS, maxRetainerSetSize} ptr
            <*> #{peek PROFILING_FLAGS, ccsLength} ptr
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, modSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, descrSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, typeSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccsSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, retainerSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, bioSelector} ptr)

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
