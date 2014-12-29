{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Accessors to GHC RTS flags.
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
module GHC.RTS.Flags
  ( RTSFlags (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , DebugFlags (..)
  , CCFlags (..)
  , ProfFlags (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  ) where

#include "Rts.h"
#include "rts/Flags.h"

import Control.Applicative
import Control.Monad

import Foreign.C.String    (peekCString)
import Foreign.C.Types     (CChar, CInt)
import Foreign.Ptr         (Ptr, nullPtr)
import Foreign.Storable    (peekByteOff)

import GHC.Base
import GHC.Enum
import GHC.IO
import GHC.Real
import GHC.Show
import GHC.Word

-- | @'Time'@ is defined as a @'StgWord64'@ in @stg/Types.h@
type Time = Word64

-- | @'nat'@ defined in @rts/Types.h@
type Nat = #{type unsigned int}

data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving (Show)

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
    toEnum e = error ("invalid enum for GiveGCStats: " ++ show e)

data GCFlags = GCFlags
    { statsFile             :: Maybe FilePath
    , giveStats             :: GiveGCStats
    , maxStkSize            :: Nat
    , initialStkSize        :: Nat
    , stkChunkSize          :: Nat
    , stkChunkBufferSize    :: Nat
    , maxHeapSize           :: Nat
    , minAllocAreaSize      :: Nat
    , minOldGenSize         :: Nat
    , heapSizeSuggestion    :: Nat
    , heapSizeSuggestionAuto :: Bool
    , oldGenFactor          :: Double
    , pcFreeHeap            :: Double
    , generations           :: Nat
    , steps                 :: Nat
    , squeezeUpdFrames      :: Bool
    , compact               :: Bool -- ^ True <=> "compact all the time"
    , compactThreshold      :: Double
    , sweep                 :: Bool
      -- ^ use "mostly mark-sweep" instead of copying for the oldest generation
    , ringBell              :: Bool
    , frontpanel            :: Bool
    , idleGCDelayTime       :: Time
    , doIdleGC              :: Bool
    , heapBase              :: Word -- ^ address to ask the OS for memory
    , allocLimitGrace       :: Word
    } deriving (Show)

data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: Time
    , ctxtSwitchTicks :: Int
    } deriving (Show)

data MiscFlags = MiscFlags
    { tickInterval          :: Time
    , installSignalHandlers :: Bool
    , machineReadable       :: Bool
    , linkerMemBase         :: Word
      -- ^ address to ask the OS for memory for the linker, 0 ==> off
    } deriving (Show)

-- | Flags to control debugging output & extra checking in various
-- subsystems.
data DebugFlags = DebugFlags
    { scheduler   :: Bool -- ^ 's'
    , interpreter :: Bool -- ^ 'i'
    , weak        :: Bool -- ^ 'w'
    , gccafs      :: Bool -- ^ 'G'
    , gc          :: Bool -- ^ 'g'
    , block_alloc :: Bool -- ^ 'b'
    , sanity      :: Bool -- ^ 'S'
    , stable      :: Bool -- ^ 't'
    , prof        :: Bool -- ^ 'p'
    , linker      :: Bool -- ^ 'l' the object linker
    , apply       :: Bool -- ^ 'a'
    , stm         :: Bool -- ^ 'm'
    , squeeze     :: Bool -- ^ 'z' stack squeezing & lazy blackholing
    , hpc         :: Bool -- ^ 'c' coverage
    , sparks      :: Bool -- ^ 'r'
    } deriving (Show)

data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresXML
    deriving (Show)

instance Enum DoCostCentres where
    fromEnum CostCentresNone    = #{const COST_CENTRES_NONE}
    fromEnum CostCentresSummary = #{const COST_CENTRES_SUMMARY}
    fromEnum CostCentresVerbose = #{const COST_CENTRES_VERBOSE}
    fromEnum CostCentresAll     = #{const COST_CENTRES_ALL}
    fromEnum CostCentresXML     = #{const COST_CENTRES_XML}

    toEnum #{const COST_CENTRES_NONE}    = CostCentresNone
    toEnum #{const COST_CENTRES_SUMMARY} = CostCentresSummary
    toEnum #{const COST_CENTRES_VERBOSE} = CostCentresVerbose
    toEnum #{const COST_CENTRES_ALL}     = CostCentresAll
    toEnum #{const COST_CENTRES_XML}     = CostCentresXML
    toEnum e = error ("invalid enum for DoCostCentres: " ++ show e)

data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving (Show)

data DoHeapProfile
    = NoHeapProfiling
    | HeapByCCS
    | HeapByMod
    | HeapByDescr
    | HeapByType
    | HeapByRetainer
    | HeapByLDV
    | HeapByClosureType
    deriving (Show)

instance Enum DoHeapProfile where
    fromEnum NoHeapProfiling   = #{const NO_HEAP_PROFILING}
    fromEnum HeapByCCS         = #{const HEAP_BY_CCS}
    fromEnum HeapByMod         = #{const HEAP_BY_MOD}
    fromEnum HeapByDescr       = #{const HEAP_BY_DESCR}
    fromEnum HeapByType        = #{const HEAP_BY_TYPE}
    fromEnum HeapByRetainer    = #{const HEAP_BY_RETAINER}
    fromEnum HeapByLDV         = #{const HEAP_BY_LDV}
    fromEnum HeapByClosureType = #{const HEAP_BY_CLOSURE_TYPE}

    toEnum #{const NO_HEAP_PROFILING}    = NoHeapProfiling
    toEnum #{const HEAP_BY_CCS}          = HeapByCCS
    toEnum #{const HEAP_BY_MOD}          = HeapByMod
    toEnum #{const HEAP_BY_DESCR}        = HeapByDescr
    toEnum #{const HEAP_BY_TYPE}         = HeapByType
    toEnum #{const HEAP_BY_RETAINER}     = HeapByRetainer
    toEnum #{const HEAP_BY_LDV}          = HeapByLDV
    toEnum #{const HEAP_BY_CLOSURE_TYPE} = HeapByClosureType
    toEnum e = error ("invalid enum for DoHeapProfile: " ++ show e)

data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: Time -- ^ time between samples
    , heapProfileIntervalTicks :: Word -- ^ ticks between samples (derived)
    , includeTSOs              :: Bool
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
    } deriving (Show)

data DoTrace
    = TraceNone
    | TraceEventLog
    | TraceStderr
    deriving (Show)

instance Enum DoTrace where
    fromEnum TraceNone     = #{const TRACE_NONE}
    fromEnum TraceEventLog = #{const TRACE_EVENTLOG}
    fromEnum TraceStderr   = #{const TRACE_STDERR}

    toEnum #{const TRACE_NONE}     = TraceNone
    toEnum #{const TRACE_EVENTLOG} = TraceEventLog
    toEnum #{const TRACE_STDERR}   = TraceStderr
    toEnum e = error ("invalid enum for DoTrace: " ++ show e)

data TraceFlags = TraceFlags
    { tracing        :: DoTrace
    , timestamp      :: Bool -- ^ show timestamp in stderr output
    , traceScheduler :: Bool -- ^ trace scheduler events
    , traceGc        :: Bool -- ^ trace GC events
    , sparksSampled  :: Bool -- ^ trace spark events by a sampled method
    , sparksFull     :: Bool -- ^ trace spark events 100% accurately
    , user           :: Bool -- ^ trace user events (emitted from Haskell code)
    } deriving (Show)

data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving (Show)

data RTSFlags = RTSFlags
    { gcFlags         :: GCFlags
    , concurrentFlags :: ConcFlags
    , miscFlags       :: MiscFlags
    , debugFlags      :: DebugFlags
    , costCentreFlags :: CCFlags
    , profilingFlags  :: ProfFlags
    , traceFlags      :: TraceFlags
    , tickyFlags      :: TickyFlags
    } deriving (Show)

foreign import ccall safe "getGcFlags"
  getGcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getConcFlags"
  getConcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getMiscFlags"
  getMiscFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getDebugFlags"
  getDebugFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getCcFlags"
  getCcFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getProfFlags" getProfFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getTraceFlags"
  getTraceFlagsPtr :: IO (Ptr ())

foreign import ccall safe "getTickyFlags"
  getTickyFlagsPtr :: IO (Ptr ())

getRTSFlags :: IO RTSFlags
getRTSFlags = do
  RTSFlags <$> getGCFlags
           <*> getConcFlags
           <*> getMiscFlags
           <*> getDebugFlags
           <*> getCCFlags
           <*> getProfFlags
           <*> getTraceFlags
           <*> getTickyFlags

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
  ptr <- getGcFlagsPtr
  GCFlags <$> (peekFilePath =<< #{peek GC_FLAGS, statsFile} ptr)
          <*> (toEnum . fromIntegral <$>
                (#{peek GC_FLAGS, giveStats} ptr :: IO Nat))
          <*> #{peek GC_FLAGS, maxStkSize} ptr
          <*> #{peek GC_FLAGS, initialStkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkBufferSize} ptr
          <*> #{peek GC_FLAGS, maxHeapSize} ptr
          <*> #{peek GC_FLAGS, minAllocAreaSize} ptr
          <*> #{peek GC_FLAGS, minOldGenSize} ptr
          <*> #{peek GC_FLAGS, heapSizeSuggestion} ptr
          <*> #{peek GC_FLAGS, heapSizeSuggestionAuto} ptr
          <*> #{peek GC_FLAGS, oldGenFactor} ptr
          <*> #{peek GC_FLAGS, pcFreeHeap} ptr
          <*> #{peek GC_FLAGS, generations} ptr
          <*> #{peek GC_FLAGS, steps} ptr
          <*> #{peek GC_FLAGS, squeezeUpdFrames} ptr
          <*> #{peek GC_FLAGS, compact} ptr
          <*> #{peek GC_FLAGS, compactThreshold} ptr
          <*> #{peek GC_FLAGS, sweep} ptr
          <*> #{peek GC_FLAGS, ringBell} ptr
          <*> #{peek GC_FLAGS, frontpanel} ptr
          <*> #{peek GC_FLAGS, idleGCDelayTime} ptr
          <*> #{peek GC_FLAGS, doIdleGC} ptr
          <*> #{peek GC_FLAGS, heapBase} ptr
          <*> #{peek GC_FLAGS, allocLimitGrace} ptr

getConcFlags :: IO ConcFlags
getConcFlags = do
  ptr <- getConcFlagsPtr
  ConcFlags <$> #{peek CONCURRENT_FLAGS, ctxtSwitchTime} ptr
            <*> #{peek CONCURRENT_FLAGS, ctxtSwitchTicks} ptr

getMiscFlags :: IO MiscFlags
getMiscFlags = do
  ptr <- getMiscFlagsPtr
  MiscFlags <$> #{peek MISC_FLAGS, tickInterval} ptr
            <*> #{peek MISC_FLAGS, install_signal_handlers} ptr
            <*> #{peek MISC_FLAGS, machineReadable} ptr
            <*> #{peek MISC_FLAGS, linkerMemBase} ptr

getDebugFlags :: IO DebugFlags
getDebugFlags = do
  ptr <- getDebugFlagsPtr
  DebugFlags <$> #{peek DEBUG_FLAGS, scheduler} ptr
             <*> #{peek DEBUG_FLAGS, interpreter} ptr
             <*> #{peek DEBUG_FLAGS, weak} ptr
             <*> #{peek DEBUG_FLAGS, gccafs} ptr
             <*> #{peek DEBUG_FLAGS, gc} ptr
             <*> #{peek DEBUG_FLAGS, block_alloc} ptr
             <*> #{peek DEBUG_FLAGS, sanity} ptr
             <*> #{peek DEBUG_FLAGS, stable} ptr
             <*> #{peek DEBUG_FLAGS, prof} ptr
             <*> #{peek DEBUG_FLAGS, linker} ptr
             <*> #{peek DEBUG_FLAGS, apply} ptr
             <*> #{peek DEBUG_FLAGS, stm} ptr
             <*> #{peek DEBUG_FLAGS, squeeze} ptr
             <*> #{peek DEBUG_FLAGS, hpc} ptr
             <*> #{peek DEBUG_FLAGS, sparks} ptr

getCCFlags :: IO CCFlags
getCCFlags = do
  ptr <- getCcFlagsPtr
  CCFlags <$> (toEnum . fromIntegral
                <$> (#{peek COST_CENTRE_FLAGS, doCostCentres} ptr :: IO Nat))
          <*> #{peek COST_CENTRE_FLAGS, profilerTicks} ptr
          <*> #{peek COST_CENTRE_FLAGS, msecsPerTick} ptr

getProfFlags :: IO ProfFlags
getProfFlags = do
  ptr <- getProfFlagsPtr
  ProfFlags <$> (toEnum <$> #{peek PROFILING_FLAGS, doHeapProfile} ptr)
            <*> #{peek PROFILING_FLAGS, heapProfileInterval} ptr
            <*> #{peek PROFILING_FLAGS, heapProfileIntervalTicks} ptr
            <*> #{peek PROFILING_FLAGS, includeTSOs} ptr
            <*> #{peek PROFILING_FLAGS, showCCSOnException} ptr
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
  ptr <- getTraceFlagsPtr
  TraceFlags <$> (toEnum . fromIntegral
                   <$> (#{peek TRACE_FLAGS, tracing} ptr :: IO CInt))
             <*> #{peek TRACE_FLAGS, timestamp} ptr
             <*> #{peek TRACE_FLAGS, scheduler} ptr
             <*> #{peek TRACE_FLAGS, gc} ptr
             <*> #{peek TRACE_FLAGS, sparks_sampled} ptr
             <*> #{peek TRACE_FLAGS, sparks_full} ptr
             <*> #{peek TRACE_FLAGS, user} ptr

getTickyFlags :: IO TickyFlags
getTickyFlags = do
  ptr <- getTickyFlagsPtr
  TickyFlags <$> #{peek TICKY_FLAGS, showTickyStats} ptr
             <*> (peekFilePath =<< #{peek TICKY_FLAGS, tickyFile} ptr)
