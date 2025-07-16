{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Base
import GHC.Internal.Data.OldList
import GHC.Internal.IORef
import GHC.Internal.IO.Unsafe (unsafePerformIO)
import GHC.Internal.Exception.Context
import GHC.Internal.Ptr
import GHC.Internal.Data.Maybe (fromMaybe)
import GHC.Internal.Stack.Types as GHC.Stack (CallStack)
import qualified GHC.Internal.Stack as HCS
import qualified GHC.Internal.ExecutionStack.Internal as ExecStack
import qualified GHC.Internal.Stack.CloneStack as CloneStack
import qualified GHC.Internal.Stack.CCS as CCS

-- | How to collect a backtrace when an exception is thrown.
data BacktraceMechanism
  -- | collect cost-centre stack backtraces (only available when built with profiling)
  = CostCentreBacktrace
  -- | collect 'HasCallStack' backtraces
  | HasCallStackBacktrace
  -- | collect backtraces via native execution stack unwinding (e.g. using DWARF debug information)
  | ExecutionBacktrace
  -- | collect backtraces from Info Table Provenance Entries
  | IPEBacktrace

data EnabledBacktraceMechanisms =
    EnabledBacktraceMechanisms
      { costCentreBacktraceEnabled   :: !Bool
      , hasCallStackBacktraceEnabled :: !Bool
      , executionBacktraceEnabled    :: !Bool
      , ipeBacktraceEnabled          :: !Bool
      }

defaultEnabledBacktraceMechanisms :: EnabledBacktraceMechanisms
defaultEnabledBacktraceMechanisms = EnabledBacktraceMechanisms
  { costCentreBacktraceEnabled   = False
  , hasCallStackBacktraceEnabled = True
  , executionBacktraceEnabled    = False
  , ipeBacktraceEnabled          = False
  }

backtraceMechanismEnabled :: BacktraceMechanism -> EnabledBacktraceMechanisms -> Bool
backtraceMechanismEnabled bm =
  case bm of
    CostCentreBacktrace   -> costCentreBacktraceEnabled
    HasCallStackBacktrace -> hasCallStackBacktraceEnabled
    ExecutionBacktrace    -> executionBacktraceEnabled
    IPEBacktrace          -> ipeBacktraceEnabled

setBacktraceMechanismEnabled
    :: BacktraceMechanism -> Bool
    -> EnabledBacktraceMechanisms
    -> EnabledBacktraceMechanisms
setBacktraceMechanismEnabled bm enabled en =
    case bm of
      CostCentreBacktrace   -> en { costCentreBacktraceEnabled = enabled }
      HasCallStackBacktrace -> en { hasCallStackBacktraceEnabled = enabled }
      ExecutionBacktrace    -> en { executionBacktraceEnabled = enabled }
      IPEBacktrace          -> en { ipeBacktraceEnabled = enabled }

enabledBacktraceMechanismsRef :: IORef EnabledBacktraceMechanisms
enabledBacktraceMechanismsRef =
    unsafePerformIO $ newIORef defaultEnabledBacktraceMechanisms
{-# NOINLINE enabledBacktraceMechanismsRef #-}

-- | Returns the currently enabled 'BacktraceMechanism's.
getEnabledBacktraceMechanisms :: IO EnabledBacktraceMechanisms
getEnabledBacktraceMechanisms = readIORef enabledBacktraceMechanismsRef

-- | Will the given 'BacktraceMechanism' be used when collecting
-- backtraces?
getBacktraceMechanismState :: BacktraceMechanism -> IO Bool
getBacktraceMechanismState bm =
    backtraceMechanismEnabled bm `fmap` getEnabledBacktraceMechanisms

-- | Set whether the given 'BacktraceMechanism' will be used when collecting
-- backtraces?
setBacktraceMechanismState :: BacktraceMechanism -> Bool -> IO ()
setBacktraceMechanismState bm enabled = do
    _ <- atomicModifyIORef'_ enabledBacktraceMechanismsRef (setBacktraceMechanismEnabled bm enabled)
    return ()

-- | How to display a backtrace when an exception is thrown.
data DisplayBacktraceMechanisms =
    DisplayBacktraceMechanisms
      { displayCostCentreBacktrace   :: Ptr CCS.CostCentreStack -> String
      , displayHasCallStackBacktrace :: HCS.CallStack -> String
      , displayExecutionBacktrace    :: ExecStack.StackTrace -> String
      , displayIpeBacktrace          :: CloneStack.StackSnapshot -> String
      }

defaultDisplayBacktraceMechanisms :: DisplayBacktraceMechanisms
defaultDisplayBacktraceMechanisms = DisplayBacktraceMechanisms
  { displayCostCentreBacktrace   = unlines . map (indent 2) . unsafePerformIO . CCS.ccsToStrings
  , displayHasCallStackBacktrace = unlines . map (indent 2 . prettyCallSite) . HCS.getCallStack
  , displayExecutionBacktrace    = unlines . map (indent 2 . flip ExecStack.showLocation "") . fromMaybe [] . ExecStack.stackFrames
  , displayIpeBacktrace          = unlines . map (indent 2 . CloneStack.prettyStackEntry) . unsafePerformIO . CloneStack.decode
  }
  where
    indent :: Int -> String -> String
    indent n s  = replicate n ' ' ++ s

    prettyCallSite (f, loc) = f ++ ", called at " ++ HCS.prettySrcLoc loc


displayBacktraceMechanismsRef :: IORef DisplayBacktraceMechanisms
displayBacktraceMechanismsRef =
    unsafePerformIO $ newIORef defaultDisplayBacktraceMechanisms
{-# NOINLINE displayBacktraceMechanismsRef #-}

-- | How are the 'Backtraces' going to be displayed?
getDisplayBacktraceMechanisms :: IO DisplayBacktraceMechanisms
getDisplayBacktraceMechanisms = readIORef displayBacktraceMechanismsRef

-- | Specify how the 'Backtraces' are displayed.
setDisplayBacktraceMechanismsState :: DisplayBacktraceMechanisms -> IO ()
setDisplayBacktraceMechanismsState dbm = do
    _ <- atomicModifyIORef'_ displayBacktraceMechanismsRef (const dbm)
    return ()

-- | A collection of backtraces, paired with a way to display each respective backtrace.
data Backtraces =
    Backtraces {
        btrCostCentre :: Maybe (Ptr CCS.CostCentreStack),
        btrDisplayCostCentre :: Ptr CCS.CostCentreStack -> String,
        btrHasCallStack :: Maybe HCS.CallStack,
        btrDisplayHasCallStack :: HCS.CallStack -> String,
        btrExecutionStack :: Maybe ExecStack.StackTrace,
        btrDisplayExecutionStack :: ExecStack.StackTrace -> String,
        btrIpe :: Maybe CloneStack.StackSnapshot,
        btrDisplayIpe :: CloneStack.StackSnapshot -> String
    }

-- | Render a set of backtraces to a human-readable string.
displayBacktraces :: Backtraces -> String
displayBacktraces bts = concat
    [ displayOne "Cost-centre stack backtrace" btrCostCentre btrDisplayCostCentre
    , displayOne "Native stack backtrace" btrExecutionStack btrDisplayExecutionStack
    , displayOne "IPE backtrace" btrIpe btrDisplayIpe
    , displayOne "HasCallStack backtrace" btrHasCallStack btrDisplayHasCallStack
    ]
  where
    displayOne :: String -> (Backtraces -> Maybe rep) -> (Backtraces -> rep -> String) -> String
    displayOne label getBt displ
      | Just bt <- getBt bts = concat [label, ":\n", displ bts bt]
      | otherwise            = ""

instance ExceptionAnnotation Backtraces where
    displayExceptionAnnotation = displayBacktraces

-- | Collect a set of 'Backtraces'.
collectBacktraces :: (?callStack :: CallStack) => IO Backtraces
collectBacktraces = HCS.withFrozenCallStack $ do
    bm <- getEnabledBacktraceMechanisms
    dpm <- getDisplayBacktraceMechanisms
    collectBacktraces' bm dpm

collectBacktraces'
    :: (?callStack :: CallStack)
    => EnabledBacktraceMechanisms -> DisplayBacktraceMechanisms -> IO Backtraces
collectBacktraces' enabled renderers = HCS.withFrozenCallStack $ do
    let collect :: BacktraceMechanism -> IO (Maybe a) -> IO (Maybe a)
        collect mech f
          | backtraceMechanismEnabled mech enabled = f
          | otherwise = return Nothing

    ccs <- collect CostCentreBacktrace $ do
        Just `fmap` CCS.getCurrentCCS ()

    exec <- collect ExecutionBacktrace $ do
        ExecStack.collectStackTrace

    ipe <- collect IPEBacktrace $ do
        stack <- CloneStack.cloneMyStack
        return (Just stack)

    hcs <- collect HasCallStackBacktrace $ do
        return (Just ?callStack)

    return (Backtraces { btrCostCentre = ccs
                       , btrDisplayCostCentre = displayCostCentreBacktrace renderers
                       , btrHasCallStack = hcs
                       , btrDisplayHasCallStack = displayHasCallStackBacktrace renderers
                       , btrExecutionStack = exec
                       , btrDisplayExecutionStack = displayExecutionBacktrace renderers
                       , btrIpe = ipe
                       , btrDisplayIpe = displayIpeBacktrace renderers
                       })
