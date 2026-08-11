{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Base (
    String, const, flip, fmap, otherwise, return, ($), (.), (>>=),
  )
import GHC.Internal.Data.OldList
import GHC.Internal.IORef
import GHC.Internal.IO.Unsafe (unsafePerformIO)
import GHC.Internal.Exception.Context
import GHC.Internal.Maybe (Maybe(..))
import GHC.Internal.Ptr
import GHC.Internal.Data.Maybe (fromMaybe, mapMaybe)
import GHC.Internal.Stack.Types as GHC.Stack (CallStack, HasCallStack)
import GHC.Internal.Stack.Annotation (SomeStackAnnotation(..))
import GHC.Internal.Data.Typeable (cast)
import qualified GHC.Internal.Stack as HCS
import qualified GHC.Internal.ExecutionStack.Internal as ExecStack
import qualified GHC.Internal.Stack.CloneStack as CloneStack
import qualified GHC.Internal.Stack.Decode as CloneStack
import qualified GHC.Internal.Stack.CCS as CCS
import GHC.Internal.Types (Bool(..), Int, IO)

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

-- | How to collect 'ExceptionAnnotation's on throwing 'Exception's.
--
data CollectExceptionAnnotationMechanism = CollectExceptionAnnotationMechanism
  { ceaCollectExceptionAnnotationMechanism :: HasCallStack => CloneStack.StackSnapshot -> IO SomeExceptionAnnotation
  }

defaultCollectExceptionAnnotationMechanism :: CollectExceptionAnnotationMechanism
defaultCollectExceptionAnnotationMechanism = CollectExceptionAnnotationMechanism
  { ceaCollectExceptionAnnotationMechanism = \snapshot -> SomeExceptionAnnotation `fmap` collectBacktracesFrom snapshot
  }

collectExceptionAnnotationMechanismRef :: IORef CollectExceptionAnnotationMechanism
collectExceptionAnnotationMechanismRef =
    unsafePerformIO $ newIORef defaultCollectExceptionAnnotationMechanism
{-# NOINLINE collectExceptionAnnotationMechanismRef #-}

-- | Returns the current callback for collecting 'ExceptionAnnotation's on throwing 'Exception's.
--
getCollectExceptionAnnotationMechanism :: IO CollectExceptionAnnotationMechanism
getCollectExceptionAnnotationMechanism = readIORef collectExceptionAnnotationMechanismRef

-- | Set the callback for collecting an 'ExceptionAnnotation'.
--
setCollectExceptionAnnotation :: ExceptionAnnotation a => (HasCallStack => IO a) -> IO ()
setCollectExceptionAnnotation collector = do
  let cea = CollectExceptionAnnotationMechanism
        { ceaCollectExceptionAnnotationMechanism = \_ -> fmap SomeExceptionAnnotation collector
        }
  _ <- atomicModifyIORef'_ collectExceptionAnnotationMechanismRef (const cea)
  return ()

-- | A collection of backtraces.
data Backtraces =
    Backtraces {
        btrCostCentre :: Maybe (Ptr CCS.CostCentreStack),
        btrHasCallStack :: Maybe HCS.CallStack,
        btrExecutionStack :: Maybe ExecStack.StackTrace,
        btrIpe :: Maybe CloneStack.StackSnapshot
    }

-- | Render a set of backtraces to a human-readable string.
displayBacktraces :: Backtraces -> String
displayBacktraces bts = concat
    [ displayOne "Cost-centre stack backtrace" btrCostCentre displayCc
    , displayOne "Native stack backtrace" btrExecutionStack displayExec
    , displayOne "IPE backtrace" btrIpe displayIpe
    , displayOne "HasCallStack backtrace" btrHasCallStack displayHsc
    ]
  where
    indent :: Int -> String -> String
    indent n s  = replicate n ' ' ++ s

    -- The unsafePerformIO here is safe as we don't currently unload cost-centres.
    displayCc   = unlines . map (indent 2) . unsafePerformIO . CCS.ccsToStrings
    displayExec = unlines . map (indent 2 . flip ExecStack.showLocation "") . fromMaybe [] . ExecStack.stackFrames
    -- The unsafePerformIO here is safe as 'StackSnapshot' makes sure neither the stack frames nor
    -- references closures can be garbage collected.
    displayIpe  = unlines . mapMaybe (fmap (indent 2) . CloneStack.prettyStackFrameWithIpe) . unsafePerformIO . CloneStack.decodeStackWithIpe
    displayHsc  = unlines . map (indent 2 . prettyCallSite) . HCS.getCallStack
      where prettyCallSite (f, loc) = f ++ ", called at " ++ HCS.prettySrcLoc loc

    displayOne :: String -> (Backtraces -> Maybe rep) -> (rep -> String) -> String
    displayOne label getBt displ
      | Just bt <- getBt bts = concat [label, ":\n", displ bt]
      | otherwise            = ""

instance ExceptionAnnotation Backtraces where
    displayExceptionAnnotation = displayBacktraces

-- | Collect 'SomeExceptionAnnotation' based on the configuration of the
-- global 'CollectExceptionAnnotationMechanism'.
--
collectExceptionAnnotation :: HasCallStack => IO SomeExceptionAnnotation
collectExceptionAnnotation = HCS.withFrozenCallStack $ do
  snapshot <- CloneStack.cloneMyStack
  cea <- getCollectExceptionAnnotationMechanism
  ceaCollectExceptionAnnotationMechanism cea snapshot

stackAnnotationsFrom :: CloneStack.StackSnapshot -> IO [SomeExceptionAnnotation]
stackAnnotationsFrom snapshot = do
  anns <- CloneStack.decodeStackAnnotations snapshot
  return (mapMaybe (\(SomeStackAnnotation a) -> cast a) anns)

collectExceptionContext :: HasCallStack => Bool -> IO [SomeExceptionAnnotation]
collectExceptionContext backtrace_desired = HCS.withFrozenCallStack $ do
  snapshot <- CloneStack.cloneMyStack
  bt <- if backtrace_desired
    then do
      cea <- getCollectExceptionAnnotationMechanism
      ann <- ceaCollectExceptionAnnotationMechanism cea snapshot
      return [ann]
    else return []
  anns <- stackAnnotationsFrom snapshot
  return (bt ++ anns)

-- | Collect a set of 'Backtraces'.
collectBacktraces :: (?callStack :: CallStack) => IO Backtraces
collectBacktraces = HCS.withFrozenCallStack $
    CloneStack.cloneMyStack >>= collectBacktracesFrom

collectBacktracesFrom
    :: (?callStack :: CallStack)
    => CloneStack.StackSnapshot -> IO Backtraces
collectBacktracesFrom snapshot = HCS.withFrozenCallStack $ do
    getEnabledBacktraceMechanisms >>= collectBacktraces' snapshot

collectBacktraces'
    :: (?callStack :: CallStack)
    => CloneStack.StackSnapshot -> EnabledBacktraceMechanisms -> IO Backtraces
collectBacktraces' snapshot enabled = HCS.withFrozenCallStack $ do
    let collect :: BacktraceMechanism -> IO (Maybe a) -> IO (Maybe a)
        collect mech f
          | backtraceMechanismEnabled mech enabled = f
          | otherwise = return Nothing

    ccs <- collect CostCentreBacktrace $ do
        Just `fmap` CCS.getCurrentCCS ()

    exec <- collect ExecutionBacktrace $ do
        ExecStack.collectStackTrace

    ipe <- collect IPEBacktrace $ do
        return (Just snapshot)

    hcs <- collect HasCallStackBacktrace $ do
        return (Just ?callStack)

    return (Backtraces { btrCostCentre = ccs
                       , btrHasCallStack = hcs
                       , btrExecutionStack = exec
                       , btrIpe = ipe
                       })
