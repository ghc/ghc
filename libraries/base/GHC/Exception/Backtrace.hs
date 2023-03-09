{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module GHC.Exception.Backtrace
    ( -- * Backtrace mechanisms
      BacktraceMechanism(..)
    , setEnabledBacktraceMechanisms
    , getEnabledBacktraceMechanisms
      -- * Collecting backtraces
    , Backtraces
    , getBacktrace
    , collectBacktraces
    , collectBacktrace
    ) where

import GHC.Base
import Data.OldList
import GHC.IORef
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Exception.Context
import GHC.Stack.Types (CallStack)
import qualified GHC.Stack as CallStack
import qualified GHC.ExecutionStack as ExecStack
import qualified GHC.Stack.CloneStack as CloneStack
import qualified GHC.Stack.CCS as CCS

-- | How to collect a backtrace when an exception is thrown.
data BacktraceMechanism rep where
  -- | collect cost-centre stack backtraces (only available when built with profiling)
  CostCentreBacktrace       :: BacktraceMechanism [String] -- TODO: Proper representation
  -- | collect backtraces from native execution stack unwinding
  ExecutionStackBacktrace   :: BacktraceMechanism String -- TODO: proper representation
  -- | collect backtraces from Info Table Provenance Entries
  IPEBacktrace              :: BacktraceMechanism [CloneStack.StackEntry]
  -- | collect 'HasCallStack' backtraces
  HasCallStackBacktrace     :: BacktraceMechanism CallStack

newtype EnabledBacktraceMechanisms =
    EnabledBacktraceMechanisms {
        backtraceMechanismEnabled :: forall a. BacktraceMechanism a -> Bool
    }

defaultEnabledBacktraceMechanisms :: EnabledBacktraceMechanisms
defaultEnabledBacktraceMechanisms = EnabledBacktraceMechanisms f
  where
    f HasCallStackBacktrace = True
    f _                     = False

enabledBacktraceMechanisms :: IORef EnabledBacktraceMechanisms
enabledBacktraceMechanisms =
    unsafePerformIO $ newIORef defaultEnabledBacktraceMechanisms
{-# NOINLINE enabledBacktraceMechanisms #-}

-- | Set how 'Control.Exception.throwIO', et al. collect backtraces.
setEnabledBacktraceMechanisms :: EnabledBacktraceMechanisms -> IO ()
setEnabledBacktraceMechanisms = writeIORef enabledBacktraceMechanisms

-- | Returns the currently enabled 'BacktraceMechanism's.
getEnabledBacktraceMechanisms :: IO EnabledBacktraceMechanisms
getEnabledBacktraceMechanisms = readIORef enabledBacktraceMechanisms

newtype Backtraces = Backtraces { getBacktrace :: forall a. BacktraceMechanism a -> Maybe a }

displayBacktraces :: Backtraces -> String
displayBacktraces (Backtraces f) = concat
    [ displayOne "Cost-centre stack backtrace" CostCentreBacktrace displayCc
    , displayOne "Native stack backtrace" ExecutionStackBacktrace displayExec
    , displayOne "IPE backtrace" IPEBacktrace displayIpe
    , displayOne "HasCallStack backtrace" HasCallStackBacktrace CallStack.prettyCallStack
    ]
  where
    indent :: Int -> String -> String
    indent n s  = replicate n ' ' ++ s

    displayCc   = intercalate "\n" . map (indent 4)
    displayExec = id
    displayIpe  = intercalate "\n" . map (indent 4 . CloneStack.prettyStackEntry)

    displayOne :: String -> BacktraceMechanism rep -> (rep -> String) -> String
    displayOne label mech displ
      | Just bt <- f mech = label ++ ":\n" ++ displ bt
      | otherwise         = ""

instance ExceptionAnnotation Backtraces where
    displayExceptionAnnotation = displayBacktraces

collectBacktraces :: (?callStack :: CallStack) => IO Backtraces
collectBacktraces = CallStack.withFrozenCallStack $ do
    EnabledBacktraceMechanisms enabled <- getEnabledBacktraceMechanisms
    let collect :: BacktraceMechanism a -> IO (Maybe a)
        collect mech
          | enabled mech = collectBacktrace mech
          | otherwise    = return Nothing

    ccs <- collect CostCentreBacktrace
    exec <- collect ExecutionStackBacktrace
    ipe <- collect IPEBacktrace
    hcs <- collect HasCallStackBacktrace
    let f :: BacktraceMechanism rep -> Maybe rep
        f CostCentreBacktrace = ccs
        f ExecutionStackBacktrace = exec
        f IPEBacktrace = ipe
        f HasCallStackBacktrace = hcs
    return (Backtraces f)

collectBacktrace :: (?callStack :: CallStack) => BacktraceMechanism a -> IO (Maybe a)
collectBacktrace CostCentreBacktrace = do
    strs <- CCS.currentCallStack
    case strs of
      [] -> return Nothing
      _  -> pure (Just strs)

collectBacktrace ExecutionStackBacktrace = do
    mst <- ExecStack.showStackTrace
    case mst of
      Nothing -> return Nothing
      Just st -> return (Just st)

collectBacktrace IPEBacktrace = do
    stack <- CloneStack.cloneMyStack
    stackEntries <- CloneStack.decode stack
    return (Just stackEntries)

collectBacktrace HasCallStackBacktrace =
    return (Just ?callStack)

