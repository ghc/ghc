{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Exception.Backtrace
    ( -- * Backtrace mechanisms
      BacktraceMechanism(..)
    , setEnabledBacktraceMechanisms
    , getEnabledBacktraceMechanisms
      -- * Collecting backtraces
    , Backtraces
    , collectBacktraces
    ) where

import GHC.Base
import Data.OldList
import GHC.IORef
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Exception.Context
import GHC.Ptr
import GHC.Stack.Types as GHC.Stack (CallStack)
import qualified GHC.Stack as HCS
import qualified GHC.ExecutionStack as ExecStack
import qualified GHC.ExecutionStack.Internal as ExecStack
import qualified GHC.Stack.CloneStack as CloneStack
import qualified GHC.Stack.CCS as CCS

-- | How to collect a backtrace when an exception is thrown.
data BacktraceMechanism
  -- | collect cost-centre stack backtraces (only available when built with profiling)
  = CostCentreBacktrace
  -- | collect 'HasCallStack' backtraces
  | HasCallStackBacktrace
  -- | collect backtraces from native execution stack unwinding
  | ExecutionBacktrace
  -- | collect backtraces from Info Table Provenance Entries
  | IPEBacktrace

newtype EnabledBacktraceMechanisms =
    EnabledBacktraceMechanisms {
        backtraceMechanismEnabled :: BacktraceMechanism -> Bool
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

data Backtraces =
    Backtraces {
        btrCostCentre :: Maybe (Ptr CCS.CostCentreStack),
        btrHasCallStack :: Maybe HCS.CallStack,
        btrExecutionStack :: Maybe [ExecStack.Location],
        btrIpe :: Maybe [CloneStack.StackEntry]
    }

displayBacktraces :: Backtraces -> String
displayBacktraces bts = concat
    [ displayOne "Cost-centre stack backtrace" btrCostCentre displayCc
    , displayOne "Native stack backtrace" btrExecutionStack displayExec
    , displayOne "IPE backtrace" btrIpe displayIpe
    , displayOne "HasCallStack backtrace" btrHasCallStack HCS.prettyCallStack
    ]
  where
    indent :: Int -> String -> String
    indent n s  = replicate n ' ' ++ s

    displayCc   = unlines . map (indent 4) . unsafePerformIO . CCS.ccsToStrings
    displayExec = unlines . map (indent 4 . flip ExecStack.showLocation "")
    displayIpe  = unlines . map (indent 4 . CloneStack.prettyStackEntry)

    displayOne :: String -> (Backtraces -> Maybe rep) -> (rep -> String) -> String
    displayOne label getBt displ
      | Just bt <- getBt bts = concat [label, ":\n", displ bt]
      | otherwise            = ""

instance ExceptionAnnotation Backtraces where
    displayExceptionAnnotation = displayBacktraces

collectBacktraces :: (?callStack :: CallStack) => IO Backtraces
collectBacktraces = HCS.withFrozenCallStack $ do
    enabled <- getEnabledBacktraceMechanisms
    let collect :: BacktraceMechanism -> IO (Maybe a) -> IO (Maybe a)
        collect mech f
          | backtraceMechanismEnabled enabled mech = f
          | otherwise = return Nothing

    ccs <- collect CostCentreBacktrace $ do
        Just `fmap` CCS.getCurrentCCS ()

    exec <- collect ExecutionBacktrace $ do
        ExecStack.getStackTrace

    ipe <- collect IPEBacktrace $ do
        stack <- CloneStack.cloneMyStack
        stackEntries <- CloneStack.decode stack
        return (Just stackEntries)

    hcs <- collect HasCallStackBacktrace $ do
        return (Just ?callStack)

    return (Backtraces { btrCostCentre = ccs
                       , btrHasCallStack = hcs
                       , btrExecutionStack = exec
                       , btrIpe = ipe
                       })
