{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception.Backtrace
-- Copyright   :  (c) The GHC Team, 2020-2021
-- Authors     :  Ben Gamari, David Eichmann, Sven Tennie
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Collect Exception backtraces with several mechanisms.
-----------------------------------------------------------------------------

module GHC.Exception.Backtrace
  ( Backtrace (..),
    setDefaultBacktraceMechanisms,
    getDefaultBacktraceMechanisms,
    showBacktraces,
    collectBacktraces,
  )
where

import Data.List
import Data.Maybe
import GHC.Base
import {-# SOURCE #-} GHC.ExecutionStack (Location, getStackTrace)
import {-# SOURCE #-} GHC.ExecutionStack.Internal (showStackFrames)
import GHC.IO.Unsafe
import {-# SOURCE #-} GHC.IORef
import GHC.Ptr
import GHC.Show
import {-# SOURCE #-} GHC.Stack
import {-# SOURCE #-} GHC.Stack.CCS

-- | An exception backtrace.
--
-- @since 4.15
data Backtrace
  = -- | a cost center profiler backtrace
    CostCenterBacktrace (Ptr GHC.Stack.CCS.CostCentreStack)
  | -- | a stack from 'GHC.Stack.HasCallStack'
    HasCallStackBacktrace GHC.Stack.CallStack
  | -- | a stack unwinding (e.g. DWARF) backtrace
    ExecutionBacktrace [GHC.ExecutionStack.Location]

-- | @since 4.15
instance Show Backtrace where
  -- TODO
  showsPrec p (CostCenterBacktrace ccs) = showsPrec p ccs
  showsPrec p (HasCallStackBacktrace ccs) = showsPrec p ccs
  showsPrec _ (ExecutionBacktrace ccs) = showStackFrames ccs

-- | How to collect a backtrace when an exception is thrown.
data BacktraceMechanism
  = -- | collect a cost center stacktrace (only available when built with profiling)
    CostCenterBacktraceMech
  | -- | use execution stack unwinding with given limit
    ExecutionStackBacktraceMech (Maybe Int)

showBacktraces :: [Backtrace] -> String
showBacktraces bts = unlines $ intersperse "" $ map show bts

currentBacktraceMechanisms :: IORef [BacktraceMechanism]
currentBacktraceMechanisms = unsafePerformIO $ newIORef []
{-# NOINLINE currentBacktraceMechanisms #-}

-- | Set how 'Control.Exception.throwIO', et al. collect backtraces.
setDefaultBacktraceMechanisms :: [BacktraceMechanism] -> IO ()
setDefaultBacktraceMechanisms = writeIORef currentBacktraceMechanisms

-- | Returns the currently selected 'BacktraceMechanism'.
getDefaultBacktraceMechanisms :: IO [BacktraceMechanism]
getDefaultBacktraceMechanisms = readIORef currentBacktraceMechanisms

-- | Collect a 'Backtrace' via the current global 'BacktraceMechanism'. See
-- 'setDefaultBacktraceMechanisms'.
collectBacktraces :: IO [Backtrace]
collectBacktraces = do
  mech <- getDefaultBacktraceMechanisms
  catMaybes `fmap` mapM collectBacktraces' mech

-- | Collect a 'Backtrace' via the given 'BacktraceMechanism'.
collectBacktraces' :: BacktraceMechanism -> IO (Maybe Backtrace)
collectBacktraces' CostCenterBacktraceMech = do
  ptr <- getCurrentCCS ()
  -- TODO: is the unit here safe? Is this dummy argument really needed? Why
  -- isn't the state token sufficient?
  return $ if ptr == nullPtr then Nothing else Just (CostCenterBacktrace ptr)
-- TODO: (Ptr GHC.Stack.CCS.CostCentreStack) really not needed here?
collectBacktraces' (ExecutionStackBacktraceMech _) = fmap ExecutionBacktrace `fmap` getStackTrace
