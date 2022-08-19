{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Exception.Backtrace where

import GHC.Base (IO)
import GHC.Stack.Types (HasCallStack)
import GHC.Exception.Context (ExceptionAnnotation)

type role BacktraceMechanism nominal

data BacktraceMechanism rep

data Backtraces

instance ExceptionAnnotation Backtraces

collectBacktraces :: HasCallStack => IO Backtraces
