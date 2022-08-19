{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Exception.Backtrace where

import GHC.Base (IO)
import GHC.Stack.Types (HasCallStack)
import GHC.Exception.Context (ExceptionAnnotation)

data Backtraces

instance ExceptionAnnotation Backtraces

-- For GHC.Exception
collectBacktraces :: HasCallStack => IO Backtraces
