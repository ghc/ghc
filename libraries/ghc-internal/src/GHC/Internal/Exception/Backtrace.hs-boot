{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Base (IO)
import GHC.Internal.Stack.Types (HasCallStack)
import GHC.Internal.Exception.Context (ExceptionAnnotation)

data Backtraces

instance ExceptionAnnotation Backtraces

-- For GHC.Exception
collectBacktraces :: HasCallStack => IO Backtraces
