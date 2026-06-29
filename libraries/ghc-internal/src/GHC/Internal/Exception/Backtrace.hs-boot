{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Stack.Types (HasCallStack)
import GHC.Internal.Types (IO)
import GHC.Internal.Exception.Context (ExceptionAnnotation, SomeExceptionAnnotation)

data Backtraces

instance ExceptionAnnotation Backtraces

-- For GHC.Internal.Conc.Sync
collectBacktraces :: HasCallStack => IO Backtraces

-- For GHC.Internal.Exception
collectExceptionAnnotation :: HasCallStack => IO SomeExceptionAnnotation
