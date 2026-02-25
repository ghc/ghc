{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Stack.Types (HasCallStack)
import GHC.Internal.Types (IO)
import GHC.Internal.Exception.Context (SomeExceptionAnnotation)

-- For GHC.Exception
collectExceptionAnnotation :: HasCallStack => IO SomeExceptionAnnotation
