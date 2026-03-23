{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Internal.Exception.Backtrace where

import GHC.Internal.Stack.Types
import GHC.Internal.Types (IO)
import GHC.Internal.Exception.Context (SomeExceptionAnnotation)
import GHC.Internal.Types as Rebindable (unpackCString#, unpackCStringUtf8#)

-- For GHC.Exception
collectExceptionAnnotation :: HasCallStack => IO SomeExceptionAnnotation
