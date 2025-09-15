module WarnDefaultedExceptionContext where

import Control.Exception.Context
import Control.Exception

-- The implicit ExceptionContext constraint here should be defaulted with a warning
exc :: SomeException
exc = SomeException (userError "uh oh")

