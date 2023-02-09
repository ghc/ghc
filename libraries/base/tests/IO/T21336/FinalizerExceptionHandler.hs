module FinalizerExceptionHandler
  ( setFinalizerExceptionHandler
  , getFinalizerExceptionHandler
  , printToStderrFinalizerExceptionHandler )
  where

import GHC.Exception     ( SomeException(..), displayException )
import GHC.IO            ( catchException )
import GHC.IO.Handle     ( hPutStr )
import GHC.IO.StdHandles ( stderr )
import GHC.Weak.Finalize ( setFinalizerExceptionHandler, getFinalizerExceptionHandler )

-- | An exception handler for Handle finalization that prints the error to
-- stderr, but doesn't rethrow it.
printToStderrFinalizerExceptionHandler :: SomeException -> IO ()
-- See Note [Handling exceptions during Handle finalization] in
-- GHC.IO.Handle.Internals
printToStderrFinalizerExceptionHandler se =
    hPutStr stderr msg `catchException` (\(SomeException _) -> return ())
  where
    msg = "Exception during weak pointer finalization (ignored): " ++ displayException se ++ "\n"
