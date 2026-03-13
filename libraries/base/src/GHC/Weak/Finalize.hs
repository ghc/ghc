{-# LANGUAGE MagicHash #-}
module GHC.Weak.Finalize
    ( -- * Handling exceptions
      -- | When an exception is thrown by a finalizer called by the
      -- garbage collector, GHC calls a global handler which can be set with
      -- 'setFinalizerExceptionHandler'. Note that any exceptions thrown by
      -- this handler will be ignored.
      setFinalizerExceptionHandler
    , getFinalizerExceptionHandler
    , printToHandleFinalizerExceptionHandler
      -- * Internal
    , GHC.Weak.Finalize.runFinalizerBatch
    ) where

import GHC.Internal.Weak.Finalize

import GHC.Internal.Base
import GHC.Internal.Exception
import GHC.Internal.IO (catchException)
import GHC.Internal.IO.Handle.Types (Handle)
import GHC.Internal.IO.Handle.Text (hPutStrLn)


{-# DEPRECATED runFinalizerBatch
    "This function is internal to GHC. It will not be exported in future." #-}
-- | Run a batch of finalizers from the garbage collector. Given an
-- array of finalizers and the length of the array, just call each one
-- in turn.
--
-- This is an internal detail of the GHC RTS weak pointer finaliser
-- mechanism. It should no longer be exported from base. There is no
-- good reason to use it. It will be removed in the next major version
-- of base (4.23.*).
--
-- See <https://github.com/haskell/core-libraries-committee/issues/342>
--
runFinalizerBatch :: Int
                  -> Array# (State# RealWorld -> State# RealWorld)
                  -> IO ()
runFinalizerBatch = GHC.Internal.Weak.Finalize.runFinalizerBatch

-- | An exception handler for 'Handle' finalization that prints the error to
-- the given 'Handle', but doesn't rethrow it.
--
-- @since base-4.18.0.0
printToHandleFinalizerExceptionHandler :: Handle -> SomeException -> IO ()
printToHandleFinalizerExceptionHandler hdl se =
    hPutStrLn hdl msg `catchException` (\(SomeException _) -> return ())
  where
    msg = "Exception during weak pointer finalization (ignored): " ++ displayException se ++ "\n"
