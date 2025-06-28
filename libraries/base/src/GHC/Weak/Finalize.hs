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

-- These imports can be removed once runFinalizerBatch is removed,
-- as can MagicHash above.
import GHC.Internal.Base (Int, Array#, IO, State#, RealWorld)


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
