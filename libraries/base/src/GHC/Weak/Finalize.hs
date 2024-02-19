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
    , runFinalizerBatch
    ) where

import GHC.Internal.Weak.Finalize
