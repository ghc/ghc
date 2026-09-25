{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module GHC.Utils.Exception
    (
    module CE,
    module GHC.Utils.Exception
    )
    where

import GHC.Prelude.Basic

import GHC.IO (catchException)
import Control.Exception as CE hiding (assert)
import Control.Monad.IO.Class
import Control.Monad.Catch

-- Monomorphised versions of exception-handling utilities
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catchException

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = CE.try

-- CQ[rethrow-helper]
-- Q: Why not base's rethrowIO? Why only SomeException, and why is the
--    pre-4.21 fallback a plain throwIO?
-- A~ base's rethrowIO wants an ExceptionWithContext; at every site here we
--    already hold a SomeException, and since base 4.22 (toException = id)
--    throwIO (NoBacktrace se) rethrows se with its context untouched. A typed
--    e would be rewrapped with an empty context by toException, so a
--    polymorphic version would silently drop the origin. On base < 4.21
--    NoBacktrace doesn't exist, and toException @SomeException dropped the
--    context anyway, so the frame a plain throwIO adds is lost downstream.
rethrowSomeException :: SomeException -> IO a
#if MIN_VERSION_base(4,21,0)
rethrowSomeException e = throwIO (NoBacktrace e)
#else
rethrowSomeException e = throwIO e
#endif

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)
