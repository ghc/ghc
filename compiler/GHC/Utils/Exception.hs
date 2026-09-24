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
import Control.Exception.Context (emptyExceptionContext)
import Control.Monad.IO.Class
import Control.Monad.Catch

-- Monomorphised versions of exception-handling utilities
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catchException

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = CE.try

-- | Remove the context, such as backtraces and annotations, from an exception.
dropExceptionContext :: SomeException -> SomeException
dropExceptionContext e = toException (ExceptionWithContext emptyExceptionContext e)

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)
