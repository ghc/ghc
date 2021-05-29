{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE ConstraintKinds #-}

module GHC.Utils.Exception
    (
    module CE,
    module GHC.Utils.Exception
    )
    where

import GHC.Prelude

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

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)
