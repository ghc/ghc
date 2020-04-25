{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE ConstraintKinds #-}

module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import GhcPrelude

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Catch

type ExceptionMonad m = (MonadCatch m, MonadThrow m, MonadMask m, MonadIO m)

