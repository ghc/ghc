{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Foo where

-- This example suggested by Yitzchak Gale

import Control.Monad.State
import Control.Monad.Error

class Error e => Game b mv e | b -> mv e where
    newBoard :: MonadState b m => m ()
	-- This method is unambiguous, because 
	-- m determines b (via a fundep in MonadState)


