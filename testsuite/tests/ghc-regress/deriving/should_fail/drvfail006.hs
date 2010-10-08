{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Testing the newtype-deriving stuff

module ShouldFail where

import Control.Monad.State

newtype T a = T (StateT Int IO a) deriving( MonadState )
	-- Here MonadState takes two type params,
	-- but exactly one is needed.