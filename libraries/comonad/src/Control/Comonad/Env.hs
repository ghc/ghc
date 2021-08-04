{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Env
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
--
-- The Env comonad (aka the Coreader, Environment, or Product comonad)
--
-- A co-Kleisli arrow in the Env comonad is isomorphic to a Kleisli arrow
-- in the reader monad.
--
-- (a -> e -> m) ~ (a, e) -> m ~ Env e a -> m
----------------------------------------------------------------------------
module Control.Comonad.Env (
  -- * ComonadEnv class
    ComonadEnv(..)
  , asks
  , local
  -- * The Env comonad
  , Env
  , env
  , runEnv
  -- * The EnvT comonad transformer
  , EnvT(..)
  , runEnvT
  -- * Re-exported modules
  , module Control.Comonad
  , module Control.Comonad.Trans.Class
  ) where

import Control.Comonad
import Control.Comonad.Env.Class (ComonadEnv(..), asks)
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env (Env, env, runEnv, EnvT(..), runEnvT, local)
