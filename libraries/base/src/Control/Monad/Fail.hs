{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Monad.Fail
-- Copyright   :  (C) 2015 David Luposchainsky,
--                (C) 2015 Herbert Valerio Riedel
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Transitional module providing the 'MonadFail' class and primitive
-- instances.
--
-- This module can be imported for defining forward compatible
-- 'MonadFail' instances:
--
-- @
-- import qualified Control.Monad.Fail as Fail
--
-- instance Monad Foo where
--   (>>=) = {- ...bind impl... -}
--
--   -- Provide legacy 'fail' implementation for when
--   -- new-style MonadFail desugaring is not enabled.
--   fail = Fail.fail
--
-- instance Fail.MonadFail Foo where
--   fail = {- ...fail implementation... -}
-- @
--
-- See <https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail>
-- for more details.
--
-- @since 4.9.0.0
--

module Control.Monad.Fail
    (MonadFail(fail)) where

import GHC.Internal.Control.Monad.Fail
