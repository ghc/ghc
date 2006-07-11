{-# OPTIONS_NHC98 -prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Instances
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- 'Functor' and 'Monad' instances for @(->) r@ and
-- 'Functor' instances for @(,) a@ and @'Either' a@.

module Control.Monad.Instances (Functor(..),Monad(..)) where

import Prelude

instance Functor ((->) r) where
	fmap = (.)

instance Monad ((->) r) where
	return = const
	f >>= k = \ r -> k (f r) r

instance Functor ((,) a) where
	fmap f (x,y) = (x, f y)

instance Functor (Either a) where
	fmap _ (Left x) = Left x
	fmap f (Right y) = Right (f y)
