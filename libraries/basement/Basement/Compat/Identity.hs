-- |
-- Module      : Basement.Compat.Identity
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Identity re-export, with a compat wrapper for older version of base that
-- do not have Data.Functor.Identity
{-# LANGUAGE CPP #-}
module Basement.Compat.Identity
    ( Identity(..)
    ) where

#if MIN_VERSION_base(4,8,0)

import Data.Functor.Identity

#else

import Basement.Compat.Base

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) fab fa = Identity $ runIdentity fab (runIdentity fa)

instance Monad Identity where
    return    = pure
    ma >>= mb = mb (runIdentity ma)

#endif
