{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functors: uniform action over a parameterized type, generalizing the
-- 'map' function on lists.

module Data.Functor
    (
      Functor(fmap),
      (<$),
      ($>),
      (<$>),
      void,
    ) where

import GHC.Base ( Functor(..), const, flip )

infixl 4 <$>

-- | An infix synonym for 'fmap'.
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 4 $>

-- | Flipped version of '<$'.
--
-- /Since: 4.7.0.0/
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | @'void' value@ discards or ignores the result of evaluation, such as the
-- return value of an 'IO' action.
void :: Functor f => f a -> f ()
void = fmap (const ())
