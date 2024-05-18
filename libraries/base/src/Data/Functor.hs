{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Functor
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-- A type @f@ is a Functor if it provides a function 'fmap' which, given any types @a@ and @b@,
-- lets you apply any function of type @(a -> b)@ to turn an @f a@ into an @f b@, preserving the
-- structure of @f@.
module Data.Functor
    (Functor(..),
     ($>),
     (<$>),
     (<&>),
     unzip,
     void
     ) where

import GHC.Internal.Data.Functor
