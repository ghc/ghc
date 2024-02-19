{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Unique
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- An abstract interface to a unique symbol generator.
--

module Data.Unique
    (-- *  Unique objects
     Unique,
     newUnique,
     hashUnique
     ) where

import GHC.Internal.Data.Unique