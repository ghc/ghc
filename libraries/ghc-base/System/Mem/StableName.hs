{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem.StableName
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- Stable names are a way of performing fast ( \(\mathcal{O}(1)\) ),
-- not-quite-exact comparison between objects.
--
-- Stable names solve the following problem: suppose you want to build
-- a hash table with Haskell objects as keys, but you want to use
-- pointer equality for comparison; maybe because the keys are large
-- and hashing would be slow, or perhaps because the keys are infinite
-- in size.  We can\'t build a hash table using the address of the
-- object as the key, because objects get moved around by the garbage
-- collector, meaning a re-hash would be necessary after every garbage
-- collection.
--
-------------------------------------------------------------------------------

module System.Mem.StableName (
  -- * Stable Names
  StableName,
  makeStableName,
  hashStableName,
  eqStableName
  ) where

import GHC.StableName
