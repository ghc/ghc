{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ParallelArrays, MagicHash #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PArr
-- Copyright   :  (c) 2001-2011 The Data Parallel Haskell team
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--

-- #hide
module GHC.PArr where

import GHC.Base

-- Representation of parallel arrays
--
-- Vanilla representation of parallel Haskell based on standard GHC arrays that is used if the
-- vectorised is /not/ used.
--
-- NB: This definition *must* be kept in sync with `TysWiredIn.parrTyCon'!
--
data [::] e = PArr !Int (Array# e)
