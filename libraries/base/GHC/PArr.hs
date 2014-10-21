{-# LANGUAGE NoImplicitPrelude #-}
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
-- BIG UGLY HACK: The desugarer special cases this module.  Despite the uses of '-XParallelArrays',
--                the desugarer does not load 'Data.Array.Parallel' into its global state. (Hence,
--                the present module may not use any other piece of '-XParallelArray' syntax.)
--
--                This will be cleaned up when we change the internal represention of '[::]' to not
--                rely on a wired-in type constructor.

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

type PArr = [::]   -- this synonym is to get access to '[::]' without using the special syntax
