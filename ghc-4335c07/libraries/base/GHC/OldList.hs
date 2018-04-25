{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.OldList
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This legacy module provides access to the list-specialised operations
-- of "Data.List". This module may go away again in future GHC versions and
-- is provided as transitional tool to access some of the list-specialised
-- operations that had to be generalised due to the implementation of the
-- <https://wiki.haskell.org/Foldable_Traversable_In_Prelude Foldable/Traversable-in-Prelude Proposal (FTP)>.
--
-- If the operations needed are available in "GHC.List", it's
-- recommended to avoid importing this module and use "GHC.List"
-- instead for now.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------

module GHC.OldList (module Data.OldList) where

import Data.OldList
