{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.InfoProv
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's info-table provenance metadata.
--
-- @since 4.18.0.0
-----------------------------------------------------------------------------

module GHC.InfoProv
    ( InfoProv(..)
    , ipLoc
    , whereFrom
      -- * Internals
    , InfoProvEnt
    , ipeProv
    , peekInfoProv
    ) where

import GHC.Base
import GHC.InfoProv.Types

-- | Get information about where a value originated from.
-- This information is stored statically in a binary when `-finfo-table-map` is
-- enabled.  The source positions will be greatly improved by also enabled debug
-- information with `-g3`. Finally you can enable `-fdistinct-constructor-tables` to
-- get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and
-- then consulting a specially generated map (by `-finfo-table-map`) to find out where we think
-- the best source position to describe that info table arose from.
--
-- @since 4.16.0.0
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom obj = getIPE obj Nothing $ \p ->
    Just `fmap` peekInfoProv (ipeProv p)
