{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.InfoProv
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's info-table provenance metadata.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- @since base-4.18.0.0
-----------------------------------------------------------------------------

module GHC.Internal.InfoProv
    ( InfoProv(..)
    , ipLoc
    , whereFrom
      -- * Internals
    , InfoProvEnt
    , ipeProv
    , peekInfoProv
    ) where

import GHC.Internal.Base
import GHC.Internal.InfoProv.Types

-- | Get information about where a value originated from.
-- This information is stored statically in a binary when @-finfo-table-map@ is
-- enabled.  The source positions will be greatly improved by also enabled debug
-- information with @-g3@. Finally you can enable @-fdistinct-constructor-tables@ to
-- get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and
-- then consulting a specially generated map (by @-finfo-table-map@) to find out where we think
-- the best source position to describe that info table arose from.
--
-- @since base-4.16.0.0
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom obj = getIPE obj Nothing $ \p ->
    Just `fmap` peekInfoProv (ipeProv p)
