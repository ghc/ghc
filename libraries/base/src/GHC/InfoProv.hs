{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.InfoProv
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
-- @since 4.18.0.0
--

module GHC.InfoProv
    ( InfoProv(..)
    , ipLoc
    , ipeProv
    , whereFrom
      -- * Internals
    , InfoProvEnt
    , peekInfoProv
    ) where

import GHC.Internal.InfoProv
