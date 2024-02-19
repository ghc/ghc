{-# LANGUAGE Safe #-}

-- |
-- Module      :  RTS.Stats
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- /The API of this module is unstable and is tightly coupled to GHC's internals./
-- If depend on it, make sure to use a tight upper bound, e.g., @base < 4.X@ rather
-- than @base < 5@, because the interface can change rapidly without much warning.
--
-- @since 4.5.0.0

module GHC.Stats
    ( -- * Runtime statistics
      RTSStats(..), GCDetails(..), RtsTime
    , getRTSStats
    , getRTSStatsEnabled
    ) where

import GHC.Internal.Stats

