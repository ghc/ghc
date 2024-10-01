{-# LANGUAGE Safe #-}

-- |
-- Module      :  RTS.Stats.Experimental
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/ghc-experimental/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- /The API of this module is unstable and is coupled to GHC's internals./ As
-- such if you depend on it, you should expect to follow GHC's releases. This
-- API could change without warning.

module GHC.Stats.Experimental
    ( -- * Runtime statistics
      RTSStats(..), GCDetails(..), RtsTime
    , getRTSStats
    , getRTSStatsEnabled
    ) where

import GHC.Internal.Stats
