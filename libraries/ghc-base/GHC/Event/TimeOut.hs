{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.TimeOut
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- Common Timer definitions shared between WinIO and RIO.
--
-------------------------------------------------------------------------------

module GHC.Event.TimeOut where

import GHC.IO
import GHC.Base

import qualified GHC.Event.PSQ as Q
import GHC.Event.Unique (Unique)

-- | A priority search queue, with timeouts as priorities.
type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

-- | A timeout registration cookie.
newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)
