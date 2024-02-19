{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Event.TimeOut
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- Common Timer definitions shared between WinIO and RIO.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-------------------------------------------------------------------------------

module GHC.Internal.Event.TimeOut
    ( TimeoutQueue
    , TimeoutCallback
    , TimeoutEdit
    , TimeoutKey(..)
    ) where

import GHC.Internal.IO
import GHC.Internal.Base

import qualified GHC.Internal.Event.PSQ as Q
import GHC.Internal.Event.Unique (Unique)

-- | A priority search queue, with timeouts as priorities.
type TimeoutQueue = Q.PSQ TimeoutCallback

-- |
-- Warning: since the 'TimeoutCallback' is called from the I/O manager, it must
-- not throw an exception or block for a long period of time.  In particular,
-- be wary of 'GHC.Internal.Control.Exception.throwTo' and 'Control.Concurrent.killThread':
-- if the target thread is making a foreign call, these functions will block
-- until the call completes.
type TimeoutCallback = IO ()

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

-- | A timeout registration cookie.
newtype TimeoutKey = TK Unique
    deriving (Eq, Ord)
