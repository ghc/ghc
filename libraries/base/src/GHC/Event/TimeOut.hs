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
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.

module GHC.Event.TimeOut
    ( TimeoutQueue
    , TimeoutCallback
    , TimeoutEdit
    , TimeoutKey(..)
    ) where

import GHC.Internal.Event.TimeOut
