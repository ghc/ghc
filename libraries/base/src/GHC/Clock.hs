{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.Clock
-- License     :  BSD-style (see the file LICENSE in this distribution)
--
-- Stability   :  internal
-- Portability :  portable
--
-- System monotonic time.
--

module GHC.Clock
    ( getMonotonicTime
    , getMonotonicTimeNSec
    ) where

import GHC.Internal.Clock
