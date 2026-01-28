{-# LANGUAGE CPP #-}

-- |
-- Module      :  GHC.Event.TimeOut
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/392>)
-- Portability :  non-portable
--
-- Common Timer definitions shared between WinIO and RIO.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.Event.TimeOut should be removed in GHC 10.02."
#endif

module GHC.Event.TimeOut
  {-# DEPRECATED "GHC.Event.TimeOut is deprecated and will be removed in GHC 10.02. See https://github.com/well-typed/reinstallable-base/tree/main/hackage-uses-of-internals/stability-risk-3 for context." #-}
    ( TimeoutQueue
    , TimeoutCallback
    , TimeoutEdit
    , TimeoutKey(..)
    ) where

import GHC.Internal.Event.TimeOut
