{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Conc.POSIX
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager
--
-- This is the I/O manager based on posix FDs for windows.
-- When using the winio manager these functions may not
-- be used as they will behave in unexpected ways.
--
-- TODO: This manager is currently the default. But we will eventually
-- switch to use winio instead.
--

module GHC.Conc.POSIX
       ( ensureIOManagerIsRunning
       , interruptIOManager

       -- * Waiting
       , threadDelay
       , registerDelay

       -- * Miscellaneous
       , asyncRead
       , asyncWrite
       , asyncDoProc

       , asyncReadBA
       , asyncWriteBA

       , module GHC.Internal.Event.Windows.ConsoleEvent
       ) where

import GHC.Internal.Conc.POSIX
import GHC.Internal.Event.Windows.ConsoleEvent
