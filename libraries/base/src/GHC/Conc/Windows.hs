{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.Internal.Conc.Windows
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager interfaces. Depending on which I/O Subsystem is used
-- requests will be routed to different places.
--
--
module GHC.Conc.Windows

#if defined(javascript_HOST_ARCH)
       () where

#else
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

       -- * Console event handler
       , module GHC.Internal.Event.Windows.ConsoleEvent
       ) where

import GHC.Internal.Conc.Windows
import GHC.Internal.Event.Windows.ConsoleEvent

#endif
