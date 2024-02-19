-- |
-- Module      :  GHC.Event.Windows.ConsoleEvent
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

module GHC.Event.Windows.ConsoleEvent (
  ConsoleEvent (..),
  start_console_handler,
  toWin32ConsoleEvent,
  win32ConsoleHandler
) where

import GHC.Internal.Event.Windows.ConsoleEvent
