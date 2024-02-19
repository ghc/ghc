-- |
-- Module      :  GHC.Event.Windows.ManagedThreadPool
-- Copyright   :  (c) Tamar Christina 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- WinIO Windows Managed Thread pool API.  This thread pool scales dynamically
-- based on demand.
--
-------------------------------------------------------------------------------

module GHC.Event.Windows.ManagedThreadPool
  ( ThreadPool(..)
  , startThreadPool
  , notifyRunning
  , notifyWaiting
  , monitorThreadPool
  ) where

import GHC.Internal.Event.Windows.ManagedThreadPool
