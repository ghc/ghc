{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Conc.WinIO
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O Completion Port interface to the one defined in
-- GHC.Event.Windows.
--
-- This module is an indirection to keep things in the same structure as before
-- but also to keep the new code where the actual I/O manager is.  As such it
-- just re-exports "GHC.Event.Windows.Thread"
--

module GHC.Conc.WinIO
       ( module GHC.Internal.Event.Windows.Thread ) where

import GHC.Internal.Event.Windows.Thread
