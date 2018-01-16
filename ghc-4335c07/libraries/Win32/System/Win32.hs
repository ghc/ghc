#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- An FFI binding to the system part of the Win32 API.
--
-----------------------------------------------------------------------------

module System.Win32
        ( module System.Win32.DLL
        , module System.Win32.File
        , module System.Win32.FileMapping
        , module System.Win32.Info
        , module System.Win32.Mem
        , module System.Win32.MinTTY
        , module System.Win32.NLS
        , module System.Win32.Process
        , module System.Win32.Registry
        , module System.Win32.Time
        , module System.Win32.Console
        , module System.Win32.Security
        , module System.Win32.Types
        , module System.Win32.Shell
        , module System.Win32.Automation
        , module System.Win32.HardLink
        , module System.Win32.SymbolicLink
        , module System.Win32.Thread
        , module System.Win32.Utils
        ) where

import System.Win32.DLL
import System.Win32.File
import System.Win32.FileMapping
import System.Win32.Info
import System.Win32.Mem
import System.Win32.MinTTY
import System.Win32.NLS hiding  ( LCID, LANGID, SortID, SubLANGID
                                , PrimaryLANGID, mAKELCID, lANGIDFROMLCID
                                , sORTIDFROMLCID, mAKELANGID, pRIMARYLANGID
                                , sUBLANGID )
import System.Win32.Process
import System.Win32.Registry
import System.Win32.Time
import System.Win32.Console
import System.Win32.Types
import System.Win32.Security
import System.Win32.Shell

import System.Win32.Automation
import System.Win32.HardLink
import System.Win32.SymbolicLink
import System.Win32.Thread
import System.Win32.Utils hiding ( try )

----------------------------------------------------------------
-- End
----------------------------------------------------------------
