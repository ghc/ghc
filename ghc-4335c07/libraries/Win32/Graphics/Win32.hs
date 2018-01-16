#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- An interface to the Microsoft Windows user interface.
-- See <http://msdn.microsoft.com/library/> under /User Interface Design
-- and Development/ and then /Windows User Interface/ for more details
-- of the underlying library.
--
-----------------------------------------------------------------------------

module Graphics.Win32 (
        module System.Win32.Types,
        module Graphics.Win32.Control,
        module Graphics.Win32.Dialogue,
        module Graphics.Win32.GDI,
        module Graphics.Win32.Icon,
        module Graphics.Win32.Key,
        module Graphics.Win32.Menu,
        module Graphics.Win32.Message,
        module Graphics.Win32.Misc,
        module Graphics.Win32.Resource,
        module Graphics.Win32.Window,
        module Graphics.Win32.LayeredWindow,
        module Graphics.Win32.Window.AnimateWindow,
        module Graphics.Win32.Window.ForegroundWindow,
        module Graphics.Win32.Window.IMM,
        module Graphics.Win32.Window.PostMessage
        ) where

import System.Win32.Types
import Graphics.Win32.Control
import Graphics.Win32.Dialogue
import Graphics.Win32.GDI
import Graphics.Win32.Icon
import Graphics.Win32.Key
import Graphics.Win32.Menu
import Graphics.Win32.Message
import Graphics.Win32.Misc
import Graphics.Win32.Resource
import Graphics.Win32.Window
import Graphics.Win32.LayeredWindow
import Graphics.Win32.Window.AnimateWindow
import Graphics.Win32.Window.ForegroundWindow
import Graphics.Win32.Window.IMM
import Graphics.Win32.Window.PostMessage
