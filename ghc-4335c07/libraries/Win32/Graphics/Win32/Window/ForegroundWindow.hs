{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.Window.ForegroundWindow
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Get/Set Foreground Window.
-}

module Graphics.Win32.Window.ForegroundWindow
  ( getForegroundWindow
  , setForegroundWindow
  , c_SetForegroundWindow
  , allowSetForegroundWindow
  , c_AllowSetForegroundWindow
  ) where

import Control.Monad            ( void )
import Graphics.Win32.GDI.Types ( HWND )
import Graphics.Win32.Window    ( getForegroundWindow )
import System.Win32.Process     ( ProcessId )

#include "windows_cconv.h"

----------------------------------------------------------------
-- | Setting Window to Foreground.
-- See: <https://github.com/haskell/win32/pull/9>,
-- <http://stackoverflow.com/questions/14297146/win32-setforegroundwindow-in-haskell>.
----------------------------------------------------------------
setForegroundWindow :: HWND -> IO Bool
setForegroundWindow = c_SetForegroundWindow

foreign import WINDOWS_CCONV safe "windows.h SetForegroundWindow"
    c_SetForegroundWindow :: HWND -> IO Bool

----------------------------------------------------------------
-- | Allow other process to set Window to Foreground
-- by using 'setForegroundWindow' function.
allowSetForegroundWindow :: ProcessId -> IO ()
allowSetForegroundWindow = void . c_AllowSetForegroundWindow

foreign import WINDOWS_CCONV safe "windows.h AllowSetForegroundWindow"
    c_AllowSetForegroundWindow :: ProcessId -> IO Bool
