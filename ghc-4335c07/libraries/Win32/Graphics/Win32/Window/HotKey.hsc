{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.Window.HotKey
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   An FFI binding to the hot key part of the Win32 API.
-}
module Graphics.Win32.Window.HotKey where

import Data.Bits                 ( (.|.) )
import Graphics.Win32.GDI.Types  ( HWND, MbHWND )
import Graphics.Win32.Key        ( VKey )
import Graphics.Win32.Message    ( WindowMessage )
import System.Win32.Types        ( UINT, BOOL, maybePtr, failIfFalse_ )
import System.Win32.Exception.Unsupported ( unsupportedVal, upgradeWindowsOS )
import System.Win32.Info.Version ( is7OrLater )

#include <windows.h>
##include "windows_cconv.h"

type FsModifiers = [FsModifier]
type FsModifier  = UINT

#{enum FsModifier,
 , mOD_ALT      = MOD_ALT
 , mOD_CONTROL  = MOD_CONTROL
 , mOD_SHIFT    = MOD_SHIFT
 , mOD_WIN      = MOD_WIN
}

-- | This parameter requires to use Windows 7 or later.
mOD_NOREPEAT :: FsModifier
mOD_NOREPEAT
  = unsupportedVal "MOD_NOREPEAT"
      is7OrLater (upgradeWindowsOS "Windows 7") 0x4000
{-
 , mOD_NOREPEAT = MOD_NOREPEAT
-}

wM_HOTKEY :: WindowMessage
wM_HOTKEY = #const WM_HOTKEY

joinModifiers :: FsModifiers -> FsModifier
joinModifiers = foldr (.|.) 0

registerHotKey :: MbHWND -> Int -> FsModifier -> VKey -> IO ()
registerHotKey mb_wnd kid md vkey =
  failIfFalse_ (unwords ["RegisterHotKey", show mb_wnd, show kid, show md, show vkey])
    $ c_RegisterHotKey (maybePtr mb_wnd) kid md vkey

foreign import WINDOWS_CCONV "windows.h RegisterHotKey"
  c_RegisterHotKey :: HWND -> Int -> UINT -> VKey -> IO BOOL

unregisterHotKey :: MbHWND -> Int -> IO ()
unregisterHotKey mb_wnd kid =
  failIfFalse_ (unwords ["UnregisterHotKey", show mb_wnd, show kid])
    $ c_UnregisterHotKey (maybePtr mb_wnd) kid

foreign import WINDOWS_CCONV "windows.h UnregisterHotKey"
  c_UnregisterHotKey :: HWND -> Int -> IO BOOL
