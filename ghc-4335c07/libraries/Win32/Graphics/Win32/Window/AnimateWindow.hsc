{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.Window.AnimateWindow
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Provide AnimatedWindow function and flags.
-}
module Graphics.Win32.Window.AnimateWindow where
import Graphics.Win32.GDI.Types ( HWND )
import System.Win32.Types       ( DWORD, BOOL, failIfFalse_ )

#include <windows.h>
##include "windows_cconv.h"
#include "winuser_compat.h"

type AnimateWindowType = DWORD

#{enum AnimateWindowType,
 , aW_SLIDE        = AW_SLIDE
 , aW_ACTIVATE     = AW_ACTIVATE
 , aW_BLEND        = AW_BLEND
 , aW_HIDE         = AW_HIDE
 , aW_CENTER       = AW_CENTER
 , aW_HOR_POSITIVE = AW_HOR_POSITIVE
 , aW_HOR_NEGATIVE = AW_HOR_NEGATIVE
 , aW_VER_POSITIVE = AW_VER_POSITIVE
 , aW_VER_NEGATIVE = AW_VER_NEGATIVE
 }

animateWindow :: HWND -> DWORD -> AnimateWindowType -> IO ()
animateWindow hwnd dwTime dwFlags
  = failIfFalse_ "AnimateWindow" $ c_AnimateWindow hwnd dwTime dwFlags

foreign import WINDOWS_CCONV "windows.h AnimateWindow"
    c_AnimateWindow :: HWND -> DWORD -> AnimateWindowType -> IO BOOL
