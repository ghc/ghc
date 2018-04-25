{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.Window.PostMessage
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Provide PostMessage function and friends.
-}
module Graphics.Win32.Window.PostMessage where

import Foreign.C.Types ( CIntPtr(..) )
import Graphics.Win32.GDI.Types ( HWND, MbHWND )
import Graphics.Win32.Message   ( WindowMessage )
import System.Win32.Types       ( DWORD, WPARAM, LPARAM, BOOL
                                , maybePtr, castUINTPtrToPtr, failIfFalse_ )

#include <windows.h>
##include "windows_cconv.h"

postMessage :: MbHWND -> WindowMessage -> WPARAM -> LPARAM -> IO ()
postMessage mb_wnd msg w l =
  failIfFalse_ (unwords ["PostMessage", show mb_wnd, show msg, show w, show l]) $
    c_PostMessage (maybePtr mb_wnd) msg w l

foreign import WINDOWS_CCONV "windows.h PostMessageW"
  c_PostMessage :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO BOOL

foreign import WINDOWS_CCONV "windows.h PostQuitMessage"
  postQuitMessage :: Int -> IO ()

postThreadMessage :: DWORD -> WindowMessage -> WPARAM -> LPARAM -> IO ()
postThreadMessage tId msg w l =
  failIfFalse_ (unwords ["PostThreadMessage", show tId, show msg, show w, show l]) $
    c_PostThreadMessage tId msg w l

foreign import WINDOWS_CCONV "windows.h PostThreadMessageW"
  c_PostThreadMessage :: DWORD -> WindowMessage -> WPARAM -> LPARAM -> IO BOOL

#{enum HWND, castUINTPtrToPtr
 , hWND_BROADCAST = (UINT_PTR)HWND_BROADCAST
 }

foreign import WINDOWS_CCONV "windows.h InSendMessage"
  inSendMessage :: IO Bool
