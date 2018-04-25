#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Key
-- Copyright   :  (c) Alastair Reid, 1997-2003, 2013 shelarcy
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module Graphics.Win32.Key where

import Control.Monad (liftM)
import Graphics.Win32.GDI.Types (HWND)
import System.Win32.Types    ( DWORD, UINT, WORD, ptrToMaybe, BOOL, SHORT,
                               failIfFalse_, failIfZero )
import Control.Exception     ( bracket )
import Foreign.Ptr           ( Ptr, nullPtr )
import Foreign.C.Types       ( CWchar(..) )
import Foreign.Marshal.Array ( allocaArray, peekArray )
import System.Win32.String   ( LPTSTR, LPCTSTR
                             , withTString, withTStringBuffer, peekTString )
import System.Win32.Thread   ( TID, getCurrentThreadId )

##include "windows_cconv.h"

#include <windows.h>
#include "winuser_compat.h"

type VKey   = DWORD

#{enum VKey,
 , vK_LBUTTON             = VK_LBUTTON
 , vK_RBUTTON             = VK_RBUTTON
 , vK_CANCEL              = VK_CANCEL
 , vK_MBUTTON             = VK_MBUTTON
 , vK_BACK                = VK_BACK
 , vK_TAB                 = VK_TAB
 , vK_CLEAR               = VK_CLEAR
 , vK_RETURN              = VK_RETURN
 , vK_SHIFT               = VK_SHIFT
 , vK_CONTROL             = VK_CONTROL
 , vK_MENU                = VK_MENU
 , vK_PAUSE               = VK_PAUSE
 , vK_CAPITAL             = VK_CAPITAL
 , vK_ESCAPE              = VK_ESCAPE
 , vK_SPACE               = VK_SPACE
 , vK_PRIOR               = VK_PRIOR
 , vK_NEXT                = VK_NEXT
 , vK_END                 = VK_END
 , vK_HOME                = VK_HOME
 , vK_LEFT                = VK_LEFT
 , vK_UP                  = VK_UP
 , vK_RIGHT               = VK_RIGHT
 , vK_DOWN                = VK_DOWN
 , vK_SELECT              = VK_SELECT
 , vK_EXECUTE             = VK_EXECUTE
 , vK_SNAPSHOT            = VK_SNAPSHOT
 , vK_INSERT              = VK_INSERT
 , vK_DELETE              = VK_DELETE
 , vK_HELP                = VK_HELP
 , vK_NUMPAD0             = VK_NUMPAD0
 , vK_NUMPAD1             = VK_NUMPAD1
 , vK_NUMPAD2             = VK_NUMPAD2
 , vK_NUMPAD3             = VK_NUMPAD3
 , vK_NUMPAD4             = VK_NUMPAD4
 , vK_NUMPAD5             = VK_NUMPAD5
 , vK_NUMPAD6             = VK_NUMPAD6
 , vK_NUMPAD7             = VK_NUMPAD7
 , vK_NUMPAD8             = VK_NUMPAD8
 , vK_NUMPAD9             = VK_NUMPAD9
 , vK_MULTIPLY            = VK_MULTIPLY
 , vK_ADD                 = VK_ADD
 , vK_SEPARATOR           = VK_SEPARATOR
 , vK_SUBTRACT            = VK_SUBTRACT
 , vK_DECIMAL             = VK_DECIMAL
 , vK_DIVIDE              = VK_DIVIDE
 , vK_F1                  = VK_F1
 , vK_F2                  = VK_F2
 , vK_F3                  = VK_F3
 , vK_F4                  = VK_F4
 , vK_F5                  = VK_F5
 , vK_F6                  = VK_F6
 , vK_F7                  = VK_F7
 , vK_F8                  = VK_F8
 , vK_F9                  = VK_F9
 , vK_F10                 = VK_F10
 , vK_F11                 = VK_F11
 , vK_F12                 = VK_F12
 , vK_F13                 = VK_F13
 , vK_F14                 = VK_F14
 , vK_F15                 = VK_F15
 , vK_F16                 = VK_F16
 , vK_F17                 = VK_F17
 , vK_F18                 = VK_F18
 , vK_F19                 = VK_F19
 , vK_F20                 = VK_F20
 , vK_F21                 = VK_F21
 , vK_F22                 = VK_F22
 , vK_F23                 = VK_F23
 , vK_F24                 = VK_F24
 , vK_NUMLOCK             = VK_NUMLOCK
 , vK_SCROLL              = VK_SCROLL
  , vK_XBUTTON1           = VK_XBUTTON1
 , vK_XBUTTON2            = VK_XBUTTON2
 , vK_KANA                = VK_KANA
 , vK_HANGUL              = VK_HANGUL
 , vK_JUNJA               = VK_JUNJA
 , vK_FINAL               = VK_FINAL
 , vK_HANJA               = VK_HANJA
 , vK_KANJI               = VK_KANJI
 , vK_CONVERT             = VK_CONVERT
 , vK_NONCONVERT          = VK_NONCONVERT
 , vK_ACCEPT              = VK_ACCEPT
 , vK_MODECHANGE          = VK_MODECHANGE
 , vK_PRINT               = VK_PRINT
 , vK_APPS                = VK_APPS
 , vK_SLEEP               = VK_SLEEP
 , vK_LWIN                = VK_LWIN
 , vK_RWIN                = VK_RWIN
 , vK_LSHIFT              = VK_LSHIFT
 , vK_RSHIFT              = VK_RSHIFT
 , vK_LCONTROL            = VK_LCONTROL
 , vK_RCONTROL            = VK_RCONTROL
 , vK_LMENU               = VK_LMENU
 , vK_RMENU               = VK_RMENU
 , vK_BROWSER_BACK        = VK_BROWSER_BACK
 , vK_BROWSER_FORWARD     = VK_BROWSER_FORWARD
 , vK_BROWSER_REFRESH     = VK_BROWSER_REFRESH
 , vK_BROWSER_STOP        = VK_BROWSER_STOP
 , vK_BROWSER_SEARCH      = VK_BROWSER_SEARCH
 , vK_BROWSER_FAVORITES   = VK_BROWSER_FAVORITES
 , vK_BROWSER_HOME        = VK_BROWSER_HOME
 , vK_VOLUME_MUTE         = VK_VOLUME_MUTE
 , vK_VOLUME_DOWN         = VK_VOLUME_DOWN
 , vK_VOLUME_UP           = VK_VOLUME_UP
 , vK_MEDIA_NEXT_TRACK    = VK_MEDIA_NEXT_TRACK
 , vK_MEDIA_PREV_TRACK    = VK_MEDIA_PREV_TRACK
 , vK_MEDIA_STOP          = VK_MEDIA_STOP
 , vK_MEDIA_PLAY_PAUSE    = VK_MEDIA_PLAY_PAUSE
 , vK_LAUNCH_MAIL         = VK_LAUNCH_MAIL
 , vK_LAUNCH_MEDIA_SELECT = VK_LAUNCH_MEDIA_SELECT
 , vK_LAUNCH_APP1         = VK_LAUNCH_APP1
 , vK_LAUNCH_APP2         = VK_LAUNCH_APP2
 , vK_OEM_1               = VK_OEM_1
 , vK_OEM_PLUS            = VK_OEM_PLUS
 , vK_OEM_COMMA           = VK_OEM_COMMA
 , vK_OEM_MINUS           = VK_OEM_MINUS
 , vK_OEM_PERIOD          = VK_OEM_PERIOD
 , vK_OEM_2               = VK_OEM_2
 , vK_OEM_3               = VK_OEM_3
 , vK_OEM_4               = VK_OEM_4
 , vK_OEM_5               = VK_OEM_5
 , vK_OEM_6               = VK_OEM_6
 , vK_OEM_7               = VK_OEM_7
 , vK_OEM_8               = VK_OEM_8
 , vK_OEM_102             = VK_OEM_102
 , vK_PROCESSKEY          = VK_PROCESSKEY
 , vK_PACKET              = VK_PACKET
 , vK_ATTN                = VK_ATTN
 , vK_CRSEL               = VK_CRSEL
 , vK_EXSEL               = VK_EXSEL
 , vK_EREOF               = VK_EREOF
 , vK_PLAY                = VK_PLAY
 , vK_ZOOM                = VK_ZOOM
 , vK_NONAME              = VK_NONAME
 , vK_PA1                 = VK_PA1
 , vK_OEM_CLEAR           = VK_OEM_CLEAR
 }
foreign import WINDOWS_CCONV unsafe "windows.h VkKeyScanExW"
    c_VkKeyScanEx :: CWchar -> HKL -> IO SHORT

foreign import WINDOWS_CCONV unsafe "windows.h MapVirtualKeyW"
    c_MapVirtualKey :: VKey -> UINT -> IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h MapVirtualKeyExW"
    c_MapVirtualKeyEx :: VKey -> UINT -> HKL -> IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h EnableWindow"
  enableWindow :: HWND -> Bool -> IO Bool

getActiveWindow :: IO (Maybe HWND)
getActiveWindow = liftM ptrToMaybe c_GetActiveWindow
foreign import WINDOWS_CCONV unsafe "windows.h GetActiveWindow"
  c_GetActiveWindow :: IO HWND

foreign import WINDOWS_CCONV unsafe "windows.h GetAsyncKeyState"
  getAsyncKeyState :: Int -> IO WORD

getFocus :: IO (Maybe HWND)
getFocus = liftM ptrToMaybe c_GetFocus
foreign import WINDOWS_CCONV unsafe "windows.h GetFocus"
  c_GetFocus :: IO HWND

foreign import WINDOWS_CCONV unsafe "windows.h GetKBCodePage"
  getKBCodePage :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h IsWindowEnabled"
  isWindowEnabled :: HWND -> IO Bool

getCurrentKeyboardLayout :: IO HKL
getCurrentKeyboardLayout = do
    tid <- getCurrentThreadId
    c_GetKeyboardLayout tid

getKeyboardLayoutList :: IO [HKL]
getKeyboardLayoutList = do
    len' <- failIfZero "GetKeyboardLayoutList" $ c_GetKeyboardLayoutList 0 nullPtr
    let len = fromIntegral len'
    allocaArray len $ \buf -> do
        _ <- failIfZero "GetKeyboardLayoutList" $ c_GetKeyboardLayoutList len  buf
        peekArray len buf

getKeyboardLayoutName :: IO String
getKeyboardLayoutName
  = withTStringBuffer 256 $ \buf -> do
       failIfFalse_ "GetKeyboardLayoutName" $ c_GetKeyboardLayoutName buf
       peekTString buf

withLoadKeyboardLayout :: KeyLayoutFlags -> (HKL -> IO a) -> IO a
withLoadKeyboardLayout flag io
  = withTStringBuffer 256 $ \buf -> do
       failIfFalse_ "GetKeyboardLayoutName" $ c_GetKeyboardLayoutName buf
       bracket (c_LoadKeyboardLayout buf flag)
               unloadKeyboardLayout
               io

withLoadKeyboardLayoutWithName :: String -> KeyLayoutFlags -> (HKL -> IO a) -> IO a
withLoadKeyboardLayoutWithName str flag io
  = withTString str $ \c_str ->
      bracket (c_LoadKeyboardLayout c_str flag)
              unloadKeyboardLayout
              io

unloadKeyboardLayout :: HKL -> IO ()
unloadKeyboardLayout
  = failIfFalse_ "UnloadKeyboardLayout" . c_UnloadKeyboardLayout

foreign import WINDOWS_CCONV unsafe "windows.h GetKeyboardLayout"
    c_GetKeyboardLayout :: TID -> IO HKL

foreign import WINDOWS_CCONV unsafe "windows.h GetKeyboardLayoutList"
    c_GetKeyboardLayoutList :: Int -> (Ptr HKL) -> IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h GetKeyboardLayoutNameW"
    c_GetKeyboardLayoutName :: LPTSTR -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h LoadKeyboardLayoutW"
    c_LoadKeyboardLayout :: LPCTSTR  -> KeyLayoutFlags -> IO HKL

foreign import WINDOWS_CCONV unsafe "windows.h UnloadKeyboardLayout"
    c_UnloadKeyboardLayout :: HKL -> IO BOOL

type HKL = Ptr ()

type KeyLayoutFlags = UINT

#{enum KeyLayoutFlags,
 , kLF_ACTIVATE      = KLF_ACTIVATE
 , kLF_NOTELLSHELL   = KLF_NOTELLSHELL
 , kLF_REORDER       = KLF_REORDER
 , kLF_REPLACELANG   = KLF_REPLACELANG
 , kLF_SUBSTITUTE_OK = KLF_SUBSTITUTE_OK
 , kLF_SETFORPROCESS = KLF_SETFORPROCESS
 }
