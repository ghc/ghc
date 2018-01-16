{-# LANGUAGE CPP #-}
{- |
   Module      :  Graphics.Win32.Window.IMM
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   An FFI binding to the IMM (Input Method Manager) part of the Win32 API.
-}
module Graphics.Win32.Window.IMM where
import Foreign.Marshal.Alloc    ( alloca )
import Foreign.Marshal.Utils    ( fromBool )
import Foreign.Ptr              ( Ptr )
import Foreign.Storable         ( peek )
import Graphics.Win32.GDI.Types ( HWND )
import Graphics.Win32.Key       ( VKey )
import System.Win32.Types       ( UINT, DWORD, LPDWORD, BOOL, failIfFalse_ )

#include <windows.h>
##include "windows_cconv.h"

type HIMC = Ptr ()

foreign import WINDOWS_CCONV "windows.h ImmGetContext"
  immGetContext     :: HWND -> IO HIMC

foreign import WINDOWS_CCONV "windows.h ImmGetOpenStatus"
  immGetOpenStatus :: HIMC -> IO BOOL

immSetOpenStatus :: HIMC -> BOOL -> IO ()
immSetOpenStatus imc flg =
  failIfFalse_ (unwords ["ImmSetOpenStatus", show imc, show flg])
    $ c_ImmSetOpenStatus imc (fromBool flg)

foreign import WINDOWS_CCONV "windows.h ImmSetOpenStatus"
  c_ImmSetOpenStatus  :: HIMC -> UINT -> IO BOOL


data IMEMode = IMEMode DWORD DWORD

immGetConversionStatus :: HIMC -> IO IMEMode
immGetConversionStatus imc =
  alloca $ \lpConv ->
  alloca $ \lpStnc -> do
    failIfFalse_ (unwords ["ImmGetConversionStatus", show imc, show lpConv, show lpStnc]) $
      c_ImmGetConversionStatus imc lpConv lpStnc
    conv <- peek lpConv
    stnc <- peek lpStnc
    return $ IMEMode conv stnc

foreign import WINDOWS_CCONV "windows.h ImmGetConversionStatus"
  c_ImmGetConversionStatus :: HIMC -> LPDWORD ->  LPDWORD -> IO BOOL

immSetConversionStatus :: HIMC -> IMEMode -> IO ()
immSetConversionStatus imc (IMEMode conv stnc) =
  failIfFalse_ (unwords ["ImmSetConversionStatus", show imc, show conv, show stnc])
    $ c_ImmSetConversionStatus imc conv stnc

foreign import WINDOWS_CCONV "windows.h ImmSetConversionStatus"
  c_ImmSetConversionStatus :: HIMC -> DWORD -> DWORD -> IO BOOL

-- iMN_SETCONVERSIONSTATUS = #const IMN_SETCONVERSIONSTATUS

#{enum DWORD,
 , iME_CMODE_ALPHANUMERIC  = IME_CMODE_ALPHANUMERIC
 , iME_CMODE_CHARCODE      = IME_CMODE_CHARCODE
 , iME_CMODE_EUDC          = IME_CMODE_EUDC
 , iME_CMODE_FIXED         = IME_CMODE_FIXED
 , iME_CMODE_FULLSHAPE     = IME_CMODE_FULLSHAPE
 , iME_CMODE_HANJACONVERT  = IME_CMODE_HANJACONVERT
 , iME_CMODE_KATAKANA      = IME_CMODE_KATAKANA
 , iME_CMODE_NATIVE        = IME_CMODE_NATIVE
 , iME_CMODE_NOCONVERSION  = IME_CMODE_NOCONVERSION
 , iME_CMODE_ROMAN         = IME_CMODE_ROMAN
 , iME_CMODE_SOFTKBD       = IME_CMODE_SOFTKBD
 , iME_CMODE_SYMBOL        = IME_CMODE_SYMBOL
 }

#{enum DWORD,
 , iME_SMODE_AUTOMATIC     = IME_SMODE_AUTOMATIC
 , iME_SMODE_NONE          = IME_SMODE_NONE
 , iME_SMODE_PHRASEPREDICT = IME_SMODE_PHRASEPREDICT
 , iME_SMODE_PLAURALCLAUSE = IME_SMODE_PLAURALCLAUSE
 , iME_SMODE_SINGLECONVERT = IME_SMODE_SINGLECONVERT
 }
{-
 , iME_SMODE_CONVERSATION  = IME_SMODE_CONVERSATION
-}

immReleaseContext :: HWND -> HIMC -> IO ()
immReleaseContext wnd imc =
  failIfFalse_ (unwords ["ImmSetOpenStatus", show wnd, show imc])
    $ c_ImmReleaseContext wnd imc

foreign import WINDOWS_CCONV "windows.h ImmReleaseContext"
  c_ImmReleaseContext :: HWND -> HIMC -> IO BOOL

foreign import WINDOWS_CCONV "windows.h ImmGetVirtualKey"
  immGetVirtualKey :: HWND -> IO VKey

immSimulateHotKey :: HWND -> DWORD -> IO ()
immSimulateHotKey hwd hkey =
  failIfFalse_ (unwords ["ImmSimulateHotKey", show hwd, show hkey])
    $ c_ImmSimulateHotKey hwd hkey

foreign import WINDOWS_CCONV "windows.h ImmSimulateHotKey"
  c_ImmSimulateHotKey :: HWND -> DWORD -> IO BOOL
