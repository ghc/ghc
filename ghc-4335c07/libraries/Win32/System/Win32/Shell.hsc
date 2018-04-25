#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Shell
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- Win32 stuff from shell32.dll
--
-----------------------------------------------------------------------------

module System.Win32.Shell (
  sHGetFolderPath,
  CSIDL,
  cSIDL_PROFILE,
  cSIDL_APPDATA,
  cSIDL_WINDOWS,
  cSIDL_PERSONAL,
  cSIDL_LOCAL_APPDATA,
  cSIDL_DESKTOPDIRECTORY,
  cSIDL_PROGRAM_FILES,
  SHGetFolderPathFlags,
  sHGFP_TYPE_CURRENT,
  sHGFP_TYPE_DEFAULT
 ) where

import System.Win32.Types
import Graphics.Win32.GDI.Types (HWND)

import Foreign
import Foreign.C
import Control.Monad
import System.IO.Error

##include "windows_cconv.h"

-- for SHGetFolderPath stuff
#define _WIN32_IE 0x500
#include <windows.h>
#include <shlobj.h>

----------------------------------------------------------------
-- SHGetFolderPath
--
-- XXX: this is deprecated in Vista and later
----------------------------------------------------------------

type CSIDL = CInt

#{enum CSIDL,
 , cSIDL_PROFILE  = CSIDL_PROFILE
 , cSIDL_APPDATA  = CSIDL_APPDATA
 , cSIDL_WINDOWS  = CSIDL_WINDOWS
 , cSIDL_PERSONAL = CSIDL_PERSONAL
 , cSIDL_LOCAL_APPDATA = CSIDL_LOCAL_APPDATA
 , cSIDL_DESKTOPDIRECTORY = CSIDL_DESKTOPDIRECTORY
 , cSIDL_PROGRAM_FILES = CSIDL_PROGRAM_FILES
 }
-- XXX there are lots more of these

type SHGetFolderPathFlags = DWORD

#{enum SHGetFolderPathFlags,
 , sHGFP_TYPE_CURRENT = SHGFP_TYPE_CURRENT
 , sHGFP_TYPE_DEFAULT = SHGFP_TYPE_DEFAULT
 }

sHGetFolderPath :: HWND -> CSIDL -> HANDLE -> SHGetFolderPathFlags -> IO String
sHGetFolderPath hwnd csidl hdl flags =
  allocaBytes ((#const MAX_PATH) * (#size TCHAR)) $ \pstr -> do
    r <- c_SHGetFolderPath hwnd csidl hdl flags pstr
    when (r < 0) $ raiseUnsupported "sHGetFolderPath"
    peekTString pstr

raiseUnsupported :: String -> IO ()
raiseUnsupported loc = 
   ioError (ioeSetErrorString (mkIOError illegalOperationErrorType loc Nothing Nothing) "unsupported operation")

foreign import WINDOWS_CCONV unsafe "SHGetFolderPathW"
  c_SHGetFolderPath :: HWND -> CInt -> HANDLE -> DWORD -> LPTSTR
                    -> IO HRESULT
