{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Console.Title
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Get/Set the title for the current console window.
-}
module System.Win32.Console.Title where

import System.Win32.String ( LPTSTR, LPCTSTR
                           , withTStringBufferLen, withTString, peekTStringLen )
import System.Win32.Types  ( BOOL, failIfFalse_, failIfZero )
import System.Win32.Word   ( DWORD )

#include <windows.h>
##include "windows_cconv.h"

getConsoleTitle :: IO String
getConsoleTitle =
  withTStringBufferLen maxLength $ \(buf, len) -> do
      len' <- failIfZero "GetConsoleTitle"
        $ c_GetConsoleTitle buf (fromIntegral len)
      peekTStringLen (buf, (fromIntegral len'))
  where
    maxLength = #const MAX_PATH

setConsoleTitle :: String -> IO ()
setConsoleTitle title =
  withTString title $ \buf ->
      failIfFalse_ (unwords ["SetConsoleTitle", title])
        $ c_SetConsoleTitle buf

foreign import WINDOWS_CCONV "windows.h GetConsoleTitleW"
  c_GetConsoleTitle :: LPTSTR -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV "windows.h SetConsoleTitleW"
  c_SetConsoleTitle :: LPCTSTR -> IO BOOL

