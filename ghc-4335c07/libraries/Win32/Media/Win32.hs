{-# LANGUAGE CPP #-}
{- |
   Module      :  Media.Win32
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Multimedia API. TODO: provide more functions ...
-}

module Media.Win32
  ( module Media.Win32
  ) where
import Control.Monad                ( unless )
import Prelude               hiding ( ioError, userError )
import System.IO.Error              ( ioError, userError )
import System.Win32.Encoding        ( encodeMultiByte, getCurrentCodePage )
import System.Win32.Types
import System.Win32.String          ( withTStringBufferLen )

type MCIERROR = DWORD

#include "windows_cconv.h"

mciSendString :: String -> IO ()
mciSendString cmd
 = withTString cmd $ \sendCmd -> do
     err <- c_mciSendString sendCmd nullPtr 0 nullPtr
     unless (err == 0)
       $ mciGetErrorString err

foreign import WINDOWS_CCONV safe "windows.h mciSendStringW"
  c_mciSendString :: LPCTSTR -> LPTSTR -> UINT -> HANDLE -> IO MCIERROR

mciGetErrorString :: MCIERROR -> IO ()
mciGetErrorString err
  = withTStringBufferLen 256 $ \(cstr, len) ->  do
      failIfFalse_ (unwords ["mciGetErrorString", show err]) $
        c_mciGetErrorString err cstr $ fromIntegral len
      msg <- peekTString cstr
      cp  <- getCurrentCodePage
      ioError $ userError $ encodeMultiByte cp msg

foreign import WINDOWS_CCONV unsafe "windows.h mciGetErrorStringW"
  c_mciGetErrorString :: MCIERROR -> LPTSTR -> UINT -> IO BOOL
