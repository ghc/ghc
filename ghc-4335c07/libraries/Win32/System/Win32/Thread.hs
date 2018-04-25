{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Thread
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   This modules provides just thread control APIs.
   This modules doesn't provide thread register control APIs. Because these APIs are used for Debugging.
-}
module System.Win32.Thread
  ( THANDLE, TID
  , getCurrentThread
  , suspendThread
  , c_SuspendThread
  , resumeThread
  , c_ResumeThread
  , withSuspendedThread
  , getThreadId
  , c_GetThreadId
  , getCurrentThreadId
  , c_GetCurrentThreadId
  ) where

import System.Win32.DebugApi
import System.Win32.Types ( failIfZero )

#include "windows_cconv.h"

getThreadId :: THANDLE -> IO TID
getThreadId = failIfZero "GetThreadId" . c_GetThreadId

getCurrentThreadId :: IO TID
getCurrentThreadId = failIfZero "GetThreadId" c_GetCurrentThreadId

foreign import WINDOWS_CCONV "windows.h GetCurrentThread"
    getCurrentThread :: IO THANDLE

foreign import WINDOWS_CCONV "windows.h GetThreadId"
    c_GetThreadId :: THANDLE -> IO TID

foreign import WINDOWS_CCONV "windows.h GetCurrentThreadId"
    c_GetCurrentThreadId :: IO TID
