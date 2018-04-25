{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.SymbolicLink
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Handling symbolic link using Win32 API. [Vista of later and desktop app only]

   Note: You should worry about UAC (User Account Control) when use this module's function in your application:

     * require to use 'Run As Administrator' to run your application.

     * or modify your application's manifect file to add
       \<requestedExecutionLevel level='requireAdministrator' uiAccess='false'/\>.
-}
module System.Win32.SymbolicLink
  ( module System.Win32.SymbolicLink
  ) where

import System.Win32.Types
import System.Win32.File ( failIfFalseWithRetry_ )

##include "windows_cconv.h"

type SymbolicLinkFlags = DWORD

#{enum SymbolicLinkFlags,
 , sYMBOLIC_LINK_FLAG_FILE      = 0x0
 , sYMBOLIC_LINK_FLAG_DIRECTORY = 0x1
}

-- | createSymbolicLink* functions don't check that file is exist or not.
--
-- NOTE: createSymbolicLink* functions are /flipped arguments/ to provide compatiblity for Unix,
-- except 'createSymbolicLink''.
-- 
-- If you want to create symbolic link by Windows way, use 'createSymbolicLink'' instead.
createSymbolicLink :: FilePath -- ^ Target file path
                   -> FilePath -- ^ Symbolic link name
                   -> SymbolicLinkFlags -> IO ()
createSymbolicLink = flip createSymbolicLink'

createSymbolicLinkFile :: FilePath -> FilePath -> IO ()
createSymbolicLinkFile target link = createSymbolicLink' link target sYMBOLIC_LINK_FLAG_FILE

createSymbolicLinkDirectory :: FilePath -> FilePath -> IO ()
createSymbolicLinkDirectory target link = createSymbolicLink' link target sYMBOLIC_LINK_FLAG_DIRECTORY

createSymbolicLink' :: FilePath -- ^ Symbolic link name
                    -> FilePath -- ^ Target file path
                    -> SymbolicLinkFlags -> IO ()
createSymbolicLink' link target flag = do
    withTString link $ \c_link ->
      withTString target $ \c_target ->
        failIfFalseWithRetry_ (unwords ["CreateSymbolicLink",show link,show target]) $
          c_CreateSymbolicLink c_link c_target flag

foreign import WINDOWS_CCONV unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink :: LPTSTR -> LPTSTR -> SymbolicLinkFlags -> IO BOOL
