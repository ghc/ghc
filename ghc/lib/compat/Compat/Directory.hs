{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Compat.Directory
-- Copyright   :  (c) The University of Glasgow 2001-2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions from System.Directory that aren't present in older versions
-- of that library.
--
-----------------------------------------------------------------------------

module Compat.Directory (
  	getAppUserDataDirectory,
  ) where

#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#endif

#if !defined(mingw32_TARGET_OS)
import System.Environment (getEnv)
#endif

getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
#if __GLASGOW_HASKELL__ && defined(mingw32_TARGET_OS)
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_APPDATA nullPtr 0 pPath
     s <- peekCString pPath
     return (s++'\\':appName)
#else
  path <- getEnv "HOME"
  return (path++'/':'.':appName)
#endif

#if __GLASGOW_HASKELL__ && defined(mingw32_TARGET_OS)
foreign import stdcall unsafe "SHGetFolderPath" 
            c_SHGetFolderPath :: Ptr () 
                              -> CInt 
                              -> Ptr () 
                              -> CInt 
                              -> CString 
                              -> IO CInt
#endif
