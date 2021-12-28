{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE CPP                  #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Windows.Paths
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Windows FilePath handling utility for GHC code.
--
-----------------------------------------------------------------------------

module GHC.IO.Windows.Paths
 (getDevicePath
 ) where

#include "windows_cconv.h"

import GHC.Base
import GHC.IO

import Foreign.C.String
import Foreign.Marshal.Alloc (free)

foreign import ccall safe "__hs_create_device_name"
    c_GetDevicePath :: CWString -> IO CWString

-- | This function converts Windows paths between namespaces. More specifically
-- It converts an explorer style path into a NT or Win32 namespace.
-- This has several caveats but they are caviats that are native to Windows and
-- not POSIX. See
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx.
-- Anything else such as raw device paths we leave untouched.  The main benefit
-- of doing any of this is that we can break the MAX_PATH restriction and also
-- access raw handles that we couldn't before.
getDevicePath :: FilePath -> IO FilePath
getDevicePath path
  = do str <- withCWString path c_GetDevicePath
       newPath <- peekCWString str
       free str
       return newPath
