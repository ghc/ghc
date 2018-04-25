{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compat.GetShortPathName
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  Windows-only
--
-- Win32 API 'GetShortPathName' function.

module Distribution.Compat.GetShortPathName ( getShortPathName )
    where

import Prelude ()
import Distribution.Compat.Prelude

#ifdef mingw32_HOST_OS

import qualified Prelude
import qualified System.Win32 as Win32
import System.Win32          (LPCTSTR, LPTSTR, DWORD)
import Foreign.Marshal.Array (allocaArray)

#ifdef x86_64_HOST_ARCH
#define WINAPI ccall
#else
#define WINAPI stdcall
#endif

foreign import WINAPI unsafe "windows.h GetShortPathNameW"
  c_GetShortPathName :: LPCTSTR -> LPTSTR -> DWORD -> Prelude.IO DWORD

-- | On Windows, retrieves the short path form of the specified path. On
-- non-Windows, does nothing. See https://github.com/haskell/cabal/issues/3185.
--
-- From MS's GetShortPathName docs:
--
--      Passing NULL for [the second] parameter and zero for cchBuffer
--      will always return the required buffer size for a
--      specified lpszLongPath.
--
getShortPathName :: FilePath -> NoCallStackIO FilePath
getShortPathName path =
  Win32.withTString path $ \c_path -> do
    c_len <- Win32.failIfZero "GetShortPathName #1 failed!" $
      c_GetShortPathName c_path Win32.nullPtr 0
    let arr_len = fromIntegral c_len
    allocaArray arr_len $ \c_out -> do
      void $ Win32.failIfZero "GetShortPathName #2 failed!" $
        c_GetShortPathName c_path c_out c_len
      Win32.peekTString c_out

#else

getShortPathName :: FilePath -> NoCallStackIO FilePath
getShortPathName path = return path

#endif
