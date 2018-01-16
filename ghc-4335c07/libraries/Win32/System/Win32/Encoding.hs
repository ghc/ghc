{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Encoding
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Enocode/Decode mutibyte charactor using Win32 API.
-}

module System.Win32.Encoding
  ( getCurrentCodePage
  , encodeMultiByte
  , encodeMultiByteIO
  , decodeMultiByte
  , decodeMultiByteIO
  , wideCharToMultiByte
  , multiByteToWideChar
  ) where

import Foreign.C.Types        (CInt(..))
import Foreign.C.String       (peekCAStringLen, withCWStringLen)
import Foreign.Marshal.Array  (allocaArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import System.Win32.Console
import System.Win32.NLS
import System.Win32.Types

#include "windows_cconv.h"

-- note CodePage = UInt which might not work on Win64.  But the Win32 package
-- also has this issue.
getCurrentCodePage :: IO DWORD
getCurrentCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

-- | The "System.IO" output functions (e.g. `putStr`) don't
-- automatically convert to multibyte string on Windows, so this
-- function is provided to make the conversion from a Unicode string
-- in the given code page to a proper multibyte string.  To get the
-- code page for the console, use `getCurrentCodePage`.
--
encodeMultiByte :: CodePage -> String -> String
encodeMultiByte cp = unsafeLocalState . encodeMultiByteIO cp

encodeMultiByteIO :: CodePage -> String -> IO String
encodeMultiByteIO _ "" = return ""
  -- WideCharToMultiByte doesn't handle empty strings
encodeMultiByteIO cp wstr =
  withCWStringLen wstr $ \(cwstr,len) -> do
    mbchars' <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte 
                cp
                0
                cwstr
                (fromIntegral len)
                nullPtr 0
                nullPtr nullPtr
    -- mbchar' is the length of buffer required
    allocaArray (fromIntegral mbchars') $ \mbstr -> do
      mbchars <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte 
                 cp
                 0
                 cwstr
                 (fromIntegral len)
                 mbstr mbchars'
                 nullPtr nullPtr
      peekCAStringLen (mbstr,fromIntegral mbchars)  -- converts [Char] to UTF-16

foreign import WINDOWS_CCONV "WideCharToMultiByte"
  wideCharToMultiByte
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCWSTR -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> LPSTR   -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPCSTR  -- lpMultiByteStr
        -> LPBOOL  -- lpbFlags
        -> IO CInt

-- | The "System.IO" input functions (e.g. `getLine`) don't
-- automatically convert to Unicode, so this function is provided to
-- make the conversion from a multibyte string in the given code page 
-- to a proper Unicode string.  To get the code page for the console,
-- use `getCurrentCodePage`.
decodeMultiByte :: CodePage -> String -> String
decodeMultiByte cp = unsafeLocalState . decodeMultiByteIO cp

-- | Because of `stringToUnicode` is unclear name, we use `decodeMultiByteIO`
-- for alias of `stringToUnicode`. 
decodeMultiByteIO :: CodePage -> String -> IO String
decodeMultiByteIO = stringToUnicode
{-# INLINE decodeMultiByteIO #-}
