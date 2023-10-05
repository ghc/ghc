{-# LANGUAGE CPP                  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{- |
   Module      :  System.Win32.Encoding
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Enocode/Decode mutibyte character using Win32 API.
-}

module GHC.IO.Windows.Encoding
  ( encodeMultiByte
  , encodeMultiByteIO
  , encodeMultiByteRawIO
  , decodeMultiByte
  , decodeMultiByteIO
  , wideCharToMultiByte
  , multiByteToWideChar
  , withGhcInternalToUTF16
  , withUTF16ToGhcInternal
  ) where

import Data.Word (Word8, Word16)
import Foreign.C.Types        (CInt(..))
import Foreign.C.String       (peekCAStringLen, peekCWStringLen,
                               withCWStringLen, withCAStringLen, )
import Foreign.Ptr (nullPtr, Ptr ())
import Foreign.Marshal.Array  (allocaArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import GHC.Windows
import GHC.IO.Encoding.CodePage (CodePage, getCurrentCodePage)
import GHC.IO
import GHC.Base
import GHC.Real

#include "windows_cconv.h"

-- | The "System.IO" output functions (e.g. `putStr`) don't
-- automatically convert to multibyte string on Windows, so this
-- function is provided to make the conversion from a Unicode string
-- in the given code page to a proper multibyte string.  To get the
-- code page for the console, use `getCurrentCodePage`.
--
encodeMultiByte :: CodePage -> String -> String
encodeMultiByte cp = unsafeLocalState . encodeMultiByteIO cp

{-# INLINE encodeMultiByteIO' #-}
-- | String must not be zero length.
encodeMultiByteIO' :: CodePage -> String -> ((LPCSTR, CInt) -> IO a) -> IO a
encodeMultiByteIO' cp wstr transformer =
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
      transformer (mbstr,fromIntegral mbchars)

-- converts [Char] to UTF-16
encodeMultiByteIO :: CodePage -> String -> IO String
encodeMultiByteIO _ "" = return ""
encodeMultiByteIO cp s = encodeMultiByteIO' cp s toString
  where toString (st,l) = peekCAStringLen (st,fromIntegral l)

-- converts [Char] to UTF-16
encodeMultiByteRawIO :: CodePage -> String -> IO (LPCSTR, CInt)
encodeMultiByteRawIO _ "" = return (nullPtr, 0)
encodeMultiByteRawIO cp s = encodeMultiByteIO' cp s toSizedCString
  where toSizedCString (st,l) = return (st, fromIntegral l)

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

-- | The `System.IO` input functions (e.g. `getLine`) don't
-- automatically convert to Unicode, so this function is provided to
-- make the conversion from a multibyte string in the given code page
-- to a proper Unicode string.  To get the code page for the console,
-- use `getConsoleCP`.
stringToUnicode :: CodePage -> String -> IO String
stringToUnicode _cp "" = return ""
     -- MultiByteToWideChar doesn't handle empty strings (#1929)
stringToUnicode cp mbstr =
  withCAStringLen mbstr $ \(cstr,len) -> do
    wchars <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                nullPtr 0
    -- wchars is the length of buffer required
    allocaArray (fromIntegral wchars) $ \cwstr -> do
      wchars' <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                cwstr wchars
      peekCWStringLen (cwstr,fromIntegral wchars')  -- converts UTF-16 to [Char]

foreign import WINDOWS_CCONV unsafe "MultiByteToWideChar"
  multiByteToWideChar
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCSTR  -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPWSTR  -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> IO CInt

decodeMultiByte :: CodePage -> String -> String
decodeMultiByte cp = unsafeLocalState . decodeMultiByteIO cp

-- | Because of `stringToUnicode` is unclear name, we use `decodeMultiByteIO`
-- for alias of `stringToUnicode`.
decodeMultiByteIO :: CodePage -> String -> IO String
decodeMultiByteIO = stringToUnicode
{-# INLINE decodeMultiByteIO #-}

foreign import WINDOWS_CCONV unsafe "MultiByteToWideChar"
  multiByteToWideChar'
        :: CodePage
        -> DWORD   -- dwFlags,
        -> Ptr Word8  -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> Ptr Word16  -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> IO CInt

-- TODO: GHC is internally UTF-32 which means we have re-encode for
--       Windows which is annoying. Switch to UTF-16 on IoNative
--       being default.
withGhcInternalToUTF16 :: Ptr Word8 -> Int -> ((Ptr Word16, CInt) -> IO a)
                       -> IO a
withGhcInternalToUTF16 ptr len fn
 = do cp <- getCurrentCodePage
      wchars <- failIfZero "withGhcInternalToUTF16" $
                  multiByteToWideChar' cp 0 ptr (fromIntegral len) nullPtr 0
      -- wchars is the length of buffer required
      allocaArray (fromIntegral wchars) $ \cwstr -> do
        wchars' <- failIfZero "withGhcInternalToUTF16" $
                    multiByteToWideChar' cp 0 ptr (fromIntegral len) cwstr wchars
        fn (cwstr, wchars')

foreign import WINDOWS_CCONV "WideCharToMultiByte"
  wideCharToMultiByte'
        :: CodePage
        -> DWORD   -- dwFlags,
        -> Ptr Word16 -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> Ptr Word8   -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPCSTR  -- lpMultiByteStr
        -> LPBOOL  -- lpbFlags
        -> IO CInt

-- TODO: GHC is internally UTF-32 which means we have re-encode for
--       Windows which is annoying. Switch to UTF-16 on IoNative
--       being default.

-- | Decode a UTF16 buffer into the given buffer in the current code page.
-- The source UTF16 buffer is filled by the function given as argument.
withUTF16ToGhcInternal :: Ptr Word8 -- Buffer to store the encoded string in.
                       -> Int       -- Length of the buffer
                       -- Function to fill source buffer.
                       ->  ( CInt       -- Size of available buffer in bytes
                          -> Ptr Word16 -- Temporary source buffer.
                          -> IO CInt    -- Actual length of buffer content.
                           )
                       -> IO Int    -- Returns number of bytes stored in buffer.
withUTF16ToGhcInternal ptr len fn
 = do cp <- getCurrentCodePage
      -- Annoyingly the IO system is very UTF-32 oriented and asks for bytes
      -- as buffer reads.  Problem is we don't know how many bytes we'll end up
      -- having as UTF-32 MultiByte encoded UTF-16. So be conservative.  We assume
      -- that a single byte may expand to atmost 1 Word16.  So assume that each
      -- byte does and divide the requested number of bytes by two since each
      -- Word16 encoded wchar may expand to only two Word8 sequences.
      let reqBytes = fromIntegral (len `div` 2)
      allocaArray reqBytes $ \w_ptr -> do
        w_len <- fn (fromIntegral reqBytes) w_ptr
        if w_len == 0
           then return 0 else do
                -- Get required length of encoding
                mbchars' <- failIfZero "withUTF16ToGhcInternal" $
                              wideCharToMultiByte' cp 0 w_ptr
                                                  (fromIntegral w_len) nullPtr
                                                  0 nullPtr nullPtr
                assert (mbchars' <= (fromIntegral len)) $ do
                  -- mbchar' is the length of buffer required
                  mbchars <- failIfZero "withUTF16ToGhcInternal" $
                                wideCharToMultiByte' cp 0 w_ptr
                                                    (fromIntegral w_len) ptr
                                                    mbchars' nullPtr nullPtr
                  return $ fromIntegral mbchars
