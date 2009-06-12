{-# OPTIONS_GHC -fno-implicit-prelude -funbox-strict-fields #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Encoding
-- Copyright   :  (c) The University of Glasgow, 2008-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Text codecs for I/O
--
-----------------------------------------------------------------------------

module GHC.IO.Encoding (
  BufferCodec(..), TextEncoding(..), TextEncoder, TextDecoder,
  latin1, latin1_encode, latin1_decode,
  utf8, 
  utf16, utf16le, utf16be,
  utf32, utf32le, utf32be, 
  localeEncoding,
  mkTextEncoding,
  ) where

import GHC.Base
import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import GHC.Word
#if !defined(mingw32_HOST_OS)
import qualified GHC.IO.Encoding.Iconv  as Iconv
#endif
import qualified GHC.IO.Encoding.Latin1 as Latin1
import qualified GHC.IO.Encoding.UTF8   as UTF8
import qualified GHC.IO.Encoding.UTF16  as UTF16
import qualified GHC.IO.Encoding.UTF32  as UTF32

#if defined(mingw32_HOST_OS)
import Data.Maybe
import GHC.IO.Exception
#endif

-- -----------------------------------------------------------------------------

latin1, utf8, utf16, utf16le, utf16be, utf32, utf32le, utf32be, localeEncoding
  :: TextEncoding

-- | The Latin1 (ISO8859-1) encoding.  This encoding maps bytes
-- directly to the first 256 Unicode code points, and is thus not a
-- complete Unicode encoding.
latin1 = Latin1.latin1_checked

-- | The UTF-8 unicode encoding
utf8 = UTF8.utf8

-- | The UTF-16 unicode encoding (a byte-order-mark should be used to
-- indicate endianness).
utf16 = UTF16.utf16

-- | The UTF-16 unicode encoding (litte-endian)
utf16le = UTF16.utf16le

-- | The UTF-16 unicode encoding (big-endian)
utf16be = UTF16.utf16be

-- | The UTF-32 unicode encoding (a byte-order-mark should be used to
-- indicate endianness).
utf32 = UTF32.utf32

-- | The UTF-32 unicode encoding (litte-endian)
utf32le = UTF32.utf32le

-- | The UTF-32 unicode encoding (big-endian)
utf32be = UTF32.utf32be

-- | The text encoding of the current locale
#if !defined(mingw32_HOST_OS)
localeEncoding = Iconv.localeEncoding
#else
localeEncoding = Latin1.latin1
#endif

-- | Acquire the named text encoding
mkTextEncoding :: String -> IO TextEncoding
#if !defined(mingw32_HOST_OS)
mkTextEncoding = Iconv.mkTextEncoding
#else
mkTextEncoding "UTF-8"    = return utf8
mkTextEncoding "UTF-16"   = return utf16
mkTextEncoding "UTF-16LE" = return utf16le
mkTextEncoding "UTF-16BE" = return utf16be
mkTextEncoding "UTF-32"   = return utf32
mkTextEncoding "UTF-32LE" = return utf32le
mkTextEncoding "UTF-32BE" = return utf32be
mkTextEncoding e = ioException
     (IOError Nothing InvalidArgument "mkTextEncoding"
          ("unknown encoding:" ++ e)  Nothing Nothing)
#endif

latin1_encode :: CharBuffer -> Buffer Word8 -> IO (CharBuffer, Buffer Word8)
latin1_encode = Latin1.latin1_encode -- unchecked, used for binary
--latin1_encode = unsafePerformIO $ do mkTextEncoder Iconv.latin1 >>= return.encode

latin1_decode :: Buffer Word8 -> CharBuffer -> IO (Buffer Word8, CharBuffer)
latin1_decode = Latin1.latin1_decode
--latin1_decode = unsafePerformIO $ do mkTextDecoder Iconv.latin1 >>= return.encode
