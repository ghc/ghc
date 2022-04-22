{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
        BufferCodec(..), TextEncoding(..), TextEncoder, TextDecoder, CodingProgress(..),
        latin1, latin1_encode, latin1_decode,
        utf8, utf8_bom,
        utf16, utf16le, utf16be,
        utf32, utf32le, utf32be,
        initLocaleEncoding,
        getLocaleEncoding, getFileSystemEncoding, getForeignEncoding,
        setLocaleEncoding, setFileSystemEncoding, setForeignEncoding,
        char8,
        mkTextEncoding,
        argvEncoding
    ) where

import GHC.Base
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
#if !defined(mingw32_HOST_OS)
import qualified GHC.IO.Encoding.Iconv as Iconv
#else
import qualified GHC.IO.Encoding.CodePage as CodePage
import Text.Read (reads)
#endif
import qualified GHC.IO.Encoding.Latin1 as Latin1
import qualified GHC.IO.Encoding.UTF8   as UTF8
import qualified GHC.IO.Encoding.UTF16  as UTF16
import qualified GHC.IO.Encoding.UTF32  as UTF32
import GHC.List
import GHC.Word

import Data.IORef
import Data.Char (toUpper)
import System.IO.Unsafe (unsafePerformIO)

-- -----------------------------------------------------------------------------

-- | The Latin1 (ISO8859-1) encoding.  This encoding maps bytes
-- directly to the first 256 Unicode code points, and is thus not a
-- complete Unicode encoding.  An attempt to write a character greater than
-- @\'\\255\'@ to a 'System.IO.Handle' using the 'latin1' encoding will result in an
-- error.
latin1  :: TextEncoding
latin1 = Latin1.latin1_checked

-- | The UTF-8 Unicode encoding
utf8  :: TextEncoding
utf8 = UTF8.utf8

-- | The UTF-8 Unicode encoding, with a byte-order-mark (BOM; the byte
-- sequence 0xEF 0xBB 0xBF).  This encoding behaves like 'utf8',
-- except that on input, the BOM sequence is ignored at the beginning
-- of the stream, and on output, the BOM sequence is prepended.
--
-- The byte-order-mark is strictly unnecessary in UTF-8, but is
-- sometimes used to identify the encoding of a file.
--
utf8_bom  :: TextEncoding
utf8_bom = UTF8.utf8_bom

-- | The UTF-16 Unicode encoding (a byte-order-mark should be used to
-- indicate endianness).
utf16  :: TextEncoding
utf16 = UTF16.utf16

-- | The UTF-16 Unicode encoding (little-endian)
utf16le  :: TextEncoding
utf16le = UTF16.utf16le

-- | The UTF-16 Unicode encoding (big-endian)
utf16be  :: TextEncoding
utf16be = UTF16.utf16be

-- | The UTF-32 Unicode encoding (a byte-order-mark should be used to
-- indicate endianness).
utf32  :: TextEncoding
utf32 = UTF32.utf32

-- | The UTF-32 Unicode encoding (little-endian)
utf32le  :: TextEncoding
utf32le = UTF32.utf32le

-- | The UTF-32 Unicode encoding (big-endian)
utf32be  :: TextEncoding
utf32be = UTF32.utf32be

-- | The Unicode encoding of the current locale
--
-- @since 4.5.0.0
getLocaleEncoding :: IO TextEncoding
{-# NOINLINE getLocaleEncoding #-}

-- | The encoding of the current locale, but allowing arbitrary
-- undecodable bytes to be round-tripped through it.
--
-- Do not expect the encoding to be Unicode-compatible: it could appear to be ASCII or anything else.
--
-- This 'TextEncoding' is used to decode and encode command line arguments
-- and environment variables on non-Windows platforms.
--
-- On Windows, this encoding *should not* be used if possible because
-- the use of code pages is deprecated: Strings should be retrieved
-- via the "wide" W-family of UTF-16 APIs instead
--
-- @since 4.5.0.0
getFileSystemEncoding :: IO TextEncoding
{-# NOINLINE getFileSystemEncoding #-}

-- | The Unicode encoding of the current locale, but where undecodable
-- bytes are replaced with their closest visual match. Used for
-- the 'Foreign.C.String.CString' marshalling functions in "Foreign.C.String"
--
-- @since 4.5.0.0
getForeignEncoding :: IO TextEncoding
{-# NOINLINE getForeignEncoding #-}

-- | Set locale encoding for your program. The locale affects
-- how 'Char's are encoded and decoded when serialized to bytes: e. g.,
-- when you read or write files ('System.IO.readFile'', 'System.IO.writeFile')
-- or use standard input/output ('System.IO.getLine', 'System.IO.putStrLn').
-- For instance, if your program prints non-ASCII characters, it is prudent to execute
--
-- > setLocaleEncoding utf8
--
-- This is necessary, but not enough on Windows, where console is
-- a stateful device, which needs to be configured using
-- @System.Win32.Console.setConsoleOutputCP@ and restored back afterwards.
-- These intricacies are covered by
-- <https://hackage.haskell.org/package/code-page code-page> package,
-- which offers a crossplatform @System.IO.CodePage.withCodePage@ bracket.
--
-- Wrong locale encoding typically causes error messages like
-- "invalid argument (cannot decode byte sequence starting from ...)"
-- or "invalid argument (cannot encode character ...)".
--
-- @since 4.5.0.0
setLocaleEncoding :: TextEncoding -> IO ()
{-# NOINLINE setLocaleEncoding #-}

-- | @since 4.5.0.0
setFileSystemEncoding :: TextEncoding -> IO ()
{-# NOINLINE setFileSystemEncoding #-}

-- | @since 4.5.0.0
setForeignEncoding :: TextEncoding -> IO ()
{-# NOINLINE setForeignEncoding #-}

(getLocaleEncoding, setLocaleEncoding)         = mkGlobal initLocaleEncoding
(getFileSystemEncoding, setFileSystemEncoding) = mkGlobal initFileSystemEncoding
(getForeignEncoding, setForeignEncoding)       = mkGlobal initForeignEncoding

mkGlobal :: a -> (IO a, a -> IO ())
mkGlobal x = unsafePerformIO $ do
    x_ref <- newIORef x
    return (readIORef x_ref, writeIORef x_ref)
{-# NOINLINE mkGlobal #-}

-- | @since 4.5.0.0
initLocaleEncoding, initFileSystemEncoding, initForeignEncoding :: TextEncoding
{-# NOINLINE initLocaleEncoding #-}
-- N.B. initLocaleEncoding is exported for use in System.IO.localeEncoding.
-- NOINLINE ensures that this result is shared.

#if !defined(mingw32_HOST_OS)
-- It is rather important that we don't just call Iconv.mkIconvEncoding here
-- because some iconvs (in particular GNU iconv) will brokenly UTF-8 encode
-- lone surrogates without complaint.
--
-- By going through our Haskell implementations of those encodings, we are
-- guaranteed to catch such errors.
--
-- FIXME: this is not a complete solution because if the locale encoding is one
-- which we don't have a Haskell-side decoder for, iconv might still ignore the
-- lone surrogate in the input.
initLocaleEncoding     = unsafePerformIO $ mkTextEncoding' ErrorOnCodingFailure Iconv.localeEncodingName
initFileSystemEncoding = unsafePerformIO $ mkTextEncoding' RoundtripFailure     Iconv.localeEncodingName
initForeignEncoding    = unsafePerformIO $ mkTextEncoding' IgnoreCodingFailure  Iconv.localeEncodingName
#else
initLocaleEncoding     = CodePage.localeEncoding
initFileSystemEncoding = CodePage.mkLocaleEncoding RoundtripFailure
initForeignEncoding    = CodePage.mkLocaleEncoding IgnoreCodingFailure
#endif

-- See Note [Windows Unicode Arguments] in rts/RtsFlags.c
-- On Windows we assume hs_init argv is in utf8 encoding.

-- | Internal encoding of argv
argvEncoding :: IO TextEncoding
#if defined(mingw32_HOST_OS)
argvEncoding = return utf8
#else
argvEncoding = getFileSystemEncoding
#endif

-- | An encoding in which Unicode code points are translated to bytes
-- by taking the code point modulo 256.  When decoding, bytes are
-- translated directly into the equivalent code point.
--
-- This encoding never fails in either direction.  However, encoding
-- discards information, so encode followed by decode is not the
-- identity.
--
-- @since 4.4.0.0
char8 :: TextEncoding
char8 = Latin1.latin1

-- | Look up the named Unicode encoding.  May fail with
--
--  * 'System.IO.Error.isDoesNotExistError' if the encoding is unknown
--
-- The set of known encodings is system-dependent, but includes at least:
--
--  * @UTF-8@
--
--  * @UTF-16@, @UTF-16BE@, @UTF-16LE@
--
--  * @UTF-32@, @UTF-32BE@, @UTF-32LE@
--
-- There is additional notation (borrowed from GNU iconv) for specifying
-- how illegal characters are handled:
--
--  * a suffix of @\/\/IGNORE@, e.g. @UTF-8\/\/IGNORE@, will cause
--    all illegal sequences on input to be ignored, and on output
--    will drop all code points that have no representation in the
--    target encoding.
--
--  * a suffix of @\/\/TRANSLIT@ will choose a replacement character
--    for illegal sequences or code points.
--
--  * a suffix of @\/\/ROUNDTRIP@ will use a PEP383-style escape mechanism
--    to represent any invalid bytes in the input as Unicode codepoints (specifically,
--    as lone surrogates, which are normally invalid in UTF-32).
--    Upon output, these special codepoints are detected and turned back into the
--    corresponding original byte.
--
--    In theory, this mechanism allows arbitrary data to be roundtripped via
--    a 'String' with no loss of data. In practice, there are two limitations
--    to be aware of:
--
--      1. This only stands a chance of working for an encoding which is an ASCII
--         superset, as for security reasons we refuse to escape any bytes smaller
--         than 128. Many encodings of interest are ASCII supersets (in particular,
--         you can assume that the locale encoding is an ASCII superset) but many
--         (such as UTF-16) are not.
--
--      2. If the underlying encoding is not itself roundtrippable, this mechanism
--         can fail. Roundtrippable encodings are those which have an injective mapping
--         into Unicode. Almost all encodings meet this criteria, but some do not. Notably,
--         Shift-JIS (CP932) and Big5 contain several different encodings of the same
--         Unicode codepoint.
--
-- On Windows, you can access supported code pages with the prefix
-- @CP@; for example, @\"CP1250\"@.
--
mkTextEncoding :: String -> IO TextEncoding
mkTextEncoding e = case mb_coding_failure_mode of
    Nothing -> unknownEncodingErr e
    Just cfm -> mkTextEncoding' cfm enc
  where
    (enc, suffix) = span (/= '/') e
    mb_coding_failure_mode = case suffix of
        ""            -> Just ErrorOnCodingFailure
        "//IGNORE"    -> Just IgnoreCodingFailure
        "//TRANSLIT"  -> Just TransliterateCodingFailure
        "//ROUNDTRIP" -> Just RoundtripFailure
        _             -> Nothing

mkTextEncoding' :: CodingFailureMode -> String -> IO TextEncoding
mkTextEncoding' cfm enc =
  case [toUpper c | c <- enc, c /= '-'] of
  -- UTF-8 and friends we can handle ourselves
    "UTF8"    -> return $ UTF8.mkUTF8 cfm
    "UTF16"   -> return $ UTF16.mkUTF16 cfm
    "UTF16LE" -> return $ UTF16.mkUTF16le cfm
    "UTF16BE" -> return $ UTF16.mkUTF16be cfm
    "UTF32"   -> return $ UTF32.mkUTF32 cfm
    "UTF32LE" -> return $ UTF32.mkUTF32le cfm
    "UTF32BE" -> return $ UTF32.mkUTF32be cfm
    -- On AIX, we want to avoid iconv, because it is either
    -- a) totally broken, or b) non-reentrant, or c) actually works.
    -- Detecting b) is difficult as you'd have to trigger the reentrancy
    -- corruption.
    -- Therefore, on AIX, we handle the popular ASCII and latin1 encodings
    -- ourselves. For consistency, we do the same on other platforms.
    -- We use `mkLatin1_checked` instead of `mkLatin1`, since the latter
    -- completely ignores the CodingFailureMode (TEST=encoding005).
    _ | isAscii -> return (Latin1.mkAscii cfm)
    _ | isLatin1 -> return (Latin1.mkLatin1_checked cfm)
#if defined(mingw32_HOST_OS)
    'C':'P':n | [(cp,"")] <- reads n -> return $ CodePage.mkCodePageEncoding cfm cp
    _ -> unknownEncodingErr (enc ++ codingFailureModeSuffix cfm)
#else
    -- Otherwise, handle other encoding needs via iconv.

    -- Unfortunately there is no good way to determine whether iconv is actually
    -- functional without telling it to do something.
    _ -> do res <- Iconv.mkIconvEncoding cfm enc
            case res of
              Just e -> return e
              Nothing -> unknownEncodingErr (enc ++ codingFailureModeSuffix cfm)
#endif
  where
    isAscii = enc `elem` asciiEncNames
    isLatin1 = enc `elem` latin1EncNames
    asciiEncNames = -- ASCII aliases specified by RFC 1345 and RFC 3808.
      [ "ANSI_X3.4-1968", "iso-ir-6", "ANSI_X3.4-1986", "ISO_646.irv:1991"
      , "US-ASCII", "us", "IBM367", "cp367", "csASCII", "ASCII", "ISO646-US"
      ]
    latin1EncNames = -- latin1 aliases specified by RFC 1345 and RFC 3808.
      [ "ISO_8859-1:1987", "iso-ir-100", "ISO_8859-1", "ISO-8859-1", "latin1",
        "l1", "IBM819", "CP819", "csISOLatin1"
      ]


latin1_encode :: CharBuffer -> Buffer Word8 -> IO (CharBuffer, Buffer Word8)
latin1_encode input output = fmap (\(_why,input',output') -> (input',output')) $ Latin1.latin1_encode input output -- unchecked, used for char8
--latin1_encode = unsafePerformIO $ do mkTextEncoder Iconv.latin1 >>= return.encode

latin1_decode :: Buffer Word8 -> CharBuffer -> IO (Buffer Word8, CharBuffer)
latin1_decode input output = fmap (\(_why,input',output') -> (input',output')) $ Latin1.latin1_decode input output
--latin1_decode = unsafePerformIO $ do mkTextDecoder Iconv.latin1 >>= return.encode

unknownEncodingErr :: String -> IO a
unknownEncodingErr e = ioException (IOError Nothing NoSuchThing "mkTextEncoding"
                                            ("unknown encoding:" ++ e)  Nothing Nothing)
