{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.String
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for primitive marshalling of C strings.
--
-- The marshalling converts each Haskell character, representing a Unicode
-- code point, to one or more bytes in a manner that, by default, is
-- determined by the current locale.  As a consequence, no guarantees
-- can be made about the relative length of a Haskell string and its
-- corresponding C string, and therefore all the marshalling routines
-- include memory allocation.  The translation between Unicode and the
-- encoding of the current locale may be lossy.
--
-----------------------------------------------------------------------------

module Foreign.C.String (   -- representation of strings in C

  -- * C strings

  CString,           -- = Ptr CChar
  CStringLen,        -- = (Ptr CChar, Int)

  -- ** Using a locale-dependent encoding

  -- | Currently these functions are identical to their @CAString@ counterparts;
  -- eventually they will use an encoding determined by the current locale.

  -- conversion of C strings into Haskell strings
  --
  peekCString,       -- :: CString    -> IO String
  peekCStringLen,    -- :: CStringLen -> IO String

  -- conversion of Haskell strings into C strings
  --
  newCString,        -- :: String -> IO CString
  newCStringLen,     -- :: String -> IO CStringLen

  -- conversion of Haskell strings into C strings using temporary storage
  --
  withCString,       -- :: String -> (CString    -> IO a) -> IO a
  withCStringLen,    -- :: String -> (CStringLen -> IO a) -> IO a

  charIsRepresentable, -- :: Char -> IO Bool

  -- ** Using 8-bit characters

  -- | These variants of the above functions are for use with C libraries
  -- that are ignorant of Unicode.  These functions should be used with
  -- care, as a loss of information can occur.

  castCharToCChar,   -- :: Char -> CChar
  castCCharToChar,   -- :: CChar -> Char

  peekCAString,      -- :: CString    -> IO String
  peekCAStringLen,   -- :: CStringLen -> IO String
  newCAString,       -- :: String -> IO CString
  newCAStringLen,    -- :: String -> IO CStringLen
  withCAString,      -- :: String -> (CString    -> IO a) -> IO a
  withCAStringLen,   -- :: String -> (CStringLen -> IO a) -> IO a

  -- * C wide strings

  -- | These variants of the above functions are for use with C libraries
  -- that encode Unicode using the C @wchar_t@ type in a system-dependent
  -- way.  The only encodings supported are
  --
  -- * UTF-32 (the C compiler defines @__STDC_ISO_10646__@), or
  --
  -- * UTF-16 (as used on Windows systems).

  CWString,          -- = Ptr CWchar
  CWStringLen,       -- = (Ptr CWchar, Int)

  peekCWString,      -- :: CWString    -> IO String
  peekCWStringLen,   -- :: CWStringLen -> IO String
  newCWString,       -- :: String -> IO CWString
  newCWStringLen,    -- :: String -> IO CWStringLen
  withCWString,      -- :: String -> (CWString    -> IO a) -> IO a
  withCWStringLen,   -- :: String -> (CWStringLen -> IO a) -> IO a

  ) where

import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Data.Word

#ifdef __GLASGOW_HASKELL__
import GHC.List
import GHC.Real
import GHC.Num
import GHC.IOBase
import GHC.Base
#else
import Data.Char ( chr, ord )
#define unsafeChr chr
#endif

-----------------------------------------------------------------------------
-- Strings

-- representation of strings in C
-- ------------------------------

-- | A C string is a reference to an array of C characters terminated by NUL.
type CString    = Ptr CChar

-- | A string with explicit length information in bytes instead of a
-- terminating NUL (allowing NUL characters in the middle of the string).
type CStringLen = (Ptr CChar, Int)

-- exported functions
-- ------------------
--
-- * the following routines apply the default conversion when converting the
--   C-land character encoding into the Haskell-land character encoding

-- | Marshal a NUL terminated C string into a Haskell string.
--
peekCString    :: CString -> IO String
peekCString = peekCAString

-- | Marshal a C string with explicit length into a Haskell string.
--
peekCStringLen           :: CStringLen -> IO String
peekCStringLen = peekCAStringLen

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCString :: String -> IO CString
newCString = newCAString

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCStringLen     :: String -> IO CStringLen
newCStringLen = newCAStringLen

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCString :: String -> (CString -> IO a) -> IO a
withCString = withCAString

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCStringLen         :: String -> (CStringLen -> IO a) -> IO a
withCStringLen = withCAStringLen

-- | Determines whether a character can be accurately encoded in a 'CString'.
-- Unrepresentable characters are converted to @\'?\'@.
--
-- Currently only Latin-1 characters are representable.
charIsRepresentable :: Char -> IO Bool
charIsRepresentable c = return (ord c < 256)

-- single byte characters
-- ----------------------
--
--   ** NOTE: These routines don't handle conversions! **

-- | Convert a C byte, representing a Latin-1 character, to the corresponding
-- Haskell character.
castCCharToChar :: CChar -> Char
castCCharToChar ch = unsafeChr (fromIntegral (fromIntegral ch :: Word8))

-- | Convert a Haskell character to a C character.
-- This function is only safe on the first 256 characters.
castCharToCChar :: Char -> CChar
castCharToCChar ch = fromIntegral (ord ch)

-- | Marshal a NUL terminated C string into a Haskell string.
--
peekCAString    :: CString -> IO String
#ifndef __GLASGOW_HASKELL__
peekCAString cp  = do
  cs <- peekArray0 nUL cp
  return (cCharsToChars cs)
#else
peekCAString cp = do
  l <- lengthArray0 nUL cp
  if l <= 0 then return "" else loop "" (l-1)
  where
    loop s i = do
        xval <- peekElemOff cp i
	let val = castCCharToChar xval
	val `seq` if i <= 0 then return (val:s) else loop (val:s) (i-1)
#endif

-- | Marshal a C string with explicit length into a Haskell string.
--
peekCAStringLen           :: CStringLen -> IO String
#ifndef __GLASGOW_HASKELL__
peekCAStringLen (cp, len)  = do
  cs <- peekArray len cp
  return (cCharsToChars cs)
#else
peekCAStringLen (cp, len) 
  | len <= 0  = return "" -- being (too?) nice.
  | otherwise = loop [] (len-1)
  where
    loop acc i = do
         xval <- peekElemOff cp i
	 let val = castCCharToChar xval
	   -- blow away the coercion ASAP.
	 if (val `seq` (i == 0))
	  then return (val:acc)
	  else loop (val:acc) (i-1)
#endif

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCAString :: String -> IO CString
#ifndef __GLASGOW_HASKELL__
newCAString  = newArray0 nUL . charsToCChars
#else
newCAString str = do
  ptr <- mallocArray0 (length str)
  let
	go [] n     = pokeElemOff ptr n nUL
    	go (c:cs) n = do pokeElemOff ptr n (castCharToCChar c); go cs (n+1)
  go str 0
  return ptr
#endif

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCAStringLen     :: String -> IO CStringLen
#ifndef __GLASGOW_HASKELL__
newCAStringLen str  = do
  a <- newArray (charsToCChars str)
  return (pairLength str a)
#else
newCAStringLen str = do
  ptr <- mallocArray0 len
  let
	go [] n     = n `seq` return ()	-- make it strict in n
    	go (c:cs) n = do pokeElemOff ptr n (castCharToCChar c); go cs (n+1)
  go str 0
  return (ptr, len)
  where
    len = length str
#endif

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCAString :: String -> (CString -> IO a) -> IO a
#ifndef __GLASGOW_HASKELL__
withCAString  = withArray0 nUL . charsToCChars
#else
withCAString str f =
  allocaArray0 (length str) $ \ptr ->
      let
	go [] n     = pokeElemOff ptr n nUL
    	go (c:cs) n = do pokeElemOff ptr n (castCharToCChar c); go cs (n+1)
      in do
      go str 0
      f ptr
#endif

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCAStringLen         :: String -> (CStringLen -> IO a) -> IO a
#ifndef __GLASGOW_HASKELL__
withCAStringLen str act  = withArray (charsToCChars str) $ act . pairLength str
#else
withCAStringLen str f =
  allocaArray len $ \ptr ->
      let
	go [] n     = n `seq` return ()	-- make it strict in n
    	go (c:cs) n = do pokeElemOff ptr n (castCharToCChar c); go cs (n+1)
      in do
      go str 0
      f (ptr,len)
  where
    len = length str
#endif

-- auxiliary definitions
-- ----------------------

-- C's end of string character
--
nUL :: CChar
nUL  = 0

-- pair a C string with the length of the given Haskell string
--
pairLength :: String -> a -> (a, Int)
pairLength  = flip (,) . length

#ifndef __GLASGOW_HASKELL__
-- cast [CChar] to [Char]
--
cCharsToChars :: [CChar] -> [Char]
cCharsToChars xs  = map castCCharToChar xs

-- cast [Char] to [CChar]
--
charsToCChars :: [Char] -> [CChar]
charsToCChars xs  = map castCharToCChar xs
#endif

-----------------------------------------------------------------------------
-- Wide strings

-- representation of wide strings in C
-- -----------------------------------

-- | A C wide string is a reference to an array of C wide characters
-- terminated by NUL.
type CWString    = Ptr CWchar

-- | A wide character string with explicit length information in bytes
-- instead of a terminating NUL (allowing NUL characters in the middle
-- of the string).
type CWStringLen = (Ptr CWchar, Int)

-- | Marshal a NUL terminated C wide string into a Haskell string.
--
peekCWString    :: CWString -> IO String
peekCWString cp  = do
  cs <- peekArray0 wNUL cp
  return (cWcharsToChars cs)

-- | Marshal a C wide string with explicit length into a Haskell string.
--
peekCWStringLen           :: CWStringLen -> IO String
peekCWStringLen (cp, len)  = do
  cs <- peekArray len cp
  return (cWcharsToChars cs)

-- | Marshal a Haskell string into a NUL terminated C wide string.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * new storage is allocated for the C wide string and must
--   be explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCWString :: String -> IO CWString
newCWString  = newArray0 wNUL . charsToCWchars

-- | Marshal a Haskell string into a C wide string (ie, wide character array)
-- with explicit length information.
--
-- * new storage is allocated for the C wide string and must
--   be explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCWStringLen     :: String -> IO CWStringLen
newCWStringLen str  = do
  a <- newArray (charsToCWchars str)
  return (pairLength str a)

-- | Marshal a Haskell string into a NUL terminated C wide string using
-- temporary storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCWString :: String -> (CWString -> IO a) -> IO a
withCWString  = withArray0 wNUL . charsToCWchars

-- | Marshal a Haskell string into a NUL terminated C wide string using
-- temporary storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCWStringLen         :: String -> (CWStringLen -> IO a) -> IO a
withCWStringLen str act  = withArray (charsToCWchars str) $ act . pairLength str

-- auxiliary definitions
-- ----------------------

wNUL :: CWchar
wNUL = 0

cWcharsToChars :: [CWchar] -> [Char]
charsToCWchars :: [Char] -> [CWchar]

#ifdef mingw32_HOST_OS

-- On Windows, wchar_t is 16 bits wide and CWString uses the UTF-16 encoding.

-- coding errors generate Chars in the surrogate range
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars = foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

#else /* !mingw32_HOST_OS */

cWcharsToChars xs  = map castCWcharToChar xs
charsToCWchars xs  = map castCharToCWchar xs

-- These conversions only make sense if __STDC_ISO_10646__ is defined
-- (meaning that wchar_t is ISO 10646, aka Unicode)

castCWcharToChar :: CWchar -> Char
castCWcharToChar ch = chr (fromIntegral ch )

castCharToCWchar :: Char -> CWchar
castCharToCWchar ch = fromIntegral (ord ch)

#endif /* !mingw32_HOST_OS */
