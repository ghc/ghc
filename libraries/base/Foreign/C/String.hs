{-# OPTIONS -fno-implicit-prelude #-}
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
-- Utilities for primitive marshaling
--
-----------------------------------------------------------------------------

module Foreign.C.String (   -- representation of strings in C

  CString,           -- = Ptr CChar
  CStringLen,        -- = (CString, Int)

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

  -- conversion between Haskell and C characters *ignoring* the encoding
  --
  castCharToCChar,   -- :: Char -> CChar
  castCCharToChar,   -- :: CChar -> Char

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

type CString    = Ptr CChar		-- conventional NUL terminates strings
type CStringLen = (CString, Int)	-- strings with explicit length


-- exported functions
-- ------------------
--
-- * the following routines apply the default conversion when converting the
--   C-land character encoding into the Haskell-land character encoding
--
--   ** NOTE: The current implementation doesn't handle conversions yet! **
--
-- * the routines using an explicit length tolerate NUL characters in the
--   middle of a string
--

-- marshal a NUL terminated C string into a Haskell string 
--
peekCString    :: CString -> IO String
#ifndef __GLASGOW_HASKELL__
peekCString cp  = do cs <- peekArray0 nUL cp; return (cCharsToChars cs)
#else
peekCString cp = do
  l <- lengthArray0 nUL cp
  if l <= 0 then return "" else loop "" (l-1)
  where
    loop s i = do
        xval <- peekElemOff cp i
	let val = castCCharToChar xval
	val `seq` if i <= 0 then return (val:s) else loop (val:s) (i-1)
#endif

-- marshal a C string with explicit length into a Haskell string 
--
peekCStringLen           :: CStringLen -> IO String
#ifndef __GLASGOW_HASKELL__
peekCStringLen (cp, len)  = do cs <- peekArray len cp; return (cCharsToChars cs)
#else
peekCStringLen (cp, len) 
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

-- marshal a Haskell string into a NUL terminated C strings
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCString :: String -> IO CString
#ifndef __GLASGOW_HASKELL__
newCString  = newArray0 nUL . charsToCChars
#else
newCString str = do
  ptr <- mallocArray0 (length str)
  let
	go [] n#     = pokeElemOff ptr (I# n#) nUL
    	go (c:cs) n# = do pokeElemOff ptr (I# n#) (castCharToCChar c); go cs (n# +# 1#)
  go str 0#
  return ptr
#endif

-- marshal a Haskell string into a C string (ie, character array) with
-- explicit length information
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCStringLen     :: String -> IO CStringLen
#ifndef __GLASGOW_HASKELL__
newCStringLen str  = do a <- newArray (charsToCChars str)
			return (pairLength str a)
#else
newCStringLen str = do
  ptr <- mallocArray0 len
  let
	go [] n#     = return ()
    	go (c:cs) n# = do pokeElemOff ptr (I# n#) (castCharToCChar c); go cs (n# +# 1#)
  go str 0#
  return (ptr, len)
  where
    len = length str
#endif

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCString :: String -> (CString -> IO a) -> IO a
#ifndef __GLASGOW_HASKELL__
withCString  = withArray0 nUL . charsToCChars
#else
withCString str f =
  allocaArray0 (length str) $ \ptr ->
      let
	go [] n#     = pokeElemOff ptr (I# n#) nUL
    	go (c:cs) n# = do pokeElemOff ptr (I# n#) (castCharToCChar c); go cs (n# +# 1#)
      in do
      go str 0#
      f ptr
#endif

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCStringLen         :: String -> (CStringLen -> IO a) -> IO a
#ifndef __GLASGOW_HASKELL__
withCStringLen str act  = withArray (charsToCChars str) $ act . pairLength str
#else
withCStringLen str f =
  allocaArray len $ \ptr ->
      let
	go [] n#     = return ()
    	go (c:cs) n# = do pokeElemOff ptr (I# n#) (castCharToCChar c); go cs (n# +# 1#)
      in do
      go str 0#
      f (ptr,len)
  where
    len = length str
#endif

-- auxilliary definitions
-- ----------------------

-- C's end of string character
--
nUL :: CChar
nUL  = 0

-- pair a C string with the length of the given Haskell string
--
pairLength :: String -> CString -> CStringLen
pairLength  = flip (,) . length

-- cast [CChar] to [Char]
--
cCharsToChars :: [CChar] -> [Char]
cCharsToChars xs  = map castCCharToChar xs

-- cast [Char] to [CChar]
--
charsToCChars :: [Char] -> [CChar]
charsToCChars xs  = map castCharToCChar xs

castCCharToChar :: CChar -> Char
castCCharToChar ch = unsafeChr (fromIntegral (fromIntegral ch :: Word8))

castCharToCChar :: Char -> CChar
castCharToCChar ch = fromIntegral (ord ch)
