% -----------------------------------------------------------------------------
% $Id: PrelCString.lhs,v 1.2 2001/01/11 19:48:28 qrczak Exp $
%
% (c) The FFI task force, 2000
%

Utilities for primitive marshaling

\begin{code}
module PrelCString where

import Monad

import PrelMarshalArray
import PrelMarshalAlloc
import PrelException
import PrelPtr
import PrelStorable
import PrelCTypes
import PrelCTypesISO
import PrelInt
import PrelByteArr
import PrelPack
import PrelBase

#ifdef __GLASGOW_HASKELL__
import PrelIOBase hiding (malloc, _malloc)
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
peekCString cp  = liftM cCharsToChars $ peekArray0 nUL cp

-- marshal a C string with explicit length into a Haskell string 
--
peekCStringLen           :: CStringLen -> IO String
peekCStringLen (cp, len)  = liftM cCharsToChars $ peekArray len cp

-- marshal a Haskell string into a NUL terminated C strings
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCString :: String -> IO CString
newCString  = newArray0 nUL . charsToCChars

-- marshal a Haskell string into a C string (ie, character array) with
-- explicit length information
--
-- * new storage is allocated for the C string and must be explicitly freed
--
newCStringLen     :: String -> IO CStringLen
newCStringLen str  = liftM (pairLength str) $ newArray (charsToCChars str)

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCString :: String -> (CString -> IO a) -> IO a
withCString  = withArray0 nUL . charsToCChars

-- marshal a Haskell string into a NUL terminated C strings using temporary
-- storage
--
-- * the Haskell string may *not* contain any NUL characters
--
-- * see the lifetime constraints of `MarshalAlloc.alloca'
--
withCStringLen         :: String -> (CStringLen -> IO a) -> IO a
withCStringLen str act  = withArray (charsToCChars str) $ act . pairLength str

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
cCharsToChars  = map castCCharToChar

-- cast [Char] to [CChar]
--
charsToCChars :: [Char] -> [CChar]
charsToCChars  = map castCharToCChar

castCCharToChar :: CChar -> Char
-- castCCharToChar ch = chr (fromIntegral (fromIntegral ch :: Word8))
-- The above produces horrible code. Word and Int modules really
-- should be cleaned up... Here is an ugly but fast version:
castCCharToChar ch = case fromIntegral (fromIntegral ch :: Int32) of
    I# i# -> C# (chr# (word2Int# (int2Word# i# `and#` int2Word# 0xFF#)))

castCharToCChar :: Char -> CChar
castCharToCChar ch = fromIntegral (ord ch)


-- unsafe CStrings
-- ---------------

#if __GLASGOW_HASKELL__
newtype UnsafeCString = UnsafeCString (ByteArray Int)
withUnsafeCString s f = f (UnsafeCString (packString s))
#else
newtype UnsafeCString = UnsafeCString (Ptr CChar)
withUnsafeCString s f = withCString s (\p -> f (UnsafeCString p))
#endif
\end{code}
