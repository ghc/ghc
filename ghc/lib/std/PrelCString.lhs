% -----------------------------------------------------------------------------
% $Id: PrelCString.lhs,v 1.5 2001/08/10 13:48:06 simonmar Exp $
%
% (c) The FFI task force, 2000
%

Utilities for primitive marshaling

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelCString where

#ifdef __GLASGOW_HASKELL__
import PrelMarshalArray
import PrelPtr
import PrelStorable
import PrelCTypes
import PrelWord
import PrelByteArr
import PrelPack
import PrelList
import PrelReal
import PrelNum
import PrelIOBase
import PrelBase
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
peekCString cp  = do cs <- peekArray0 nUL cp; return (cCharsToChars cs)

-- marshal a C string with explicit length into a Haskell string 
--
peekCStringLen           :: CStringLen -> IO String
peekCStringLen (cp, len)  = do cs <- peekArray len cp; return (cCharsToChars cs)

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
newCStringLen str  = do a <- newArray (charsToCChars str)
			return (pairLength str a)

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
castCCharToChar ch = unsafeChr (fromIntegral (fromIntegral ch :: Word8))

castCharToCChar :: Char -> CChar
castCharToCChar ch = fromIntegral (ord ch)
\end{code}
