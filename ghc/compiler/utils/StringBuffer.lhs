%
% (c) The University of Glasgow, 1997-2003
%
\section{String buffers}

Buffers for scanning string input stored in external arrays.

\begin{code}
module StringBuffer
       (
        StringBuffer(..),
	-- non-abstract for vs\/HaskellService

	 -- * Creation\/destruction
        hGetStringBuffer,     -- :: FilePath     -> IO StringBuffer
	stringToStringBuffer, -- :: String       -> IO StringBuffer

         -- * Lookup
	currentChar,       -- :: StringBuffer -> Char
	prevChar,          -- :: StringBuffer -> Char -> Char
	lookAhead,         -- :: StringBuffer -> Int  -> Char
	atEnd,		   -- :: StringBuffer -> Bool

	-- * Moving
	stepOn, stepOnBy,

        -- * Conversion
        lexemeToString,     -- :: StringBuffer -> Int -> String
        lexemeToFastString, -- :: StringBuffer -> Int -> FastString

	 -- * Parsing integers
	parseInteger,
       ) where

#include "HsVersions.h"

import FastString
import Panic

import GLAEXTS

import Foreign

#if __GLASGOW_HASKELL__ < 503
import PrelIOBase
import PrelHandle
#else
import GHC.IOBase
import GHC.IO		( slurpFile )
#endif

import IO			( openFile, hFileSize, IOMode(ReadMode),
				  hClose )
#if __GLASGOW_HASKELL__ >= 601
import System.IO		( openBinaryFile )
#else
import IOExts                   ( openFileEx, IOModeEx(..) )
#endif

#if __GLASGOW_HASKELL__ < 503
import IArray			( listArray )
import ArrayBase		( UArray(..) )
import MutableArray
import IOExts			( hGetBufBA )
#else
import Data.Array.IArray	( listArray )
import Data.Array.MArray 	( unsafeFreeze, newArray_ )
import Data.Array.Base		( UArray(..)  )
import Data.Array.IO		( IOArray, hGetArray )
#endif

import Char			( ord )

#if __GLASGOW_HASKELL__ < 601
openBinaryFile fp mode = openFileEx fp (BinaryMode mode)
#endif
-- -----------------------------------------------------------------------------
-- The StringBuffer type

-- A StringBuffer is a ByteArray# with a pointer into it.  We also cache
-- the length of the ByteArray# for speed.

data StringBuffer
 = StringBuffer
     ByteArray#
     Int#         -- length
     Int#         -- current pos

instance Show StringBuffer where
	showsPrec _ s = showString "<stringbuffer>"

-- -----------------------------------------------------------------------------
-- Creation / Destruction

hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname = do
   h <- openBinaryFile fname ReadMode
   size <- hFileSize h
   let size_i@(I# sz#) = fromIntegral size
#if __GLASGOW_HASKELL__ < 503
   arr <- stToIO (newCharArray (0,size_i-1))
   r <- hGetBufBA h arr size_i
#else
   arr <- newArray_ (0,size_i-1)
   r <- if size_i == 0 then return 0 else hGetArray h arr size_i
#endif
   hClose h
   if (r /= size_i)
	then ioError (userError "short read of file")
	else do
#if __GLASGOW_HASKELL__ < 503
   frozen <- stToIO (unsafeFreezeByteArray arr)
   case frozen of
      ByteArray _ _ bytearr# -> return (StringBuffer bytearr# sz# 0#)
#else
   frozen <- unsafeFreeze arr
   case frozen of
      UArray _ _ bytearr# -> return (StringBuffer bytearr# sz# 0#)
#endif

#if __GLASGOW_HASKELL__ >= 502
stringToStringBuffer str = do
  let size@(I# sz#) = length str
      arr = listArray (0,size-1) (map (fromIntegral.ord) str)
		 :: UArray Int Word8
  case arr of
	UArray _ _ bytearr# -> return (StringBuffer bytearr# sz# 0#)
#else
stringToStringBuffer = panic "stringToStringBuffer: not implemented"
#endif

-- -----------------------------------------------------------------------------
-- Lookup

currentChar  :: StringBuffer -> Char
currentChar (StringBuffer arr# l# current#) =
  ASSERT(current# <# l#)
  C# (indexCharArray# arr# current#)

prevChar :: StringBuffer -> Char -> Char
prevChar (StringBuffer _ _ 0#) deflt = deflt
prevChar s deflt = lookAhead s (-1)

lookAhead :: StringBuffer -> Int  -> Char
lookAhead (StringBuffer arr# l# c#) (I# i#) =
  ASSERT(off <# l#  && off >=# 0#)
  C# (indexCharArray# arr# off)
 where 
   off = c# +# i#

-- -----------------------------------------------------------------------------
-- Moving

stepOn :: StringBuffer -> StringBuffer
stepOn s = stepOnBy 1 s

stepOnBy :: Int -> StringBuffer -> StringBuffer
stepOnBy (I# i#) (StringBuffer fo# l# c#) = StringBuffer fo# l# (c# +# i#)

atEnd :: StringBuffer -> Bool
atEnd (StringBuffer _ l# c#) = l# ==# c#

-- -----------------------------------------------------------------------------
-- Conversion

lexemeToString :: StringBuffer -> Int -> String
lexemeToString _ 0 = ""
lexemeToString (StringBuffer arr# _ current#) (I# len#) = unpack current#
 where
    end = current# +# len#

    unpack nh
      | nh >=# end  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# arr# nh

lexemeToFastString :: StringBuffer -> Int -> FastString
lexemeToFastString _ 0 = mkFastString ""
lexemeToFastString (StringBuffer fo _ current#) (I# len) =
    mkFastSubStringBA# fo current# len

-- -----------------------------------------------------------------------------
-- Parsing integer strings in various bases

parseInteger :: StringBuffer -> Int -> Integer -> (Char->Int) -> Integer
parseInteger buf len radix to_int 
  = go 0 0
  where go i x | i == len  = x
	       | otherwise = go (i+1) (x * radix + toInteger (to_int (lookAhead buf i)))
\end{code}
