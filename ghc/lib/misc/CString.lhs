%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Working with C strings}

A collection of lower-level functions to help converting between
C strings and Haskell Strings (packed or otherwise).

A more user-friendly Haskell interface to packed string representation
is the PackedString interface.

\begin{code}
module CString 
	(
   	  unpackCString      -- :: Addr -> [Char]
  	, unpackNBytes       -- :: Addr -> Int -> [Char]
	, unpackNBytesST     -- :: Addr -> Int -> ST s [Char]
	, unpackNBytesAccST  -- :: Addr -> Int -> [Char] -> ST s [Char]
	, unpackCString#     -- :: Addr# -> [Char]	 **
	, unpackNBytes#      -- :: Addr# -> Int# -> [Char] **
	, unpackNBytesST#    -- :: Addr# -> Int# -> ST s [Char]

	    -- terrrible names...
	, unpackCStringIO     -- :: Addr -> IO String
	, unpackCStringLenIO  -- :: Addr -> Int -> IO String
	, unpackNBytesIO      -- :: Addr -> Int -> IO [Char]
	, unpackNBytesAccIO   -- :: Addr -> Int -> [Char] -> IO [Char]
	, unpackNBytesBAIO    -- :: ByteArray Int -> Int -> IO [Char]
	, unpackNBytesAccBAIO -- :: ByteArray Int -> Int -> [Char] -> IO [Char]

	, packString	     -- :: [Char] -> ByteArray Int
	, packStringST	     -- :: [Char] -> ST s (ByteArray Int)
	, packStringIO	     -- :: [Char] -> IO (ByteArray Int)
	, packNBytesST	     -- :: Int -> [Char] -> ByteArray Int
	, packCString#	     -- :: [Char] -> ByteArray#

	, unpackCStringBA    -- :: ByteArray Int -> [Char]
	, unpackNBytesBA     -- :: ByteArray Int -> Int  -> [Char]
	, unpackCStringBA#   -- :: ByteArray#    -> Int# -> [Char]
	, unpackNBytesBA#    -- :: ByteArray#    -> Int# -> [Char]

	  -- unmarshaling (char*) vectors.
	, unvectorize        -- :: Addr -> Int -> IO [String]
	, vectorize	     -- :: [[Char]] -> IO (ByteArray Int)


	, allocChars         -- :: Int -> IO (MutableByteArray RealWorld Int)
	, allocWords         -- :: Int -> IO (MutableByteArray RealWorld Int)
	, freeze	     -- :: MutableByteArray RealWorld Int -> IO (ByteArray Int)
	, strcpy	     -- :: Addr -> IO String

	) where

import PrelPack
import GlaExts
import Addr
import PrelIOBase ( IO(..) )

\end{code}

\begin{code}
packStringIO :: [Char] -> IO (ByteArray Int)
packStringIO str = stToIO (packStringST str)
\end{code}

\begin{code}
unpackCStringIO :: Addr -> IO String
unpackCStringIO addr
 | addr == ``NULL'' = return ""
 | otherwise        = unpack 0#
  where
    unpack nh = do
       ch <- readCharOffAddr addr (I# nh)
       if ch == '\0'
        then return []
	else do
	   ls <- unpack (nh +# 1#)
	   return (ch : ls)

-- unpack 'len' chars
unpackCStringLenIO :: Addr -> Int -> IO String
unpackCStringLenIO addr l@(I# len#)
 | len# <# 0#  = ioError (userError ("CString.unpackCStringLenIO: negative length (" ++ show l ++ ")"))
 | len# ==# 0# = return ""
 | otherwise   = unpack [] (len# -# 1#)
  where
    unpack acc 0# = do
       ch <- readCharOffAddr addr (I# 0#)
       return (ch:acc)
    unpack acc nh = do
       ch <- readCharOffAddr addr (I# nh)
       unpack (ch:acc) (nh -# 1#)

unpackNBytesIO     :: Addr -> Int -> IO [Char]
unpackNBytesIO a l = stToIO (unpackNBytesST a l)

unpackNBytesAccIO  :: Addr -> Int -> [Char] -> IO [Char]
unpackNBytesAccIO a l acc = stToIO (unpackNBytesAccST a l acc)

unpackNBytesBAIO     :: ByteArray Int -> Int -> IO [Char]
unpackNBytesBAIO ba l = unpackNBytesAccBAIO ba l []

-- note: no bounds checking!
unpackNBytesAccBAIO :: ByteArray Int -> Int -> [Char] -> IO [Char]
unpackNBytesAccBAIO _ 0  rest = return rest
unpackNBytesAccBAIO (ByteArray _ ba) (I# len#) rest = unpack rest (len# -# 1#)
  where
    unpack acc i# 
      | i# <# 0#   = return acc
      | otherwise  = 
	 case indexCharArray# ba i# of
	   ch -> unpack (C# ch : acc) (i# -# 1#)

\end{code}

Turn a NULL-terminated vector of null-terminated strings into a string list
(ToDo: create a module of common marshaling functions)

\begin{code}
unvectorize :: Addr -> Int -> IO [String]
unvectorize ptr n
  | str == ``NULL'' = return []
  | otherwise = do
	x  <- unpackCStringIO str
	xs <- unvectorize ptr (n+1)
	return (x : xs)
  where
   str = indexAddrOffAddr ptr n

\end{code}

 Turn a string list into a NULL-terminated vector of null-terminated
strings No indices...I hate indices.  Death to Ix.

\begin{code}
vectorize :: [String] -> IO (ByteArray Int)
vectorize vs = do
  arr <- allocWords (len + 1)
  fill arr 0 vs
  freeze arr
 where
    len :: Int
    len = length vs

    fill :: MutableByteArray RealWorld Int -> Int -> [String] -> IO ()
    fill arr n [] =
	_casm_ ``((PP_)%0)[%1] = NULL;'' arr n
    fill arr n (x:xs) =
	packStringIO x			    >>= \ barr ->
        _casm_ ``((PP_)%0)[%1] = (P_)%2;'' arr n barr
					    >>= \ () ->
	fill arr (n+1) xs

\end{code}

Allocating chunks of memory in the Haskell heap, leaving
out the bounds - use with care.

\begin{code}
-- Allocate a mutable array of characters with no indices.
allocChars :: Int -> IO (MutableByteArray RealWorld Int)
allocChars (I# size#) = IO $ \ s# ->
    case newCharArray# size# s# of
      (# s2#, barr# #) ->
	(# s2#, (MutableByteArray (I# 1#, I# size#) barr#) #)

allocWords :: Int -> IO (MutableByteArray RealWorld Int)
allocWords (I# size#) = IO $ \ s# ->
    case newIntArray# size# s# of
      (# s2#, barr# #) ->
	(# s2#, (MutableByteArray (I# 1#, I# size#) barr#) #)

-- Freeze these index-free mutable arrays
freeze :: MutableByteArray RealWorld Int -> IO (ByteArray Int)
freeze (MutableByteArray ixs arr#) = IO $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of
      (# s2#, frozen# #) ->
	(# s2#, (ByteArray ixs frozen#) #)

-- Copy a null-terminated string from outside the heap to
-- Haskellized nonsense inside the heap
strcpy :: Addr -> IO String
strcpy str = unpackCStringIO str

\end{code}
