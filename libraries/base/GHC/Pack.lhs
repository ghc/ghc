% ------------------------------------------------------------------------------
% $Id: Pack.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1997-2000
%

\section[GHC.Pack]{Packing/unpacking bytes}

This module provides a small set of low-level functions for packing
and unpacking a chunk of bytes. Used by code emitted by the compiler
plus the prelude libraries.

The programmer level view of packed strings is provided by a GHC
system library PackedString.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Pack
       (
	-- (**) - emitted by compiler.

	packCString#,      -- :: [Char] -> ByteArray#  **
	packString,	   -- :: [Char] -> ByteArray Int
	packStringST,      -- :: [Char] -> ST s (ByteArray Int)
	packNBytesST,      -- :: Int -> [Char] -> ST s (ByteArray Int)

	unpackCString,     -- :: Ptr a -> [Char]
	unpackCStringST,   -- :: Ptr a -> ST s [Char]
	unpackNBytes,      -- :: Ptr a -> Int -> [Char]
	unpackNBytesST,    -- :: Ptr a -> Int -> ST s [Char]
	unpackNBytesAccST, -- :: Ptr a -> Int -> [Char] -> ST s [Char]
	unpackNBytesAccST#,-- :: Ptr a -> Int -> [Char] -> ST s [Char]
	unpackCString#,    -- :: Addr# -> [Char]	 **
	unpackNBytes#,     -- :: Addr# -> Int# -> [Char] **
	unpackNBytesST#,   -- :: Addr# -> Int# -> ST s [Char]

	unpackCStringBA,   -- :: ByteArray Int -> [Char]
	unpackNBytesBA,    -- :: ByteArray Int -> Int  -> [Char]
	unpackCStringBA#,  -- :: ByteArray#    -> Int# -> [Char]
	unpackNBytesBA#,   -- :: ByteArray#    -> Int# -> [Char]


	unpackFoldrCString#,  -- **
	unpackAppendCString#,  -- **

	new_ps_array,		-- Int# -> ST s (MutableByteArray s Int)
	write_ps_array,		-- MutableByteArray s Int -> Int# -> Char# -> ST s () 
	freeze_ps_array		-- MutableByteArray s Int -> Int# -> ST s (ByteArray Int)

       ) 
	where

import GHC.Base
import {-# SOURCE #-} GHC.Err ( error )
import GHC.List ( length )
import GHC.ST
import GHC.Num
import GHC.ByteArr
import Foreign.Ptr

\end{code}

%*********************************************************
%*							*
\subsection{Unpacking Ptrs}
%*							*
%*********************************************************

Primitives for converting Addrs pointing to external
sequence of bytes into a list of @Char@s:

\begin{code}
unpackCString :: Ptr a -> [Char]
unpackCString a@(Ptr addr)
  | a == nullPtr  = []
  | otherwise	   = unpackCString# addr
     
unpackNBytes :: Ptr a -> Int -> [Char]
unpackNBytes (Ptr addr) (I# l) = unpackNBytes# addr l

unpackCStringST  :: Ptr a{- ptr. to NUL terminated string-} -> ST s [Char]
unpackCStringST a@(Ptr addr)
  | a == nullPtr  = return []
  | otherwise	   = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = return []
      | otherwise	   = do
		ls <- unpack (nh +# 1#)
		return ((C# ch ) : ls)
      where
	ch = indexCharOffAddr# addr nh

unpackNBytesST :: Ptr a -> Int -> ST s [Char]
unpackNBytesST (Ptr addr) (I# l) = unpackNBytesAccST# addr l []

unpackNBytesAccST :: Ptr a -> Int -> [Char] -> ST s [Char]
unpackNBytesAccST (Ptr addr) (I# l) rest = unpackNBytesAccST# addr l rest

unpackNBytesST# :: Addr# -> Int# -> ST s [Char]
unpackNBytesST# addr# l#   = unpackNBytesAccST# addr# l# []

unpackNBytesAccST# :: Addr# -> Int# -> [Char] -> ST s [Char]
unpackNBytesAccST# _addr 0#   rest = return rest
unpackNBytesAccST#  addr len# rest = unpack rest (len# -# 1#)
  where
    unpack acc i# 
      | i# <# 0#  = return acc
      | otherwise  = 
	 case indexCharOffAddr# addr i# of
	  ch -> unpack (C# ch : acc) (i# -# 1#)

\end{code}

%********************************************************
%*							*
\subsection{Unpacking ByteArrays}
%*							*
%********************************************************

Converting byte arrays into list of chars:

\begin{code}
unpackCStringBA :: ByteArray Int -> [Char]
unpackCStringBA (ByteArray l@(I# l#) u@(I# u#) bytes) 
 | l > u     = []
 | otherwise = unpackCStringBA# bytes (u# -# l# +# 1#)

{-
 unpack until NUL or end of BA is reached, whatever comes first.
-}
unpackCStringBA# :: ByteArray# -> Int# -> [Char]
unpackCStringBA# bytes len
 = unpack 0#
 where
    unpack nh
      | nh >=# len         || 
        ch `eqChar#` '\0'#    = []
      | otherwise	      = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh

unpackNBytesBA :: ByteArray Int -> Int -> [Char]
unpackNBytesBA (ByteArray l u bytes) i
 = unpackNBytesBA# bytes len#
   where
    len# = case max 0 (min i len) of I# v# -> v#
    len | l > u     = 0
        | otherwise = u-l+1

unpackNBytesBA# :: ByteArray# -> Int# -> [Char]
unpackNBytesBA# _bytes 0#   = []
unpackNBytesBA#  bytes len# = unpack [] (len# -# 1#)
   where
    unpack acc i#
     | i# <# 0#  = acc
     | otherwise = 
          case indexCharArray# bytes i# of
	    ch -> unpack (C# ch : acc) (i# -# 1#)

\end{code}


%********************************************************
%*							*
\subsection{Packing Strings}
%*							*
%********************************************************

Converting a list of chars into a packed @ByteArray@ representation.

\begin{code}
packCString#	     :: [Char]          -> ByteArray#
packCString# str = case (packString str) of { ByteArray _ _ bytes -> bytes }

packString :: [Char] -> ByteArray Int
packString str = runST (packStringST str)

packStringST :: [Char] -> ST s (ByteArray Int)
packStringST str =
  let len = length str  in
  packNBytesST len str

packNBytesST :: Int -> [Char] -> ST s (ByteArray Int)
packNBytesST (I# length#) str =
  {- 
   allocate an array that will hold the string
   (not forgetting the NUL byte at the end)
  -}
 new_ps_array (length# +# 1#) >>= \ ch_array ->
   -- fill in packed string from "str"
 fill_in ch_array 0# str   >>
   -- freeze the puppy:
 freeze_ps_array ch_array length#
 where
  fill_in :: MutableByteArray s Int -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   write_ps_array arr_in# idx (chr# 0#) >>
   return ()

  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 >>
   fill_in arr_in# (idx +# 1#) cs

\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...

\begin{code}
new_ps_array	:: Int# -> ST s (MutableByteArray s Int)
write_ps_array	:: MutableByteArray s Int -> Int# -> Char# -> ST s () 
freeze_ps_array :: MutableByteArray s Int -> Int# -> ST s (ByteArray Int)

new_ps_array size = ST $ \ s ->
    case (newByteArray# size s)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot bot barr# #) }
  where
    bot = error "new_ps_array"

write_ps_array (MutableByteArray _ _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    (# s2#, () #) }

-- same as unsafeFreezeByteArray
freeze_ps_array (MutableByteArray _ _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray 0 (I# len#) frozen# #) }
\end{code}


