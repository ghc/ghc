%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
%
\section[PackBase]{Packing/unpacking bytes}

This module provides a small set of low-level functions for packing
and unpacking a chunk of bytes. Used by code emitted by the compiler
plus the prelude libraries.

The programmer level view of packed strings is provided by a GHC system library
PackedString.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PackBase 
       (
	-- (**) - emitted by compiler.

	packCString#,      -- :: [Char] -> ByteArray#  **
	packString,	   -- :: [Char] -> ByteArray Int
	packStringST,      -- :: [Char] -> ST s (ByteArray Int)
	packNBytesST,      -- :: Int -> [Char] -> ST s (ByteArray Int)

	unpackCString,     -- :: Addr -> [Char]
	unpackNBytes,      -- :: Addr -> Int -> [Char]
	unpackNBytesST,    -- :: Addr -> Int -> ST s [Char]
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

import PrelBase
import {-# SOURCE #-} GHCerr ( error )
import PrelList ( length )
import STBase
import ArrBase
import Addr
import UnsafeST ( runST )

\end{code}

%*********************************************************
%*							*
\subsection{Unpacking Addrs}
%*							*
%*********************************************************

Primitives for converting Addrs pointing to external
sequence of bytes into a list of @Char@s:

\begin{code}
unpackCString  :: Addr{- ptr. to NUL terminated string-} -> [Char]
unpackCString a@(A# addr) = 
  if a == ``NULL'' then
     []
  else
     unpackCString# addr

unpackCString# :: Addr#  -> [Char]
unpackCString# addr 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackNBytes :: Addr -> Int -> [Char]
unpackNBytes (A# addr) (I# l) = unpackNBytes# addr l

unpackNBytesST :: Addr -> Int -> ST s [Char]
unpackNBytesST (A# addr) (I# l) = unpackNBytesST# addr l

unpackNBytes#      :: Addr# -> Int#   -> [Char]
  -- This one is called by the compiler to unpack literal strings with NULs in them; rare.
unpackNBytes# addr len
  = unpack 0#
    where
     unpack i
      | i >=# len  = []
      | otherwise  = C# ch : unpack (i +# 1#)
      where
	ch = indexCharOffAddr# addr i

unpackNBytesST# :: Addr# -> Int# -> ST s [Char]
unpackNBytesST# addr len
  = unpack 0#
  where
    unpack i 
      | i >=# len  = return []
      | otherwise  = 
	 case indexCharOffAddr# addr i of
	  ch -> unpack (i +# 1#) >>= \ ls -> return (C# ch : ls)

\end{code}

%********************************************************
%*							*
\subsection{Unpacking ByteArrays}
%*							*
%********************************************************

Converting byte arrays into list of chars:

\begin{code}
unpackCStringBA :: ByteArray Int -> [Char]
unpackCStringBA (ByteArray (l@(I# l#),u@(I# u#)) bytes) 
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
unpackNBytesBA (ByteArray (l,u) bytes) i
 = unpackNBytesBA# bytes len#
   where
    len# = case max 0 (min i len) of I# v# -> v#
    len | u > l     = 0
        | otherwise = u-l+1

unpackNBytesBA# :: ByteArray# -> Int# -> [Char]
unpackNBytesBA# bytes nh 
 = unpack 0#
   where
    unpack i
     | i >=# nh  = []
     | otherwise = C# ch : unpack (i +# 1#)
      where
	ch = indexCharArray# bytes i
\end{code}


%********************************************************
%*							*
\subsection{Packing Strings}
%*							*
%********************************************************

Converting a list of chars into a packed @ByteArray@ representation.

\begin{code}
packCString#	     :: [Char]          -> ByteArray#
packCString# str = case (packString str) of { ByteArray _ bytes -> bytes }

packString :: [Char] -> ByteArray Int
packString str = runST (packStringST str)

packStringST :: [Char] -> ST s (ByteArray Int)
packStringST str =
  let len = length str  in
  packNBytesST len str

packNBytesST :: Int -> [Char] -> ST s (ByteArray Int)
packNBytesST len@(I# length#) str =
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
    case (newCharArray# size s)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray bot barr#) }
  where
    bot = error "new_ps_array"

write_ps_array (MutableByteArray _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    STret s2# () }

-- same as unsafeFreezeByteArray
freeze_ps_array (MutableByteArray _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray (0,I# len#) frozen#) }
\end{code}


%********************************************************
%*							*
\subsection{Misc}
%*							*
%********************************************************

The compiler may emit these two

\begin{code}
unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackFoldrCString#  :: Addr# -> (Char  -> a -> a) -> a -> a 
unpackFoldrCString# addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | otherwise	   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh
\end{code}
