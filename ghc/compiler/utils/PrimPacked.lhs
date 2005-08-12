%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section{Basic ops on packed representations}

Some basic operations for working on packed representations of series
of bytes (character strings). Used by the interface lexer input
subsystem, mostly.

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module PrimPacked (
	Ptr(..), nullPtr, plusAddr#,
	BA(..),
	packString, 	   -- :: String -> (Int, BA)
	unpackNBytesBA,    -- :: BA -> Int -> [Char]
        strLength,	   -- :: Ptr CChar -> Int
        copyPrefixStr,	   -- :: Addr# -> Int -> BA
        copySubStrBA,	   -- :: BA -> Int -> Int -> BA
        eqStrPrefix,	   -- :: Addr# -> ByteArray# -> Int# -> Bool
        eqStrPrefixBA,	   -- :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
 ) where

-- This #define suppresses the "import FastString" that
-- HsVersions otherwise produces
#define COMPILING_FAST_STRING
#include "HsVersions.h"

import GLAEXTS
import UNSAFE_IO	( unsafePerformIO )

import MONAD_ST
import Foreign

#if __GLASGOW_HASKELL__ < 503
import PrelST
#else
import GHC.ST
#endif

#if __GLASGOW_HASKELL__ >= 504
import GHC.Ptr	( Ptr(..) )
#elif __GLASGOW_HASKELL__ >= 500
import Ptr	( Ptr(..) )
#endif

#if __GLASGOW_HASKELL__ < 504
import PrelIOBase	( IO(..) )
#else
import GHC.IOBase	( IO(..) )
#endif
\end{code}

Compatibility: 4.08 didn't have the Ptr type.

\begin{code}
#if __GLASGOW_HASKELL__ <= 408
data Ptr a = Ptr Addr# deriving (Eq, Ord)

nullPtr :: Ptr a
nullPtr = Ptr (int2Addr# 0#)
#endif

#if __GLASGOW_HASKELL__ <= 500
-- plusAddr# is a primop in GHC > 5.00
plusAddr# :: Addr# -> Int# -> Addr#
plusAddr# a# i# = int2Addr# (addr2Int# a# +# i#)
#endif
\end{code}

Wrapper types for bytearrays

\begin{code}
data BA    = BA  ByteArray#
data MBA s = MBA (MutableByteArray# s)
\end{code}

\begin{code}
packString :: String -> (Int, BA)
packString str = (l, arr)
 where
  l@(I# length#) = length str

  arr = runST (do
    ch_array <- new_ps_array length#
      -- fill in packed string from "str"
    fill_in ch_array 0# str
      -- freeze the puppy:
    freeze_ps_array ch_array length#
   )

  fill_in :: MBA s -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   return ()
  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 >>
   fill_in arr_in# (idx +# 1#) cs
\end{code}

Unpacking a string

\begin{code}
unpackNBytesBA :: BA -> Int -> [Char]
unpackNBytesBA (BA bytes) (I# len)
 = unpack 0#
 where
    unpack nh
      | nh >=# len  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh
\end{code}

Copying a char string prefix into a byte array.

\begin{code}
copyPrefixStr :: Addr# -> Int -> BA
copyPrefixStr a# len@(I# length#) = copy' length#
 where
   copy' length# = runST (do
     {- allocate an array that will hold the string
     -}
     ch_array <- new_ps_array length#
     {- Revert back to Haskell-only solution for the moment.
     	_ccall_ memcpy ch_array (A# a) len        >>=  \ () ->
     	write_ps_array ch_array length# (chr# 0#) >>
     -}
     -- fill in packed string from "addr"
     fill_in ch_array 0#
     -- freeze the puppy:
     freeze_ps_array ch_array length#
    )

   fill_in :: MBA s -> Int# -> ST s ()
   fill_in arr_in# idx
      | idx ==# length#
      = return ()
      | otherwise
      = case (indexCharOffAddr# a# idx) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }
\end{code}

Copying out a substring, assume a 0-indexed string:
(and positive lengths, thank you).

\begin{code}
#ifdef UNUSED
copySubStr :: Addr# -> Int -> Int -> BA
copySubStr a# (I# start#) length =
  copyPrefixStr (a# `plusAddr#` start#)  length
#endif

copySubStrBA :: BA -> Int -> Int -> BA
copySubStrBA (BA barr#) (I# start#) len@(I# length#) = ba
 where
  ba = runST (do
     -- allocate an array that will hold the string
    ch_array <- new_ps_array length#
     -- fill in packed string from "addr"
    fill_in ch_array 0#
     -- freeze the puppy:
    freeze_ps_array ch_array length#
   )

  fill_in :: MBA s -> Int# -> ST s ()
  fill_in arr_in# idx
      | idx ==# length#
      = return ()
      | otherwise
      = case (indexCharArray# barr# (start# +# idx)) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }
\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...
[Copied from PackBase; no real reason -- UGH]

\begin{code}
new_ps_array	:: Int# -> ST s (MBA s)
write_ps_array	:: MBA s -> Int# -> Char# -> ST s () 
freeze_ps_array :: MBA s -> Int# -> ST s BA

#if __GLASGOW_HASKELL__ < 411
#define NEW_BYTE_ARRAY newCharArray#
#else 
#define NEW_BYTE_ARRAY newPinnedByteArray#
#endif

new_ps_array size = ST $ \ s ->
    case (NEW_BYTE_ARRAY size s)  of { (# s2#, barr# #) ->
    (# s2#, MBA barr# #) }

write_ps_array (MBA barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    (# s2#, () #) }

-- same as unsafeFreezeByteArray
freeze_ps_array (MBA arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, BA frozen# #) }
\end{code}


Compare two equal-length strings for equality:

\begin{code}
eqStrPrefix :: Addr# -> ByteArray# -> Int# -> Bool
eqStrPrefix a# barr# len# = 
  inlinePerformIO $ do
   x <- memcmp_ba a# barr# (I# len#)
   return (x == 0)

#ifdef UNUSED
eqCharStrPrefix :: Addr# -> Addr# -> Int# -> Bool
eqCharStrPrefix a1# a2# len# = 
  inlinePerformIO $ do
   x <- memcmp a1# a2# (I# len#)
   return (x == 0)
#endif

eqStrPrefixBA :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixBA b1# b2# start# len# = 
  inlinePerformIO $ do
    x <- memcmp_baoff_ba b2# (I# start#) b1# (I# len#)
    return (x == 0)

#ifdef UNUSED
eqCharStrPrefixBA :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
eqCharStrPrefixBA a# b2# start# len# = 
  inlinePerformIO $ do
    x <- memcmp_baoff b2# (I# start#) a# (I# len#) 
    return (x == 0)
#endif
\end{code}

\begin{code}
-- Just like unsafePerformIO, but we inline it.  This is safe when
-- there are no side effects, and improves performance.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

#if __GLASGOW_HASKELL__ <= 408
strLength (Ptr a#) = ghc_strlen a#
foreign import ccall unsafe "ghc_strlen" 
  ghc_strlen :: Addr# -> Int
#else
foreign import ccall unsafe "ghc_strlen" 
  strLength :: Ptr () -> Int
#endif

foreign import ccall unsafe "ghc_memcmp"
  memcmp :: Addr# -> Addr# -> Int -> IO Int

foreign import ccall unsafe "ghc_memcmp" 
  memcmp_ba :: Addr# -> ByteArray# -> Int -> IO Int

foreign import ccall unsafe "ghc_memcmp_off"
  memcmp_baoff :: ByteArray# -> Int -> Addr# -> Int -> IO Int

foreign import ccall unsafe "ghc_memcmp_off"
  memcmp_baoff_ba :: ByteArray# -> Int -> ByteArray# -> Int -> IO Int
\end{code}
