%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section{Basic ops on packed representations}

Some basic operations for working on packed representations of series
of bytes (character strings). Used by the interface lexer input
subsystem, mostly.

\begin{code}
{-# OPTIONS -monly-3-regs -optc-DNON_POSIX_SOURCE #-}
module PrimPacked
       (
        strLength,          -- :: _Addr -> Int
        copyPrefixStr,      -- :: _Addr -> Int -> ByteArray Int
        copySubStr,         -- :: _Addr -> Int -> Int -> ByteArray Int
        copySubStrBA,       -- :: ByteArray Int -> Int -> Int -> ByteArray Int

        eqStrPrefix,        -- :: Addr# -> ByteArray# -> Int# -> Bool
        eqCharStrPrefix,    -- :: Addr# -> Addr# -> Int# -> Bool
        eqStrPrefixBA,      -- :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
        eqCharStrPrefixBA,  -- :: Addr# -> ByteArray# -> Int# -> Int# -> Bool

        addrOffset#         -- :: Addr# -> Int# -> Addr# 
       ) where

-- This #define suppresses the "import FastString" that
-- HsVersions otherwise produces
#define COMPILING_FAST_STRING
#include "HsVersions.h"

import GlaExts
#if __GLASGOW_HASKELL__ < 411
import PrelAddr	( Addr(..) )
#else
import Addr	( Addr(..) )
#endif
import ST
import Foreign

#if __GLASGOW_HASKELL__ < 503
import PrelST
#else
import GHC.ST
#endif

\end{code} 

Return the length of a @\\NUL@ terminated character string:

\begin{code}
strLength :: Addr -> Int
strLength a =
 unsafePerformIO (
    _ccall_ strlen a  >>= \ len@(I# _) ->
    return len
 )
{-# NOINLINE strLength #-}
\end{code}

Copying a char string prefix into a byte array,
{\em assuming} the prefix does not contain any
NULs.

\begin{code}
copyPrefixStr :: Addr -> Int -> ByteArray Int
copyPrefixStr (A# a) len@(I# length#) =
 runST (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  (new_ps_array (length# +# 1#))             >>= \ ch_array ->
{- Revert back to Haskell-only solution for the moment.
   _ccall_ memcpy ch_array (A# a) len        >>=  \ () ->
   write_ps_array ch_array length# (chr# 0#) >>
-}
   -- fill in packed string from "addr"
  fill_in ch_array 0#			     >>
   -- freeze the puppy:
  freeze_ps_array ch_array length#	     >>= \ barr ->
  return barr )
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = case (indexCharOffAddr# a idx) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }

\end{code}

Copying out a substring, assume a 0-indexed string:
(and positive lengths, thank you).

\begin{code}
copySubStr :: Addr -> Int -> Int -> ByteArray Int
copySubStr a start length =
  unsafePerformIO (
    _casm_ `` %r= (char *)((char *)%0 + (int)%1); '' a start 
                                                     >>= \ a_start ->
    return (copyPrefixStr a_start length))

-- step on (char *) pointer by x units.
addrOffset# :: Addr# -> Int# -> Addr# 
addrOffset# a# i# =
  case unsafePerformIO (_casm_ ``%r=(char *)((char *)%0 + (int)%1); '' (A# a#) (I# i#)) of
    A# a -> a

copySubStrBA :: ByteArray Int -> Int -> Int -> ByteArray Int
copySubStrBA (ByteArray _ _ barr#) (I# start#) len@(I# length#) =
 runST (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  new_ps_array (length# +# 1#)  >>= \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   	>>
   -- freeze the puppy:
  freeze_ps_array ch_array length#)
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = case (indexCharArray# barr# (start# +# idx)) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }
\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...
[Copied from PackBase; no real reason -- UGH]

\begin{code}
new_ps_array	:: Int# -> ST s (MutableByteArray s Int)
write_ps_array	:: MutableByteArray s Int -> Int# -> Char# -> ST s () 
freeze_ps_array :: MutableByteArray s Int -> Int# -> ST s (ByteArray Int)

new_ps_array size = ST $ \ s ->
#if __GLASGOW_HASKELL__ < 411
    case (newCharArray# size s)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot bot barr# #) }
#else /* 411 and higher */
    case (newByteArray# size s)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot bot barr# #) }
#endif
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


Compare two equal-length strings for equality:

\begin{code}
eqStrPrefix :: Addr# -> ByteArray# -> Int# -> Bool
eqStrPrefix a# barr# len# = 
  unsafePerformIO (
   _ccall_ strncmp (A# a#) (ByteArray bot bot barr#) (I# len#) >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
   bot :: Int
   bot = error "eqStrPrefix"

eqCharStrPrefix :: Addr# -> Addr# -> Int# -> Bool
eqCharStrPrefix a1# a2# len# = 
  unsafePerformIO (
   _ccall_ strncmp (A# a1#) (A# a2#) (I# len#) >>= \ (I# x#) ->
   return (x# ==# 0#))

eqStrPrefixBA :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixBA b1# b2# start# len# = 
  unsafePerformIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (ByteArray bot bot b2#) 
	  (I# start#) 
          (ByteArray bot bot b1#) 
          (I# len#)                  >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
   bot :: Int
   bot = error "eqStrPrefixBA"

eqCharStrPrefixBA :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
eqCharStrPrefixBA a# b2# start# len# = 
  unsafePerformIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (ByteArray bot bot b2#) 
	  (I# start#) 
          (A# a#)
          (I# len#)                  >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
   bot :: Int
   bot = error "eqCharStrPrefixBA"
\end{code}
