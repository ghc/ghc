%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section{Basic ops on packed representations}

Some basic operations for working on packed representations of series
of bytes (character strings). Used by the interface lexer input
subsystem, mostly.

\begin{code}
module PrimPacked
       (
        strLength,          -- :: _Addr -> Int
        copyPrefixStr,      -- :: _Addr -> Int -> ByteArray Int
        copySubStr,         -- :: _Addr -> Int -> Int -> ByteArray Int
        copySubStrFO,       -- :: ForeignObj -> Int -> Int -> ByteArray Int
        copySubStrBA,       -- :: ByteArray Int -> Int -> Int -> ByteArray Int

        eqStrPrefix,        -- :: Addr# -> ByteArray# -> Int# -> Bool
        eqCharStrPrefix,    -- :: Addr# -> Addr# -> Int# -> Bool
        eqStrPrefixBA,      -- :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
        eqCharStrPrefixBA,  -- :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
        eqStrPrefixFO,      -- :: ForeignObj# -> ByteArray# -> Int# -> Int# -> Bool

        addrOffset#         -- :: Addr# -> Int# -> Addr# 
       ) where

-- This #define suppresses the "import FastString" that
-- HsVersions otherwise produces
#define COMPILING_FAST_STRING
#include "HsVersions.h"

import GlaExts
import Addr	( Addr(..) )
import ST
import Foreign
-- ForeignObj is now exported abstractly.
#if __GLASGOW_HASKELL__ >= 303
import PrelForeign   ( ForeignObj(..) )
#endif

#if __GLASGOW_HASKELL__ < 301
import ArrBase  	( StateAndMutableByteArray#(..), 
			  StateAndByteArray#(..) )
import STBase
#elif __GLASGOW_HASKELL__ < 400
import PrelArr  	( StateAndMutableByteArray#(..), 
			  StateAndByteArray#(..) )
import PrelST
#else
import PrelST
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
  freeze_ps_array ch_array length#	     `thenStrictlyST` \ barr ->
  returnStrictlyST barr )
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = case (indexCharOffAddr# a idx) of { ch ->
	write_ps_array arr_in# idx ch `seqStrictlyST`
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
\end{code}

pCopying a sub-string out of a ForeignObj

\begin{code}
copySubStrFO :: ForeignObj -> Int -> Int -> ByteArray Int
copySubStrFO (ForeignObj fo) (I# start#) len@(I# length#) =
 runST (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  new_ps_array (length# +# 1#)  `thenStrictlyST` \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   `seqStrictlyST`
   -- freeze the puppy:
  freeze_ps_array ch_array length#)
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = case (indexCharOffForeignObj# fo (idx +# start#)) of { ch ->
	write_ps_array arr_in# idx ch `seqStrictlyST`
	fill_in arr_in# (idx +# 1#) }

-- step on (char *) pointer by x units.
addrOffset# :: Addr# -> Int# -> Addr# 
addrOffset# a# i# =
  case unsafePerformIO (_casm_ ``%r=(char *)((char *)%0 + (int)%1); '' (A# a#) (I# i#)) of
    A# a -> a

copySubStrBA :: ByteArray Int -> Int -> Int -> ByteArray Int
#if __GLASGOW_HASKELL__ >= 405
copySubStrBA (ByteArray _ _ barr#) (I# start#) len@(I# length#) =
#else
copySubStrBA (ByteArray _ barr#) (I# start#) len@(I# length#) =
#endif
 runST (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  new_ps_array (length# +# 1#)  `thenStrictlyST` \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   	`seqStrictlyST`
   -- freeze the puppy:
  freeze_ps_array ch_array length#)
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = case (indexCharArray# barr# (start# +# idx)) of { ch ->
	write_ps_array arr_in# idx ch `seqStrictlyST`
	fill_in arr_in# (idx +# 1#) }

\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...
[Copied from PackBase; no real reason -- UGH]

\begin{code}
new_ps_array	:: Int# -> ST s (MutableByteArray s Int)
write_ps_array	:: MutableByteArray s Int -> Int# -> Char# -> ST s () 
freeze_ps_array :: MutableByteArray s Int -> Int# -> ST s (ByteArray Int)

new_ps_array size = ST $ \ s ->
#if __GLASGOW_HASKELL__ < 400
    case (newCharArray# size s)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray bot barr#) }
#elif __GLASGOW_HASKELL__ < 405
    case (newCharArray# size s)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot barr# #) }
#else
    case (newCharArray# size s)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray bot bot barr# #) }
#endif
  where
    bot = error "new_ps_array"

#if __GLASGOW_HASKELL__ < 400
write_ps_array (MutableByteArray _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    STret s2# () }
#elif __GLASGOW_HASKELL__ < 405
write_ps_array (MutableByteArray _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    (# s2#, () #) }
#else
write_ps_array (MutableByteArray _ _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    (# s2#, () #) }
#endif

-- same as unsafeFreezeByteArray
#if __GLASGOW_HASKELL__ < 400
freeze_ps_array (MutableByteArray _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray (0,I# len#) frozen#) }
#elif __GLASGOW_HASKELL__ < 405
freeze_ps_array (MutableByteArray _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray (0,I# len#) frozen# #) }
#else
freeze_ps_array (MutableByteArray _ _ arr#) len# = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray 0 (I# len#) frozen# #) }
#endif
\end{code}


Compare two equal-length strings for equality:

\begin{code}
eqStrPrefix :: Addr# -> ByteArray# -> Int# -> Bool
eqStrPrefix a# barr# len# = 
  unsafePerformIO (
#if __GLASGOW_HASKELL__ < 405
   _ccall_ strncmp (A# a#) (ByteArray bot barr#) (I# len#) >>= \ (I# x#) ->
#else
   _ccall_ strncmp (A# a#) (ByteArray bot bot barr#) (I# len#) >>= \ (I# x#) ->
#endif
   return (x# ==# 0#))
  where
#if __GLASGOW_HASKELL__ < 405
   bot :: (Int,Int)
#else
   bot :: Int
#endif
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
#if __GLASGOW_HASKELL__ < 405
	  (ByteArray bot b2#)
#else
	  (ByteArray bot bot b2#) 
#endif 
	  (I# start#) 
#if __GLASGOW_HASKELL__ < 405
          (ByteArray bot b1#) 
#else
          (ByteArray bot bot b1#) 
#endif
          (I# len#)                  >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
#if __GLASGOW_HASKELL__ < 405
   bot :: (Int,Int)
#else
   bot :: Int
#endif
   bot = error "eqStrPrefixBA"

eqCharStrPrefixBA :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
eqCharStrPrefixBA a# b2# start# len# = 
  unsafePerformIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
#if __GLASGOW_HASKELL__ < 405
	  (ByteArray bot b2#) 
#else
	  (ByteArray bot bot b2#) 
#endif
	  (I# start#) 
          (A# a#)
          (I# len#)                  >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
#if __GLASGOW_HASKELL__ < 405
   bot :: (Int,Int)
#else
   bot :: Int
#endif
   bot = error "eqCharStrPrefixBA"

eqStrPrefixFO :: ForeignObj# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixFO fo# barr# start# len# = 
  unsafePerformIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (ForeignObj fo#) 
	  (I# start#) 
#if __GLASGOW_HASKELL__ < 405
          (ByteArray bot barr#) 
#else
          (ByteArray bot bot barr#) 
#endif
          (I# len#)                  >>= \ (I# x#) ->
   return (x# ==# 0#))
  where
#if __GLASGOW_HASKELL__ < 405
   bot :: (Int,Int)
#else
   bot :: Int
#endif
   bot = error "eqStrPrefixFO"
\end{code}
