%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
%
\section{Basic ops on packed representations}

Some basic operations for working on packed representations of series
of bytes (character strings). Used by the interface lexer input
subsystem, mostly.

\begin{code}
#include "HsVersions.h"

module PrimPacked
       (
        strLength,          -- :: _Addr -> Int
        copyPrefixStr,      -- :: _Addr -> Int -> _ByteArray Int
        copySubStr,         -- :: _Addr -> Int -> Int -> _ByteArray Int
        copySubStrFO,       -- :: ForeignObj -> Int -> Int -> _ByteArray Int
        copySubStrBA,       -- :: _ByteArray Int -> Int -> Int -> _ByteArray Int

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 205
        stringToByteArray,  -- :: String -> _ByteArray Int
	byteArrayToString,  -- :: _ByteArray Int -> String
#endif

        eqStrPrefix,        -- :: Addr# -> ByteArray# -> Int# -> Bool
        eqCharStrPrefix,    -- :: Addr# -> Addr# -> Int# -> Bool
        eqStrPrefixBA,      -- :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
        eqCharStrPrefixBA,  -- :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
        eqStrPrefixFO,      -- :: ForeignObj# -> ByteArray# -> Int# -> Int# -> Bool

        addrOffset#,        -- :: Addr# -> Int# -> Addr# 
        indexCharOffFO#     -- :: ForeignObj# -> Int# -> Char#
       ) where

#if __GLASGOW_HASKELL__ <= 201
import PreludeGlaST
import PreludeGlaMisc
#else
import GlaExts
import Foreign
import GHC
import ArrBase
import ST
import STBase

# if __GLASGOW_HASKELL__ == 202
import PrelBase ( Char(..) )
# endif

# if __GLASGOW_HASKELL__ >= 206
import PackBase
# endif

#endif

\end{code} 

Return the length of a @\\NUL@ terminated character string:

\begin{code}
strLength :: _Addr -> Int
strLength a =
 unsafePerformPrimIO (
    _ccall_ strlen a  `thenPrimIO` \ len@(I# _) ->
    returnPrimIO len
 )

\end{code}

Copying a char string prefix into a byte array,
{\em assuming} the prefix does not contain any
NULs.

\begin{code}
copyPrefixStr :: _Addr -> Int -> _ByteArray Int
copyPrefixStr (A# a) len@(I# length#) =
 unsafePerformPrimIO (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  (new_ps_array (length# +# 1#))             `thenPrimIO` \ ch_array ->
{- Revert back to Haskell-only solution for the moment.
   _ccall_ memcpy ch_array (A# a) len        `thenPrimIO`  \ () ->
   write_ps_array ch_array length# (chr# 0#) `seqPrimIO`
-}
   -- fill in packed string from "addr"
  fill_in ch_array 0#			     `seqPrimIO`
   -- freeze the puppy:
  freeze_ps_array ch_array)
  where
    fill_in :: _MutableByteArray s Int -> Int# -> _ST s ()

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
copySubStr :: _Addr -> Int -> Int -> _ByteArray Int
copySubStr a start length =
  unsafePerformPrimIO (
    _casm_ `` %r= (char *)((char *)%0 + (int)%1); '' a start 
                                                     `thenPrimIO` \ a_start ->
    returnPrimIO (copyPrefixStr a_start length))
\end{code}

Copying a sub-string out of a ForeignObj

\begin{code}
copySubStrFO :: _ForeignObj -> Int -> Int -> _ByteArray Int
copySubStrFO (_ForeignObj fo) (I# start#) len@(I# length#) =
 unsafePerformPrimIO (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  new_ps_array (length# +# 1#)  `thenStrictlyST` \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   `seqStrictlyST`
   -- freeze the puppy:
  freeze_ps_array ch_array)
  where
    fill_in :: _MutableByteArray s Int -> Int# -> _ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = case (indexCharOffFO# fo (idx +# start#)) of { ch ->
	write_ps_array arr_in# idx ch `seqStrictlyST`
	fill_in arr_in# (idx +# 1#) }

{- ToDo: add FO primitives.. -}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <=205
indexCharOffFO# :: ForeignObj# -> Int# -> Char#
indexCharOffFO# fo# i# = 
  case unsafePerformPrimIO (_casm_ ``%r=(char)*((char *)%0 + (int)%1); '' (_ForeignObj fo#) (I# i#)) of
    C# c -> c
#else
indexCharOffFO# :: ForeignObj# -> Int# -> Char#
indexCharOffFO# fo i = indexCharOffForeignObj# fo i
#endif

-- step on (char *) pointer by x units.
addrOffset# :: Addr# -> Int# -> Addr# 
addrOffset# a# i# =
  case unsafePerformPrimIO (_casm_ ``%r=(char *)((char *)%0 + (int)%1); '' (A# a#) (I# i#)) of
    A# a -> a

copySubStrBA :: _ByteArray Int -> Int -> Int -> _ByteArray Int
copySubStrBA (_ByteArray _ barr#) (I# start#) len@(I# length#) =
 unsafePerformPrimIO (
  {- allocate an array that will hold the string
    (not forgetting the NUL at the end)
  -}
  new_ps_array (length# +# 1#)  `thenStrictlyST` \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   `seqStrictlyST`
   -- freeze the puppy:
  freeze_ps_array ch_array)
  where
    fill_in :: _MutableByteArray s Int -> Int# -> _ST s ()

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

\begin{code}
new_ps_array	:: Int# -> _ST s (_MutableByteArray s Int)
write_ps_array	:: _MutableByteArray s Int -> Int# -> Char# -> _ST s () 
freeze_ps_array :: _MutableByteArray s Int -> _ST s (_ByteArray Int)

new_ps_array size =
    MkST ( \ (S# s) ->
    case (newCharArray# size s)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray (0, max 0 (I# (size -# 1#))) barr#, S# s2#)})

write_ps_array (_MutableByteArray _ barr#) n ch =
    MkST ( \ (S# s#) ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    ((), S# s2#)})

-- same as unsafeFreezeByteArray
freeze_ps_array (_MutableByteArray ixs arr#) =
    MkST ( \ (S# s#) ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) })
\end{code}

Compare two equal-length strings for equality:

\begin{code}
eqStrPrefix :: Addr# -> ByteArray# -> Int# -> Bool
eqStrPrefix a# barr# len# = 
  unsafePerformPrimIO (
   _ccall_ strncmp (A# a#) (_ByteArray bottom barr#) (I# len#) `thenPrimIO` \ (I# x#) ->
   returnPrimIO (x# ==# 0#))
  where
   bottom :: (Int,Int)
   bottom = error "eqStrPrefix"

eqCharStrPrefix :: Addr# -> Addr# -> Int# -> Bool
eqCharStrPrefix a1# a2# len# = 
  unsafePerformPrimIO (
   _ccall_ strncmp (A# a1#) (A# a2#) (I# len#) `thenPrimIO` \ (I# x#) ->
   returnPrimIO (x# ==# 0#))
  where
   bottom :: (Int,Int)
   bottom = error "eqStrPrefix"

eqStrPrefixBA :: ByteArray# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixBA b1# b2# start# len# = 
  unsafePerformPrimIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (_ByteArray bottom b2#) 
	  (I# start#) 
          (_ByteArray bottom b1#) 
          (I# len#)                  `thenPrimIO` \ (I# x#) ->
   returnPrimIO (x# ==# 0#))
  where
   bottom :: (Int,Int)
   bottom = error "eqStrPrefixBA"

eqCharStrPrefixBA :: Addr# -> ByteArray# -> Int# -> Int# -> Bool
eqCharStrPrefixBA a# b2# start# len# = 
  unsafePerformPrimIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (_ByteArray bottom b2#) 
	  (I# start#) 
          (A# a#)
          (I# len#)                  `thenPrimIO` \ (I# x#) ->
   returnPrimIO (x# ==# 0#))
  where
   bottom :: (Int,Int)
   bottom = error "eqCharStrPrefixBA"

eqStrPrefixFO :: ForeignObj# -> ByteArray# -> Int# -> Int# -> Bool
eqStrPrefixFO fo# barr# start# len# = 
  unsafePerformPrimIO (
   _casm_ ``%r=(int)strncmp((char *)%0+(int)%1,%2,%3); '' 
	  (_ForeignObj fo#) 
	  (I# start#) 
          (_ByteArray bottom barr#) 
          (I# len#)                  `thenPrimIO` \ (I# x#) ->
   returnPrimIO (x# ==# 0#))
  where
   bottom :: (Int,Int)
   bottom = error "eqStrPrefixFO"
\end{code}

\begin{code}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 205	
byteArrayToString :: _ByteArray Int -> String
byteArrayToString (_ByteArray (I# start#,I# end#) barr#) =
 unpack start#
 where
  unpack nh#
   | nh# >=# end# = []
   | otherwise    = C# ch : unpack (nh# +# 1#)
     where
      ch = indexCharArray# barr# nh#
#elif defined(__GLASGOW_HASKELL__)
byteArrayToString :: _ByteArray Int -> String
byteArrayToString = unpackCStringBA
#else
#error "byteArrayToString: cannot handle this!"
#endif

\end{code}


\begin{code}
stringToByteArray :: String -> (_ByteArray Int)
#if __GLASGOW_HASKELL__ >= 206
stringToByteArray = packString
#elif defined(__GLASGOW_HASKELL__)
stringToByteArray str = _runST (packStringST str)

packStringST :: [Char] -> _ST s (_ByteArray Int)
packStringST str =
  let len = length str  in
  packNCharsST len str

packNCharsST :: Int -> [Char] -> _ST s (_ByteArray Int)
packNCharsST len@(I# length#) str =
  {- 
   allocate an array that will hold the string
   (not forgetting the NUL byte at the end)
  -}
 new_ps_array (length# +# 1#) `thenStrictlyST` \ ch_array ->
   -- fill in packed string from "str"
 fill_in ch_array 0# str      `seqStrictlyST`
   -- freeze the puppy:
 freeze_ps_array ch_array     `thenStrictlyST` \ (_ByteArray _ frozen#) ->
 returnStrictlyST (_ByteArray (0,len) frozen#)
 where
  fill_in :: _MutableByteArray s Int -> Int# -> [Char] -> _ST s ()
  fill_in arr_in# idx [] =
   write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
   returnStrictlyST ()

  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 `seqStrictlyST`
   fill_in arr_in# (idx +# 1#) cs
#else
#error "stringToByteArray: cannot handle this"
#endif

\end{code}
