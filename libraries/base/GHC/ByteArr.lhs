% -----------------------------------------------------------------------------
% $Id: ByteArr.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[GHC.ByteArr]{Module @GHC.ByteArr@}

Byte-arrays are flat arrays of non-pointers only.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.ByteArr where

import {-# SOURCE #-} GHC.Err ( error )
import GHC.Num
import GHC.Arr
import GHC.Float
import GHC.ST
import GHC.Base
\end{code}

%*********************************************************
%*							*
\subsection{The @Array@ types}
%*							*
%*********************************************************

\begin{code}
data Ix ix => ByteArray ix      	= ByteArray	   ix ix ByteArray#
data Ix ix => MutableByteArray s ix     = MutableByteArray ix ix (MutableByteArray# s)

instance CCallable (ByteArray ix)
instance CCallable (MutableByteArray RealWorld ix)
	-- Note the RealWorld!  You can only ccall with MutableByteArray args
	-- which are in the real world.  When this was missed out, the result
	-- was that a CCallOpId had a free tyvar, and since the compiler doesn't
	-- expect that it didn't get zonked or substituted.  Bad news.

instance Eq (MutableByteArray s ix) where
	MutableByteArray _ _ arr1# == MutableByteArray _ _ arr2#
		= sameMutableByteArray# arr1# arr2#
\end{code}

%*********************************************************
%*							*
\subsection{Operations on mutable arrays}
%*							*
%*********************************************************

\begin{code}
newCharArray, newIntArray, newFloatArray, newDoubleArray
	 :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

{-# SPECIALIZE newCharArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (MutableByteArray s Int) #-}

newCharArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newByteArray# (cHAR_SCALE n#) s#) of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newIntArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newByteArray# (wORD_SCALE n#) s#) of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newWordArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newByteArray# (wORD_SCALE n#) s#) of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newFloatArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newByteArray# (fLOAT_SCALE n#) s#) of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

newDoubleArray (l,u) = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newByteArray# (dOUBLE_SCALE n#) s#) of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray l u barr# #) }}

#include "config.h"

  -- Char arrays really contain only 8-bit bytes for compatibility.
cHAR_SCALE   n = 1# *# n
wORD_SCALE   n = (case SIZEOF_VOID_P :: Int of I# x -> x *# n)
dOUBLE_SCALE n = (case SIZEOF_DOUBLE :: Int of I# x -> x *# n)
fLOAT_SCALE  n = (case SIZEOF_FLOAT  :: Int of I# x -> x *# n)

readCharArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
readIntArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
readFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
readDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

{-# SPECIALIZE readCharArray   :: MutableByteArray s Int -> Int -> ST s Char #-}
{-# SPECIALIZE readIntArray    :: MutableByteArray s Int -> Int -> ST s Int #-}
--NO:{-# SPECIALIZE readFloatArray  :: MutableByteArray s Int -> Int -> ST s Float #-}
{-# SPECIALIZE readDoubleArray :: MutableByteArray s Int -> Int -> ST s Double #-}

readCharArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, C# r# #) }}

readIntArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, I# r# #) }}

readFloatArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n)	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, F# r# #) }}

readDoubleArray (MutableByteArray l u barr#) n = ST $ \ s# ->
    case (index (l,u) n) 	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, D# r# #) }}

--Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
indexCharArray   :: Ix ix => ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => ByteArray ix -> ix -> Int
indexFloatArray  :: Ix ix => ByteArray ix -> ix -> Float
indexDoubleArray :: Ix ix => ByteArray ix -> ix -> Double

{-# SPECIALIZE indexCharArray   :: ByteArray Int -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: ByteArray Int -> Int -> Int #-}
--NO:{-# SPECIALIZE indexFloatArray  :: ByteArray Int -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: ByteArray Int -> Int -> Double #-}

indexCharArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexFloatArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (ByteArray l u barr#) n
  = case (index (l,u) n) 	    	of { I# n# ->
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}

writeCharArray   :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
writeFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
writeDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

{-# SPECIALIZE writeCharArray   :: MutableByteArray s Int -> Int -> Char -> ST s () #-}
{-# SPECIALIZE writeIntArray    :: MutableByteArray s Int -> Int -> Int  -> ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: MutableByteArray s Int -> Int -> Float -> ST s () #-}
{-# SPECIALIZE writeDoubleArray :: MutableByteArray s Int -> Int -> Double -> ST s () #-}

writeCharArray (MutableByteArray l u barr#) n (C# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeIntArray (MutableByteArray l u barr#) n (I# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    (# s2#, () #) }}

writeFloatArray (MutableByteArray l u barr#) n (F# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    (# s2#, () #) }}

writeDoubleArray (MutableByteArray l u barr#) n (D# ele) = ST $ \ s# ->
    case index (l,u) n	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    (# s2#, () #) }}
\end{code}
