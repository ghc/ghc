%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[PrelArrExtra]{Module @PrelArrExtra@}

The following functions should be in PrelArr, but need -monly-2-regs
to compile.  So as not to compile the whole of PrelArr with
-monly-2-regs, the culprits have been moved out into a separate
module.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelArrExtra where

import Ix
import PrelArr
import PrelByteArr
import PrelST
import PrelIOBase
import PrelBase
import PrelGHC
\end{code}

%*********************************************************
%*							*
\subsection{Moving between mutable and immutable}
%*							*
%*********************************************************

\begin{code}
freezeByteArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALISE freezeByteArray :: MutableByteArray s Int -> ST s (ByteArray Int) #-}

-- This coercion of memcpy to the ST monad is safe, because memcpy
-- only modifies its destination operand, which is already MutableByteArray.
freezeByteArray (MutableByteArray l u arr) = ST $ \ s ->
	let n = sizeofMutableByteArray# arr in
	case (newCharArray# n s)                   of { (# s, newarr #) -> 
	case ((unsafeCoerce# memcpy) newarr arr n s) of { (# s, () #) ->
	case unsafeFreezeByteArray# newarr s       of { (# s, frozen #) ->
	(# s, ByteArray l u frozen #) }}}

foreign import "memcpy" unsafe 
  memcpy :: MutableByteArray# RealWorld -> ByteArray# -> Int# -> IO ()

unsafeFreezeByteArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALIZE unsafeFreezeByteArray :: MutableByteArray s Int -> ST s (ByteArray Int)
  #-}

unsafeFreezeByteArray (MutableByteArray l u arr#) = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray l u frozen# #) }
\end{code}
