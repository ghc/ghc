%
% (c) The AQUA Project, Glasgow University, 1994-1998
%

\section[PrelAddr]{Module @PrelAddr@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelAddr (
	  Addr(..)
	, AddrOff(..)
	, nullAddr		-- :: Addr
	, alignAddr		-- :: Addr -> Int -> Addr
	, plusAddr		-- :: Addr -> AddrOff -> Addr
	, minusAddr		-- :: Addr -> Addr -> AddrOff

	, indexAddrOffAddr	-- :: Addr -> Int -> Addr

	, Word(..)
	, wordToInt
	, intToWord

	, Word64(..)
	, Int64(..)
   ) where

import PrelGHC
import PrelBase

infixl 5 `plusAddr`, `minusAddr`
\end{code}

\begin{code}
data Addr = A# Addr# 	deriving (Eq, Ord)
data Word = W# Word# 	deriving (Eq, Ord)

newtype AddrOff = AddrOff# Int

nullAddr :: Addr
nullAddr = A# (int2Addr# 0#)

alignAddr :: Addr -> Int -> Addr
alignAddr addr@(A# a) (I# i)
  = case addr2Int# a	of { ai ->
    case remInt# ai i	of {
      0# -> addr;
      n  -> A# (int2Addr# (ai +# (i -# n))) }}

plusAddr :: Addr -> AddrOff -> Addr
plusAddr (A# addr) (AddrOff# (I# off)) = A# (int2Addr# (addr2Int# addr +# off))

minusAddr :: Addr -> Addr -> AddrOff
minusAddr (A# a1) (A# a2) = AddrOff# (I# (addr2Int# a1 -# addr2Int# a2))

instance CCallable Addr
instance CReturnable Addr

instance CCallable Word
instance CReturnable Word

wordToInt :: Word -> Int
wordToInt (W# w#) = I# (word2Int# w#)

intToWord :: Int -> Word
intToWord (I# i#) = W# (int2Word# i#)

#if WORD_SIZE_IN_BYTES == 8
data Word64 = W64# Word#
data Int64  = I64# Int#
#else
data Word64 = W64# Word64# --deriving (Eq, Ord) -- Glasgow extension
data Int64  = I64# Int64#  --deriving (Eq, Ord) -- Glasgow extension
#endif

instance CCallable   Word64
instance CReturnable Word64

instance CCallable   Int64
instance CReturnable Int64

indexAddrOffAddr   :: Addr -> Int -> Addr
indexAddrOffAddr (A# addr#) n
  = case n  	    	    	    	of { I# n# ->
    case indexAddrOffAddr# addr# n# 	of { r# ->
    (A# r#)}}

\end{code}

