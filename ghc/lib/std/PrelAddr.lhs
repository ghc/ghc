%
% (c) The AQUA Project, Glasgow University, 1994-1998
%

\section[PrelAddr]{Module @PrelAddr@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelAddr (
	  Addr(..)
	, nullAddr			-- :: Addr
	, plusAddr			-- :: Addr -> Int -> Addr
	, indexAddrOffAddr	        -- :: Addr -> Int -> Addr

   ) where

import PrelGHC
import PrelBase
import PrelST
import PrelCCall
\end{code}

\begin{code}
data Addr = A# Addr# 	deriving (Eq, Ord)

instance Show Addr where
   showsPrec p (A# a) = showsPrec p (I# (addr2Int# a))

nullAddr = ``NULL'' :: Addr

plusAddr :: Addr -> Int -> Addr
plusAddr (A# addr) (I# off) = A# (int2Addr# (addr2Int# addr +# off))

instance CCallable Addr
instance CCallable Addr#
instance CReturnable Addr

indexAddrOffAddr   :: Addr -> Int -> Addr
indexAddrOffAddr (A# addr#) n
  = case n  	    	    	    	of { I# n# ->
    case indexAddrOffAddr# addr# n# 	of { r# ->
    (A# r#)}}

\end{code}

