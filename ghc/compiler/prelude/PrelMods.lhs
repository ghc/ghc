%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelMods]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod 

[oh dear, looks like the recursive module monster caught up with
 and gobbled whoever was writing the above :-) -- SOF ]

\begin{code}
module PrelMods
        (
        mkTupNameStr, mkUbxTupNameStr,

	pREL_GHC, pRELUDE, mONAD, rATIO, iX, mAIN, pREL_MAIN, pREL_ERR,
	pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP, pREL_ADDR, pREL_READ,
	pREL_PACK, pREL_CONC, pREL_IO_BASE, pREL_ST, pREL_ARR, pREL_FOREIGN,
	pREL_STABLE,

	iNT, wORD
	) where

#include "HsVersions.h"

import OccName	( Module, mkModule )
import Util	( nOfThem )
import Panic	( panic )
\end{code}

\begin{code}
pREL_GHC, pRELUDE, mONAD, rATIO, iX, mAIN, pREL_MAIN, pREL_ERR      :: Module
pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP, pREL_ADDR, pREL_READ      :: Module	
pREL_PACK, pREL_CONC, pREL_IO_BASE, pREL_ST, pREL_ARR	  	    :: Module	
pREL_FOREIGN, pREL_STABLE					    :: Module


pRELUDE	     = mkModule "Prelude"
pREL_GHC     = mkModule "PrelGHC"	   -- Primitive types and values
pREL_BASE    = mkModule "PrelBase"
pREL_READ    = mkModule "PrelRead"
pREL_NUM     = mkModule "PrelNum"
pREL_LIST    = mkModule "PrelList"
pREL_TUP     = mkModule "PrelTup"
pREL_PACK    = mkModule "PrelPack"
pREL_CONC    = mkModule "PrelConc"
pREL_IO_BASE = mkModule "PrelIOBase"
pREL_ST	     = mkModule "PrelST"
pREL_ARR     = mkModule "PrelArr"
pREL_FOREIGN = mkModule "PrelForeign"
pREL_STABLE  = mkModule "PrelStable"
pREL_ADDR    = mkModule "PrelAddr"
pREL_ERR     = mkModule "PrelErr"

mONAD	     = mkModule "Monad"
rATIO	     = mkModule "Ratio"
iX	     = mkModule "Ix"

pREL_MAIN    = mkModule "PrelMain"
mAIN	     = mkModule "Main"

iNT, wORD   :: Module

iNT	     = mkModule "Int"
wORD	     = mkModule "Word"

\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr, mkUbxTupNameStr :: Int -> (Module, FAST_STRING)

mkTupNameStr 0 = (pREL_BASE, SLIT("()"))
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = (pREL_TUP, _PK_ "(,)")   -- not strictly necessary
mkTupNameStr 3 = (pREL_TUP, _PK_ "(,,)")  -- ditto
mkTupNameStr 4 = (pREL_TUP, _PK_ "(,,,)") -- ditto
mkTupNameStr n = (pREL_TUP, _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkUbxTupNameStr 0 = panic "Name.mkUbxTupNameStr: 0 ???"
mkUbxTupNameStr 1 = (pREL_GHC, _PK_ "(# #)") -- 1 and 0 both make sense!!!
mkUbxTupNameStr 2 = (pREL_GHC, _PK_ "(#,#)")
mkUbxTupNameStr 3 = (pREL_GHC, _PK_ "(#,,#)")
mkUbxTupNameStr 4 = (pREL_GHC, _PK_ "(#,,,#)")
mkUbxTupNameStr n = (pREL_GHC, _PK_ ("(#" ++ nOfThem (n-1) ',' ++ "#)"))
\end{code}


