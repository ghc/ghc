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

import OccName	( Module, mkSrcModule )
import Util	( nOfThem )
import Panic	( panic )
\end{code}

\begin{code}
pREL_GHC, pRELUDE, mONAD, rATIO, iX, mAIN, pREL_MAIN, pREL_ERR      :: Module
pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP, pREL_ADDR, pREL_READ      :: Module	
pREL_PACK, pREL_CONC, pREL_IO_BASE, pREL_ST, pREL_ARR	  	    :: Module	
pREL_FOREIGN, pREL_STABLE					    :: Module

pRELUDE	     = mkSrcModule "Prelude"
pREL_GHC     = mkSrcModule "PrelGHC"	   -- Primitive types and values
pREL_BASE    = mkSrcModule "PrelBase"
pREL_READ    = mkSrcModule "PrelRead"
pREL_NUM     = mkSrcModule "PrelNum"
pREL_LIST    = mkSrcModule "PrelList"
pREL_TUP     = mkSrcModule "PrelTup"
pREL_PACK    = mkSrcModule "PrelPack"
pREL_CONC    = mkSrcModule "PrelConc"
pREL_IO_BASE = mkSrcModule "PrelIOBase"
pREL_ST	     = mkSrcModule "PrelST"
pREL_ARR     = mkSrcModule "PrelArr"
pREL_FOREIGN = mkSrcModule "PrelForeign"
pREL_STABLE  = mkSrcModule "PrelStable"
pREL_ADDR    = mkSrcModule "PrelAddr"
pREL_ERR     = mkSrcModule "PrelErr"

mONAD	     = mkSrcModule "Monad"
rATIO	     = mkSrcModule "Ratio"
iX	     = mkSrcModule "Ix"

pREL_MAIN    = mkSrcModule "PrelMain"
mAIN	     = mkSrcModule "Main"

iNT, wORD   :: Module

iNT	     = mkSrcModule "Int"
wORD	     = mkSrcModule "Word"

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


