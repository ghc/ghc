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

	pREL_GHC, pREL_BASE, pREL_ADDR, pREL_STABLE,
	pREL_IO_BASE, pREL_PACK, pREL_ERR,

	pREL_GHC_Name, pRELUDE_Name, mONAD_Name, rATIO_Name, 
	iX_Name, mAIN_Name, pREL_MAIN_Name, pREL_ERR_Name,
	pREL_BASE_Name, pREL_NUM_Name, pREL_LIST_Name, 
	pREL_TUP_Name, pREL_ADDR_Name, pREL_READ_Name,
	pREL_PACK_Name, pREL_CONC_Name, pREL_IO_BASE_Name, 
	pREL_ST_Name, pREL_ARR_Name, pREL_FOREIGN_Name,
	pREL_STABLE_Name, pREL_SHOW_Name, pREL_ENUM_Name, iNT_Name, wORD_Name
	) where

#include "HsVersions.h"

import Module	( Module, ModuleName, mkPrelModule, mkSrcModule )
import Util	( nOfThem )
import Panic	( panic )
\end{code}

\begin{code}
pRELUDE_Name      = mkSrcModule  "Prelude"
pREL_GHC_Name     = mkSrcModule "PrelGHC"	   -- Primitive types and values
pREL_BASE_Name    = mkSrcModule "PrelBase"
pREL_ENUM_Name    = mkSrcModule "PrelEnum"
pREL_SHOW_Name    = mkSrcModule "PrelShow"
pREL_READ_Name    = mkSrcModule "PrelRead"
pREL_NUM_Name     = mkSrcModule "PrelNum"
pREL_LIST_Name    = mkSrcModule "PrelList"
pREL_TUP_Name     = mkSrcModule "PrelTup"
pREL_PACK_Name    = mkSrcModule "PrelPack"
pREL_CONC_Name    = mkSrcModule "PrelConc"
pREL_IO_BASE_Name = mkSrcModule "PrelIOBase"
pREL_ST_Name	  = mkSrcModule "PrelST"
pREL_ARR_Name     = mkSrcModule "PrelArr"
pREL_FOREIGN_Name = mkSrcModule "PrelForeign"
pREL_STABLE_Name  = mkSrcModule "PrelStable"
pREL_ADDR_Name    = mkSrcModule "PrelAddr"
pREL_ERR_Name     = mkSrcModule "PrelErr"

mONAD_Name	 = mkSrcModule "Monad"
rATIO_Name	 = mkSrcModule "Ratio"
iX_Name	    	 = mkSrcModule "Ix"
pREL_MAIN_Name   = mkSrcModule "PrelMain"
mAIN_Name	 = mkSrcModule "Main"
iNT_Name	 = mkSrcModule "Int"
wORD_Name	 = mkSrcModule "Word"

pREL_GHC     = mkPrelModule pREL_GHC_Name
pREL_BASE    = mkPrelModule pREL_BASE_Name
pREL_ADDR    = mkPrelModule pREL_ADDR_Name
pREL_STABLE  = mkPrelModule pREL_STABLE_Name
pREL_IO_BASE = mkPrelModule pREL_IO_BASE_Name
pREL_PACK    = mkPrelModule pREL_PACK_Name
pREL_ERR     = mkPrelModule pREL_ERR_Name
\end{code}

%************************************************************************
%*									*
\subsection{Constructing the names of tuples
%*									*
%************************************************************************

\begin{code}
mkTupNameStr, mkUbxTupNameStr :: Int -> (ModuleName, FAST_STRING)

mkTupNameStr 0 = (pREL_BASE_Name, SLIT("()"))
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = (pREL_TUP_Name, _PK_ "(,)")   -- not strictly necessary
mkTupNameStr 3 = (pREL_TUP_Name, _PK_ "(,,)")  -- ditto
mkTupNameStr 4 = (pREL_TUP_Name, _PK_ "(,,,)") -- ditto
mkTupNameStr n = (pREL_TUP_Name, _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkUbxTupNameStr 0 = panic "Name.mkUbxTupNameStr: 0 ???"
mkUbxTupNameStr 1 = (pREL_GHC_Name, _PK_ "(# #)") -- 1 and 0 both make sense!!!
mkUbxTupNameStr 2 = (pREL_GHC_Name, _PK_ "(#,#)")
mkUbxTupNameStr 3 = (pREL_GHC_Name, _PK_ "(#,,#)")
mkUbxTupNameStr 4 = (pREL_GHC_Name, _PK_ "(#,,,#)")
mkUbxTupNameStr n = (pREL_GHC_Name, _PK_ ("(#" ++ nOfThem (n-1) ',' ++ "#)"))
\end{code}


