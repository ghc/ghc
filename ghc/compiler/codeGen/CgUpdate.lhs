%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CgUpdate]{Manipulating update frames}

\begin{code}
#include "HsVersions.h"

module CgUpdate ( pushUpdateFrame ) where

import Ubiq{-uitous-}

import CgMonad
import AbsCSyn

import CgCompInfo	( sTD_UF_SIZE, sCC_STD_UF_SIZE )
import CgStackery	( allocUpdateFrame )
import CmdLineOpts	( opt_SccProfilingOn )
import Util		( assertPanic )
\end{code}


%********************************************************
%*							*
%*		Setting up update frames		*
%*							*
%********************************************************
\subsection[setting-update-frames]{Setting up update frames}

@pushUpdateFrame@ $updatee$ pushes a general update frame which
points to $updatee$ as the thing to be updated.  It is only used
when a thunk has just been entered, so the (real) stack pointers
are guaranteed to be nicely aligned with the top of stack.
@pushUpdateFrame@ adjusts the virtual and tail stack pointers
to reflect the frame pushed.

\begin{code}
pushUpdateFrame :: CAddrMode -> CAddrMode -> Code -> Code

pushUpdateFrame updatee vector code
  = let
	profiling_on = opt_SccProfilingOn

	-- frame_size *includes* the return address
	frame_size = if profiling_on
		     then sCC_STD_UF_SIZE
		     else sTD_UF_SIZE
    in
    getEndOfBlockInfo 	    	    	`thenFC` \ eob_info ->
    ASSERT(case eob_info of { EndOfBlockInfo _ _ InRetReg -> True; _ -> False})
    allocUpdateFrame frame_size vector (\ _ ->

		-- Emit the push macro
	    absC (CMacroStmt PUSH_STD_UPD_FRAME [
			updatee,
			int_CLit0, 	-- Known to be zero because we have just
			int_CLit0	-- entered a thunk
	    ])
	    `thenC` code
    )

int_CLit0 = mkIntCLit 0 -- out here to avoid pushUpdateFrame CAF (sigh)

{- ---------------------
    What actually happens is something like this; but it got macro-ised

  = pushOnBStack (CReg CurCostCentre)			`thenFC` \ _ ->
    pushOnBStack (CReg SuA)				`thenFC` \ _ ->
    pushOnBStack (CReg SuB)				`thenFC` \ _ ->
    pushOnBStack updatee				`thenFC` \ _ ->
    pushOnBStack (CLabel sTD_UPD_RET_VEC_LABEL CodePtrRep) `thenFC` \ _ ->

	-- MAKE SuA, SuB POINT TO TOP OF A,B STACKS
	-- Remember, SpB hasn't yet been incremented to account for the
	-- 4-word update frame which has been pushed.
	-- This code seems crude, but effective...
    absC (AbsCStmts (CAssign (CReg SuA) (CReg SpA))
		    (CAssign (CReg SuB) (CAddr (SpBRel 0 4))))
-------------------------- -}
\end{code}
