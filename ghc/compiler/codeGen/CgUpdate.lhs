%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[CgUpdate]{Manipulating update frames}

\begin{code}
#include "HsVersions.h"

module CgUpdate (
	pushUpdateFrame -- OLD: , evalPushRCCFrame
    ) where

import StgSyn
import CgMonad
import AbsCSyn

import CgCompInfo	( sTD_UF_SIZE, cON_UF_SIZE,
			  sCC_STD_UF_SIZE, sCC_CON_UF_SIZE,
			  spARelToInt, spBRelToInt
			)
import CgStackery	( allocUpdateFrame )
import CgUsages
import CmdLineOpts	( GlobalSwitch(..) )
import Util
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
  = isSwitchSetC SccProfilingOn		`thenFC` \ profiling_on ->
    let
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
    pushOnBStack (CLabel sTD_UPD_RET_VEC_LABEL CodePtrKind) `thenFC` \ _ ->

	-- MAKE SuA, SuB POINT TO TOP OF A,B STACKS
	-- Remember, SpB hasn't yet been incremented to account for the
	-- 4-word update frame which has been pushed.
	-- This code seems crude, but effective...
    absC (AbsCStmts (CAssign (CReg SuA) (CReg SpA))
		    (CAssign (CReg SuB) (CAddr (SpBRel 0 4))))
-------------------------- -}
\end{code}

@evalPushRCCFrame@ pushes a frame to restore the cost centre, and
deallocates stuff from the A and B stack if evaluation profiling. No
actual update is required so no closure to update is passed.
@evalPushRCCFrame@ is called for an @scc@ expression and on entry to a
single-entry thunk: no update reqd but cost centre manipulation is.

\begin{code}
{- OLD: WDP: 94/06

evalPushRCCFrame :: Bool -> Code -> Code

evalPushRCCFrame prim code
  = isSwitchSetC SccProfiling_Eval	`thenFC` \ eval_profiling ->

    if (not eval_profiling) then 
	code
    else

    	-- Find out how many words of stack must will be
	--   deallocated at the end of the basic block
	-- As we push stuff onto the B stack we must make the
	-- RCC frame dealocate the B stack words

    	-- We dont actually push things onto the A stack so we
	--   can treat the A stack as if these words were not there
	--   i.e. we subtract them from the A stack offset
	-- They will be popped by the current block of code

 	-- Tell downstream code about the update frame on the B stack
    allocUpdateFrame 
		sCC_RCC_UF_SIZE 
		(panic "pushEvalRCCFrame: mkRestoreCostCentreLbl")
		(\ (old_args_spa, old_args_spb, upd_frame_offset) ->

    getSpARelOffset old_args_spa	`thenFC` \ old_args_spa_rel ->
    getSpBRelOffset upd_frame_offset	`thenFC` \ upd_frame_rel ->

    let b_wds_to_pop = upd_frame_offset - old_args_spb
    in

	-- Allocate enough space on the B stack for the frame

    evalCostCentreC
	     (if prim then 
		     "PUSH_RCC_FRAME_RETURN"
		 else
		     "PUSH_RCC_FRAME_VECTOR")
	     [
		mkIntCLit (spARelToInt old_args_spa_rel),
			{- Place on A stack to ``draw the line'' -}
		mkIntCLit (spBRelToInt upd_frame_rel),
			{- Ditto B stk.  The update frame is pushed starting 
			   just above here -}
		mkIntCLit 0,
			{- Number of words of A below the line, which must be
			   popped to get to the tail-call position -}
		mkIntCLit b_wds_to_pop
			{- Ditto B stk -}
	     ]				`thenC`

    code


	-- If we actually pushed things onto the A stack we have
	--   to arrange for the RCC frame to pop these as well
  	-- Would need to tell downstream code about the update frame
	--   both the A and B stacks
    )
-}
\end{code}
