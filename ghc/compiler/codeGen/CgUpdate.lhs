%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgUpdate]{Manipulating update frames}

\begin{code}
module CgUpdate ( pushUpdateFrame, reserveSeqFrame, pushSeqFrame ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn

import Constants	( uF_SIZE, sCC_UF_SIZE, sEQ_FRAME_SIZE )
import PrimRep		( PrimRep(..) )
import CgStackery	( allocUpdateFrame )
import CgUsages		( getSpRelOffset )
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
pushUpdateFrame :: CAddrMode -> Code -> Code

pushUpdateFrame updatee code
  = let
	-- frame_size *includes* the return address
	frame_size = if opt_SccProfilingOn
		     then sCC_UF_SIZE
		     else uF_SIZE
    in
    getEndOfBlockInfo 	    	    	`thenFC` \ eob_info ->
    ASSERT(case eob_info of { EndOfBlockInfo _ (OnStack _) -> True; 
			      _ -> False})
    allocUpdateFrame frame_size (

		-- Emit the push macro
	    absC (CMacroStmt PUSH_UPD_FRAME [
			updatee,
			int_CLit0 	-- Known to be zero because we have just
	    ])
	    `thenC` code
    )

int_CLit0 = mkIntCLit 0 -- out here to avoid pushUpdateFrame CAF (sigh)

\end{code}

We push a SEQ frame just before evaluating the scrutinee of a case, if
the scrutinee has a polymorphic or function type.  The SEQ frame acts
as a barrier in case the scrutinee evaluates to a partial application.

reserveSeqFrame takes the EndOfBlockInfo for the case expression and
updates the sequel to a SeqFrame, reserving room for the frame at
args_sp.  When the scrutinee comes around to pushing a return address,
it will also push the SEQ frame, using pushSeqFrame.

\begin{code}
reserveSeqFrame :: EndOfBlockInfo -> EndOfBlockInfo
reserveSeqFrame (EndOfBlockInfo args_sp (CaseAlts amode stuff)) 
  = EndOfBlockInfo (args_sp + sEQ_FRAME_SIZE) (SeqFrame amode stuff)

pushSeqFrame :: VirtualSpOffset -> FCode VirtualSpOffset
pushSeqFrame args_sp
  = getSpRelOffset args_sp  `thenFC` \ sp_rel ->
    absC (CMacroStmt PUSH_SEQ_FRAME [CAddr sp_rel]) `thenC`
    returnFC (args_sp - sEQ_FRAME_SIZE)
\end{code}
