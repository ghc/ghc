%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgUpdate]{Manipulating update frames}

\begin{code}
module CgUpdate ( pushUpdateFrame ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn

import CgStackery	( allocStackTop, updateFrameSize, setStackFrame )
import CgUsages		( getVirtSp )
import Panic		( assertPanic )
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
  = 
#ifdef DEBUG
    getEndOfBlockInfo 	    	    	`thenFC` \ eob_info ->
    ASSERT(case eob_info of { EndOfBlockInfo _ (OnStack _) -> True; 
			      _ -> False})
#endif

    allocStackTop updateFrameSize	`thenFC` \ _ ->
    getVirtSp				`thenFC` \ vsp ->

    setStackFrame vsp			`thenC`

    setEndOfBlockInfo (EndOfBlockInfo vsp UpdateCode) (

		-- Emit the push macro
	    absC (CMacroStmt PUSH_UPD_FRAME [
			updatee,
			int_CLit0  -- we just entered a closure, so must be zero
	    ])
	    `thenC` code
    )

int_CLit0 = mkIntCLit 0 -- out here to avoid pushUpdateFrame CAF (sigh)
\end{code}
