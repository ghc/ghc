%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
% $Id: CgLetNoEscape.lhs,v 1.26 2004/09/30 10:35:47 simonpj Exp $
%
%********************************************************
%*							*
\section[CgLetNoEscape]{Handling ``let-no-escapes''}
%*							*
%********************************************************

\begin{code}
module CgLetNoEscape ( cgLetNoEscapeClosure ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr ( cgExpr )

import StgSyn
import CgMonad

import CgBindery	( CgIdInfo, letNoEscapeIdInfo, nukeDeadBindings	)
import CgCase		( restoreCurrentCostCentre )
import CgCon		( bindUnboxedTupleComponents )
import CgHeapery	( unbxTupleHeapCheck )
import CgInfoTbls	( emitDirectReturnTarget )
import CgStackery	( allocStackTop, deAllocStackTop, getSpRelOffset )
import Cmm		( CmmStmt(..) )
import CmmUtils		( mkLblExpr, oneStmt )
import CLabel		( mkReturnInfoLabel )
import ClosureInfo	( mkLFLetNoEscape )
import CostCentre       ( CostCentreStack )
import Id		( Id, idName )
import Var		( idUnique )
import SMRep		( retAddrSizeW )
import BasicTypes	( RecFlag(..) )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[what-is-non-escaping]{What {\em is} a ``non-escaping let''?}
%*									*
%************************************************************************

[The {\em code} that detects these things is elsewhere.]

Consider:
\begin{verbatim}
	let x = fvs \ args -> e
	in
	 	if ... then x else
		if ... then x else ...
\end{verbatim}
@x@ is used twice (so we probably can't unfold it), but when it is
entered, the stack is deeper than it was when the definition of @x@
happened.  Specifically, if instead of allocating a closure for @x@,
we saved all @x@'s fvs on the stack, and remembered the stack depth at
that moment, then whenever we enter @x@ we can simply set the stack
pointer(s) to these remembered (compile-time-fixed) values, and jump
to the code for @x@.

All of this is provided x is:
\begin{enumerate}
\item
non-updatable;
\item
guaranteed to be entered before the stack retreats -- ie x is not
buried in a heap-allocated closure, or passed as an argument to something;
\item
all the enters have exactly the right number of arguments,
no more no less;
\item
all the enters are tail calls; that is, they return to the
caller enclosing the definition of @x@.
\end{enumerate}

Under these circumstances we say that @x@ is {\em non-escaping}.

An example of when (4) does {\em not} hold:
\begin{verbatim}
	let x = ...
	in case x of ...alts...
\end{verbatim}

Here, @x@ is certainly entered only when the stack is deeper than when
@x@ is defined, but here it must return to \tr{...alts...} So we can't
just adjust the stack down to @x@'s recalled points, because that
would lost @alts@' context.

Things can get a little more complicated.  Consider:
\begin{verbatim}
	let y = ...
	in let x = fvs \ args -> ...y...
	in ...x...
\end{verbatim}

Now, if @x@ is used in a non-escaping way in \tr{...x...}, {\em and}
@y@ is used in a non-escaping way in \tr{...y...}, {\em then} @y@ is
non-escaping.

@x@ can even be recursive!  Eg:
\begin{verbatim}
	letrec x = [y] \ [v] -> if v then x True else ...
	in
		...(x b)...
\end{verbatim}


%************************************************************************
%*									*
\subsection[codeGen-for-non-escaping]{Generating code for a ``non-escaping let''}
%*									*
%************************************************************************


Generating code for this is fun.  It is all very very similar to what
we do for a case expression.  The duality is between
\begin{verbatim}
	let-no-escape x = b
	in e
\end{verbatim}
and
\begin{verbatim}
	case e of ... -> b
\end{verbatim}

That is, the RHS of @x@ (ie @b@) will execute {\em later}, just like
the alternative of the case; it needs to be compiled in an environment
in which all volatile bindings are forgotten, and the free vars are
bound only to stable things like stack locations..  The @e@ part will
execute {\em next}, just like the scrutinee of a case.

First, we need to save all @x@'s free vars
on the stack, if they aren't there already.

\begin{code}
cgLetNoEscapeClosure
	:: Id			-- binder
	-> CostCentreStack   	-- NB: *** NOT USED *** ToDo (WDP 94/06)
	-> StgBinderInfo	-- NB: ditto
	-> SRT
	-> StgLiveVars		-- variables live in RHS, including the binders
				-- themselves in the case of a recursive group
	-> EndOfBlockInfo       -- where are we going to?
	-> Maybe VirtualSpOffset -- Slot for current cost centre
	-> RecFlag		-- is the binding recursive?
	-> [Id]			-- args (as in \ args -> body)
    	-> StgExpr		-- body (as in above)
	-> FCode (Id, CgIdInfo)

-- ToDo: deal with the cost-centre issues

cgLetNoEscapeClosure 
	bndr cc binder_info srt full_live_in_rhss 
	rhs_eob_info cc_slot rec args body
  = let
	arity   = length args
	lf_info = mkLFLetNoEscape arity
    in
    -- saveVolatileVarsAndRegs done earlier in cgExpr.

    do  { (vSp, _) <- forkEvalHelp rhs_eob_info

		(do { allocStackTop retAddrSizeW
		    ; nukeDeadBindings full_live_in_rhss })

		(do { deAllocStackTop retAddrSizeW
		    ; abs_c <- forkProc $ cgLetNoEscapeBody bndr cc 
						  cc_slot args body

			-- Ignore the label that comes back from
			-- mkRetDirectTarget.  It must be conjured up elswhere
		    ; emitDirectReturnTarget (idName bndr) abs_c srt
		    ; return () })

	; returnFC (bndr, letNoEscapeIdInfo bndr vSp lf_info) }
\end{code}

\begin{code}
cgLetNoEscapeBody :: Id		-- Name of the joint point
		  -> CostCentreStack
		  -> Maybe VirtualSpOffset
		  -> [Id]	-- Args
		  -> StgExpr	-- Body
		  -> Code

cgLetNoEscapeBody bndr cc cc_slot all_args body = do
  { (arg_regs, ptrs, nptrs, ret_slot) <- bindUnboxedTupleComponents all_args

     -- restore the saved cost centre.  BUT: we must not free the stack slot
     -- containing the cost centre, because it might be needed for a
     -- recursive call to this let-no-escape.
  ; restoreCurrentCostCentre cc_slot False{-don't free-}

	-- Enter the closures cc, if required
  ; -- enterCostCentreCode closure_info cc IsFunction

 	-- The "return address" slot doesn't have a return address in it;
	-- but the heap-check needs it filled in if the heap-check fails.
	-- So we pass code to fill it in to the heap-check macro
  ; sp_rel <- getSpRelOffset ret_slot

  ; let	lbl 	       = mkReturnInfoLabel (idUnique bndr)
	frame_hdr_asst = oneStmt (CmmStore sp_rel (mkLblExpr lbl))

	-- Do heap check [ToDo: omit for non-recursive case by recording in
	--	in envt and absorbing at call site]
  ; unbxTupleHeapCheck arg_regs ptrs nptrs frame_hdr_asst 
			(cgExpr body)
  }
\end{code}
