%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
%********************************************************
%*							*
\section[CgLetNoEscape]{Handling ``let-no-escapes''}
%*							*
%********************************************************

\begin{code}
#include "HsVersions.h"

module CgLetNoEscape ( cgLetNoEscapeClosure ) where

import StgSyn
import CgMonad
import AbsCSyn

import CgBindery	-- various things
import CgExpr		( cgExpr )
import CgHeapery	( heapCheck )
import CgRetConv	( assignRegs )
import CgStackery	( mkVirtStkOffsets )
import CgUsages		( setRealAndVirtualSps, getVirtSps )
import CLabelInfo	( mkStdEntryLabel )
import ClosureInfo	( mkLFLetNoEscape )
import Id		( getIdKind )
import Util
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
entered, the stack is deeper than it was then the definition of @x@
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
	-> CostCentre 	   	-- NB: *** NOT USED *** ToDo (WDP 94/06)
	-> StgBinderInfo	-- NB: ditto
	-> PlainStgLiveVars	-- variables live in RHS, including the binders
				-- themselves in the case of a recursive group
        -> EndOfBlockInfo       -- where are we going to?
        -> Maybe VirtualSpBOffset -- Slot for current cost centre
	-> [Id]			-- args (as in \ args -> body)
    	-> PlainStgExpr		-- body (as in above)
	-> FCode (Id, CgIdInfo)

-- ToDo: deal with the cost-centre issues

cgLetNoEscapeClosure binder cc bi full_live_in_rhss rhs_eob_info maybe_cc_slot args body
  = let
	arity   = length args
	lf_info = mkLFLetNoEscape arity full_live_in_rhss{-used???-}
    in
    forkEvalHelp 
        rhs_eob_info
	(nukeDeadBindings full_live_in_rhss)
	(forkAbsC (cgLetNoEscapeBody args body)) 
	    	    	    	     	`thenFC` \ (vA, vB, code) ->
    let
	label = mkStdEntryLabel binder -- arity
    in
    absC (CCodeBlock label code) `thenC` 
    returnFC (binder, letNoEscapeIdInfo binder vA vB lf_info)
\end{code}

\begin{code}
cgLetNoEscapeBody :: [Id]		-- Args
		  -> PlainStgExpr	-- Body
		  -> Code

cgLetNoEscapeBody all_args rhs
  = getVirtSps		`thenFC` \ (vA, vB) ->
    getIntSwitchChkrC	`thenFC` \ isw_chkr ->
    let
	arg_kinds	= map getIdKind all_args
	(arg_regs, _)	= assignRegs isw_chkr [{-nothing live-}] arg_kinds
    	stk_args	= drop (length arg_regs) all_args

    	-- stk_args is the args which are passed on the stack at the fast-entry point
    	-- Using them, we define the stack layout
    	(spA_stk_args, spB_stk_args, stk_bxd_w_offsets, stk_ubxd_w_offsets)
	  = mkVirtStkOffsets 
		vA vB 		-- Initial virtual SpA, SpB
		getIdKind 
		stk_args
    in

	-- Bind args to appropriate regs/stk locns
    bindArgsToRegs all_args arg_regs		    `thenC`
    mapCs bindNewToAStack stk_bxd_w_offsets	    `thenC`
    mapCs bindNewToBStack stk_ubxd_w_offsets	    `thenC`
    setRealAndVirtualSps spA_stk_args spB_stk_args  `thenC`

{- 	ToDo: NOT SURE ABOUT COST CENTRES!
	-- Enter the closures cc, if required
	lexEnterCCcode closure_info maybe_cc  	    `thenC`
-}

	-- [No need for stack check; forkEvalHelp dealt with that]

	-- Do heap check [ToDo: omit for non-recursive case by recording in
	--	in envt and absorbing at call site]
    heapCheck arg_regs False {- Node doesn't point to it -}  (
	      -- heapCheck *encloses* the rest

	-- Compile the body
    cgExpr rhs
    )
\end{code}
