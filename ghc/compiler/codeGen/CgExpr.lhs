%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%********************************************************
%*							*
\section[CgExpr]{Converting @StgExpr@s}
%*							*
%********************************************************

\begin{code}
#include "HsVersions.h"

module CgExpr ( cgExpr, cgSccExpr, getPrimOpArgAmodes ) where

import Ubiq{-uitous-}
import CgLoop2	-- here for paranoia-checking

import StgSyn
import CgMonad
import AbsCSyn

import AbsCUtils	( mkAbsCStmts, mkAbstractCs )
import CgBindery	( getArgAmodes )
import CgCase		( cgCase, saveVolatileVarsAndRegs )
import CgClosure	( cgRhsClosure )
import CgCon		( buildDynCon, cgReturnDataCon )
import CgHeapery	( allocHeap )
import CgLetNoEscape	( cgLetNoEscapeClosure )
import CgRetConv	( dataReturnConvAlg, ctrlReturnConvAlg,
			  DataReturnConvention(..), CtrlReturnConvention(..),
			  assignPrimOpResultRegs, makePrimOpArgsRobust
			)
import CgTailCall	( cgTailCall, performReturn,
			  mkDynamicAlgReturnCode, mkPrimReturnCode
			)
import CLabel		( mkPhantomInfoTableLabel, mkInfoTableVecTblLabel )
import ClosureInfo	( mkClosureLFInfo )
import CostCentre	( setToAbleCostCentre, isDupdCC )
import HeapOffs		( VirtualSpBOffset(..) )
import Id		( mkIdSet, unionIdSets, GenId{-instance Outputable-} )
import PprStyle		( PprStyle(..) )
import PrimOp		( primOpCanTriggerGC, primOpHeapReq, HeapRequirement(..),
			  getPrimOpResultInfo, PrimOp(..), PrimOpResultInfo(..)
			)
import PrimRep		( getPrimRepSize, PrimRep(..) )
import TyCon		( tyConDataCons )
import Util		( panic, pprPanic, assertPanic )
\end{code}

This module provides the support code for @StgToAbstractC@ to deal
with STG {\em expressions}.  See also @CgClosure@, which deals
with closures, and @CgCon@, which deals with constructors.

\begin{code}
cgExpr	:: StgExpr		-- input
	-> Code			-- output
\end{code}

%********************************************************
%*							*
%*		Tail calls				*
%*							*
%********************************************************

``Applications'' mean {\em tail calls}, a service provided by module
@CgTailCall@.  This includes literals, which show up as
@(STGApp (StgLitArg 42) [])@.

\begin{code}
cgExpr (StgApp fun args live_vars) = cgTailCall fun args live_vars
\end{code}

%********************************************************
%*							*
%*		STG ConApps  (for inline versions)	*
%*							*
%********************************************************

\begin{code}
cgExpr (StgCon con args live_vars)
  = getArgAmodes args `thenFC` \ amodes ->
    cgReturnDataCon con amodes (all zero_size args) live_vars
  where
    zero_size atom = getPrimRepSize (getArgPrimRep atom) == 0
\end{code}

%********************************************************
%*							*
%*		STG PrimApps  (unboxed primitive ops)	*
%*							*
%********************************************************

Here is where we insert real live machine instructions.

\begin{code}
cgExpr x@(StgPrim op args live_vars)
  = ASSERT(op /= SeqOp) -- can't handle SeqOp
    getPrimOpArgAmodes op args	`thenFC` \ arg_amodes ->
    let
	result_regs   = assignPrimOpResultRegs op
	result_amodes = map CReg result_regs
	may_gc  = primOpCanTriggerGC op
	dyn_tag = head result_amodes
	    -- The tag from a primitive op returning an algebraic data type
	    -- is returned in the first result_reg_amode
    in
    (if may_gc then
	-- Use registers for args, and assign args to the regs
	-- (Can-trigger-gc primops guarantee to have their args in regs)
	let
	    (arg_robust_amodes, liveness_mask, arg_assts)
	      = makePrimOpArgsRobust op arg_amodes

	    liveness_arg = mkIntCLit liveness_mask
	in
 	returnFC (
	    arg_assts,
	    COpStmt result_amodes op
		    (pin_liveness op liveness_arg arg_robust_amodes)
		    liveness_mask
		    [{-no vol_regs-}]
	)
     else
	-- Use args from their current amodes.
	let
	  liveness_mask = panic "cgExpr: liveness of non-GC-ing primop touched\n"
	in
 	returnFC (
	    COpStmt result_amodes op arg_amodes liveness_mask [{-no vol_regs-}],
	    AbsCNop
	)
    )				`thenFC` \ (do_before_stack_cleanup,
					     do_just_before_jump) ->

    case (getPrimOpResultInfo op) of

	ReturnsPrim kind ->
	    performReturn do_before_stack_cleanup
    	    	    	  (\ sequel -> robustifySequel may_gc sequel
							`thenFC` \ (ret_asst, sequel') ->
			   absC (ret_asst `mkAbsCStmts` do_just_before_jump)
							`thenC`
			   mkPrimReturnCode sequel')
			  live_vars

	ReturnsAlg tycon ->
	    profCtrC SLIT("RET_NEW_IN_REGS") [num_of_fields]	`thenC`

	    performReturn do_before_stack_cleanup
			  (\ sequel -> robustifySequel may_gc sequel
						    	`thenFC` \ (ret_asst, sequel') ->
			   absC (mkAbstractCs [ret_asst,
					       do_just_before_jump,
					       info_ptr_assign])
			-- Must load info ptr here, not in do_just_before_stack_cleanup,
			-- because the info-ptr reg clashes with argument registers
			-- for the primop
								`thenC`
				      mkDynamicAlgReturnCode tycon dyn_tag sequel')
			  live_vars
	    where

	    -- Here, the destination _can_ be an update frame, so we need to make sure that
	    -- infoptr (R2) is loaded with the constructor's info ptr.

		info_ptr_assign = CAssign (CReg infoptr) info_lbl

		info_lbl
		  = case (ctrlReturnConvAlg tycon) of
		      VectoredReturn   _ -> vec_lbl
		      UnvectoredReturn _ -> dir_lbl

		vec_lbl  = CTableEntry (CLbl (mkInfoTableVecTblLabel tycon) DataPtrRep)
    	    	    	        dyn_tag DataPtrRep

		data_con = head (tyConDataCons tycon)

		(dir_lbl, num_of_fields)
		  = case (dataReturnConvAlg data_con) of
		      ReturnInRegs rs
			-> (CLbl (mkPhantomInfoTableLabel data_con) DataPtrRep,
			    mkIntCLit (length rs)) -- for ticky-ticky only

    	    	      ReturnInHeap
			-> pprPanic "CgExpr: can't return prim in heap:" (ppr PprDebug data_con)
			  -- Never used, and no point in generating
			  -- the code for it!
  where
    -- for all PrimOps except ccalls, we pin the liveness info
    -- on as the first "argument"
    -- ToDo: un-duplicate?

    pin_liveness (CCallOp _ _ _ _ _) _ args = args
    pin_liveness other_op liveness_arg args
      = liveness_arg :args

    -- We only need to worry about the sequel when we may GC and the
    -- sequel is OnStack.  If that's the case, arrange to pull the
    -- sequel out into RetReg before performing the primOp.

    robustifySequel True sequel@(OnStack _) =
	sequelToAmode sequel			`thenFC` \ amode ->
	returnFC (CAssign (CReg RetReg) amode, InRetReg)
    robustifySequel _ sequel = returnFC (AbsCNop, sequel)
\end{code}

%********************************************************
%*							*
%*		Case expressions			*
%*							*
%********************************************************
Case-expression conversion is complicated enough to have its own
module, @CgCase@.
\begin{code}

cgExpr (StgCase expr live_vars save_vars uniq alts)
  = cgCase expr live_vars save_vars uniq alts
\end{code}


%********************************************************
%*							*
%* 		Let and letrec				*
%*							*
%********************************************************
\subsection[let-and-letrec-codegen]{Converting @StgLet@ and @StgLetrec@}

\begin{code}
cgExpr (StgLet (StgNonRec name rhs) expr)
  = cgRhs name rhs	`thenFC` \ (name, info) ->
    addBindC name info 	`thenC`
    cgExpr expr

cgExpr (StgLet (StgRec pairs) expr)
  = fixC (\ new_bindings -> addBindsC new_bindings `thenC`
			    listFCs [ cgRhs b e | (b,e) <- pairs ]
    ) `thenFC` \ new_bindings ->

    addBindsC new_bindings `thenC`
    cgExpr expr
\end{code}

\begin{code}
cgExpr (StgLetNoEscape live_in_whole_let live_in_rhss bindings body)
  =    	-- Figure out what volatile variables to save
    nukeDeadBindings live_in_whole_let	`thenC`
    saveVolatileVarsAndRegs live_in_rhss
    	    `thenFC` \ (save_assts, rhs_eob_info, maybe_cc_slot) ->

	-- ToDo: cost centre???

	-- Save those variables right now!
    absC save_assts				`thenC`

	-- Produce code for the rhss
	-- and add suitable bindings to the environment
    cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot bindings `thenC`

	-- Do the body
    setEndOfBlockInfo rhs_eob_info (cgExpr body)
\end{code}


%********************************************************
%*							*
%*		SCC Expressions				*
%*							*
%********************************************************
\subsection[scc-codegen]{Converting StgSCC}

SCC expressions are treated specially. They set the current cost
centre.

For evaluation scoping we also need to save the cost centre in an
``restore CC frame''. We only need to do this once before setting all
nested SCCs.

\begin{code}
cgExpr scc_expr@(StgSCC ty cc expr) = cgSccExpr scc_expr
\end{code}

@cgSccExpr@ (also used in \tr{CgClosure}):
We *don't* set the cost centre for CAF/Dict cost centres
[Likewise Subsumed and NoCostCentre, but they probably
don't exist in an StgSCC expression.]
\begin{code}
cgSccExpr (StgSCC ty cc expr)
  = (if setToAbleCostCentre cc then
	costCentresC SLIT("SET_CCC")
	    [mkCCostCentre cc, mkIntCLit (if isDupdCC cc then 1 else 0)]
     else
	nopC)		`thenC`
    cgSccExpr expr
cgSccExpr other
  = cgExpr other
\end{code}

%********************************************************
%*							*
%*		Non-top-level bindings			*
%*							*
%********************************************************
\subsection[non-top-level-bindings]{Converting non-top-level bindings}

@cgBinding@ is only used for let/letrec, not for unboxed bindings.
So the kind should always be @PtrRep@.

We rely on the support code in @CgCon@ (to do constructors) and
in @CgClosure@ (to do closures).

\begin{code}
cgRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
	-- the Id is passed along so a binding can be set up

cgRhs name (StgRhsCon maybe_cc con args)
  = getArgAmodes args		`thenFC` \ amodes ->
    buildDynCon name maybe_cc con amodes (all zero_size args)
				`thenFC` \ idinfo ->
    returnFC (name, idinfo)
  where
    zero_size atom = getPrimRepSize (getArgPrimRep atom) == 0

cgRhs name (StgRhsClosure cc bi fvs upd_flag args body)
  = cgRhsClosure name cc bi fvs args body lf_info
  where
    lf_info = mkClosureLFInfo False{-not top level-} fvs upd_flag args body
\end{code}

\begin{code}
cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot (StgNonRec binder rhs)
  = cgLetNoEscapeRhs live_in_rhss rhs_eob_info maybe_cc_slot binder rhs
    	    	    	    	`thenFC` \ (binder, info) ->
    addBindC binder info

cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot (StgRec pairs)
  = fixC (\ new_bindings ->
		addBindsC new_bindings 	`thenC`
		listFCs [ cgLetNoEscapeRhs full_live_in_rhss rhs_eob_info
			  maybe_cc_slot b e | (b,e) <- pairs ]
    ) `thenFC` \ new_bindings ->

    addBindsC new_bindings
  where
    -- We add the binders to the live-in-rhss set so that we don't
    -- delete the bindings for the binder from the environment!
    full_live_in_rhss = live_in_rhss `unionIdSets` (mkIdSet [b | (b,r) <- pairs])

cgLetNoEscapeRhs
    :: StgLiveVars	-- Live in rhss
    -> EndOfBlockInfo
    -> Maybe VirtualSpBOffset
    -> Id
    -> StgRhs
    -> FCode (Id, CgIdInfo)

cgLetNoEscapeRhs full_live_in_rhss rhs_eob_info maybe_cc_slot binder
		 (StgRhsClosure cc bi _ upd_flag args body)
  = -- We could check the update flag, but currently we don't switch it off
    -- for let-no-escaped things, so we omit the check too!
    -- case upd_flag of
    --     Updatable -> panic "cgLetNoEscapeRhs"	-- Nothing to update!
    --     other     -> cgLetNoEscapeClosure binder cc bi live_in_whole_let live_in_rhss args body
    cgLetNoEscapeClosure binder cc bi full_live_in_rhss rhs_eob_info maybe_cc_slot args body

-- For a constructor RHS we want to generate a single chunk of code which
-- can be jumped to from many places, which will return the constructor.
-- It's easy; just behave as if it was an StgRhsClosure with a ConApp inside!
cgLetNoEscapeRhs full_live_in_rhss rhs_eob_info maybe_cc_slot binder
    	    	 (StgRhsCon cc con args)
  = cgLetNoEscapeClosure binder cc stgArgOcc{-safe-} full_live_in_rhss rhs_eob_info maybe_cc_slot
	[] 	--No args; the binder is data structure, not a function
	(StgCon con args full_live_in_rhss)
\end{code}

Some PrimOps require a {\em fixed} amount of heap allocation.  Rather
than tidy away ready for GC and do a full heap check, we simply
allocate a completely uninitialised block in-line, just like any other
thunk/constructor allocation, and pass it to the PrimOp as its first
argument.  Remember! The PrimOp is entirely responsible for
initialising the object.  In particular, the PrimOp had better not
trigger GC before it has filled it in, and even then it had better
make sure that the GC can find the object somehow.

Main current use: allocating SynchVars.

\begin{code}
getPrimOpArgAmodes op args
  = getArgAmodes args		`thenFC` \ arg_amodes ->

    case primOpHeapReq op of
	FixedHeapRequired size -> allocHeap size `thenFC` \ amode ->
     	    	    	    	  returnFC (amode : arg_amodes)

	_   	    	       -> returnFC arg_amodes
\end{code}


