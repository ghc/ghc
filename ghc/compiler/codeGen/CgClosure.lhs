%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CgClosure]{Code generation for closures}

This module provides the support code for @StgToAbstractC@ to deal
with {\em closures} on the RHSs of let(rec)s.  See also
@CgCon@, which deals with constructors.

\begin{code}
#include "HsVersions.h"

module CgClosure ( cgTopRhsClosure, cgRhsClosure ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(CgLoop2)	( cgExpr )

import CgMonad
import AbsCSyn
import StgSyn

import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CgBindery	( getCAddrMode, getArgAmodes,
			  getCAddrModeAndInfo, bindNewToNode,
			  bindNewToAStack, bindNewToBStack,
			  bindNewToReg, bindArgsToRegs,
			  stableAmodeIdInfo, heapIdInfo, CgIdInfo
			)
import Constants	( spARelToInt, spBRelToInt )
import CgUpdate		( pushUpdateFrame )
import CgHeapery	( allocDynClosure, heapCheck
			  , heapCheckOnly, fetchAndReschedule, yield  -- HWL
			)
import CgRetConv	( ctrlReturnConvAlg, dataReturnConvAlg, 
			  CtrlReturnConvention(..), DataReturnConvention(..)
			)
import CgStackery	( getFinalStackHW, mkVirtStkOffsets,
			  adjustRealSps
			)
import CgUsages		( getVirtSps, setRealAndVirtualSps,
			  getSpARelOffset, getSpBRelOffset,
			  getHpRelOffset
			)
import CLabel		( mkClosureLabel, mkConUpdCodePtrVecLabel, mkFastEntryLabel,
			  mkStdUpdCodePtrVecLabel, mkStdUpdVecTblLabel,
			  mkErrorStdEntryLabel, mkRednCountsLabel
			)
import ClosureInfo	-- lots and lots of stuff
import CmdLineOpts	( opt_ForConcurrent, opt_GranMacros )
import CostCentre	( useCurrentCostCentre, currentOrSubsumedCosts,
			  noCostCentreAttached, costsAreSubsumed,
			  isCafCC, isDictCC, overheadCostCentre, showCostCentre
			)
import HeapOffs		( SYN_IE(VirtualHeapOffset) )
import Id		( idType, idPrimRep, 
			  showId, getIdStrictness, dataConTag,
			  emptyIdSet,
			  GenId{-instance Outputable-}
			)
import ListSetOps	( minusList )
import Maybes		( maybeToBool )
import Outputable	( Outputable(..){-instances-} ) -- ToDo:rm
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-}, TyCon{-ditto-} )
import Pretty		( prettyToUn, ppBesides, ppChar, ppPStr, ppCat, ppStr )
import PrimRep		( isFollowableRep, PrimRep(..) )
import TyCon		( isPrimTyCon, tyConDataCons )
import Unpretty		( uppShow )
import Util		( isIn, panic, pprPanic, assertPanic, pprTrace{-ToDo:rm-} )

myWrapperMaybe = panic "CgClosure.myWrapperMaybe (ToDo)"
showTypeCategory = panic "CgClosure.showTypeCategory (ToDo)"
getWrapperArgTypeCategories = panic "CgClosure.getWrapperArgTypeCategories (ToDo)"
\end{code}

%********************************************************
%*							*
\subsection[closures-no-free-vars]{Top-level closures}
%*							*
%********************************************************

For closures bound at top level, allocate in static space.
They should have no free variables.

\begin{code}
cgTopRhsClosure :: Id
		-> CostCentre	-- Optional cost centre annotation
		-> StgBinderInfo
		-> [Id]		-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure name cc binder_info args body lf_info
  = 	-- LAY OUT THE OBJECT
    let
	closure_info = layOutStaticNoFVClosure name lf_info
    in

	-- GENERATE THE INFO TABLE (IF NECESSARY)
    forkClosureBody (closureCodeBody binder_info closure_info
					 cc args body)
    							`thenC`

	-- BUILD VAP INFO TABLES IF NECESSARY
	-- Don't build Vap info tables etc for
	-- a function whose result is an unboxed type,
	-- because we can never have thunks with such a type.
    (if closureReturnsUnboxedType closure_info then
	nopC
    else
	let
	    bind_the_fun = addBindC name cg_id_info	-- It's global!
	in
	cgVapInfoTables True {- Top level -} bind_the_fun binder_info name args lf_info
    ) `thenC`

	-- BUILD THE OBJECT (IF NECESSARY)
    (if staticClosureRequired name binder_info lf_info
     then
	let
	    cost_centre = mkCCostCentre cc
	in
	absC (CStaticClosure
		closure_label	-- Labelled with the name on lhs of defn
		closure_info
	    	cost_centre
		[])		-- No fields
     else
	nopC
    ) `thenC`

    returnFC (name, cg_id_info)
  where
    closure_label = mkClosureLabel name
    cg_id_info    = stableAmodeIdInfo name (CLbl closure_label PtrRep) lf_info
\end{code}

%********************************************************
%*							*
\subsection[non-top-level-closures]{Non top-level closures}
%*							*
%********************************************************

For closures with free vars, allocate in heap.

===================== OLD PROBABLY OUT OF DATE COMMENTS =============

-- Closures which (a) have no fvs and (b) have some args (i.e.
-- combinator functions), are allocated statically, just as if they
-- were top-level closures.  We can't get a space leak that way
-- (because they are HNFs) and it saves allocation.

-- Lexical Scoping: Problem
-- These top level function closures will be inherited, possibly
-- to a different cost centre scope set before entering.

-- Evaluation Scoping: ok as already in HNF

-- Should rely on floating mechanism to achieve this floating to top level.
-- As let floating will avoid floating which breaks cost centre attribution
-- everything will be OK.

-- Disabled: because it breaks lexical-scoped cost centre semantics.
-- cgRhsClosure binder cc bi [] upd_flag args@(_:_) body
--  = cgTopRhsClosure binder cc bi upd_flag args body

===================== END OF OLD PROBABLY OUT OF DATE COMMENTS =============

\begin{code}
cgRhsClosure	:: Id
		-> CostCentre	-- Optional cost centre annotation
		-> StgBinderInfo
		-> [Id]		-- Free vars
		-> [Id]		-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgRhsClosure binder cc binder_info fvs args body lf_info
  | maybeToBool maybe_std_thunk		-- AHA!  A STANDARD-FORM THUNK
  -- ToDo: check non-primitiveness (ASSERT)
  = (
	-- LAY OUT THE OBJECT
    getArgAmodes std_thunk_payload		`thenFC` \ amodes ->
    let
	(closure_info, amodes_w_offsets)
	  = layOutDynClosure binder getAmodeRep amodes lf_info

	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in
	-- BUILD THE OBJECT
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )
		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)

  where
    maybe_std_thunk	   = getStandardFormThunkInfo lf_info
    Just std_thunk_payload = maybe_std_thunk
\end{code}

Here's the general case.
\begin{code}
cgRhsClosure binder cc binder_info fvs args body lf_info
  = (
  	-- LAY OUT THE OBJECT
	--
	-- If the binder is itself a free variable, then don't store
	-- it in the closure.  Instead, just bind it to Node on entry.
	-- NB we can be sure that Node will point to it, because we
	-- havn't told mkClosureLFInfo about this; so if the binder
	-- *was* a free var of its RHS, mkClosureLFInfo thinks it *is*
	-- stored in the closure itself, so it will make sure that
	-- Node points to it...
    let
	is_elem	       = isIn "cgRhsClosure"

	binder_is_a_fv = binder `is_elem` fvs
	reduced_fvs    = if binder_is_a_fv
			 then fvs `minusList` [binder]
			 else fvs
    in
    mapFCs getCAddrModeAndInfo reduced_fvs	`thenFC` \ amodes_and_info ->
    let
	fvs_w_amodes_and_info	      = reduced_fvs `zip` amodes_and_info

	closure_info :: ClosureInfo
	bind_details :: [((Id, (CAddrMode, LambdaFormInfo)), VirtualHeapOffset)]

	(closure_info, bind_details)
	  = layOutDynClosure binder get_kind fvs_w_amodes_and_info lf_info

	bind_fv ((id, (_, lf_info)), offset) = bindNewToNode id offset lf_info

	amodes_w_offsets = [(amode,offset) | ((_, (amode,_)), offset) <- bind_details]

	get_kind (id, amode_and_info) = idPrimRep id
    in
	-- BUILD ITS INFO TABLE AND CODE
    forkClosureBody (
		-- Bind the fvs
	    mapCs bind_fv bind_details `thenC`

	  	-- Bind the binder itself, if it is a free var
	    (if binder_is_a_fv then
		bindNewToReg binder node lf_info
	    else
		nopC)					`thenC`

		-- Compile the body
	    closureCodeBody binder_info closure_info cc args body
    )	`thenC`

	-- BUILD VAP INFO TABLES IF NECESSARY
	-- Don't build Vap info tables etc for
	-- a function whose result is an unboxed type,
	-- because we can never have thunks with such a type.
    (if closureReturnsUnboxedType closure_info then
	nopC
    else
	cgVapInfoTables False {- Not top level -} nopC binder_info binder args lf_info
    ) `thenC`

	-- BUILD THE OBJECT
    let
	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)
\end{code}

@cgVapInfoTables@ generates both Vap info tables, if they are required
at all.  It calls @cgVapInfoTable@ to generate each Vap info table,
along with its entry code.

\begin{code}
-- Don't generate Vap info tables for thunks; only for functions
cgVapInfoTables top_level perhaps_bind_the_fun binder_info fun [{- no args; a thunk! -}] lf_info
  = nopC

cgVapInfoTables top_level perhaps_bind_the_fun binder_info fun args lf_info
  = 	-- BUILD THE STANDARD VAP-ENTRY STUFF IF NECESSARY
    (if stdVapRequired binder_info then
	cgVapInfoTable perhaps_bind_the_fun Updatable fun args fun_in_payload lf_info
    else
	nopC
    )		`thenC`

		-- BUILD THE NO-UPDATE VAP-ENTRY STUFF IF NECESSARY
    (if noUpdVapRequired binder_info then
	cgVapInfoTable perhaps_bind_the_fun SingleEntry fun args fun_in_payload lf_info
    else
	nopC
    )

  where
    fun_in_payload = not top_level

cgVapInfoTable perhaps_bind_the_fun upd_flag fun args fun_in_payload fun_lf_info
  = let
	-- The vap_entry_rhs is a manufactured STG expression which
	-- looks like the RHS of any binding which is going to use the vap-entry
	-- point of the function.  Each of these bindings will look like:
	--
	--	x = [a,b,c] \upd [] -> f a b c
	--
	-- If f is not top-level, then f is one of the free variables too,
	-- hence "payload_ids" isn't the same as "arg_ids".
	--
	stg_args      = map StgVarArg args
	vap_entry_rhs = StgApp (StgVarArg fun) stg_args emptyIdSet
									-- Empty live vars

	arg_ids_w_info = [(name,mkLFArgument) | name <- args]
	payload_ids_w_info | fun_in_payload = (fun,fun_lf_info) : arg_ids_w_info
			   | otherwise	    = arg_ids_w_info

	payload_ids | fun_in_payload = fun : args		-- Sigh; needed for mkClosureLFInfo
		    | otherwise	     = args

	vap_lf_info   = mkVapLFInfo payload_ids upd_flag fun stg_args fun_in_payload
		-- It's not top level, even if we're currently compiling a top-level
		-- function, because any VAP *use* of this function will be for a
		-- local thunk, thus
		--		let x = f p q	-- x isn't top level!
		--		in ...

	get_kind (id, info) = idPrimRep id

	payload_bind_details :: [((Id, LambdaFormInfo), VirtualHeapOffset)]
	(closure_info, payload_bind_details) = layOutDynClosure
							fun
							get_kind payload_ids_w_info
							vap_lf_info
		-- The dodgy thing is that we use the "fun" as the
		-- Id to give to layOutDynClosure.  This Id gets embedded in
		-- the closure_info it returns.  But of course, the function doesn't
		-- have the right type to match the Vap closure.  Never mind,
		-- a hack in closureType spots the special case.  Otherwise that
		-- Id is just used for label construction, which is OK.

	bind_fv ((id,lf_info), offset) = bindNewToNode id offset lf_info
    in

	-- BUILD ITS INFO TABLE AND CODE
    forkClosureBody (

		-- Bind the fvs; if the fun is not in the payload, then bind_the_fun tells
		-- how to bind it.  If it is in payload it'll be bound by payload_bind_details.
	    perhaps_bind_the_fun 		`thenC`
	    mapCs bind_fv payload_bind_details	`thenC`

		-- Generate the info table and code
	    closureCodeBody NoStgBinderInfo
			    closure_info
			    useCurrentCostCentre
			    [] 	-- No args; it's a thunk
			    vap_entry_rhs
    )
\end{code}
%************************************************************************
%*									*
\subsection[code-for-closures]{The code for closures}
%*									*
%************************************************************************

\begin{code}
closureCodeBody :: StgBinderInfo
		-> ClosureInfo	-- Lots of information about this closure
		-> CostCentre	-- Optional cost centre attached to closure
		-> [Id]
		-> StgExpr
		-> Code
\end{code}

There are two main cases for the code for closures.  If there are {\em
no arguments}, then the closure is a thunk, and not in normal form.
So it should set up an update frame (if it is shared).  Also, it has
no argument satisfaction check, so fast and slow entry-point labels
are the same.

\begin{code}
closureCodeBody binder_info closure_info cc [] body
  = -- thunks cannot have a primitive type!
#ifdef DEBUG
    let
	(has_tycon, tycon)
	  = case (closureType closure_info) of
	      Nothing       -> (False, panic "debug")
	      Just (tc,_,_) -> (True,  tc)
    in
    if has_tycon && isPrimTyCon tycon then
	pprPanic "closureCodeBody:thunk:prim type!" (ppr PprDebug tycon)
    else
#endif
    getAbsC body_code 	`thenFC` \ body_absC ->
    moduleName		`thenFC` \ mod_name ->

    absC (CClosureInfoAndCode closure_info body_absC Nothing
			      stdUpd (cl_descr mod_name)
			      (dataConLiveness closure_info))
  where
    cl_descr mod_name = closureDescription mod_name (closureId closure_info) [] body

    body_addr   = CLbl (entryLabelFromCI closure_info) CodePtrRep
    body_code   = profCtrC SLIT("ENT_THK") [] 			`thenC`
		  thunkWrapper closure_info (
			-- We only enter cc after setting up update so that cc
			-- of enclosing scope will be recorded in update frame
			-- CAF/DICT functions will be subsumed by this enclosing cc
		    enterCostCentreCode closure_info cc IsThunk	`thenC`
		    cgExpr body)

    stdUpd      = CLbl mkErrorStdEntryLabel CodePtrRep
\end{code}

If there is {\em at least one argument}, then this closure is in
normal form, so there is no need to set up an update frame.  On the
other hand, we do have to check that there are enough args, and
perform an update if not!

The Macros for GrAnSim are produced at the beginning of the
argSatisfactionCheck (by calling fetchAndReschedule).  There info if
Node points to closure is available. -- HWL

\begin{code}
closureCodeBody binder_info closure_info cc all_args body
  = getEntryConvention id lf_info
		       (map idPrimRep all_args)		`thenFC` \ entry_conv ->
    let
	-- Arg mapping for standard (slow) entry point; all args on stack
    	(spA_all_args, spB_all_args, all_bxd_w_offsets, all_ubxd_w_offsets)
	   = mkVirtStkOffsets
		0 0 		-- Initial virtual SpA, SpB
		idPrimRep
		all_args

	-- Arg mapping for the fast entry point; as many args as poss in
	-- registers; the rest on the stack
    	-- 	arg_regs are the registers used for arg passing
	-- 	stk_args are the args which are passed on the stack
	--
    	arg_regs = case entry_conv of
		DirectEntry lbl arity regs -> regs
    	    	ViaNode	| is_concurrent	   -> []
		other 		           -> panic "closureCodeBody:arg_regs"

	num_arg_regs = length arg_regs
	
    	(reg_args, stk_args) = splitAt num_arg_regs all_args

    	(spA_stk_args, spB_stk_args, stk_bxd_w_offsets, stk_ubxd_w_offsets)
	  = mkVirtStkOffsets
		0 0 		-- Initial virtual SpA, SpB
		idPrimRep
		stk_args

	-- HWL; Note: empty list of live regs in slow entry code
	-- Old version (reschedule combined with heap check);
	-- see argSatisfactionCheck for new version
	--slow_entry_code = forceHeapCheck [node] True slow_entry_code'
	--		  where node = VanillaReg PtrRep 1
	--slow_entry_code = forceHeapCheck [] True slow_entry_code'

    	slow_entry_code
      	  = profCtrC SLIT("ENT_FUN_STD") []		    `thenC`

		-- Bind args, and record expected position of stk ptrs
	    mapCs bindNewToAStack all_bxd_w_offsets	    `thenC`
	    mapCs bindNewToBStack all_ubxd_w_offsets	    `thenC`
	    setRealAndVirtualSps spA_all_args spB_all_args  `thenC`

	    argSatisfactionCheck closure_info all_args	    `thenC`

	    -- OK, so there are enough args.  Now we need to stuff as
	    -- many of them in registers as the fast-entry code
	    -- expects Note that the zipWith will give up when it hits
	    -- the end of arg_regs.

	    mapFCs getCAddrMode all_args		    `thenFC` \ stk_amodes ->
	    absC (mkAbstractCs (zipWith assign_to_reg arg_regs stk_amodes)) `thenC`

	    -- Now adjust real stack pointers
	    adjustRealSps spA_stk_args spB_stk_args		`thenC`

    	    absC (CFallThrough (CLbl fast_label CodePtrRep))

	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

	-- HWL
	-- Old version (reschedule combined with heap check);
	-- see argSatisfactionCheck for new version
	-- fast_entry_code = forceHeapCheck [] True fast_entry_code'

	fast_entry_code
	  = profCtrC SLIT("ENT_FUN_DIRECT") [
		    CLbl (mkRednCountsLabel id) PtrRep,
		    CString (_PK_ (showId PprDebug id)),
		    mkIntCLit stg_arity,	-- total # of args
		    mkIntCLit spA_stk_args,	-- # passed on A stk
		    mkIntCLit spB_stk_args,	-- B stk (rest in regs)
		    CString (_PK_ (map (showTypeCategory . idType) all_args)),
		    CString SLIT(""), CString SLIT("")

-- Nuked for now; see comment at end of file
--		    CString (_PK_ (show_wrapper_name wrapper_maybe)),
--		    CString (_PK_ (show_wrapper_arg_kinds wrapper_maybe))

		]			`thenC`

		-- Bind args to regs/stack as appropriate, and
		-- record expected position of sps
	    bindArgsToRegs reg_args arg_regs		    `thenC`
	    mapCs bindNewToAStack stk_bxd_w_offsets	    `thenC`
	    mapCs bindNewToBStack stk_ubxd_w_offsets	    `thenC`
	    setRealAndVirtualSps spA_stk_args spB_stk_args  `thenC`

		-- Enter the closures cc, if required
	    enterCostCentreCode closure_info cc IsFunction  `thenC`

		-- Do the business
	    funWrapper closure_info arg_regs (cgExpr body)
    in
 	-- Make a labelled code-block for the slow and fast entry code
    forkAbsC (if slow_code_needed then slow_entry_code else absC AbsCNop)
				`thenFC` \ slow_abs_c ->
    forkAbsC fast_entry_code	`thenFC` \ fast_abs_c ->
    moduleName			`thenFC` \ mod_name ->

	-- Now either construct the info table, or put the fast code in alone
	-- (We never have slow code without an info table)
    absC (
      if info_table_needed then
	CClosureInfoAndCode closure_info slow_abs_c (Just fast_abs_c)
			stdUpd (cl_descr mod_name)
			(dataConLiveness closure_info)
      else
	CCodeBlock fast_label fast_abs_c
    )
  where
    is_concurrent = opt_ForConcurrent
    stg_arity = length all_args
    lf_info = closureLFInfo closure_info

    cl_descr mod_name = closureDescription mod_name id all_args body

    	-- Figure out what is needed and what isn't
    slow_code_needed   = slowFunEntryCodeRequired id binder_info
    info_table_needed  = funInfoTableRequired id binder_info lf_info

	-- Manufacture labels
    id	       = closureId closure_info
    fast_label = mkFastEntryLabel id stg_arity
    stdUpd     = CLbl mkErrorStdEntryLabel CodePtrRep

{- OLD... see note at end of file
    wrapper_maybe = get_ultimate_wrapper Nothing id
      where
    	get_ultimate_wrapper deflt x -- walk all the way up a "wrapper chain"
	  = case (myWrapperMaybe x) of
	      Nothing -> deflt
	      Just xx -> get_ultimate_wrapper (Just xx) xx

    show_wrapper_name Nothing   = ""
    show_wrapper_name (Just xx) = showId PprDebug xx

    show_wrapper_arg_kinds Nothing   = ""
    show_wrapper_arg_kinds (Just xx)
      = case (getWrapperArgTypeCategories (idType xx) (getIdStrictness xx)) of
	  Nothing  -> ""
	  Just str -> str
-}
\end{code}

For lexically scoped profiling we have to load the cost centre from
the closure entered, if the costs are not supposed to be inherited.
This is done immediately on entering the fast entry point.

Load current cost centre from closure, if not inherited.
Node is guaranteed to point to it, if profiling and not inherited.

\begin{code}
data IsThunk = IsThunk | IsFunction -- Bool-like, local
--#ifdef DEBUG
	deriving Eq
--#endif

enterCostCentreCode :: ClosureInfo -> CostCentre -> IsThunk -> Code

enterCostCentreCode closure_info cc is_thunk
  = costCentresFlag	`thenFC` \ profiling_on ->
    if not profiling_on then
	nopC
    else
	ASSERT(not (noCostCentreAttached cc))

	if costsAreSubsumed cc then
	    --ASSERT(isToplevClosure closure_info)
	    --ASSERT(is_thunk == IsFunction)
	    (if isToplevClosure closure_info && is_thunk == IsFunction then \x->x else pprTrace "enterCostCenterCode:" (ppCat [ppr PprDebug (is_thunk == IsFunction){-, ppr PprDebug closure_info-}, ppStr (showCostCentre PprDebug False cc)])) $
	    costCentresC SLIT("ENTER_CC_FSUB") []

	else if currentOrSubsumedCosts cc then 
	    -- i.e. current; subsumed dealt with above
	    -- get CCC out of the closure, where we put it when we alloc'd
	    case is_thunk of 
		IsThunk    -> costCentresC SLIT("ENTER_CC_TCL") [CReg node]
		IsFunction -> costCentresC SLIT("ENTER_CC_FCL") [CReg node]

	else if isCafCC cc && isToplevClosure closure_info then
	    ASSERT(is_thunk == IsThunk)
	    costCentresC SLIT("ENTER_CC_CAF") [mkCCostCentre cc]

	else -- we've got a "real" cost centre right here in our hands...
	    case is_thunk of 
		IsThunk    -> costCentresC SLIT("ENTER_CC_T") [mkCCostCentre cc]
		IsFunction -> if isCafCC cc || isDictCC cc
			      then costCentresC SLIT("ENTER_CC_FCAF") [mkCCostCentre cc]
			      else costCentresC SLIT("ENTER_CC_FLOAD") [mkCCostCentre cc]
\end{code}

%************************************************************************
%*									*
\subsubsection[pre-closure-code-stuff]{Pre-closure-code code}
%*									*
%************************************************************************

The argument-satisfaction check code is placed after binding
the arguments to their stack locations. Hence, the virtual stack
pointer is pointing after all the args, and virtual offset 1 means
the base of frame and hence most distant arg.  Hence
virtual offset 0 is just beyond the most distant argument; the
relative offset of this word tells how many words of arguments
are expected.

\begin{code}
argSatisfactionCheck :: ClosureInfo -> [Id] -> Code

argSatisfactionCheck closure_info [] = nopC

argSatisfactionCheck closure_info args
  = -- safest way to determine which stack last arg will be on:
    -- look up CAddrMode that last arg is bound to;
    -- getAmodeRep;
    -- check isFollowableRep.

    nodeMustPointToIt (closureLFInfo closure_info) 	`thenFC` \ node_points ->

    let
       emit_gran_macros = opt_GranMacros
    in

    -- HWL  ngo' ngoq:
    -- absC (CMacroStmt GRAN_FETCH []) 			`thenC`
    -- forceHeapCheck [] node_points (absC AbsCNop)			`thenC`
    (if emit_gran_macros 
      then if node_points 
             then fetchAndReschedule  [] node_points 
             else yield [] node_points
      else absC AbsCNop)                       `thenC`

    getCAddrMode (last args) 				`thenFC` \ last_amode ->

    if (isFollowableRep (getAmodeRep last_amode)) then
	getSpARelOffset 0 	`thenFC` \ (SpARel spA off) ->
	let
	    a_rel_int = spARelToInt spA off
	    a_rel_arg = mkIntCLit a_rel_int
	in
	ASSERT(a_rel_int /= 0)
	if node_points then
	    absC (CMacroStmt ARGS_CHK_A [a_rel_arg])
	else
	    absC (CMacroStmt ARGS_CHK_A_LOAD_NODE [a_rel_arg, set_Node_to_this])
    else
	getSpBRelOffset 0 	`thenFC` \ (SpBRel spB off) ->
	let
	    b_rel_int = spBRelToInt spB off
	    b_rel_arg = mkIntCLit b_rel_int
	in
	ASSERT(b_rel_int /= 0)
	if node_points then
	    absC (CMacroStmt ARGS_CHK_B [b_rel_arg])
	else
	    absC (CMacroStmt ARGS_CHK_B_LOAD_NODE [b_rel_arg, set_Node_to_this])
  where
    -- We must tell the arg-satis macro whether Node is pointing to
    -- the closure or not.  If it isn't so pointing, then we give to
    -- the macro the (static) address of the closure.

    set_Node_to_this = CLbl (closureLabelFromCI closure_info) PtrRep
\end{code}

%************************************************************************
%*									*
\subsubsection[closure-code-wrappers]{Wrappers around closure code}
%*									*
%************************************************************************

\begin{code}
thunkWrapper:: ClosureInfo -> Code -> Code
thunkWrapper closure_info thunk_code
  = 	-- Stack and heap overflow checks
    nodeMustPointToIt (closureLFInfo closure_info)  	`thenFC` \ node_points ->

    let
       emit_gran_macros = opt_GranMacros
    in
	-- HWL: insert macros for GrAnSim; 2 versions depending on liveness of node
	-- (we prefer fetchAndReschedule-style context switches to yield ones)
    (if emit_gran_macros 
      then if node_points 
             then fetchAndReschedule  [] node_points 
             else yield [] node_points
      else absC AbsCNop)                       `thenC`

    stackCheck closure_info [] node_points (	-- stackCheck *encloses* the rest

	-- heapCheck must be after stackCheck: if stchk fails
	-- new stack space is allocated from the heap which
	-- would violate any previous heapCheck

    heapCheck [] node_points (			-- heapCheck *encloses* the rest
    	-- The "[]" says there are no live argument registers

	-- Overwrite with black hole if necessary
    blackHoleIt closure_info 			`thenC`

    setupUpdate closure_info (			-- setupUpdate *encloses* the rest

	-- Finally, do the business
    thunk_code
    )))

funWrapper :: ClosureInfo 	-- Closure whose code body this is
	   -> [MagicId] 	-- List of argument registers (if any)
	   -> Code		-- Body of function being compiled
	   -> Code
funWrapper closure_info arg_regs fun_body
  = 	-- Stack overflow check
    nodeMustPointToIt (closureLFInfo closure_info)  	`thenFC` \ node_points ->
    let
       emit_gran_macros = opt_GranMacros
    in
    -- HWL   chu' ngoq:
    (if emit_gran_macros
      then yield  arg_regs node_points
      else absC AbsCNop)                                 `thenC`

    stackCheck closure_info arg_regs node_points (
	-- stackCheck *encloses* the rest

    heapCheck arg_regs node_points (
	-- heapCheck *encloses* the rest

	-- Finally, do the business
    fun_body
    ))
\end{code}

%************************************************************************
%*									*
\subsubsubsection[overflow-checks]{Stack and heap overflow wrappers}
%*									*
%************************************************************************

Assumption: virtual and real stack pointers are currently exactly aligned.

\begin{code}
stackCheck :: ClosureInfo
	   -> [MagicId] 		-- Live registers
	   -> Bool 			-- Node required to point after check?
	   -> Code
	   -> Code

stackCheck closure_info regs node_reqd code
  = getFinalStackHW (\ aHw -> \ bHw ->	-- Both virtual stack offsets

    getVirtSps		`thenFC` \ (vSpA, vSpB) ->

    let a_headroom_reqd = aHw - vSpA	-- Virtual offsets are positive integers
	b_headroom_reqd = bHw - vSpB
    in

    absC (if (a_headroom_reqd == 0 && b_headroom_reqd == 0) then
		AbsCNop
	  else
		CMacroStmt STK_CHK [mkIntCLit liveness_mask,
				    mkIntCLit a_headroom_reqd,
				    mkIntCLit b_headroom_reqd,
				    mkIntCLit vSpA,
				    mkIntCLit vSpB,
				    mkIntCLit (if returns_prim_type then 1 else 0),
				    mkIntCLit (if node_reqd         then 1 else 0)]
    	 )
	-- The test is *inside* the absC, to avoid black holes!

    `thenC` code
    )
  where
    all_regs = if node_reqd then node:regs else regs
    liveness_mask = mkLiveRegsMask all_regs

    returns_prim_type = closureReturnsUnboxedType closure_info
\end{code}

%************************************************************************
%*									*
\subsubsubsection[update-and-BHs]{Update and black-hole wrappers}
%*									*
%************************************************************************


\begin{code}
blackHoleIt :: ClosureInfo -> Code	-- Only called for thunks
blackHoleIt closure_info
  = noBlackHolingFlag	`thenFC` \ no_black_holing ->

    if (blackHoleOnEntry no_black_holing closure_info)
    then
	absC (if closureSingleEntry(closure_info) then
		CMacroStmt UPD_BH_SINGLE_ENTRY [CReg node]
	      else
		CMacroStmt UPD_BH_UPDATABLE [CReg node])
	-- Node always points to it; see stg-details
    else
	nopC
\end{code}

\begin{code}
setupUpdate :: ClosureInfo -> Code -> Code	-- Only called for thunks
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent ENTER_CC_TCL

setupUpdate closure_info code
 = if (closureUpdReqd closure_info) then
	link_caf_if_needed	`thenFC` \ update_closure ->
    	pushUpdateFrame update_closure vector code
   else
	profCtrC SLIT("UPDF_OMITTED") [] `thenC`
	code
 where
   link_caf_if_needed :: FCode CAddrMode	-- Returns amode for closure to be updated
   link_caf_if_needed
     = if not (isStaticClosure closure_info) then
	  returnFC (CReg node)
       else

	  -- First we must allocate a black hole, and link the
	  -- CAF onto the CAF list

		-- Alloc black hole specifying CC_HDR(Node) as the cost centre
		--   Hack Warning: Using a CLitLit to get CAddrMode !
	  let
	      use_cc   = CLitLit SLIT("CC_HDR(R1.p)") PtrRep
	      blame_cc = use_cc
	  in
	  allocDynClosure (blackHoleClosureInfo closure_info) use_cc blame_cc []
							`thenFC` \ heap_offset ->
	  getHpRelOffset heap_offset			`thenFC` \ hp_rel ->
	  let  amode = CAddr hp_rel
	  in
	  absC (CMacroStmt UPD_CAF [CReg node, amode])
						  	`thenC`
	  returnFC amode

   vector
     = case (closureType closure_info) of
    	Nothing -> CReg StdUpdRetVecReg
    	Just (spec_tycon, _, spec_datacons) ->
	    case (ctrlReturnConvAlg spec_tycon) of
    	      UnvectoredReturn 1 ->
       	    	let
		    spec_data_con = head spec_datacons
		    only_tag = dataConTag spec_data_con

    	    	    direct = case (dataReturnConvAlg spec_data_con) of
    	    	        ReturnInRegs _ -> mkConUpdCodePtrVecLabel spec_tycon only_tag
    	    	    	ReturnInHeap   -> mkStdUpdCodePtrVecLabel spec_tycon only_tag

    	    	    vectored = mkStdUpdVecTblLabel spec_tycon
    	    	in
    	    	    CUnVecLbl direct vectored

	      UnvectoredReturn _ -> CReg StdUpdRetVecReg
	      VectoredReturn _   -> CLbl (mkStdUpdVecTblLabel spec_tycon) DataPtrRep
\end{code}

%************************************************************************
%*									*
\subsection[CgClosure-Description]{Profiling Closure Description.}
%*									*
%************************************************************************

For "global" data constructors the description is simply occurrence
name of the data constructor itself (see \ref{CgConTbls-info-tables}).

Otherwise it is determind by @closureDescription@ from the let
binding information.

\begin{code}
closureDescription :: FAST_STRING	-- Module
		   -> Id		-- Id of closure binding
		   -> [Id]		-- Args
		   -> StgExpr	-- Body
		   -> String

	-- Not called for StgRhsCon which have global info tables built in
	-- CgConTbls.lhs with a description generated from the data constructor

closureDescription mod_name name args body
  = uppShow 0 (prettyToUn (
	ppBesides [ppChar '<',
		   ppPStr mod_name,
		   ppChar '.',
		   ppr PprDebug name,
		   ppChar '>']))
\end{code}

\begin{code}
chooseDynCostCentres cc args fvs body
  = let
	use_cc -- cost-centre we record in the object
	  = if currentOrSubsumedCosts cc
	    then CReg CurCostCentre
	    else mkCCostCentre cc

	blame_cc -- cost-centre on whom we blame the allocation
	  = case (args, fvs, body) of
	      ([], [just1], StgApp (StgVarArg fun) [{-no args-}] _)
		| just1 == fun
		-> mkCCostCentre overheadCostCentre
	      _ -> use_cc

	    -- if it's an utterly trivial RHS, then it must be
	    -- one introduced by boxHigherOrderArgs for profiling,
	    -- so we charge it to "OVERHEAD".
    in
    (use_cc, blame_cc)
\end{code}



========================================================================
OLD CODE THAT EMITTED INFORMATON FOR QUANTITATIVE ANALYSIS

It's pretty wierd, so I've nuked it for now.  SLPJ Nov 96

\begin{pseudocode}
getWrapperArgTypeCategories
	:: Type				-- wrapper's type
	-> StrictnessInfo bdee		-- strictness info about its args
	-> Maybe String

getWrapperArgTypeCategories _ NoStrictnessInfo	    = Nothing
getWrapperArgTypeCategories _ BottomGuaranteed
  = trace "getWrapperArgTypeCategories:BottomGuaranteed!" Nothing  -- wrong
getWrapperArgTypeCategories _ (StrictnessInfo [] _) = Nothing

getWrapperArgTypeCategories ty (StrictnessInfo arg_info _)
  = Just (mkWrapperArgTypeCategories ty arg_info)

mkWrapperArgTypeCategories
	:: Type		-- wrapper's type
	-> [Demand]	-- info about its arguments
	-> String	-- a string saying lots about the args

mkWrapperArgTypeCategories wrapper_ty wrap_info
  = case (splitFunTyExpandingDicts wrapper_ty) of { (arg_tys,_) ->
    map do_one (wrap_info `zip` (map showTypeCategory arg_tys)) }
  where
    -- ToDo: this needs FIXING UP (it was a hack anyway...)
    do_one (WwPrim, _) = 'P'
    do_one (WwEnum, _) = 'E'
    do_one (WwStrict, arg_ty_char) = arg_ty_char
    do_one (WwUnpack _ _, arg_ty_char)
      = if arg_ty_char `elem` "CIJFDTS"
	then toLower arg_ty_char
	else if arg_ty_char == '+' then 't'
	else trace ("mkWrapp..:funny char:"++[arg_ty_char]) '-'
    do_one (other_wrap_info, _) = '-'
\end{pseudocode}

