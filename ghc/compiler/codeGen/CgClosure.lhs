%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgClosure.lhs,v 1.60 2003/05/14 09:13:53 simonmar Exp $
%
\section[CgClosure]{Code generation for closures}

This module provides the support code for @StgToAbstractC@ to deal
with {\em closures} on the RHSs of let(rec)s.  See also
@CgCon@, which deals with constructors.

\begin{code}
module CgClosure ( cgTopRhsClosure, 
		   cgStdRhsClosure, 
		   cgRhsClosure, 
		   closureCodeBody ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr ( cgExpr )

import CgMonad
import CgBindery
import CgUpdate		( pushUpdateFrame )
import CgHeapery
import CgStackery
import CgUsages
import ClosureInfo	-- lots and lots of stuff

import AbsCUtils	( getAmodeRep, mkAbstractCs )
import AbsCSyn
import CLabel

import StgSyn
import CmdLineOpts	( opt_GranMacros, opt_SccProfilingOn, opt_DoTickyProfiling )
import CostCentre	
import Id		( Id, idName, idType, idPrimRep )
import Name		( Name, isInternalName )
import Module		( Module, pprModule )
import ListSetOps	( minusList )
import PrimRep		( PrimRep(..), getPrimRepSize )
import PprType          ( showTypeCategory )
import Util		( isIn, splitAtList )
import CmdLineOpts	( opt_SccProfilingOn )
import Outputable
import FastString

import Name             ( nameOccName )
import OccName          ( occNameFS )
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
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> SRT
		-> [Id]		-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure id ccs binder_info srt args body lf_info
  = 
    let
    	name          = idName id
    in
    -- LAY OUT THE OBJECT
    getSRTInfo name srt		`thenFC` \ srt_info ->
    moduleName			`thenFC` \ mod_name ->
    let
    	name          = idName id
	descr         = closureDescription mod_name name
	closure_info  = layOutStaticNoFVClosure id lf_info srt_info descr
	closure_label = mkClosureLabel name
    	cg_id_info    = stableAmodeIdInfo id (CLbl closure_label PtrRep) lf_info
    in

	-- BUILD THE OBJECT (IF NECESSARY)
    (
     ({- if staticClosureRequired name binder_info lf_info
      then -}
	absC (mkStaticClosure closure_label closure_info ccs [] True)
      {- else
	nopC -}
     )
    							`thenC`

	-- GENERATE THE INFO TABLE (IF NECESSARY)
    forkClosureBody (closureCodeBody binder_info closure_info
					 ccs args body)

    ) `thenC`

    returnFC (id, cg_id_info)

\end{code}

%********************************************************
%*							*
\subsection[non-top-level-closures]{Non top-level closures}
%*							*
%********************************************************

For closures with free vars, allocate in heap.

\begin{code}
cgStdRhsClosure
	:: Id
	-> CostCentreStack	-- Optional cost centre annotation
	-> StgBinderInfo
	-> [Id]			-- Free vars
	-> [Id]			-- Args
	-> StgExpr
	-> LambdaFormInfo
	-> [StgArg]		-- payload
	-> FCode (Id, CgIdInfo)

cgStdRhsClosure binder cc binder_info fvs args body lf_info payload
		-- AHA!  A STANDARD-FORM THUNK
  = (
	-- LAY OUT THE OBJECT
    getArgAmodes payload		`thenFC` \ amodes ->
    moduleName				`thenFC` \ mod_name ->
    let
	descr = closureDescription mod_name (idName binder)

	(closure_info, amodes_w_offsets)
	  = layOutDynClosure binder getAmodeRep amodes lf_info NoC_SRT descr
		-- No SRT for a standard-form closure

	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in

	-- BUILD THE OBJECT
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )
		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)
\end{code}

Here's the general case.

\begin{code}
cgRhsClosure	:: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> SRT
		-> [Id]			-- Free vars
		-> [Id]			-- Args
		-> StgExpr
		-> LambdaFormInfo
		-> FCode (Id, CgIdInfo)

cgRhsClosure binder cc binder_info srt fvs args body lf_info
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

	name = idName binder
    in

    mapFCs getCAddrModeAndInfo reduced_fvs	`thenFC` \ fvs_w_amodes_and_info ->
    getSRTInfo name srt				`thenFC` \ srt_info ->
    moduleName					`thenFC` \ mod_name ->
    let
	descr = closureDescription mod_name (idName binder)

	closure_info :: ClosureInfo
	bind_details :: [((Id, CAddrMode, LambdaFormInfo), VirtualHeapOffset)]

	(closure_info, bind_details)
	  = layOutDynClosure binder get_kind
			     fvs_w_amodes_and_info lf_info srt_info descr

	bind_fv ((id, _, lf_info), offset) = bindNewToNode id offset lf_info

	amodes_w_offsets = [(amode,offset) | ((_,amode,_), offset) <- bind_details]

	get_kind (id, _, _) = idPrimRep id
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

	-- BUILD THE OBJECT
    let
	(use_cc, blame_cc) = chooseDynCostCentres cc args fvs body
    in
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
    )		`thenFC` \ heap_offset ->

	-- RETURN
    returnFC (binder, heapIdInfo binder heap_offset lf_info)
\end{code}

%************************************************************************
%*									*
\subsection[code-for-closures]{The code for closures}
%*									*
%************************************************************************

\begin{code}
closureCodeBody :: StgBinderInfo
		-> ClosureInfo	   -- Lots of information about this closure
		-> CostCentreStack -- Optional cost centre attached to closure
		-> [Id]
		-> StgExpr
		-> Code
\end{code}

There are two main cases for the code for closures.  If there are {\em
no arguments}, then the closure is a thunk, and not in normal form.
So it should set up an update frame (if it is shared).

\begin{code}
closureCodeBody binder_info closure_info cc [] body
  = -- thunks cannot have a primitive type!
    getAbsC body_code 	`thenFC` \ body_absC ->

    absC (CClosureInfoAndCode closure_info body_absC)
  where
    is_box  = case body of { StgApp fun [] -> True; _ -> False }

    ticky_ent_lit = if (isStaticClosure closure_info)
                    then FSLIT("TICK_ENT_STATIC_THK")
                    else FSLIT("TICK_ENT_DYN_THK")

    body_code   = profCtrC ticky_ent_lit []			`thenC`
		  -- node always points when profiling, so this is ok:
		  ldvEnter					`thenC`
		  thunkWrapper closure_info (
			-- We only enter cc after setting up update so
			-- that cc of enclosing scope will be recorded
			-- in update frame CAF/DICT functions will be
			-- subsumed by this enclosing cc
		    enterCostCentreCode closure_info cc IsThunk	is_box `thenC`
		    cgExpr body
		  )

\end{code}

If there is /at least one argument/, then this closure is in
normal form, so there is no need to set up an update frame.

The Macros for GrAnSim are produced at the beginning of the
argSatisfactionCheck (by calling fetchAndReschedule).  There info if
Node points to closure is available. -- HWL

\begin{code}
closureCodeBody binder_info closure_info cc all_args body
  = let arg_reps = map idPrimRep all_args in

    getEntryConvention name lf_info arg_reps  `thenFC` \ entry_conv ->

    let
	-- Arg mapping for the entry point; as many args as poss in
	-- registers; the rest on the stack
    	-- 	arg_regs are the registers used for arg passing
	-- 	stk_args are the args which are passed on the stack
	--
	-- Args passed on the stack are not tagged.
	--
    	arg_regs = case entry_conv of
		DirectEntry lbl arity regs -> regs
		_ -> panic "closureCodeBody"
    in

    -- If this function doesn't have a specialised ArgDescr, we need
    -- to generate the function's arg bitmap, slow-entry code, and
    -- register-save code for the heap-check failure
    --
    (case closureFunInfo closure_info of
	Just (_, ArgGen slow_lbl liveness) -> 
		absC (maybeLargeBitmap liveness) `thenC`
		absC (mkSlowEntryCode name slow_lbl arg_regs arg_reps) `thenC`
		returnFC (mkRegSaveCode arg_regs arg_reps)

	other -> returnFC AbsCNop
     )		
	`thenFC` \ reg_save_code ->

    -- get the current virtual Sp (it might not be zero, eg. if we're
    -- compiling a let-no-escape).
    getVirtSp `thenFC` \vSp ->

    let
    	(reg_args, stk_args) = splitAtList arg_regs all_args

    	(sp_stk_args, stk_offsets)
	  = mkVirtStkOffsets vSp idPrimRep stk_args

	entry_code = do
		mod_name <- moduleName
		profCtrC FSLIT("TICK_CTR") [ 
			CLbl ticky_ctr_label DataPtrRep,
			mkCString (mkFastString (ppr_for_ticky_name mod_name name)),
			mkIntCLit stg_arity,	-- total # of args
			mkIntCLit sp_stk_args,	-- # passed on stk
			mkCString (mkFastString (map (showTypeCategory . idType) all_args))
			] 
		let prof = 
			profCtrC ticky_ent_lit [
				CLbl ticky_ctr_label DataPtrRep
			] 

		-- Bind args to regs/stack as appropriate, and
		-- record expected position of sps.
		bindArgsToRegs reg_args arg_regs		    
		mapCs bindNewToStack stk_offsets		    
		setRealAndVirtualSp sp_stk_args		    

		-- Enter the closures cc, if required
		enterCostCentreCode closure_info cc IsFunction False

		-- Do the business
		funWrapper closure_info arg_regs reg_save_code
			(prof >> cgExpr body)
    in

    setTickyCtrLabel ticky_ctr_label (

      forkAbsC entry_code	`thenFC` \ entry_abs_c ->
      moduleName		`thenFC` \ mod_name ->

      -- Now construct the info table
      absC (CClosureInfoAndCode closure_info entry_abs_c)
    )
  where
    ticky_ctr_label = mkRednCountsLabel name

    ticky_ent_lit = 
        if (isStaticClosure closure_info)
        then FSLIT("TICK_ENT_STATIC_FUN_DIRECT")
        else FSLIT("TICK_ENT_DYN_FUN_DIRECT")
        
    stg_arity = length all_args
    lf_info = closureLFInfo closure_info

	-- Manufacture labels
    name       = closureName closure_info


-- When printing the name of a thing in a ticky file, we want to
-- give the module name even for *local* things.   We print
-- just "x (M)" rather that "M.x" to distinguish them from the global kind.
ppr_for_ticky_name mod_name name
  | isInternalName name = showSDocDebug (ppr name <+> (parens (ppr mod_name)))
  | otherwise	     = showSDocDebug (ppr name)
\end{code}

The "slow entry" code for a function.  This entry point takes its
arguments on the stack.  It loads the arguments into registers
according to the calling convention, and jumps to the function's
normal entry point.  The function's closure is assumed to be in
R1/node.

The slow entry point is used in two places:

 (a) unknown calls: eg. stg_PAP_entry 
 (b) returning from a heap-check failure

\begin{code}
mkSlowEntryCode :: Name -> CLabel -> [MagicId] -> [PrimRep] -> AbstractC
mkSlowEntryCode name lbl regs reps
   = CCodeBlock lbl (
	mkAbstractCs [assts, stk_adj, jump]
      )
  where
     stk_offsets = scanl (\off rep -> off - getPrimRepSize rep) 0 reps

     assts = mkAbstractCs (zipWith3 mk_asst reps regs stk_offsets)
     mk_asst rep reg offset = CAssign (CReg reg) (CVal (spRel 0 offset) rep)

     stk_adj = CAssign (CReg Sp) (CAddr (spRel 0 stk_final_offset))
     stk_final_offset = head (drop (length regs) stk_offsets)

     jump = CJump (CLbl (mkEntryLabel name) CodePtrRep)

mkRegSaveCode :: [MagicId] -> [PrimRep] -> AbstractC
mkRegSaveCode regs reps 
  = mkAbstractCs [stk_adj, assts]
  where
     stk_adj = CAssign (CReg Sp) (CAddr (spRel 0 (negate stk_final_offset)))

     stk_final_offset = head (drop (length regs) stk_offsets)
     stk_offsets = scanl (\off rep -> off - getPrimRepSize rep) 0 reps

     assts = mkAbstractCs (zipWith3 mk_asst reps regs stk_offsets)
     mk_asst rep reg offset = CAssign (CVal (spRel 0 offset) rep) (CReg reg) 
\end{code}

For lexically scoped profiling we have to load the cost centre from
the closure entered, if the costs are not supposed to be inherited.
This is done immediately on entering the fast entry point.

Load current cost centre from closure, if not inherited.
Node is guaranteed to point to it, if profiling and not inherited.

\begin{code}
data IsThunk = IsThunk | IsFunction -- Bool-like, local
-- #ifdef DEBUG
	deriving Eq
-- #endif

enterCostCentreCode 
   :: ClosureInfo -> CostCentreStack
   -> IsThunk
   -> Bool	-- is_box: this closure is a special box introduced by SCCfinal
   -> Code

enterCostCentreCode closure_info ccs is_thunk is_box
  = if not opt_SccProfilingOn then
	nopC
    else
	ASSERT2(not (noCCSAttached ccs), ppr (closureName closure_info) <+> ppr ccs)

	if isSubsumedCCS ccs then
	    ASSERT(isToplevClosure closure_info)
	    ASSERT(is_thunk == IsFunction)
	    costCentresC FSLIT("ENTER_CCS_FSUB") []
 
	else if isDerivedFromCurrentCCS ccs then 
	    if re_entrant && not is_box
		then costCentresC FSLIT("ENTER_CCS_FCL") [CReg node]
		else costCentresC FSLIT("ENTER_CCS_TCL") [CReg node]

	else if isCafCCS ccs then
	    ASSERT(isToplevClosure closure_info)
	    ASSERT(is_thunk == IsThunk)
		-- might be a PAP, in which case we want to subsume costs
	    if re_entrant
		then costCentresC FSLIT("ENTER_CCS_FSUB") []
		else costCentresC FSLIT("ENTER_CCS_CAF") c_ccs

	else panic "enterCostCentreCode"

   where
	c_ccs = [mkCCostCentreStack ccs]
	re_entrant = closureReEntrant closure_info
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
    nodeMustPointToIt (closureLFInfo closure_info) `thenFC` \ node_points ->

    -- HWL: insert macros for GrAnSim; 2 versions depending on liveness of node
    -- (we prefer fetchAndReschedule-style context switches to yield ones)
    (if opt_GranMacros
       then if node_points 
              then fetchAndReschedule [] node_points 
              else yield [] node_points
       else absC AbsCNop)                       `thenC`

    let closure_lbl
		| node_points = Nothing
		| otherwise   = Just (closureLabelFromCI closure_info)
    in

        -- stack and/or heap checks
    thunkChecks closure_lbl (

	-- Overwrite with black hole if necessary
    blackHoleIt closure_info node_points  `thenC`

    setupUpdate closure_info (			-- setupUpdate *encloses* the rest

	-- Finally, do the business
    thunk_code
    ))

funWrapper :: ClosureInfo 	-- Closure whose code body this is
	   -> [MagicId] 	-- List of argument registers (if any)
	   -> AbstractC		-- reg saves for the heap check failure
	   -> Code		-- Body of function being compiled
	   -> Code
funWrapper closure_info arg_regs reg_save_code fun_body
  = 	-- Stack overflow check
    nodeMustPointToIt (closureLFInfo closure_info)  `thenFC` \ node_points ->

    -- enter for Ldv profiling
    (if node_points then ldvEnter else nopC)	    `thenC`

    (if opt_GranMacros
       then yield arg_regs node_points
       else absC AbsCNop)                           `thenC`

    let closure_lbl
		| node_points = Nothing
		| otherwise   = Just (closureLabelFromCI closure_info)
    in

        -- heap and/or stack checks
    funEntryChecks closure_lbl reg_save_code (

	-- Finally, do the business
    fun_body
    )
\end{code}


%************************************************************************
%*									*
\subsubsubsection[update-and-BHs]{Update and black-hole wrappers}
%*									*
%************************************************************************


\begin{code}
blackHoleIt :: ClosureInfo -> Bool -> Code	-- Only called for closures with no args

blackHoleIt closure_info node_points
  = if blackHoleOnEntry closure_info && node_points
    then
	let
	  info_label = infoTableLabelFromCI closure_info
	  args = [ CLbl info_label DataPtrRep ]
	in
	absC (if closureSingleEntry(closure_info) then
		CMacroStmt UPD_BH_SINGLE_ENTRY args
	      else
		CMacroStmt UPD_BH_UPDATABLE args)
    else
	nopC
\end{code}

\begin{code}
setupUpdate :: ClosureInfo -> Code -> Code	-- Only called for closures with no args
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent ENTER_CC_TCL

-- I've tidied up the code for this function, but it should still do the same as
-- it did before (modulo ticky stuff).  KSW 1999-04.
setupUpdate closure_info code
 = if closureReEntrant closure_info
   then
     code
   else
     case (closureUpdReqd closure_info, isStaticClosure closure_info) of
       (False,False) -> profCtrC FSLIT("TICK_UPDF_OMITTED") [] `thenC`
	                code
       (False,True ) -> (if opt_DoTickyProfiling
                         then
                         -- blackhole the SE CAF
                           link_caf seCafBlackHoleClosureInfo `thenFC` \ _ -> nopC
                         else
                           nopC)                                                       `thenC`
                        profCtrC FSLIT("TICK_UPD_CAF_BH_SINGLE_ENTRY") [mkCString cl_name] `thenC`
                        profCtrC FSLIT("TICK_UPDF_OMITTED") []                           `thenC`
	                code
       (True ,False) -> pushUpdateFrame (CReg node) code
       (True ,True ) -> -- blackhole the (updatable) CAF:
                        link_caf cafBlackHoleClosureInfo           `thenFC` \ update_closure ->
                        profCtrC FSLIT("TICK_UPD_CAF_BH_UPDATABLE") [mkCString cl_name]    `thenC`
                        pushUpdateFrame update_closure code
 where
   cl_name :: FastString
   cl_name  = (occNameFS . nameOccName . closureName) closure_info

   link_caf :: (ClosureInfo -> ClosureInfo)  -- function yielding BH closure_info
            -> FCode CAddrMode	             -- Returns amode for closure to be updated
   link_caf bhCI
     = -- To update a CAF we must allocate a black hole, link the CAF onto the
       -- CAF list, then update the CAF to point to the fresh black hole.
       -- This function returns the address of the black hole, so it can be
       -- updated with the new value when available.

             -- Alloc black hole specifying CC_HDR(Node) as the cost centre
       let
           use_cc   = CMacroExpr PtrRep CCS_HDR [nodeReg]
           blame_cc = use_cc
       in
       allocDynClosure (bhCI closure_info) use_cc blame_cc []  `thenFC` \ heap_offset ->
       getHpRelOffset heap_offset                              `thenFC` \ hp_rel ->
       let  amode = CAddr hp_rel
       in
       absC (CMacroStmt UPD_CAF [CReg node, amode])            `thenC`
       returnFC amode
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
closureDescription :: Module		-- Module
		   -> Name		-- Id of closure binding
		   -> String

	-- Not called for StgRhsCon which have global info tables built in
	-- CgConTbls.lhs with a description generated from the data constructor

closureDescription mod_name name
  = showSDoc (
	hcat [char '<',
		   pprModule mod_name,
		   char '.',
		   ppr name,
		   char '>'])
\end{code}
  
\begin{code}
chooseDynCostCentres ccs args fvs body
  = let
	use_cc -- cost-centre we record in the object
	  = if currentOrSubsumedCCS ccs
	    then CReg CurCostCentre
	    else mkCCostCentreStack ccs

	blame_cc -- cost-centre on whom we blame the allocation
	  = case (args, fvs, body) of
	      ([], _, StgApp fun [{-no args-}])
		-> mkCCostCentreStack overheadCCS
	      _ -> use_cc

	    -- if it's an utterly trivial RHS, then it must be
	    -- one introduced by boxHigherOrderArgs for profiling,
	    -- so we charge it to "OVERHEAD".

	    -- This looks like a HACK to me --SDM
    in
    (use_cc, blame_cc)
\end{code}
