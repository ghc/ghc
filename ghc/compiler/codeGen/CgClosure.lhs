%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgClosure.lhs,v 1.70 2005/05/17 13:47:39 simonmar Exp $
%
\section[CgClosure]{Code generation for closures}

This module provides the support code for @StgToAbstractC@ to deal
with {\em closures} on the RHSs of let(rec)s.  See also
@CgCon@, which deals with constructors.

\begin{code}
module CgClosure ( cgTopRhsClosure, 
		   cgStdRhsClosure, 
		   cgRhsClosure,
		   emitBlackHoleCode,
		   ) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr ( cgExpr )

import CgMonad
import CgBindery
import CgHeapery
import CgStackery	( mkVirtStkOffsets, pushUpdateFrame, getVirtSp,
			  setRealAndVirtualSp )
import CgProf		( chooseDynCostCentres, ldvEnter, enterCostCentre,
			  costCentreFrom )
import CgTicky
import CgParallel	( granYield, granFetchAndReschedule )
import CgInfoTbls	( emitClosureCodeAndInfoTable, getSRTInfo )
import CgCallConv	( assignCallRegs, mkArgDescr )
import CgUtils		( emitDataLits, addIdReps, cmmRegOffW, 
			  emitRtsCallWithVols )
import ClosureInfo	-- lots and lots of stuff
import SMRep		( CgRep, cgRepSizeW, argMachRep, fixedHdrSize, WordOff,
			  idCgRep )
import MachOp		( MachHint(..) )
import Cmm
import CmmUtils		( CmmStmts, mkStmts, oneStmt, plusStmts, noStmts,
			  mkLblExpr )
import CLabel
import StgSyn
import StaticFlags	( opt_DoTickyProfiling )
import CostCentre	
import Id		( Id, idName, idType )
import Name		( Name, isExternalName )
import Module		( Module, pprModule )
import ListSetOps	( minusList )
import Util		( isIn, mapAccumL, zipWithEqual )
import BasicTypes	( TopLevelFlag(..) )
import Constants	( oFFSET_StgInd_indirectee, wORD_SIZE )
import Outputable
import FastString
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
		-> UpdateFlag
		-> [Id]		-- Args
		-> StgExpr
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure id ccs binder_info srt upd_flag args body = do
  {	-- LAY OUT THE OBJECT
    let name = idName id
  ; lf_info  <- mkClosureLFInfo id TopLevel [] upd_flag args
  ; srt_info <- getSRTInfo name srt
  ; mod_name <- moduleName
  ; let descr         = closureDescription mod_name name
	closure_info  = mkClosureInfo True id lf_info 0 0 srt_info descr
	closure_label = mkLocalClosureLabel name
    	cg_id_info    = stableIdInfo id (mkLblExpr closure_label) lf_info
	closure_rep   = mkStaticClosureFields closure_info ccs True []

  	 -- BUILD THE OBJECT, AND GENERATE INFO TABLE (IF NECESSARY)
  ; emitDataLits closure_label closure_rep
  ; forkClosureBody (closureCodeBody binder_info closure_info
				     ccs args body)

  ; returnFC (id, cg_id_info) }
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

cgStdRhsClosure bndr cc bndr_info fvs args body lf_info payload 
  = do	-- AHA!  A STANDARD-FORM THUNK
  {	-- LAY OUT THE OBJECT
    amodes <- getArgAmodes payload
  ; mod_name <- moduleName
  ; let (tot_wds, ptr_wds, amodes_w_offsets) 
	    = mkVirtHeapOffsets (isLFThunk lf_info) amodes

	descr	     = closureDescription mod_name (idName bndr)
	closure_info = mkClosureInfo False 	-- Not static
				     bndr lf_info tot_wds ptr_wds 
				     NoC_SRT	-- No SRT for a std-form closure
				     descr
		
  ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body

	-- BUILD THE OBJECT
  ; heap_offset <- allocDynClosure closure_info use_cc blame_cc amodes_w_offsets

	-- RETURN
  ; returnFC (bndr, heapIdInfo bndr heap_offset lf_info) }
\end{code}

Here's the general case.

\begin{code}
cgRhsClosure	:: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> SRT
		-> [Id]			-- Free vars
		-> UpdateFlag
		-> [Id]			-- Args
		-> StgExpr
		-> FCode (Id, CgIdInfo)

cgRhsClosure bndr cc bndr_info srt fvs upd_flag args body = do
  { 	-- LAY OUT THE OBJECT
	-- If the binder is itself a free variable, then don't store
	-- it in the closure.  Instead, just bind it to Node on entry.
	-- NB we can be sure that Node will point to it, because we
	-- havn't told mkClosureLFInfo about this; so if the binder
	-- _was_ a free var of its RHS, mkClosureLFInfo thinks it *is*
	-- stored in the closure itself, so it will make sure that
	-- Node points to it...
    let
	name 	     = idName bndr
	is_elem	     = isIn "cgRhsClosure"
	bndr_is_a_fv = bndr `is_elem` fvs
	reduced_fvs | bndr_is_a_fv = fvs `minusList` [bndr]
		    | otherwise	   = fvs

  ; lf_info <- mkClosureLFInfo bndr NotTopLevel fvs upd_flag args
  ; fv_infos <- mapFCs getCgIdInfo reduced_fvs
  ; srt_info <- getSRTInfo name srt
  ; mod_name <- moduleName
  ; let	bind_details :: [(CgIdInfo, VirtualHpOffset)]
	(tot_wds, ptr_wds, bind_details) 
	   = mkVirtHeapOffsets (isLFThunk lf_info) (map add_rep fv_infos)

	add_rep info = (cgIdInfoArgRep info, info)

	descr	     = closureDescription mod_name name
	closure_info = mkClosureInfo False	-- Not static
				     bndr lf_info tot_wds ptr_wds
				     srt_info descr

	-- BUILD ITS INFO TABLE AND CODE
  ; forkClosureBody (do
	{	-- Bind the fvs
	  let bind_fv (info, offset) 
		= bindNewToNode (cgIdInfoId info) offset (cgIdInfoLF info)
	; mapCs bind_fv bind_details

	  	-- Bind the binder itself, if it is a free var
	; whenC bndr_is_a_fv (bindNewToReg bndr nodeReg lf_info)
	
		-- Compile the body
	; closureCodeBody bndr_info closure_info cc args body })

	-- BUILD THE OBJECT
  ; let
	to_amode (info, offset) = do { amode <- idInfoToAmode info
				     ; return (amode, offset) }
  ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body
  ; amodes_w_offsets <- mapFCs to_amode bind_details
  ; heap_offset <- allocDynClosure closure_info use_cc blame_cc amodes_w_offsets

	-- RETURN
  ; returnFC (bndr, heapIdInfo bndr heap_offset lf_info) }


mkClosureLFInfo :: Id		-- The binder
		-> TopLevelFlag	-- True of top level
		-> [Id]		-- Free vars
		-> UpdateFlag 	-- Update flag
		-> [Id] 	-- Args
		-> FCode LambdaFormInfo
mkClosureLFInfo bndr top fvs upd_flag args
  | null args = return (mkLFThunk (idType bndr) top fvs upd_flag)
  | otherwise = do { arg_descr <- mkArgDescr (idName bndr) args
		   ; return (mkLFReEntrant top fvs args arg_descr) }
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
NB: Thunks cannot have a primitive type!

\begin{code}
closureCodeBody binder_info cl_info cc [{- No args i.e. thunk -}] body = do
  { body_absC <- getCgStmts $ do
	{ tickyEnterThunk cl_info
	; ldvEnter (CmmReg nodeReg)  -- NB: Node always points when profiling
	; thunkWrapper cl_info $ do
		-- We only enter cc after setting up update so
		-- that cc of enclosing scope will be recorded
		-- in update frame CAF/DICT functions will be
		-- subsumed by this enclosing cc
	    { enterCostCentre cl_info cc body
	    ; cgExpr body }
	}
    
  ; emitClosureCodeAndInfoTable cl_info [] body_absC }
\end{code}

If there is /at least one argument/, then this closure is in
normal form, so there is no need to set up an update frame.

The Macros for GrAnSim are produced at the beginning of the
argSatisfactionCheck (by calling fetchAndReschedule).  There info if
Node points to closure is available. -- HWL

\begin{code}
closureCodeBody binder_info cl_info cc args body 
  = ASSERT( length args > 0 )
  do { 	-- Get the current virtual Sp (it might not be zero, 
	-- eg. if we're compiling a let-no-escape).
    vSp <- getVirtSp
  ; let (reg_args, other_args) = assignCallRegs (addIdReps args)
	(sp_top, stk_args)     = mkVirtStkOffsets vSp other_args

	-- Allocate the global ticky counter
  ; let ticky_ctr_lbl = mkRednCountsLabel (closureName cl_info)
  ; emitTickyCounter cl_info args sp_top

   	-- ...and establish the ticky-counter 
	-- label for this block
  ; setTickyCtrLabel ticky_ctr_lbl $ do

    	-- Emit the slow-entry code
  { reg_save_code <- mkSlowEntryCode cl_info reg_args

	-- Emit the main entry code
  ; blks <- forkProc $
	    mkFunEntryCode cl_info cc reg_args stk_args
			   sp_top reg_save_code body
  ; emitClosureCodeAndInfoTable cl_info [] blks
  }}



mkFunEntryCode :: ClosureInfo
	       -> CostCentreStack
	       -> [(Id,GlobalReg)] 	  -- Args in regs
	       -> [(Id,VirtualSpOffset)]  -- Args on stack
	       -> VirtualSpOffset	  -- Last allocated word on stack
	       -> CmmStmts 		  -- Register-save code in case of GC
	       -> StgExpr
	       -> Code
-- The main entry code for the closure
mkFunEntryCode cl_info cc reg_args stk_args sp_top reg_save_code body = do
  { 	-- Bind args to regs/stack as appropriate,
	-- and record expected position of sps
  ; bindArgsToRegs  reg_args
  ; bindArgsToStack stk_args
  ; setRealAndVirtualSp sp_top

	-- Enter the cost-centre, if required
	-- ToDo: It's not clear why this is outside the funWrapper,
	--	 but the tickyEnterFun is inside. Perhaps we can put
	--	 them together?
  ; enterCostCentre cl_info cc body

	-- Do the business
  ; funWrapper cl_info reg_args reg_save_code $ do
	{ tickyEnterFun cl_info
	; cgExpr body }
  }
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
mkSlowEntryCode :: ClosureInfo -> [(Id,GlobalReg)] -> FCode CmmStmts
-- If this function doesn't have a specialised ArgDescr, we need
-- to generate the function's arg bitmap, slow-entry code, and
-- register-save code for the heap-check failure
-- Here, we emit the slow-entry code, and 
-- return the register-save assignments
mkSlowEntryCode cl_info reg_args
  | Just (_, ArgGen _) <- closureFunInfo cl_info
  = do 	{ emitSimpleProc slow_lbl (emitStmts load_stmts)
	; return save_stmts }
  | otherwise = return noStmts
  where
     name = closureName cl_info
     slow_lbl = mkSlowEntryLabel name

     load_stmts = mkStmts load_assts `plusStmts` mkStmts [stk_adj_pop, jump_to_entry]
     save_stmts = oneStmt stk_adj_push `plusStmts`  mkStmts save_assts

     reps_w_regs :: [(CgRep,GlobalReg)]
     reps_w_regs = [(idCgRep id, reg) | (id,reg) <- reverse reg_args]
     (final_stk_offset, stk_offsets)
	= mapAccumL (\off (rep,_) -> (off + cgRepSizeW rep, off))
		    0 reps_w_regs

     load_assts = zipWithEqual "mk_load" mk_load reps_w_regs stk_offsets
     mk_load (rep,reg) offset = CmmAssign (CmmGlobal reg) 
					  (CmmLoad (cmmRegOffW spReg offset)
						   (argMachRep rep))

     save_assts = zipWithEqual "mk_save" mk_save reps_w_regs stk_offsets
     mk_save (rep,reg) offset = ASSERT( argMachRep rep == globalRegRep reg )
				CmmStore (cmmRegOffW spReg offset) 
					 (CmmReg (CmmGlobal reg))

     stk_adj_pop   = CmmAssign spReg (cmmRegOffW spReg final_stk_offset)
     stk_adj_push  = CmmAssign spReg (cmmRegOffW spReg (- final_stk_offset))
     jump_to_entry = CmmJump (mkLblExpr (enterLocalIdLabel name)) []
\end{code}


%************************************************************************
%*									*
\subsubsection[closure-code-wrappers]{Wrappers around closure code}
%*									*
%************************************************************************

\begin{code}
thunkWrapper:: ClosureInfo -> Code -> Code
thunkWrapper closure_info thunk_code = do
  { let node_points = nodeMustPointToIt (closureLFInfo closure_info)

    -- HWL: insert macros for GrAnSim; 2 versions depending on liveness of node
    -- (we prefer fetchAndReschedule-style context switches to yield ones)
  ; if node_points 
    then granFetchAndReschedule [] node_points 
    else granYield 		[] node_points

        -- Stack and/or heap checks
  ; thunkEntryChecks closure_info $ do
      	{	-- Overwrite with black hole if necessary
	  whenC (blackHoleOnEntry closure_info && node_points)
 	        (blackHoleIt closure_info)
	; setupUpdate closure_info thunk_code }
		-- setupUpdate *encloses* the thunk_code
  }

funWrapper :: ClosureInfo 	-- Closure whose code body this is
	   -> [(Id,GlobalReg)] 	-- List of argument registers (if any)
	   -> CmmStmts		-- reg saves for the heap check failure
	   -> Code		-- Body of function being compiled
	   -> Code
funWrapper closure_info arg_regs reg_save_code fun_body = do
  { let node_points = nodeMustPointToIt (closureLFInfo closure_info)

   	-- Enter for Ldv profiling
  ; whenC node_points (ldvEnter (CmmReg nodeReg))

	-- GranSim yeild poin
  ; granYield arg_regs node_points

        -- Heap and/or stack checks wrap the function body
  ; funEntryChecks closure_info reg_save_code 
		   fun_body
  }
\end{code}


%************************************************************************
%*									*
\subsubsubsection[update-and-BHs]{Update and black-hole wrappers}
%*									*
%************************************************************************


\begin{code}
blackHoleIt :: ClosureInfo -> Code
-- Only called for closures with no args
-- Node points to the closure
blackHoleIt closure_info = emitBlackHoleCode (closureSingleEntry closure_info)

emitBlackHoleCode :: Bool -> Code
emitBlackHoleCode is_single_entry 
  | eager_blackholing = do 
	tickyBlackHole (not is_single_entry)
	stmtC (CmmStore (CmmReg nodeReg) (CmmLit (CmmLabel bh_lbl)))
  | otherwise = 
	nopC
  where
    bh_lbl | is_single_entry = mkRtsDataLabel SLIT("stg_SE_BLACKHOLE_info")
	   | otherwise	     = mkRtsDataLabel SLIT("stg_BLACKHOLE_info")

	-- If we wanted to do eager blackholing with slop filling,
	-- we'd need to do it at the *end* of a basic block, otherwise
	-- we overwrite the free variables in the thunk that we still
	-- need.  We have a patch for this from Andy Cheadle, but not
	-- incorporated yet. --SDM [6/2004]
	--
	-- Profiling needs slop filling (to support LDV profiling), so
	-- currently eager blackholing doesn't work with profiling.
	--
	-- TICKY_TICKY needs EAGER_BLACKHOLING to verify no double-entries of
	-- single-entry thunks.
    eager_blackholing 
	| opt_DoTickyProfiling = True
	| otherwise            = False

\end{code}

\begin{code}
setupUpdate :: ClosureInfo -> Code -> Code	-- Only called for closures with no args
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent enterCostCentre
setupUpdate closure_info code
  | closureReEntrant closure_info
  = code

  | not (isStaticClosure closure_info)
  = if closureUpdReqd closure_info
    then do { tickyPushUpdateFrame;  pushUpdateFrame (CmmReg nodeReg) code }
    else do { tickyUpdateFrameOmitted; code }
 
  | otherwise	-- A static closure
  = do 	{ tickyUpdateBhCaf closure_info

	; if closureUpdReqd closure_info
	  then do	-- Blackhole the (updatable) CAF:
		{ upd_closure <- link_caf closure_info True
	 	; pushUpdateFrame upd_closure code }
	  else do
		{ 	-- No update reqd, you'd think we don't need to 
			-- black-hole it. But when ticky-ticky is on, we 
			-- black-hole it regardless, to catch errors in which
			-- an allegedly single-entry closure is entered twice
			--
			-- We discard the pointer returned by link_caf, because
			-- we don't push an update frame
		  whenC opt_DoTickyProfiling -- Blackhole even a SE CAF
			(link_caf closure_info False >> nopC)
		; tickyUpdateFrameOmitted
		; code }
    }


-----------------------------------------------------------------------------
-- Entering a CAF
--
-- When a CAF is first entered, it creates a black hole in the heap,
-- and updates itself with an indirection to this new black hole.
--
-- We update the CAF with an indirection to a newly-allocated black
-- hole in the heap.  We also set the blocking queue on the newly
-- allocated black hole to be empty.
--
-- Why do we make a black hole in the heap when we enter a CAF?
--    
--     - for a  generational garbage collector, which needs a fast
--       test for whether an updatee is in an old generation or not
--
--     - for the parallel system, which can implement updates more
--       easily if the updatee is always in the heap. (allegedly).
--
-- When debugging, we maintain a separate CAF list so we can tell when
-- a CAF has been garbage collected.

-- newCAF must be called before the itbl ptr is overwritten, since
-- newCAF records the old itbl ptr in order to do CAF reverting
-- (which Hugs needs to do in order that combined mode works right.)
--

-- ToDo [Feb 04]  This entire link_caf nonsense could all be moved
-- into the "newCAF" RTS procedure, which we call anyway, including
-- the allocation of the black-hole indirection closure.
-- That way, code size would fall, the CAF-handling code would 
-- be closer together, and the compiler wouldn't need to know
-- about off_indirectee etc.

link_caf :: ClosureInfo
	 -> Bool		-- True <=> updatable, False <=> single-entry
         -> FCode CmmExpr       -- Returns amode for closure to be updated
-- To update a CAF we must allocate a black hole, link the CAF onto the
-- CAF list, then update the CAF to point to the fresh black hole.
-- This function returns the address of the black hole, so it can be
-- updated with the new value when available.  The reason for all of this
-- is that we only want to update dynamic heap objects, not static ones,
-- so that generational GC is easier.
link_caf cl_info is_upd = do
  { 	-- Alloc black hole specifying CC_HDR(Node) as the cost centre
  ; let	use_cc   = costCentreFrom (CmmReg nodeReg)
        blame_cc = use_cc
  ; hp_offset <- allocDynClosure bh_cl_info use_cc blame_cc []
  ; hp_rel    <- getHpRelOffset hp_offset

	-- Call the RTS function newCAF to add the CAF to the CafList
	-- so that the garbage collector can find them
	-- This must be done *before* the info table pointer is overwritten, 
	-- because the old info table ptr is needed for reversion
  ; emitRtsCallWithVols SLIT("newCAF") [(CmmReg nodeReg,PtrHint)] [node]
	-- node is live, so save it.

	-- Overwrite the closure with a (static) indirection 
	-- to the newly-allocated black hole
  ; stmtsC [ CmmStore (cmmRegOffW nodeReg off_indirectee) hp_rel
	   , CmmStore (CmmReg nodeReg) ind_static_info ]

  ; returnFC hp_rel }
  where
    bh_cl_info :: ClosureInfo
    bh_cl_info | is_upd    = cafBlackHoleClosureInfo   cl_info
	       | otherwise = seCafBlackHoleClosureInfo cl_info

    ind_static_info :: CmmExpr
    ind_static_info = mkLblExpr mkIndStaticInfoLabel

    off_indirectee :: WordOff
    off_indirectee = fixedHdrSize + oFFSET_StgInd_indirectee*wORD_SIZE
\end{code}


%************************************************************************
%*									*
\subsection[CgClosure-Description]{Profiling Closure Description.}
%*									*
%************************************************************************

For "global" data constructors the description is simply occurrence
name of the data constructor itself.  Otherwise it is determined by
@closureDescription@ from the let binding information.

\begin{code}
closureDescription :: Module		-- Module
		   -> Name		-- Id of closure binding
		   -> String
	-- Not called for StgRhsCon which have global info tables built in
	-- CgConTbls.lhs with a description generated from the data constructor
closureDescription mod_name name
  = showSDocDump (char '<' <>
		    (if isExternalName name
		      then ppr name -- ppr will include the module name prefix
		      else pprModule mod_name <> char '.' <> ppr name) <>
		    char '>')
   -- showSDocDump, because we want to see the unique on the Name.
\end{code}
  
