-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: bindings
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmBind ( 
	cgTopRhsClosure, 
	cgBind,
	emitBlackHoleCode
  ) where

#include "HsVersions.h"

import StgCmmMonad
import StgCmmExpr
import StgCmmEnv
import StgCmmCon
import StgCmmHeap
import StgCmmProf
import StgCmmTicky
import StgCmmGran
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure

import MkZipCfgCmm
import CoreSyn		( AltCon(..) )
import SMRep
import Cmm
import CmmUtils
import CLabel
import StgSyn
import CostCentre	
import Id
import Name
import Module
import ListSetOps
import Util
import BasicTypes
import Constants
import Outputable
import FastString
import Maybes

import Data.List

------------------------------------------------------------------------
--		Top-level bindings
------------------------------------------------------------------------

-- For closures bound at top level, allocate in static space.
-- They should have no free variables.

cgTopRhsClosure :: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> UpdateFlag
		-> SRT
		-> [Id]		-- Args
		-> StgExpr
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure id ccs binder_info upd_flag srt args body = do
  {	-- LAY OUT THE OBJECT
    let name = idName id
  ; lf_info  <- mkClosureLFInfo id TopLevel [] upd_flag args
  ; srt_info <- getSRTInfo srt
  ; mod_name <- getModuleName
  ; let descr         = closureDescription mod_name name
	closure_info  = mkClosureInfo True id lf_info 0 0 srt_info descr
	closure_label = mkLocalClosureLabel name (idCafInfo id)
    	cg_id_info    = litIdInfo id lf_info (CmmLabel closure_label)
	closure_rep   = mkStaticClosureFields closure_info ccs True []

  	 -- BUILD THE OBJECT, AND GENERATE INFO TABLE (IF NECESSARY)
  ; emitDataLits closure_label closure_rep
  ; forkClosureBody $ do
	{ node <- bindToReg id lf_info
	; closureCodeBody binder_info closure_info
			  ccs srt_info node args body }

  ; returnFC (id, cg_id_info) }

------------------------------------------------------------------------
--		Non-top-level bindings
------------------------------------------------------------------------

cgBind :: StgBinding -> FCode ()
cgBind (StgNonRec name rhs)
  = do	{ (name, info) <- cgRhs name rhs
	; addBindC name info }

cgBind (StgRec pairs)
  = do	{ new_binds <- fixC (\ new_binds -> 
		do { addBindsC new_binds
		   ; listFCs [ cgRhs b e | (b,e) <- pairs ] })
	; addBindsC new_binds }

--------------------
cgRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
   -- The Id is passed along so a binding can be set up

cgRhs name (StgRhsCon maybe_cc con args)
  = do	{ idinfo <- buildDynCon name maybe_cc con args
	; return (name, idinfo) }

cgRhs name (StgRhsClosure cc bi fvs upd_flag srt args body)
  = mkRhsClosure name cc bi fvs upd_flag srt args body

------------------------------------------------------------------------
--		Non-constructor right hand sides
------------------------------------------------------------------------

mkRhsClosure :: Id -> CostCentreStack -> StgBinderInfo
	     -> [Id]			-- Free vars
	     -> UpdateFlag -> SRT
	     -> [Id]			-- Args
	     -> StgExpr
	     -> FCode (Id, CgIdInfo) 

{- mkRhsClosure looks for two special forms of the right-hand side:
	a) selector thunks
	b) AP thunks

If neither happens, it just calls mkClosureLFInfo.  You might think
that mkClosureLFInfo should do all this, but it seems wrong for the
latter to look at the structure of an expression

Note [Selectors]
~~~~~~~~~~~~~~~~
We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
	 case the_fv of
	   con a_1 ... a_n -> a_i

Note [Ap thunks]
~~~~~~~~~~~~~~~~
A more generic AP thunk of the form

	x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.

-}

---------- Note [Selectors] ------------------
mkRhsClosure	bndr cc bi
		[the_fv]   		-- Just one free var
		upd_flag		-- Updatable thunk
		_srt
		[]			-- A thunk
		body@(StgCase (StgApp scrutinee [{-no args-}])
		      _ _ _ _   -- ignore uniq, etc.
		      (AlgAlt _)
		      [(DataAlt con, params, _use_mask,
			    (StgApp selectee [{-no args-}]))])
  |  the_fv == scrutinee		-- Scrutinee is the only free variable
  && maybeToBool maybe_offset		-- Selectee is a component of the tuple
  && offset_into_int <= mAX_SPEC_SELECTEE_SIZE	-- Offset is small enough
  = -- NOT TRUE: ASSERT(is_single_constructor)
    -- The simplifier may have statically determined that the single alternative
    -- is the only possible case and eliminated the others, even if there are
    -- other constructors in the datatype.  It's still ok to make a selector
    -- thunk in this case, because we *know* which constructor the scrutinee
    -- will evaluate to.
    --
    -- srt is discarded; it must be empty
    cgStdThunk bndr cc bi body lf_info [StgVarArg the_fv]
  where
    lf_info 		  = mkSelectorLFInfo bndr offset_into_int
				 (isUpdatable upd_flag)
    (_, params_w_offsets) = layOutDynConstr con (addIdReps params)
			-- Just want the layout
    maybe_offset	  = assocMaybe params_w_offsets selectee
    Just the_offset 	  = maybe_offset
    offset_into_int       = the_offset - fixedHdrSize

---------- Note [Ap thunks] ------------------
mkRhsClosure    bndr cc bi
		fvs
		upd_flag
		_srt
		[]			-- No args; a thunk
		body@(StgApp fun_id args)

  | args `lengthIs` (arity-1)
 	&& all isFollowableArg (map idCgRep fvs) 
 	&& isUpdatable upd_flag
 	&& arity <= mAX_SPEC_AP_SIZE 

 		   -- Ha! an Ap thunk
  = cgStdThunk bndr cc bi body lf_info payload
  where
	lf_info = mkApLFInfo bndr upd_flag arity
	-- the payload has to be in the correct order, hence we can't
 	-- just use the fvs.
	payload = StgVarArg fun_id : args
	arity 	= length fvs

---------- Default case ------------------
mkRhsClosure bndr cc bi fvs upd_flag srt args body
  = do	{ 	-- LAY OUT THE OBJECT
	-- If the binder is itself a free variable, then don't store
	-- it in the closure.  Instead, just bind it to Node on entry.
	-- NB we can be sure that Node will point to it, because we
	-- havn't told mkClosureLFInfo about this; so if the binder
	-- _was_ a free var of its RHS, mkClosureLFInfo thinks it *is*
	-- stored in the closure itself, so it will make sure that
	-- Node points to it...
	; let
		is_elem	     = isIn "cgRhsClosure"
		bndr_is_a_fv = bndr `is_elem` fvs
		reduced_fvs | bndr_is_a_fv = fvs `minusList` [bndr]
			    | otherwise	   = fvs

		
	-- MAKE CLOSURE INFO FOR THIS CLOSURE
	; lf_info <- mkClosureLFInfo bndr NotTopLevel fvs upd_flag args
	; mod_name <- getModuleName
	; c_srt <- getSRTInfo srt
	; let	name  = idName bndr
		descr = closureDescription mod_name name
		fv_details :: [(Id, VirtualHpOffset)]
		(tot_wds, ptr_wds, fv_details) 
		   = mkVirtHeapOffsets (isLFThunk lf_info) 
				       (addIdReps reduced_fvs)
		closure_info = mkClosureInfo False	-- Not static
					     bndr lf_info tot_wds ptr_wds
					     c_srt descr

	-- BUILD ITS INFO TABLE AND CODE
	; forkClosureBody $ do
		{   -- Bind the binder itself
		    -- It does no harm to have it in the envt even if
		    -- it's not a free variable; and we need a reg for it
		  node <- bindToReg bndr lf_info

		    -- Bind the free variables
		; mapCs (bind_fv node) fv_details
	
		    -- And compile the body
		; closureCodeBody bi closure_info cc c_srt node args body }

	-- BUILD THE OBJECT
	; (use_cc, blame_cc) <- chooseDynCostCentres cc args body
        ; emit (mkComment $ mkFastString "calling allocDynClosure")
	; tmp <- allocDynClosure closure_info use_cc blame_cc 
				 (mapFst StgVarArg fv_details)
	
	-- RETURN
	; return (bndr, regIdInfo bndr lf_info tmp) }
  where
      -- A function closure pointer may be tagged, so we
      -- must take it into account when accessing the free variables.
     tag = tagForArity (length args)

     bind_fv node (id, off) 
	= do { reg <- rebindToReg id
	     ; emit $ mkTaggedObjectLoad reg node off tag }

-------------------------
cgStdThunk
	:: Id
	-> CostCentreStack	-- Optional cost centre annotation
	-> StgBinderInfo	-- XXX: not used??
	-> StgExpr
	-> LambdaFormInfo
	-> [StgArg]			-- payload
	-> FCode (Id, CgIdInfo)

cgStdThunk bndr cc _bndr_info body lf_info payload
  = do	-- AHA!  A STANDARD-FORM THUNK
  {	-- LAY OUT THE OBJECT
    mod_name <- getModuleName
  ; let (tot_wds, ptr_wds, payload_w_offsets) 
	    = mkVirtHeapOffsets (isLFThunk lf_info) (addArgReps payload)

	descr = closureDescription mod_name (idName bndr)
	closure_info = mkClosureInfo False 	-- Not static
				     bndr lf_info tot_wds ptr_wds 
				     NoC_SRT	-- No SRT for a std-form closure
				     descr

  ; (use_cc, blame_cc) <- chooseDynCostCentres cc [{- no args-}] body

	-- BUILD THE OBJECT
  ; tmp <- allocDynClosure closure_info use_cc blame_cc payload_w_offsets

	-- RETURN
  ; returnFC (bndr, regIdInfo bndr lf_info tmp) }

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


------------------------------------------------------------------------
--		The code for closures}
------------------------------------------------------------------------

closureCodeBody :: StgBinderInfo   -- XXX: unused?
		-> ClosureInfo	   -- Lots of information about this closure
		-> CostCentreStack -- Optional cost centre attached to closure
		-> C_SRT
		-> LocalReg	   -- The closure itself; first argument
				   -- The Id is in scope already, bound to this reg
	 	-> [Id]
		-> StgExpr
		-> FCode ()

{- There are two main cases for the code for closures.  

* If there are *no arguments*, then the closure is a thunk, and not in
  normal form. So it should set up an update frame (if it is
  shared). NB: Thunks cannot have a primitive type!

* If there is *at least one* argument, then this closure is in
  normal form, so there is no need to set up an update frame.

  The Macros for GrAnSim are produced at the beginning of the
  argSatisfactionCheck (by calling fetchAndReschedule).  
  There info if Node points to closure is available. -- HWL -}

closureCodeBody _binder_info cl_info cc srt node args body 
  | null args	-- No args i.e. thunk
  = do  { code <- getCode $ thunkCode cl_info cc srt node body
	; emitClosureCodeAndInfoTable cl_info [node] code }

closureCodeBody _binder_info cl_info cc srt node args body 
  = ASSERT( length args > 0 )
    do	{ 	-- Allocate the global ticky counter,
	   	-- and establish the ticky-counter 
		-- label for this block
	  let ticky_ctr_lbl = mkRednCountsLabel (closureName cl_info) $ clHasCafRefs cl_info
	; emitTickyCounter cl_info args
	; setTickyCtrLabel ticky_ctr_lbl $ do

--	-- XXX: no slow-entry code for now
--    	-- Emit the slow-entry code
--	{ reg_save_code <- mkSlowEntryCode cl_info reg_args

	-- Emit the main entry code
	; let node_points = nodeMustPointToIt (closureLFInfo cl_info)
	; arg_regs <- bindArgsToRegs args
	; blks <- forkProc $ getCode $ do
		{ enterCostCentre cl_info cc body
		; tickyEnterFun cl_info
		; whenC node_points (ldvEnterClosure cl_info)
		; granYield arg_regs node_points

			-- Main payload
		; entryHeapCheck node arg_regs srt $
		  cgExpr body }

	; emitClosureCodeAndInfoTable cl_info (node:arg_regs) blks
  }

{-
-----------------------------------------
-- The "slow entry" code for a function.  This entry point takes its
-- arguments on the stack.  It loads the arguments into registers
-- according to the calling convention, and jumps to the function's
-- normal entry point.  The function's closure is assumed to be in
-- R1/node.
-- 
-- The slow entry point is used in two places:
-- 
-- (a) unknown calls: eg. stg_PAP_entry 
--  (b) returning from a heap-check failure

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
     mk_save (rep,reg) offset = ASSERT( argMachRep rep == globalRegType reg )
				CmmStore (cmmRegOffW spReg offset) 
					 (CmmReg (CmmGlobal reg))

     stk_adj_pop   = CmmAssign spReg (cmmRegOffW spReg final_stk_offset)
     stk_adj_push  = CmmAssign spReg (cmmRegOffW spReg (- final_stk_offset))
     jump_to_entry = CmmJump (mkLblExpr (enterLocalIdLabel name)) []
-}

-----------------------------------------
thunkCode :: ClosureInfo -> CostCentreStack -> C_SRT -> LocalReg -> StgExpr -> FCode ()
thunkCode cl_info cc srt node body 
  = do	{ let node_points = nodeMustPointToIt (closureLFInfo cl_info)

   	; tickyEnterThunk cl_info
	; ldvEnterClosure cl_info  -- NB: Node always points when profiling
	; granThunk node_points

        -- Heap overflow check
	; entryHeapCheck node [] srt $ do
      	{	-- Overwrite with black hole if necessary
		-- but *after* the heap-overflow check
	  whenC (blackHoleOnEntry cl_info && node_points)
 	        (blackHoleIt cl_info)

		-- Push update frame
	; setupUpdate cl_info node

	    	-- We only enter cc after setting up update so
	    	-- that cc of enclosing scope will be recorded
	    	-- in update frame CAF/DICT functions will be
	    	-- subsumed by this enclosing cc
	; enterCostCentre cl_info cc body

	; cgExpr body } }


------------------------------------------------------------------------
--		Update and black-hole wrappers
------------------------------------------------------------------------

blackHoleIt :: ClosureInfo -> FCode ()
-- Only called for closures with no args
-- Node points to the closure
blackHoleIt closure_info = emitBlackHoleCode (closureSingleEntry closure_info)

emitBlackHoleCode :: Bool -> FCode ()
emitBlackHoleCode is_single_entry 
  | eager_blackholing = do 
	tickyBlackHole (not is_single_entry)
	emit (mkStore (CmmReg nodeReg) (CmmLit (CmmLabel bh_lbl)))
  | otherwise = 
	nopC
  where
    bh_lbl | is_single_entry = mkRtsDataLabel (sLit "stg_SE_BLACKHOLE_info")
	   | otherwise	     = mkRtsDataLabel (sLit "stg_BLACKHOLE_info")

	-- If we wanted to do eager blackholing with slop filling,
	-- we'd need to do it at the *end* of a basic block, otherwise
	-- we overwrite the free variables in the thunk that we still
	-- need.  We have a patch for this from Andy Cheadle, but not
	-- incorporated yet. --SDM [6/2004]
	--
	-- Profiling needs slop filling (to support LDV profiling), so
	-- currently eager blackholing doesn't work with profiling.
	--
        -- Previously, eager blackholing was enabled when ticky-ticky
        -- was on. But it didn't work, and it wasn't strictly necessary 
        -- to bring back minimal ticky-ticky, so now EAGER_BLACKHOLING 
        -- is unconditionally disabled. -- krc 1/2007

    eager_blackholing = False 

setupUpdate :: ClosureInfo -> LocalReg -> FCode ()
	-- Nota Bene: this function does not change Node (even if it's a CAF),
	-- so that the cost centre in the original closure can still be
	-- extracted by a subsequent enterCostCentre
setupUpdate closure_info node
  | closureReEntrant closure_info
  = return ()

  | not (isStaticClosure closure_info)
  = if closureUpdReqd closure_info
    then do { tickyPushUpdateFrame; pushUpdateFrame node }
    else tickyUpdateFrameOmitted
 
  | otherwise	-- A static closure
  = do 	{ tickyUpdateBhCaf closure_info

	; if closureUpdReqd closure_info
	  then do	-- Blackhole the (updatable) CAF:
		{ upd_closure <- link_caf closure_info True
		; pushUpdateFrame upd_closure }
	  else tickyUpdateFrameOmitted
    }

pushUpdateFrame :: LocalReg -> FCode ()
pushUpdateFrame cl_reg
  = emit (mkAddToContext (mkLblExpr mkUpdInfoLabel) 
			 [CmmReg (CmmLocal cl_reg)])

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
         -> FCode LocalReg      -- Returns amode for closure to be updated
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
  ; hp_rel <- allocDynClosure bh_cl_info use_cc blame_cc []

	-- Call the RTS function newCAF to add the CAF to the CafList
	-- so that the garbage collector can find them
	-- This must be done *before* the info table pointer is overwritten, 
	-- because the old info table ptr is needed for reversion
  ; emitRtsCallWithVols (sLit "newCAF") [(CmmReg nodeReg,AddrHint)] [node] False
	-- node is live, so save it.

	-- Overwrite the closure with a (static) indirection 
	-- to the newly-allocated black hole
  ; emit (mkStore (cmmRegOffW nodeReg off_indirectee) (CmmReg (CmmLocal hp_rel)) <*>
	  mkStore (CmmReg nodeReg) ind_static_info)

  ; return hp_rel }
  where
    bh_cl_info :: ClosureInfo
    bh_cl_info | is_upd    = cafBlackHoleClosureInfo   cl_info
	       | otherwise = seCafBlackHoleClosureInfo cl_info

    ind_static_info :: CmmExpr
    ind_static_info = mkLblExpr mkIndStaticInfoLabel

    off_indirectee :: WordOff
    off_indirectee = fixedHdrSize + oFFSET_StgInd_indirectee*wORD_SIZE


------------------------------------------------------------------------
--		Profiling 
------------------------------------------------------------------------

-- For "global" data constructors the description is simply occurrence
-- name of the data constructor itself.  Otherwise it is determined by
-- @closureDescription@ from the let binding information.

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
  
