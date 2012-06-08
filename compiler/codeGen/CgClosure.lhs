%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgClosure]{Code generation for closures}

This module provides the support code for @StgToAbstractC@ to deal
with {\em closures} on the RHSs of let(rec)s.  See also
@CgCon@, which deals with constructors.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
import CgStackery
import CgProf
import CgTicky
import CgParallel
import CgInfoTbls
import CgCallConv
import CgUtils
import ClosureInfo
import SMRep
import OldCmm
import OldCmmUtils
import CLabel
import StgSyn
import CostCentre	
import Id
import Name
import Module
import ListSetOps
import Util
import BasicTypes
import StaticFlags
import DynFlags
import Outputable
import FastString

import Data.List
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
		-> UpdateFlag
		-> [Id]		-- Args
		-> StgExpr
		-> FCode (Id, CgIdInfo)

cgTopRhsClosure id ccs binder_info upd_flag args body = do
  {	-- LAY OUT THE OBJECT
    let name = idName id
  ; lf_info  <- mkClosureLFInfo id TopLevel [] upd_flag args
  ; srt_info <- getSRTInfo
  ; mod_name <- getModuleName
  ; let descr         = closureDescription mod_name name
	closure_info  = mkClosureInfo True id lf_info 0 0 srt_info descr
	closure_label = mkLocalClosureLabel name $ idCafInfo id
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

cgStdRhsClosure bndr _cc _bndr_info _fvs _args _body lf_info payload
  = do	-- AHA!  A STANDARD-FORM THUNK
  {	-- LAY OUT THE OBJECT
    amodes <- getArgAmodes payload
  ; mod_name <- getModuleName
  ; let (tot_wds, ptr_wds, amodes_w_offsets) 
	    = mkVirtHeapOffsets (isLFThunk lf_info) amodes

	descr	     = closureDescription mod_name (idName bndr)
	closure_info = mkClosureInfo False 	-- Not static
				     bndr lf_info tot_wds ptr_wds 
				     NoC_SRT	-- No SRT for a std-form closure
				     descr
		
--  ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body

	-- BUILD THE OBJECT
  ; heap_offset <- allocDynClosure closure_info curCCS curCCS amodes_w_offsets

	-- RETURN
  ; returnFC (bndr, heapIdInfo bndr heap_offset lf_info) }
\end{code}

Here's the general case.

\begin{code}
cgRhsClosure	:: Id
		-> CostCentreStack	-- Optional cost centre annotation
		-> StgBinderInfo
		-> [Id]			-- Free vars
		-> UpdateFlag
		-> [Id]			-- Args
		-> StgExpr
		-> FCode (Id, CgIdInfo)

cgRhsClosure bndr cc bndr_info fvs upd_flag args body = do
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
	bndr_is_a_fv = bndr `elem` fvs
	reduced_fvs | bndr_is_a_fv = fvs `minusList` [bndr]
		    | otherwise	   = fvs

  ; lf_info <- mkClosureLFInfo bndr NotTopLevel fvs upd_flag args
  ; fv_infos <- mapFCs getCgIdInfo reduced_fvs
  ; srt_info <- getSRTInfo
  ; mod_name <- getModuleName
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
	  let 
              -- A function closure pointer may be tagged, so we
              -- must take it into account when accessing the free variables.
              mbtag       = tagForArity (length args)
              bind_fv (info, offset)
                | Just tag <- mbtag
                = bindNewToUntagNode (cgIdInfoId info) offset (cgIdInfoLF info) tag
                | otherwise
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
--  ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body
  ; amodes_w_offsets <- mapFCs to_amode bind_details
  ; heap_offset <- allocDynClosure closure_info curCCS curCCS amodes_w_offsets

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
closureCodeBody _binder_info cl_info _cc [{- No args i.e. thunk -}] body = do
  { body_absC <- getCgStmts $ do
	{ tickyEnterThunk cl_info
	; ldvEnterClosure cl_info  -- NB: Node always points when profiling
	; thunkWrapper cl_info $ do
		-- We only enter cc after setting up update so
		-- that cc of enclosing scope will be recorded
                -- in the update frame
            { enterCostCentreThunk (CmmReg nodeReg)
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
closureCodeBody _binder_info cl_info cc args body 
  = ASSERT( length args > 0 )
  do { 	-- Get the current virtual Sp (it might not be zero, 
	-- eg. if we're compiling a let-no-escape).
    vSp <- getVirtSp
  ; let (reg_args, other_args) = assignCallRegs (addIdReps args)
	(sp_top, stk_args)     = mkVirtStkOffsets vSp other_args

	-- Allocate the global ticky counter
  ; let ticky_ctr_lbl = mkRednCountsLabel (closureName cl_info) (clHasCafRefs cl_info)
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

        -- Do the business
  ; funWrapper cl_info reg_args reg_save_code $ do
	{ tickyEnterFun cl_info
        ; enterCostCentreFun cc
              (CmmMachOp mo_wordSub [ CmmReg nodeReg
                                    , CmmLit (mkIntCLit (funTag cl_info)) ])
              (node : map snd reg_args) -- live regs

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
     has_caf_refs = clHasCafRefs cl_info
     slow_lbl = mkSlowEntryLabel name has_caf_refs

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
     mk_save (rep,reg) offset = ASSERT( argMachRep rep `cmmEqType` globalRegType reg )
				CmmStore (cmmRegOffW spReg offset) 
					 (CmmReg (CmmGlobal reg))

     stk_adj_pop   = CmmAssign spReg (cmmRegOffW spReg final_stk_offset)
     stk_adj_push  = CmmAssign spReg (cmmRegOffW spReg (- final_stk_offset))
     live_regs     = Just $ map snd reps_w_regs
     jump_to_entry = CmmJump (mkLblExpr (entryLabelFromCI cl_info)) live_regs
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
      	{
          -- Overwrite with black hole if necessary
        ; whenC (blackHoleOnEntry closure_info && node_points)
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
        live        = Just $ map snd arg_regs

  {-
        -- Debugging: check that R1 has the correct tag
  ; let tag = funTag closure_info
  ; whenC (tag /= 0 && node_points) $ do
        l <- newLabelC
        stmtC (CmmCondBranch (CmmMachOp mo_wordEq [cmmGetTag (CmmReg nodeReg),
                                                   CmmLit (mkIntCLit tag)]) l)
        stmtC (CmmStore (CmmLit (mkWordCLit 0)) (CmmLit (mkWordCLit 0)))
        labelC l
  -}

   	-- Enter for Ldv profiling
  ; whenC node_points (ldvEnterClosure closure_info)

	-- GranSim yeild poin
  ; granYield arg_regs node_points

        -- Heap and/or stack checks wrap the function body
  ; funEntryChecks closure_info reg_save_code live fun_body
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
emitBlackHoleCode is_single_entry = do
  dflags <- getDynFlags

  -- Eager blackholing is normally disabled, but can be turned on with
  -- -feager-blackholing.  When it is on, we replace the info pointer
  -- of the thunk with stg_EAGER_BLACKHOLE_info on entry.
  
  -- If we wanted to do eager blackholing with slop filling, we'd need
  -- to do it at the *end* of a basic block, otherwise we overwrite
  -- the free variables in the thunk that we still need.  We have a
  -- patch for this from Andy Cheadle, but not incorporated yet. --SDM
  -- [6/2004]
  --
  -- Previously, eager blackholing was enabled when ticky-ticky was
  -- on. But it didn't work, and it wasn't strictly necessary to bring
  -- back minimal ticky-ticky, so now EAGER_BLACKHOLING is
  -- unconditionally disabled. -- krc 1/2007
  
  -- Note the eager-blackholing check is here rather than in blackHoleOnEntry,
  -- because emitBlackHoleCode is called from CmmParse.

  let  eager_blackholing =  not opt_SccProfilingOn
                         && dopt Opt_EagerBlackHoling dflags
             -- Profiling needs slop filling (to support LDV
             -- profiling), so currently eager blackholing doesn't
             -- work with profiling.

  whenC eager_blackholing $ do
    tickyBlackHole (not is_single_entry)
    stmtsC [
       CmmStore (cmmOffsetW (CmmReg nodeReg) fixedHdrSize)
                (CmmReg (CmmGlobal CurrentTSO)),
       CmmCall (CmmPrim MO_WriteBarrier Nothing) [] [] CmmMayReturn,
       CmmStore (CmmReg nodeReg) (CmmReg (CmmGlobal EagerBlackholeInfo))
     ]
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
  = do
   if not (closureUpdReqd closure_info)
      then do tickyUpdateFrameOmitted; code
      else do
          tickyPushUpdateFrame
          dflags <- getDynFlags
          if blackHoleOnEntry closure_info &&
             not opt_SccProfilingOn && dopt Opt_EagerBlackHoling dflags
               then pushBHUpdateFrame (CmmReg nodeReg) code
               else pushUpdateFrame   (CmmReg nodeReg) code
  
  | otherwise	-- A static closure
  = do 	{ tickyUpdateBhCaf closure_info

	; if closureUpdReqd closure_info
	  then do	-- Blackhole the (updatable) CAF:
		{ upd_closure <- link_caf closure_info True
		; pushBHUpdateFrame upd_closure code }
	  else do
		{ -- krc: removed some ticky-related code here.
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
link_caf cl_info _is_upd = do
  { 	-- Alloc black hole specifying CC_HDR(Node) as the cost centre
  ; let	use_cc   = costCentreFrom (CmmReg nodeReg)
        blame_cc = use_cc
        tso      = CmmReg (CmmGlobal CurrentTSO)
  ; hp_offset <- allocDynClosure bh_cl_info use_cc blame_cc [(tso,fixedHdrSize)]
  ; hp_rel    <- getHpRelOffset hp_offset

	-- Call the RTS function newCAF to add the CAF to the CafList
	-- so that the garbage collector can find them
	-- This must be done *before* the info table pointer is overwritten, 
	-- because the old info table ptr is needed for reversion
  ; ret <- newTemp bWord
  ; emitRtsCallGen [CmmHinted ret NoHint] rtsPackageId (fsLit "newCAF")
      [ CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint,
        CmmHinted (CmmReg nodeReg) AddrHint,
        CmmHinted hp_rel AddrHint ]
      (Just [node])
	-- node is live, so save it.

  -- see Note [atomic CAF entry] in rts/sm/Storage.c
  ; emitIf (CmmMachOp mo_wordEq [ CmmReg (CmmLocal ret), CmmLit zeroCLit]) $
        -- re-enter R1.  Doing this directly is slightly dodgy; we're
        -- assuming lots of things, like the stack pointer hasn't
        -- moved since we entered the CAF.
        let target = entryCode (closureInfoPtr (CmmReg nodeReg)) in
        stmtC (CmmJump target $ Just [node])

  ; returnFC hp_rel }
  where
    bh_cl_info :: ClosureInfo
    bh_cl_info = cafBlackHoleClosureInfo cl_info
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
  = showSDocDumpOneLine (char '<' <>
		    (if isExternalName name
		      then ppr name -- ppr will include the module name prefix
		      else pprModule mod_name <> char '.' <> ppr name) <>
		    char '>')
   -- showSDocDumpOneLine, because we want to see the unique on the Name.
\end{code}
  
