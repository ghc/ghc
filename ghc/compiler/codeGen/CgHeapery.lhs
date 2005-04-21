%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgHeapery.lhs,v 1.46 2005/04/21 15:28:20 simonmar Exp $
%
\section[CgHeapery]{Heap management functions}

\begin{code}
module CgHeapery (
	initHeapUsage, getVirtHp, setVirtHp, setRealHp, 
	getHpRelOffset,	hpRel,

	funEntryChecks, thunkEntryChecks, 
	altHeapCheck, unbxTupleHeapCheck, 
	hpChkGen, hpChkNodePointsAssignSp0,
	stkChkGen, stkChkNodePoints,

	layOutDynConstr, layOutStaticConstr,
	mkVirtHeapOffsets, mkStaticClosureFields, mkStaticClosure,

	allocDynClosure, emitSetDynHdr
    ) where

#include "HsVersions.h"

import Constants	( mIN_UPD_SIZE )
import StgSyn		( AltType(..) )
import CLabel		( CLabel, mkRtsCodeLabel )
import CgUtils		( mkWordCLit, cmmRegOffW, cmmOffsetW,
			  cmmOffsetExprB )
import CgMonad
import CgProf		( staticProfHdr, profDynAlloc, dynProfHdr )
import CgTicky		( staticTickyHdr, tickyDynAlloc, tickyAllocHeap )
import CgParallel	( staticGranHdr, staticParHdr, doGranAllocate )
import CgStackery	( getFinalStackHW, getRealSp )
import CgCallConv	( mkRegLiveness )
import ClosureInfo	( closureSize, staticClosureNeedsLink, 
			  mkConInfo,  closureNeedsUpdSpace,
			  infoTableLabelFromCI, closureLabelFromCI,
			  nodeMustPointToIt, closureLFInfo, 			
			  ClosureInfo )
import SMRep		( CgRep(..), cgRepSizeW, separateByPtrFollowness,
			  WordOff, fixedHdrSize, thunkHdrSize,
			  isVoidArg, primRepToCgRep )

import Cmm		( CmmLit(..), CmmStmt(..), CmmExpr(..), GlobalReg(..),
			  CmmReg(..), hpReg, nodeReg, spReg )
import MachOp		( mo_wordULt, mo_wordUGt, mo_wordSub )
import CmmUtils		( mkIntCLit, CmmStmts, noStmts, oneStmt, plusStmts,
			  mkStmts )
import Id		( Id )
import DataCon		( DataCon )
import TyCon		( tyConPrimRep )
import CostCentre	( CostCentreStack )
import Util		( mapAccumL, filterOut )
import Constants	( wORD_SIZE )
import DynFlags	( DynFlags )
import Outputable

import GLAEXTS

\end{code}


%************************************************************************
%*									*
\subsection[CgUsages-heapery]{Monad things for fiddling with heap usage}
%*									*
%************************************************************************

The heap always grows upwards, so hpRel is easy

\begin{code}
hpRel :: VirtualHpOffset 	-- virtual offset of Hp
      -> VirtualHpOffset 	-- virtual offset of The Thing
      -> WordOff			-- integer word offset
hpRel hp off = off - hp
\end{code}

@initHeapUsage@ applies a function to the amount of heap that it uses.
It initialises the heap usage to zeros, and passes on an unchanged
heap usage.

It is usually a prelude to performing a GC check, so everything must
be in a tidy and consistent state.

rje: Note the slightly suble fixed point behaviour needed here

\begin{code}
initHeapUsage :: (VirtualHpOffset -> Code) -> Code
initHeapUsage fcode
  = do	{ orig_hp_usage <- getHpUsage
	; setHpUsage initHpUsage
	; fixC (\heap_usage2 -> do
		{ fcode (heapHWM heap_usage2)
		; getHpUsage })
	; setHpUsage orig_hp_usage }

setVirtHp :: VirtualHpOffset -> Code
setVirtHp new_virtHp
  = do	{ hp_usage <- getHpUsage
	; setHpUsage (hp_usage {virtHp = new_virtHp}) }

getVirtHp :: FCode VirtualHpOffset
getVirtHp 
  = do	{ hp_usage <- getHpUsage
	; return (virtHp hp_usage) }

setRealHp ::  VirtualHpOffset -> Code
setRealHp new_realHp
  = do	{ hp_usage <- getHpUsage
	; setHpUsage (hp_usage {realHp = new_realHp}) }

getHpRelOffset :: VirtualHpOffset -> FCode CmmExpr
getHpRelOffset virtual_offset
  = do	{ hp_usg <- getHpUsage
	; return (cmmRegOffW hpReg (hpRel (realHp hp_usg) virtual_offset)) }
\end{code}


%************************************************************************
%*									*
		Layout of heap objects
%*									*
%************************************************************************

\begin{code}
layOutDynConstr, layOutStaticConstr
	:: DynFlags
	-> DataCon 	
	-> [(CgRep,a)]
	-> (ClosureInfo,
	    [(a,VirtualHpOffset)])

layOutDynConstr    = layOutConstr False
layOutStaticConstr = layOutConstr True

layOutConstr  is_static dflags data_con args
   = (mkConInfo dflags is_static data_con tot_wds ptr_wds,
      things_w_offsets)
  where
    (tot_wds,		 --  #ptr_wds + #nonptr_wds
     ptr_wds,		 --  #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets False{-not a thunk-} args
\end{code}

@mkVirtHeapOffsets@ always returns boxed things with smaller offsets
than the unboxed things, and furthermore, the offsets in the result
list

\begin{code}
mkVirtHeapOffsets
	  :: Bool		-- True <=> is a thunk
	  -> [(CgRep,a)]	-- Things to make offsets for
	  -> (WordOff,		-- _Total_ number of words allocated
	      WordOff,		-- Number of words allocated for *pointers*
	      [(a, VirtualHpOffset)])
				-- Things with their offsets from start of 
				--  object in order of increasing offset

-- First in list gets lowest offset, which is initial offset + 1.

mkVirtHeapOffsets is_thunk things
  = let non_void_things		      = filterOut (isVoidArg . fst) things
	(ptrs, non_ptrs)    	      = separateByPtrFollowness non_void_things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
    (tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    hdr_size 	| is_thunk   = thunkHdrSize
		| otherwise  = fixedHdrSize

    computeOffset wds_so_far (rep, thing)
      = (wds_so_far + cgRepSizeW rep, (thing, hdr_size + wds_so_far))
\end{code}


%************************************************************************
%*									*
		Lay out a static closure
%*									*
%************************************************************************

Make a static closure, adding on any extra padding needed for CAFs,
and adding a static link field if necessary.

\begin{code}
mkStaticClosureFields 
	:: ClosureInfo 
	-> CostCentreStack 
	-> Bool 		-- Has CAF refs
	-> [CmmLit]		-- Payload
	-> [CmmLit]		-- The full closure
mkStaticClosureFields cl_info ccs caf_refs payload
  = mkStaticClosure info_lbl ccs payload padding_wds 
	static_link_field saved_info_field
  where
    info_lbl = infoTableLabelFromCI cl_info

    -- CAFs must have consistent layout, regardless of whether they
    -- are actually updatable or not.  The layout of a CAF is:
    --
    --        3 saved_info
    --        2 static_link
    --        1 indirectee
    --        0 info ptr
    --
    -- the static_link and saved_info fields must always be in the same
    -- place.  So we use closureNeedsUpdSpace rather than
    -- closureUpdReqd here:

    is_caf = closureNeedsUpdSpace cl_info

    padding_wds
	| not is_caf = []
	| otherwise  = replicate n (mkIntCLit 0) -- a bunch of 0s
	where n = max 0 (mIN_UPD_SIZE - length payload)

    static_link_field
	| is_caf || staticClosureNeedsLink cl_info = [static_link_value]
	| otherwise				   = []

    saved_info_field
	| is_caf     = [mkIntCLit 0]
	| otherwise  = []

	-- for a static constructor which has NoCafRefs, we set the
	-- static link field to a non-zero value so the garbage
	-- collector will ignore it.
    static_link_value
	| caf_refs	= mkIntCLit 0
	| otherwise	= mkIntCLit 1


mkStaticClosure :: CLabel -> CostCentreStack -> [CmmLit]
  -> [CmmLit] -> [CmmLit] -> [CmmLit] -> [CmmLit]
mkStaticClosure info_lbl ccs payload padding_wds static_link_field saved_info_field
  =  [CmmLabel info_lbl]
  ++ variable_header_words
  ++ payload
  ++ padding_wds
  ++ static_link_field
  ++ saved_info_field
  where
    variable_header_words
	=  staticGranHdr
	++ staticParHdr
	++ staticProfHdr ccs
	++ staticTickyHdr
\end{code}

%************************************************************************
%*									*
\subsection[CgHeapery-heap-overflow]{Heap overflow checking}
%*									*
%************************************************************************

The new code  for heapChecks. For GrAnSim the code for doing a heap check
and doing a context switch has been separated. Especially, the HEAP_CHK
macro only performs a heap check. THREAD_CONTEXT_SWITCH should be used for
doing a context switch. GRAN_FETCH_AND_RESCHEDULE must be put at the
beginning of every slow entry code in order to simulate the fetching of
closures. If fetching is necessary (i.e. current closure is not local) then
an automatic context switch is done.

--------------------------------------------------------------
A heap/stack check at a function or thunk entry point.

\begin{code}
funEntryChecks :: ClosureInfo -> CmmStmts -> Code -> Code
funEntryChecks cl_info reg_save_code code 
  = hpStkCheck cl_info True reg_save_code code

thunkEntryChecks :: ClosureInfo -> Code -> Code
thunkEntryChecks cl_info code 
  = hpStkCheck cl_info False noStmts code

hpStkCheck :: ClosureInfo	-- Function closure
	   -> Bool 		-- Is a function? (not a thunk)
	   -> CmmStmts		-- Register saves
	   -> Code
	   -> Code

hpStkCheck cl_info is_fun reg_save_code code
  =  getFinalStackHW	$ \ spHw -> do
	{ sp <- getRealSp
	; let stk_words = spHw - sp
	; initHeapUsage	$ \ hpHw  -> do
	    {	-- Emit heap checks, but be sure to do it lazily so 
		-- that the conditionals on hpHw don't cause a black hole
	      codeOnly $ do
		{ do_checks stk_words hpHw full_save_code rts_label
		; tickyAllocHeap hpHw }
	    ; setRealHp hpHw
	    ; code }
	}
  where
    node_asst 
	| nodeMustPointToIt (closureLFInfo cl_info)
	= noStmts
	| otherwise
	= oneStmt (CmmAssign nodeReg (CmmLit (CmmLabel closure_lbl)))
    closure_lbl = closureLabelFromCI cl_info

    full_save_code = node_asst `plusStmts` reg_save_code

    rts_label | is_fun    = CmmReg (CmmGlobal GCFun)
				-- Function entry point
	      | otherwise = CmmReg (CmmGlobal GCEnter1)
				-- Thunk or case return
	-- In the thunk/case-return case, R1 points to a closure
	-- which should be (re)-entered after GC
\end{code}

Heap checks in a case alternative are nice and easy, provided this is
a bog-standard algebraic case.  We have in our hand:

       * one return address, on the stack,
       * one return value, in Node.

the canned code for this heap check failure just pushes Node on the
stack, saying 'EnterGHC' to return.  The scheduler will return by
entering the top value on the stack, which in turn will return through
the return address, getting us back to where we were.  This is
therefore only valid if the return value is *lifted* (just being
boxed isn't good enough).

For primitive returns, we have an unlifted value in some register
(either R1 or FloatReg1 or DblReg1).  This means using specialised
heap-check code for these cases.

\begin{code}
altHeapCheck 
    :: AltType	-- PolyAlt, PrimAlt, AlgAlt, but *not* UbxTupAlt
		--	(Unboxed tuples are dealt with by ubxTupleHeapCheck)
    -> Code	-- Continuation
    -> Code
altHeapCheck alt_type code
  = initHeapUsage $ \ hpHw -> do
	{ codeOnly $ do
	     { do_checks 0 {- no stack chk -} hpHw
			 noStmts {- nothign to save -}
			 (rts_label alt_type)
	     ; tickyAllocHeap hpHw }
	; setRealHp hpHw
	; code }
  where
    rts_label PolyAlt = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_unpt_r1")))
      	-- Do *not* enter R1 after a heap check in
	-- a polymorphic case.  It might be a function
	-- and the entry code for a function (currently)
	-- applies it
	--
	-- However R1 is guaranteed to be a pointer

    rts_label (AlgAlt tc) = stg_gc_enter1
	-- Enter R1 after the heap check; it's a pointer
 	
    rts_label (PrimAlt tc)
      = CmmLit $ CmmLabel $ 
	case primRepToCgRep (tyConPrimRep tc) of
	  VoidArg   -> mkRtsCodeLabel SLIT( "stg_gc_noregs")
	  FloatArg  -> mkRtsCodeLabel SLIT( "stg_gc_f1")
	  DoubleArg -> mkRtsCodeLabel SLIT( "stg_gc_d1")
	  LongArg   -> mkRtsCodeLabel SLIT( "stg_gc_l1")
				-- R1 is boxed but unlifted: 
	  PtrArg    -> mkRtsCodeLabel SLIT( "stg_gc_unpt_r1")
				-- R1 is unboxed:
	  NonPtrArg -> mkRtsCodeLabel SLIT( "stg_gc_unbx_r1")

    rts_label (UbxTupAlt _) = panic "altHeapCheck"
\end{code}


Unboxed tuple alternatives and let-no-escapes (the two most annoying
constructs to generate code for!)  For unboxed tuple returns, there
are an arbitrary number of possibly unboxed return values, some of
which will be in registers, and the others will be on the stack.  We
always organise the stack-resident fields into pointers &
non-pointers, and pass the number of each to the heap check code.

\begin{code}
unbxTupleHeapCheck 
	:: [(Id, GlobalReg)]	-- Live registers
	-> WordOff	-- no. of stack slots containing ptrs
	-> WordOff	-- no. of stack slots containing nonptrs
	-> CmmStmts	-- code to insert in the failure path
	-> Code
	-> Code

unbxTupleHeapCheck regs ptrs nptrs fail_code code
  -- We can't manage more than 255 pointers/non-pointers 
  -- in a generic heap check.
  | ptrs > 255 || nptrs > 255 = panic "altHeapCheck"
  | otherwise 
  = initHeapUsage $ \ hpHw -> do
	{ codeOnly $ do { do_checks 0 {- no stack check -} hpHw
				    full_fail_code rts_label
			; tickyAllocHeap hpHw }
	; setRealHp hpHw
	; code }
  where
    full_fail_code  = fail_code `plusStmts` oneStmt assign_liveness
    assign_liveness = CmmAssign (CmmGlobal (VanillaReg 9)) 	-- Ho ho ho!
				(CmmLit (mkWordCLit liveness))
    liveness 	    = mkRegLiveness regs ptrs nptrs
    rts_label	    = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("stg_gc_ut")))

\end{code}


%************************************************************************
%*									*
		Heap/Stack Checks.
%*									*
%************************************************************************

When failing a check, we save a return address on the stack and
jump to a pre-compiled code fragment that saves the live registers
and returns to the scheduler.

The return address in most cases will be the beginning of the basic
block in which the check resides, since we need to perform the check
again on re-entry because someone else might have stolen the resource
in the meantime.

\begin{code}
do_checks :: WordOff	-- Stack headroom
	  -> WordOff	-- Heap  headroom
	  -> CmmStmts	-- Assignments to perform on failure
	  -> CmmExpr	-- Rts address to jump to on failure
	  -> Code
do_checks 0 0 _ _   = nopC
do_checks stk hp reg_save_code rts_lbl
  = do_checks' (CmmLit (mkIntCLit (stk*wORD_SIZE))) 
	       (CmmLit (mkIntCLit (hp*wORD_SIZE)))
	 (stk /= 0) (hp /= 0) reg_save_code rts_lbl

-- The offsets are now in *bytes*
do_checks' stk_expr hp_expr stk_nonzero hp_nonzero reg_save_code rts_lbl
  = do	{ doGranAllocate hp_expr

	-- Emit a block for the heap-check-failure code
	; blk_id <- forkLabelledCode $ do
			{ whenC hp_nonzero $
				stmtC (CmmAssign (CmmGlobal HpAlloc) hp_expr)
			; emitStmts reg_save_code
			; stmtC (CmmJump rts_lbl []) }

	-- Check for stack overflow *FIRST*; otherwise
	-- we might bumping Hp and then failing stack oflo
	; whenC stk_nonzero
		(stmtC (CmmCondBranch stk_oflo blk_id))

	; whenC hp_nonzero
		(stmtsC [CmmAssign hpReg 
				(cmmOffsetExprB (CmmReg hpReg) hp_expr),
		        CmmCondBranch hp_oflo blk_id]) 
		-- Bump heap pointer, and test for heap exhaustion
		-- Note that we don't move the heap pointer unless the 
		-- stack check succeeds.  Otherwise we might end up
		-- with slop at the end of the current block, which can 
		-- confuse the LDV profiler.
    }
  where
	-- Stk overflow if (Sp - stk_bytes < SpLim)
    stk_oflo = CmmMachOp mo_wordULt 
		  [CmmMachOp mo_wordSub [CmmReg spReg, stk_expr],
		   CmmReg (CmmGlobal SpLim)]

	-- Hp overflow if (Hpp > HpLim)
	-- (Hp has been incremented by now)
	-- HpLim points to the LAST WORD of valid allocation space.
    hp_oflo = CmmMachOp mo_wordUGt 
		  [CmmReg hpReg, CmmReg (CmmGlobal HpLim)]
\end{code}

%************************************************************************
%*									*
     Generic Heap/Stack Checks - used in the RTS
%*									*
%************************************************************************

\begin{code}
hpChkGen :: CmmExpr -> CmmExpr -> CmmExpr -> Code
hpChkGen bytes liveness reentry
  = do_checks' (CmmLit (mkIntCLit 0)) bytes False True assigns stg_gc_gen
  where
    assigns = mkStmts [
    		CmmAssign (CmmGlobal (VanillaReg 9))  liveness,
    		CmmAssign (CmmGlobal (VanillaReg 10)) reentry
		]

-- a heap check where R1 points to the closure to enter on return, and
-- we want to assign to Sp[0] on failure (used in AutoApply.cmm:BUILD_PAP).
hpChkNodePointsAssignSp0 :: CmmExpr -> CmmExpr -> Code
hpChkNodePointsAssignSp0 bytes sp0
  = do_checks' (CmmLit (mkIntCLit 0)) bytes False True assign stg_gc_enter1
  where assign = oneStmt (CmmStore (CmmReg spReg) sp0)

stkChkGen :: CmmExpr -> CmmExpr -> CmmExpr -> Code
stkChkGen bytes liveness reentry
  = do_checks' bytes (CmmLit (mkIntCLit 0)) True False assigns stg_gc_gen
  where
    assigns = mkStmts [
    		CmmAssign (CmmGlobal (VanillaReg 9))  liveness,
    		CmmAssign (CmmGlobal (VanillaReg 10)) reentry
		]

stkChkNodePoints :: CmmExpr -> Code
stkChkNodePoints bytes
  = do_checks' bytes (CmmLit (mkIntCLit 0)) True False noStmts stg_gc_enter1

stg_gc_gen = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("stg_gc_gen")))
stg_gc_enter1 = CmmReg (CmmGlobal GCEnter1)
\end{code}

%************************************************************************
%*									*
\subsection[initClosure]{Initialise a dynamic closure}
%*									*
%************************************************************************

@allocDynClosure@ puts the thing in the heap, and modifies the virtual Hp
to account for this.

\begin{code}
allocDynClosure
	:: ClosureInfo
	-> CmmExpr 		-- Cost Centre to stick in the object
	-> CmmExpr 		-- Cost Centre to blame for this alloc
				-- (usually the same; sometimes "OVERHEAD")

	-> [(CmmExpr, VirtualHpOffset)]	-- Offsets from start of the object
					-- ie Info ptr has offset zero.
	-> FCode VirtualHpOffset	-- Returns virt offset of object

allocDynClosure cl_info use_cc blame_cc amodes_with_offsets
  = do	{ virt_hp <- getVirtHp

	-- FIND THE OFFSET OF THE INFO-PTR WORD
	; let	info_offset = virt_hp + 1
		-- info_offset is the VirtualHpOffset of the first
		-- word of the new object
		-- Remember, virtHp points to last allocated word, 
		-- ie 1 *before* the info-ptr word of new object.

		info_ptr = CmmLit (CmmLabel (infoTableLabelFromCI cl_info))
		hdr_w_offsets = initDynHdr info_ptr use_cc `zip` [0..]

	-- SAY WHAT WE ARE ABOUT TO DO
	; profDynAlloc cl_info use_cc	
		-- ToDo: This is almost certainly wrong
		-- We're ignoring blame_cc. But until we've
		-- fixed the boxing hack in chooseDynCostCentres etc,
		-- we're worried about making things worse by "fixing"
		-- this part to use blame_cc!

	; tickyDynAlloc cl_info

	-- ALLOCATE THE OBJECT
	; base <- getHpRelOffset info_offset
	; hpStore base (hdr_w_offsets ++ amodes_with_offsets)

	-- BUMP THE VIRTUAL HEAP POINTER
	; setVirtHp (virt_hp + closureSize cl_info)
	
	-- RETURN PTR TO START OF OBJECT
	; returnFC info_offset }


initDynHdr :: CmmExpr 
	   -> CmmExpr		-- Cost centre to put in object
	   -> [CmmExpr]
initDynHdr info_ptr cc
  =  [info_ptr]
     	-- ToDo: Gransim stuff
	-- ToDo: Parallel stuff
  ++ dynProfHdr cc
	-- No ticky header

hpStore :: CmmExpr -> [(CmmExpr, VirtualHpOffset)] -> Code
-- Store the item (expr,off) in base[off]
hpStore base es
  = stmtsC [ CmmStore (cmmOffsetW base off) val 
	   | (val, off) <- es ]

emitSetDynHdr :: CmmExpr -> CmmExpr -> CmmExpr -> Code
emitSetDynHdr base info_ptr ccs 
  = hpStore base (zip (initDynHdr info_ptr ccs) [0..])
\end{code}
