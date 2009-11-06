-----------------------------------------------------------------------------
--
-- Stg to C--: heap management functions
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmHeap (
	getVirtHp, setVirtHp, setRealHp, 
	getHpRelOffset,	hpRel,

	entryHeapCheck,	altHeapCheck,

	layOutDynConstr, layOutStaticConstr,
	mkVirtHeapOffsets, mkStaticClosureFields, mkStaticClosure,

	allocDynClosure, emitSetDynHdr
    ) where

#include "HsVersions.h"

import StgSyn
import CLabel
import StgCmmLayout
import StgCmmUtils
import StgCmmMonad
import StgCmmProf
import StgCmmTicky
import StgCmmGran
import StgCmmClosure
import StgCmmEnv

import MkZipCfgCmm

import SMRep
import CmmExpr
import CmmUtils
import DataCon
import TyCon
import CostCentre
import Outputable
import Module
import FastString( mkFastString, FastString, fsLit )
import Constants


-----------------------------------------------------------
--		Layout of heap objects
-----------------------------------------------------------

layOutDynConstr, layOutStaticConstr
	:: DataCon -> [(PrimRep, a)]
	-> (ClosureInfo, [(NonVoid a, VirtualHpOffset)])
-- No Void arguments in result

layOutDynConstr    = layOutConstr False
layOutStaticConstr = layOutConstr True

layOutConstr :: Bool -> DataCon -> [(PrimRep, a)]
	     -> (ClosureInfo, [(NonVoid a, VirtualHpOffset)])
layOutConstr is_static data_con args
   = (mkConInfo is_static data_con tot_wds ptr_wds,
      things_w_offsets)
  where
    (tot_wds,		 --  #ptr_wds + #nonptr_wds
     ptr_wds,		 --  #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets False{-not a thunk-} args


-----------------------------------------------------------
--		Initialise dynamic heap objects
-----------------------------------------------------------

allocDynClosure
	:: ClosureInfo
	-> CmmExpr 		-- Cost Centre to stick in the object
	-> CmmExpr 		-- Cost Centre to blame for this alloc
				-- (usually the same; sometimes "OVERHEAD")

	-> [(NonVoid StgArg, VirtualHpOffset)]	-- Offsets from start of the object
					        -- ie Info ptr has offset zero.
					        -- No void args in here
	-> FCode (LocalReg, CmmAGraph)

-- allocDynClosure allocates the thing in the heap, 
-- and modifies the virtual Hp to account for this.
-- The second return value is the graph that sets the value of the
-- returned LocalReg, which should point to the closure after executing
-- the graph.

-- Note [Return a LocalReg]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- allocDynClosure returns a LocalReg, not a (Hp+8) CmmExpr.
-- Reason:
--	...allocate object...
--	obj = Hp + 8	
--	y = f(z)
--	...here obj is still valid,
--	   but Hp+8 means something quite different...


allocDynClosure cl_info use_cc _blame_cc args_w_offsets
  = do	{ virt_hp <- getVirtHp

	-- SAY WHAT WE ARE ABOUT TO DO
	; tickyDynAlloc cl_info
	; profDynAlloc cl_info use_cc	
		-- ToDo: This is almost certainly wrong
		-- We're ignoring blame_cc. But until we've
		-- fixed the boxing hack in chooseDynCostCentres etc,
		-- we're worried about making things worse by "fixing"
		-- this part to use blame_cc!

	-- FIND THE OFFSET OF THE INFO-PTR WORD
	; let	info_offset = virt_hp + 1
		-- info_offset is the VirtualHpOffset of the first
		-- word of the new object
		-- Remember, virtHp points to last allocated word, 
		-- ie 1 *before* the info-ptr word of new object.

		info_ptr = CmmLit (CmmLabel (infoTableLabelFromCI cl_info))

	-- ALLOCATE THE OBJECT
	; base <- getHpRelOffset info_offset
        ; emit (mkComment $ mkFastString "allocDynClosure")
	; emitSetDynHdr base info_ptr  use_cc
	; let (args, offsets) = unzip args_w_offsets
	; cmm_args <- mapM getArgAmode args	-- No void args 
	; hpStore base cmm_args offsets

	-- BUMP THE VIRTUAL HEAP POINTER
	; setVirtHp (virt_hp + closureSize cl_info)
	
	-- Assign to a temporary and return
	-- Note [Return a LocalReg]
	; hp_rel <- getHpRelOffset info_offset
	; getCodeR $ assignTemp hp_rel }

emitSetDynHdr :: CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitSetDynHdr base info_ptr ccs 
  = hpStore base header [0..]
  where
    header :: [CmmExpr]
    header = [info_ptr] ++ dynProfHdr ccs
     	-- ToDo: Gransim stuff
	-- ToDo: Parallel stuff
	-- No ticky header

hpStore :: CmmExpr -> [CmmExpr] -> [VirtualHpOffset] -> FCode ()
-- Store the item (expr,off) in base[off]
hpStore base vals offs
  = emit (catAGraphs (zipWith mk_store vals offs))
  where
    mk_store val off = mkStore (cmmOffsetW base off) val 


-----------------------------------------------------------
--		Layout of static closures
-----------------------------------------------------------

-- Make a static closure, adding on any extra padding needed for CAFs,
-- and adding a static link field if necessary.

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
	| otherwise  = ASSERT(null payload) [mkIntCLit 0]

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
  ++ concatMap padLitToWord payload
  ++ padding_wds
  ++ static_link_field
  ++ saved_info_field
  where
    variable_header_words
	=  staticGranHdr
	++ staticParHdr
	++ staticProfHdr ccs
	++ staticTickyHdr

-- JD: Simon had ellided this padding, but without it the C back end asserts failure.
-- Maybe it's a bad assertion, and this padding is indeed unnecessary?
padLitToWord :: CmmLit -> [CmmLit]
padLitToWord lit = lit : padding pad_length
  where width = typeWidth (cmmLitType lit)
        pad_length = wORD_SIZE - widthInBytes width :: Int

        padding n | n <= 0 = []
                  | n `rem` 2 /= 0 = CmmInt 0 W8  : padding (n-1)
                  | n `rem` 4 /= 0 = CmmInt 0 W16 : padding (n-2)
                  | n `rem` 8 /= 0 = CmmInt 0 W32 : padding (n-4)
                  | otherwise      = CmmInt 0 W64 : padding (n-8)

-----------------------------------------------------------
--		Heap overflow checking
-----------------------------------------------------------

{- Note [Heap checks]
   ~~~~~~~~~~~~~~~~~~
Heap checks come in various forms.  We provide the following entry
points to the runtime system, all of which use the native C-- entry
convention.

  * gc() performs garbage collection and returns
    nothing to its caller

  * A series of canned entry points like
	r = gc_1p( r )
    where r is a pointer.  This performs gc, and
    then returns its argument r to its caller.
    
  * A series of canned entry points like
	gcfun_2p( f, x, y )
    where f is a function closure of arity 2
    This performs garbage collection, keeping alive the
    three argument ptrs, and then tail-calls f(x,y)

These are used in the following circumstances

* entryHeapCheck: Function entry
    (a) With a canned GC entry sequence
        f( f_clo, x:ptr, y:ptr ) {
    	     Hp = Hp+8
    	     if Hp > HpLim goto L
    	     ...
          L: HpAlloc = 8
             jump gcfun_2p( f_clo, x, y ) }
     Note the tail call to the garbage collector;
     it should do no register shuffling  

    (b) No canned sequence
        f( f_clo, x:ptr, y:ptr, ...etc... ) {
    	  T: Hp = Hp+8
    	     if Hp > HpLim goto L
    	     ...
          L: HpAlloc = 8
             call gc() 	-- Needs an info table
	     goto T }

* altHeapCheck: Immediately following an eval
  Started as 
	case f x y of r { (p,q) -> rhs }
  (a) With a canned sequence for the results of f
       (which is the very common case since
       all boxed cases return just one pointer
	   ...
	   r = f( x, y )
	K: 	-- K needs an info table
	   Hp = Hp+8
	   if Hp > HpLim goto L
	   ...code for rhs...

	L: r = gc_1p( r )
	   goto K }

	Here, the info table needed by the call 
	to gc_1p should be the *same* as the
	one for the call to f; the C-- optimiser 
	spots this sharing opportunity)

   (b) No canned sequence for results of f
       Note second info table
	   ...
	   (r1,r2,r3) = call f( x, y )
	K: 
	   Hp = Hp+8
	   if Hp > HpLim goto L
	   ...code for rhs...

	L: call gc()	-- Extra info table here
	   goto K

* generalHeapCheck: Anywhere else
  e.g. entry to thunk
       case branch *not* following eval, 
       or let-no-escape
  Exactly the same as the previous case:

	K: 	-- K needs an info table
	   Hp = Hp+8
	   if Hp > HpLim goto L
	   ...

	L: call gc()
	   goto K
-}

--------------------------------------------------------------
-- A heap/stack check at a function or thunk entry point.

entryHeapCheck :: Maybe LocalReg -- Function (closure environment)
	       -> Int           -- Arity -- not same as length args b/c of voids
	       -> [LocalReg]	-- Non-void args (empty for thunk)
	       -> FCode ()
	       -> FCode ()

entryHeapCheck fun arity args code
  = do updfr_sz <- getUpdFrameOff
       heapCheck True (gc_call updfr_sz) code   -- The 'fun' keeps relevant CAFs alive
  where
    args'     = case fun of Just f  -> f : args
                            Nothing -> args
    arg_exprs = map (CmmReg . CmmLocal) args'
    gc_call updfr_sz
        | arity == 0 = mkJumpGC (CmmReg (CmmGlobal GCEnter1)) arg_exprs updfr_sz
        | otherwise  = case gc_lbl args' of
                         Just _lbl -> panic "StgCmmHeap.entryHeapCheck: gc_lbl not finished"
				     -- mkJumpGC (CmmLit (CmmLabel (mkRtsCodeLabel lbl)))
                                     --         arg_exprs updfr_sz
                         Nothing  -> mkCall generic_gc (GC, GC) [] [] updfr_sz

    gc_lbl :: [LocalReg] -> Maybe FastString
{-
    gc_lbl [reg]
	| isGcPtrType ty  = Just (sLit "stg_gc_unpt_r1") -- "stg_gc_fun_1p"
	| isFloatType ty  = case width of
			      W32 -> Just (sLit "stg_gc_f1") -- "stg_gc_fun_f1"
			      W64 -> Just (sLit "stg_gc_d1") -- "stg_gc_fun_d1"
			      _other -> Nothing
	| otherwise	  = case width of
			      W32 -> Just (sLit "stg_gc_unbx_r1") -- "stg_gc_fun_unbx_r1"
			      W64 -> Just (sLit "stg_gc_l1") -- "stg_gc_fun_unbx_l1"
			      _other -> Nothing	-- Narrow cases
	where
	  ty = localRegType reg
	  width = typeWidth ty
-}

    gc_lbl regs = gc_lbl_ptrs (map (isGcPtrType . localRegType) regs)

    gc_lbl_ptrs :: [Bool] -> Maybe FastString
    -- JD: TEMPORARY -- UNTIL THOSE FUNCTIONS EXIST...
    --gc_lbl_ptrs [True,True]      = Just (sLit "stg_gc_fun_2p")
    --gc_lbl_ptrs [True,True,True] = Just (sLit "stg_gc_fun_3p")
    gc_lbl_ptrs _ = Nothing
    			

altHeapCheck :: [LocalReg] -> FCode a -> FCode a
altHeapCheck regs code
  = do updfr_sz <- getUpdFrameOff
       heapCheck False (gc_call updfr_sz) code
  where
    gc_call updfr_sz
	| null regs = mkCall generic_gc (GC, GC) [] [] updfr_sz

	| Just _gc_lbl <- rts_label regs	-- Canned call
	= panic "StgCmmHeap.altHeapCheck: rts_label not finished"
		-- mkCall    (CmmLit (CmmLabel (mkRtsCodeLabel gc_lbl))) (GC, GC)
		--	    regs (map (CmmReg . CmmLocal) regs) updfr_sz
	| otherwise		-- No canned call, and non-empty live vars
	= mkCall generic_gc (GC, GC) [] [] updfr_sz

{-
    rts_label [reg] 
	| isGcPtrType ty  = Just (sLit "stg_gc_unpt_r1")
	| isFloatType ty  = case width of
			      W32 -> Just (sLit "stg_gc_f1")
			      W64 -> Just (sLit "stg_gc_d1")
			      _other -> Nothing
	| otherwise	  = case width of
			      W32 -> Just (sLit "stg_gc_unbx_r1")
			      W64 -> Just (sLit "stg_gc_l1") -- "stg_gc_fun_unbx_l1"
			      _other -> Nothing	-- Narrow cases
	where
	  ty = localRegType reg
	  width = typeWidth ty
-}

    rts_label _ = Nothing


generic_gc :: CmmExpr	-- The generic GC procedure; no params, no resuls
generic_gc = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "stg_gc_noregs")))
-- JD: TEMPORARY -- UNTIL THOSE FUNCTIONS EXIST...
-- generic_gc = CmmLit (CmmLabel (mkRtsCodeLabel (sLit "stg_gc_fun")))

-------------------------------
heapCheck :: Bool -> CmmAGraph -> FCode a -> FCode a
heapCheck checkStack do_gc code
  = getHeapUsage $ \ hpHw ->
    do	{ emit $ do_checks checkStack hpHw do_gc
	     	-- Emit heap checks, but be sure to do it lazily so 
		-- that the conditionals on hpHw don't cause a black hole
	; tickyAllocHeap hpHw
	; doGranAllocate hpHw
	; setRealHp hpHw
	; code }

do_checks :: Bool       -- Should we check the stack?
          -> WordOff	-- Heap headroom
          -> CmmAGraph	-- What to do on failure
          -> CmmAGraph
do_checks checkStack alloc do_gc
  = withFreshLabel "gc" $ \ loop_id ->
    withFreshLabel "gc" $ \ gc_id   ->
      mkLabel loop_id 
      <*> (let hpCheck = if alloc == 0 then mkNop
                         else mkAssign hpReg bump_hp <*>
                              mkCmmIfThen hp_oflo (save_alloc <*> mkBranch gc_id)
           in if checkStack then
                mkCmmIfThenElse sp_oflo (mkBranch gc_id) hpCheck
              else hpCheck)
      <*> mkComment (mkFastString "outOfLine should follow:")
      <*> outOfLine (mkLabel gc_id 
                     <*> mkComment (mkFastString "outOfLine here")
                     <*> do_gc
                     <*> mkBranch loop_id)
		-- Test for stack pointer exhaustion, then
		-- bump heap pointer, and test for heap exhaustion
		-- Note that we don't move the heap pointer unless the 
		-- stack check succeeds.  Otherwise we might end up
		-- with slop at the end of the current block, which can 
		-- confuse the LDV profiler.
  where
    alloc_lit = CmmLit (mkIntCLit (alloc*wORD_SIZE))	-- Bytes
    bump_hp   = cmmOffsetExprB (CmmReg hpReg) alloc_lit

	-- Sp overflow if (Sp - CmmHighStack < SpLim)
    sp_oflo = CmmMachOp mo_wordULt 
		  [CmmMachOp (MO_Sub (typeWidth (cmmRegType spReg)))
                             [CmmReg spReg, CmmLit CmmHighStackMark],
                   CmmReg spLimReg]
	-- Hp overflow if (Hp > HpLim)
	-- (Hp has been incremented by now)
	-- HpLim points to the LAST WORD of valid allocation space.
    hp_oflo = CmmMachOp mo_wordUGt 
		  [CmmReg hpReg, CmmReg (CmmGlobal HpLim)]

    save_alloc = mkAssign (CmmGlobal HpAlloc) alloc_lit

{-

{- Unboxed tuple alternatives and let-no-escapes (the two most annoying
constructs to generate code for!)  For unboxed tuple returns, there
are an arbitrary number of possibly unboxed return values, some of
which will be in registers, and the others will be on the stack.  We
always organise the stack-resident fields into pointers &
non-pointers, and pass the number of each to the heap check code. -}

unbxTupleHeapCheck 
	:: [(Id, GlobalReg)]	-- Live registers
	-> WordOff	-- no. of stack slots containing ptrs
	-> WordOff	-- no. of stack slots containing nonptrs
	-> CmmAGraph	-- code to insert in the failure path
	-> FCode ()
	-> FCode ()

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
    rts_label	    = CmmLit (CmmLabel (mkRtsCodeLabel (sLit "stg_gc_ut")))


{- Old Gransim comment -- I have no idea whether it still makes sense (SLPJ Sep07)
For GrAnSim the code for doing a heap check and doing a context switch
has been separated. Especially, the HEAP_CHK macro only performs a
heap check. THREAD_CONTEXT_SWITCH should be used for doing a context
switch. GRAN_FETCH_AND_RESCHEDULE must be put at the beginning of
every slow entry code in order to simulate the fetching of
closures. If fetching is necessary (i.e. current closure is not local)
then an automatic context switch is done. -}


When failing a check, we save a return address on the stack and
jump to a pre-compiled code fragment that saves the live registers
and returns to the scheduler.

The return address in most cases will be the beginning of the basic
block in which the check resides, since we need to perform the check
again on re-entry because someone else might have stolen the resource
in the meantime.

%************************************************************************
%*									*
     Generic Heap/Stack Checks - used in the RTS
%*									*
%************************************************************************

\begin{code}
hpChkGen :: CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
hpChkGen bytes liveness reentry
  = do_checks' bytes True assigns stg_gc_gen
  where
    assigns = mkStmts [
    		CmmAssign (CmmGlobal (VanillaReg 9))  liveness,
    		CmmAssign (CmmGlobal (VanillaReg 10)) reentry
		]

-- a heap check where R1 points to the closure to enter on return, and
-- we want to assign to Sp[0] on failure (used in AutoApply.cmm:BUILD_PAP).
hpChkNodePointsAssignSp0 :: CmmExpr -> CmmExpr -> FCode ()
hpChkNodePointsAssignSp0 bytes sp0
  = do_checks' bytes True assign stg_gc_enter1
  where assign = oneStmt (CmmStore (CmmReg spReg) sp0)

stg_gc_gen    = CmmLit (CmmLabel (mkRtsCodeLabel (sLit "stg_gc_gen")))
\end{code}

-}
