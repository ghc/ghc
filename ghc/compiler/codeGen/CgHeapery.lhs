%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgHeapery.lhs,v 1.13 1999/01/26 16:16:33 simonm Exp $
%
\section[CgHeapery]{Heap management functions}

\begin{code}
module CgHeapery (
	fastEntryChecks, altHeapCheck, thunkChecks,
	allocDynClosure

        -- new functions, basically inserting macro calls into Code -- HWL
        ,fetchAndReschedule, yield
    ) where

#include "HsVersions.h"

import AbsCSyn
import CLabel
import CgMonad

import CgStackery	( getFinalStackHW, mkTaggedStkAmodes, mkTagAssts )
import SMRep		( fixedHdrSize )
import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CgUsages		( getVirtAndRealHp, getRealSp, setVirtHp, setRealHp,
			  initHeapUsage
			)
import ClosureInfo	( closureSize, closureGoodStuffSize,
			  slopSize, allocProfilingMsg, ClosureInfo,
			  closureSMRep
			)
import PrimRep		( PrimRep(..), isFollowableRep )
import CmdLineOpts	( opt_SccProfilingOn )
import GlaExts
import Outputable

#ifdef DEBUG
import PprAbsC		( pprMagicId ) -- tmp
#endif
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

-----------------------------------------------------------------------------
A heap/stack check at a fast entry point.

\begin{code}

fastEntryChecks
	:: [MagicId]			-- Live registers
	-> [(VirtualSpOffset,Int)]	-- stack slots to tag
	-> CLabel			-- return point
	-> Bool				-- node points to closure
	-> Code
	-> Code

fastEntryChecks regs tags ret node_points code
  =  mkTagAssts tags			         `thenFC` \tag_assts ->
     getFinalStackHW				 (\ spHw -> 
     getRealSp					 `thenFC` \ sp ->
     let stk_words = spHw - sp in
     initHeapUsage				 (\ hp_words  ->

     ( if all_pointers then -- heap checks are quite easy
	  absC (checking_code stk_words hp_words tag_assts 
		    free_reg (length regs))

       else -- they are complicated

	  -- save all registers on the stack and adjust the stack pointer.
	  -- ToDo: find the initial all-pointer segment and don't save them.

	  mkTaggedStkAmodes sp addrmode_regs 
	  	  `thenFC` \(new_sp, stk_assts, more_tag_assts) ->

	  -- only let the extra stack assignments affect the stack
	  -- high water mark if we were doing a stack check anyway;
	  -- otherwise we end up generating unnecessary stack checks.
	  -- Careful about knot-tying loops!
	  let real_stk_words =  if new_sp - sp > stk_words && stk_words /= 0
					then new_sp - sp
					else stk_words
	  in

	  let adjust_sp = CAssign (CReg Sp) (CAddr (spRel sp new_sp)) in

	  absC (checking_code real_stk_words hp_words 
	            (mkAbstractCs [tag_assts, stk_assts, more_tag_assts,
				   adjust_sp])
	            (CReg node) 0)

      ) `thenC`

      setRealHp hp_words `thenC`
      code))

  where
	
    checking_code stk hp assts ret regs
	| node_points = do_checks_np stk hp assts (regs+1) -- ret not required
        | otherwise   = do_checks    stk hp assts ret regs

    -- When node points to the closure for the function:

    do_checks_np
	:: Int				-- stack headroom
	-> Int				-- heap  headroom
	-> AbstractC			-- assignments to perform on failure
	-> Int				-- number of pointer registers live
	-> AbstractC
    do_checks_np 0 0 _ _ = AbsCNop
    do_checks_np 0 hp_words tag_assts ptrs =
	    CCheck HP_CHK_NP [
		  mkIntCLit hp_words,
		  mkIntCLit ptrs
	         ]
	         tag_assts
    do_checks_np stk_words 0 tag_assts ptrs =
	    CCheck STK_CHK_NP [
		  mkIntCLit stk_words,
		  mkIntCLit ptrs
		 ]
		 tag_assts
    do_checks_np stk_words hp_words tag_assts ptrs =
	    CCheck HP_STK_CHK_NP [
		  mkIntCLit stk_words,
		  mkIntCLit hp_words,
		  mkIntCLit ptrs
		 ]
		 tag_assts

    -- When node doesn't point to the closure (we need an explicit retn addr)

    do_checks 
	:: Int				-- stack headroom
	-> Int				-- heap  headroom
	-> AbstractC			-- assignments to perform on failure
	-> CAddrMode			-- a register to hold the retn addr.
	-> Int				-- number of pointer registers live
	-> AbstractC

    do_checks 0 0 _ _ _ = AbsCNop
    do_checks 0 hp_words tag_assts ret_reg ptrs =
	    CCheck HP_CHK [
		  mkIntCLit hp_words,
		  CLbl ret CodePtrRep,
		  ret_reg,
		  mkIntCLit ptrs
		 ]
		 tag_assts
    do_checks stk_words 0 tag_assts ret_reg ptrs =
	    CCheck STK_CHK [
		  mkIntCLit stk_words,
		  CLbl ret CodePtrRep,
		  ret_reg,
		  mkIntCLit ptrs
		 ]
		 tag_assts
    do_checks stk_words hp_words tag_assts ret_reg ptrs =
	    CCheck HP_STK_CHK [
		  mkIntCLit stk_words,
		  mkIntCLit hp_words,
		  CLbl ret CodePtrRep,
		  ret_reg,
		  mkIntCLit ptrs
		 ]
		 tag_assts

    free_reg  = case length regs + 1 of 
		       IBOX(x) -> CReg (VanillaReg PtrRep x)

    all_pointers = all pointer regs
    pointer (VanillaReg rep _) = isFollowableRep rep
    pointer _ = False

    addrmode_regs = map CReg regs

-- Checking code for thunks is just a special case of fast entry points:

thunkChecks :: CLabel -> Bool -> Code -> Code
thunkChecks ret node_points code = fastEntryChecks [] [] ret node_points code
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
boxed isn't good enough).  Only a PtrRep will do.

For primitive returns, we have an unlifted value in some register
(either R1 or FloatReg1 or DblReg1).  This means using specialised
heap-check code for these cases.

For unboxed tuple returns, there are an arbitrary number of possibly
unboxed return values, some of which will be in registers, and the
others will be on the stack, with gaps left for tagging the unboxed
objects.  If a heap check is required, we need to fill in these tags.

The code below will cover all cases for the x86 architecture (where R1
is the only VanillaReg ever used).  For other architectures, we'll
have to do something about saving and restoring the other registers.

\begin{code}
altHeapCheck 
	:: Bool				-- is an algebraic alternative
	-> [MagicId]			-- live registers
	-> [(VirtualSpOffset,Int)]	-- stack slots to tag
	-> AbstractC
	-> Maybe CLabel			-- ret address if not on top of stack.
	-> Code
	-> Code

-- unboxed tuple alternatives and let-no-escapes (the two most annoying
-- constructs to generate code for!):

altHeapCheck is_fun regs tags fail_code (Just ret_addr) code
  = mkTagAssts tags `thenFC` \tag_assts1 ->
    let tag_assts = mkAbstractCs [fail_code, tag_assts1]
    in
    initHeapUsage (\ hHw -> do_heap_chk hHw tag_assts `thenC` code)
  where
    do_heap_chk words_required tag_assts
      = absC (if words_required == 0
		then  AbsCNop
		else  checking_code tag_assts)  `thenC`
	setRealHp words_required

      where
      	non_void_regs = filter (/= VoidReg) regs

	checking_code tag_assts = 
	  case non_void_regs of

	    -- this will cover all cases for x86
	    [VanillaReg rep ILIT(1)] 

	       | isFollowableRep rep ->
	          CCheck HP_CHK_UT_ALT
		      [mkIntCLit words_required, mkIntCLit 1, mkIntCLit 0,
			CReg (VanillaReg RetRep ILIT(2)),
			CLbl ret_addr RetRep]
		      tag_assts

	       | otherwise ->
	          CCheck HP_CHK_UT_ALT
		      [mkIntCLit words_required, mkIntCLit 0, mkIntCLit 1,
			CReg (VanillaReg RetRep ILIT(2)),
			CLbl ret_addr RetRep]
		      tag_assts

	    several_regs ->
                let liveness = mkRegLiveness several_regs
 		in
		CCheck HP_CHK_GEN
		     [mkIntCLit words_required, 
		      mkIntCLit (IBOX(word2Int# liveness)),
		      CLbl ret_addr RetRep] 
		     tag_assts

-- normal algebraic and primitive case alternatives:

altHeapCheck is_fun regs [] AbsCNop Nothing code
  = initHeapUsage (\ hHw -> do_heap_chk hHw `thenC` code)
  where
    do_heap_chk :: HeapOffset -> Code
    do_heap_chk words_required
      = absC (if words_required == 0
		then  AbsCNop
		else  checking_code)  `thenC`
	setRealHp words_required

      where
        non_void_regs = filter (/= VoidReg) regs

	checking_code = 
          case non_void_regs of

	    -- No regs live: probably a Void return
	    [] ->
	       CCheck HP_CHK_NOREGS [mkIntCLit words_required] AbsCNop

	    -- The SEQ case (polymophic/function typed case branch)
	    [VanillaReg rep ILIT(1)]
		|  rep == PtrRep
 		&& is_fun ->
	          CCheck HP_CHK_SEQ_NP
			[mkIntCLit words_required, mkIntCLit 1{-regs live-}]
			AbsCNop

	    -- R1 is lifted (the common case)
	    [VanillaReg rep ILIT(1)]
	        | rep == PtrRep ->
	          CCheck HP_CHK_NP
			[mkIntCLit words_required, mkIntCLit 1{-regs live-}]
			AbsCNop

	    -- R1 is boxed, but unlifted
		| isFollowableRep rep ->
		  CCheck HP_CHK_UNPT_R1 [mkIntCLit words_required] AbsCNop

	    -- R1 is unboxed
		| otherwise ->
		  CCheck HP_CHK_UNBX_R1 [mkIntCLit words_required] AbsCNop

	    -- FloatReg1
	    [FloatReg ILIT(1)] ->
		  CCheck HP_CHK_F1 [mkIntCLit words_required] AbsCNop

	    -- DblReg1
	    [DoubleReg ILIT(1)] ->
		  CCheck HP_CHK_D1 [mkIntCLit words_required] AbsCNop

	    -- LngReg1
	    [LongReg _ ILIT(1)] ->
		  CCheck HP_CHK_L1 [mkIntCLit words_required] AbsCNop

#ifdef DEBUG
	    _ -> panic ("CgHeapery.altHeapCheck: unimplemented heap-check, live regs = " ++ showSDoc (sep (map pprMagicId non_void_regs)))
#endif

-- build up a bitmap of the live pointer registers

mkRegLiveness :: [MagicId] -> Word#
mkRegLiveness [] = int2Word# 0#
mkRegLiveness (VanillaReg rep i : regs) 
   | isFollowableRep rep = ((int2Word# 1#) `shiftL#` (i -# 1#)) 
				`or#` mkRegLiveness regs
   | otherwise           = mkRegLiveness regs

-- Emit macro for simulating a fetch and then reschedule

fetchAndReschedule ::   [MagicId]               -- Live registers
			-> Bool                 -- Node reqd?
			-> Code

fetchAndReschedule regs node_reqd  =
      if (node `elem` regs || node_reqd)
	then fetch_code `thenC` reschedule_code
	else absC AbsCNop
      where
	all_regs = if node_reqd then node:regs else regs
	liveness_mask = 0 {-XXX: mkLiveRegsMask all_regs-}

	reschedule_code = absC  (CMacroStmt GRAN_RESCHEDULE [
				 mkIntCLit liveness_mask,
				 mkIntCLit (if node_reqd then 1 else 0)])

	 --HWL: generate GRAN_FETCH macro for GrAnSim
	 --     currently GRAN_FETCH and GRAN_FETCH_AND_RESCHEDULE are miai
	fetch_code = absC (CMacroStmt GRAN_FETCH [])
\end{code}

The @GRAN_YIELD@ macro is taken from JSM's  code for Concurrent Haskell. It
allows to context-switch at  places where @node@ is  not alive (it uses the
@Continue@ rather  than the @EnterNodeCode@  function in the  RTS). We emit
this kind of macro at the beginning of the following kinds of basic bocks:
\begin{itemize}
 \item Slow entry code where node is not alive (see @CgClosure.lhs@). Normally 
       we use @fetchAndReschedule@ at a slow entry code.
 \item Fast entry code (see @CgClosure.lhs@).
 \item Alternatives in case expressions (@CLabelledCode@ structures), provided
       that they are not inlined (see @CgCases.lhs@). These alternatives will 
       be turned into separate functions.
\end{itemize}

\begin{code}
yield ::   [MagicId]               -- Live registers
             -> Bool                 -- Node reqd?
             -> Code 

yield regs node_reqd =
      -- NB: node is not alive; that's why we use DO_YIELD rather than 
      --     GRAN_RESCHEDULE 
      yield_code
      where
        all_regs = if node_reqd then node:regs else regs
        liveness_mask = 0 {-XXX: mkLiveRegsMask all_regs-}

        yield_code = absC (CMacroStmt GRAN_YIELD [mkIntCLit liveness_mask])
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
	-> CAddrMode		-- Cost Centre to stick in the object
	-> CAddrMode		-- Cost Centre to blame for this alloc
				-- (usually the same; sometimes "OVERHEAD")

	-> [(CAddrMode, VirtualHeapOffset)]	-- Offsets from start of the object
						-- ie Info ptr has offset zero.
	-> FCode VirtualHeapOffset		-- Returns virt offset of object

allocDynClosure closure_info use_cc blame_cc amodes_with_offsets
  = getVirtAndRealHp				`thenFC` \ (virtHp, realHp) ->

	-- FIND THE OFFSET OF THE INFO-PTR WORD
	-- virtHp points to last allocated word, ie 1 *before* the
	-- info-ptr word of new object.
    let  info_offset = virtHp + 1

	-- do_move IS THE ASSIGNMENT FUNCTION
	 do_move (amode, offset_from_start)
	   = CAssign (CVal (hpRel realHp
				  (info_offset + offset_from_start))
			   (getAmodeRep amode))
		     amode
    in
	-- SAY WHAT WE ARE ABOUT TO DO
    profCtrC (allocProfilingMsg closure_info)
			   [mkIntCLit (closureGoodStuffSize closure_info),
			    mkIntCLit slop_size]	`thenC`

	-- GENERATE THE CODE
    absC ( mkAbstractCs (
	   [ cInitHdr closure_info (hpRel realHp info_offset) use_cc ]
	   ++ (map do_move amodes_with_offsets)))	`thenC`

	-- GENERATE CC PROFILING MESSAGES
    costCentresC SLIT("CCS_ALLOC") [blame_cc, mkIntCLit closure_size]
					 		`thenC`

	-- BUMP THE VIRTUAL HEAP POINTER
    setVirtHp (virtHp + closure_size)			`thenC`

	-- RETURN PTR TO START OF OBJECT
    returnFC info_offset
  where
    closure_size = closureSize closure_info
    slop_size    = slopSize closure_info

-- Avoid hanging on to anything in the CC field when we're not profiling.

cInitHdr closure_info amode cc 
  | opt_SccProfilingOn = CInitHdr closure_info amode cc
  | otherwise          = CInitHdr closure_info amode (panic "absent cc")
	
\end{code}
