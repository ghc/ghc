%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgHeapery.lhs,v 1.20 2000/01/13 14:33:58 hwloidl Exp $
%
\section[CgHeapery]{Heap management functions}

\begin{code}
module CgHeapery (
	fastEntryChecks, altHeapCheck, thunkChecks,
	allocDynClosure, inPlaceAllocDynClosure

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
import Unique		( Unique )
import CmdLineOpts	( opt_SccProfilingOn, opt_GranMacros )
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

     getTickyCtrLabel `thenFC` \ ticky_ctr ->

     ( if all_pointers then -- heap checks are quite easy
          -- HWL: gran-yield immediately before heap check proper
          --(if node `elem` regs
          --   then yield regs True
          --   else absC AbsCNop ) `thenC`
	  absC (checking_code stk_words hp_words tag_assts 
			free_reg (length regs) ticky_ctr)

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
	            (CReg node) 0 ticky_ctr)

      ) `thenC`

      setRealHp hp_words `thenC`
      code))

  where
	
    checking_code stk hp assts ret regs ctr
        = mkAbstractCs 
	  [ real_check,
            if hp == 0 then AbsCNop 
	    else profCtrAbsC SLIT("TICK_ALLOC_HEAP") 
		  [ mkIntCLit hp, CLbl ctr DataPtrRep ]
	  ]

        where real_check
		  | node_points = do_checks_np stk hp assts (regs+1)
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
	-> Maybe Unique			-- uniq of ret address (possibly)
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
      = getTickyCtrLabel `thenFC` \ ctr ->
	absC ( if words_required == 0
		  then  AbsCNop
		  else  mkAbstractCs 
			[ checking_code tag_assts,
          	          profCtrAbsC SLIT("TICK_ALLOC_HEAP") 
			    [ mkIntCLit words_required, CLbl ctr DataPtrRep ]
			]
	)  `thenC`
	setRealHp words_required

      where
      	non_void_regs = filter (/= VoidReg) regs

	checking_code tag_assts = 
	  case non_void_regs of

{- no: there might be stuff on top of the retn. addr. on the stack.
	    [{-no regs-}] ->
		CCheck HP_CHK_NOREGS
		    [mkIntCLit words_required]
		    tag_assts
-}
	    -- this will cover all cases for x86
	    [VanillaReg rep ILIT(1)] 

	       | isFollowableRep rep ->
	          CCheck HP_CHK_UT_ALT
		      [mkIntCLit words_required, mkIntCLit 1, mkIntCLit 0,
			CReg (VanillaReg RetRep ILIT(2)),
			CLbl (mkReturnInfoLabel ret_addr) RetRep]
		      tag_assts

	       | otherwise ->
	          CCheck HP_CHK_UT_ALT
		      [mkIntCLit words_required, mkIntCLit 0, mkIntCLit 1,
			CReg (VanillaReg RetRep ILIT(2)),
			CLbl (mkReturnInfoLabel ret_addr) RetRep]
		      tag_assts

	    several_regs ->
                let liveness = mkRegLiveness several_regs
 		in
		CCheck HP_CHK_GEN
		     [mkIntCLit words_required, 
		      mkIntCLit (IBOX(word2Int# liveness)),
			-- HP_CHK_GEN needs a direct return address,
			-- not an info table (might be different if
			-- we're not assembly-mangling/tail-jumping etc.)
		      CLbl (mkReturnPtLabel ret_addr) RetRep] 
		     tag_assts

-- normal algebraic and primitive case alternatives:

altHeapCheck is_fun regs [] AbsCNop Nothing code
  = initHeapUsage (\ hHw -> do_heap_chk hHw `thenC` code)
		      
  where
    do_heap_chk :: HeapOffset -> Code
    do_heap_chk words_required
      = getTickyCtrLabel `thenFC` \ ctr ->
	absC ( if words_required == 0
		 then  AbsCNop
		 else  mkAbstractCs 
		       [ checking_code,
          	         profCtrAbsC SLIT("TICK_ALLOC_HEAP") 
			    [ mkIntCLit words_required, CLbl ctr DataPtrRep ]
		       ]
	)  `thenC`
	setRealHp words_required

      where
        non_void_regs = filter (/= VoidReg) regs

	checking_code = 
          case non_void_regs of

	    -- No regs live: probably a Void return
	    [] ->
	       CCheck HP_CHK_NOREGS [mkIntCLit words_required] AbsCNop

	    -- The SEQ case (polymophic/function typed case branch)
	    -- We need this case because the closure in Node won't return
	    -- directly when we enter it (it could be a function), so the
	    -- heap check code needs to push a seq frame on top of the stack.
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
mkRegLiveness []  =  int2Word# 0#
mkRegLiveness (VanillaReg rep i : regs) | isFollowableRep rep 
  =  ((int2Word# 1#) `shiftL#` (i -# 1#)) `or#` mkRegLiveness regs
mkRegLiveness (_ : regs)  =  mkRegLiveness regs

-- The two functions below are only used in a GranSim setup
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
        liveness_mask = mkRegLiveness regs
	reschedule_code = absC  (CMacroStmt GRAN_RESCHEDULE [
                                 mkIntCLit (IBOX(word2Int# liveness_mask)), 
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
   if opt_GranMacros && node_reqd
     then yield_code
     else absC AbsCNop
   where
     -- all_regs = if node_reqd then node:regs else regs
     liveness_mask = mkRegLiveness regs
     yield_code = 
       absC (CMacroStmt GRAN_YIELD 
                          [mkIntCLit (IBOX(word2Int# liveness_mask))])
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
\end{code}

Occasionally we can update a closure in place instead of allocating
new space for it.  This is the function that does the business, assuming:

	- node points to the closure to be overwritten

	- the new closure doesn't contain any pointers if we're
	  using a generational collector.

\begin{code}
inPlaceAllocDynClosure
	:: ClosureInfo
	-> CAddrMode		-- Pointer to beginning of closure
	-> CAddrMode		-- Cost Centre to stick in the object

	-> [(CAddrMode, VirtualHeapOffset)]	-- Offsets from start of the object
						-- ie Info ptr has offset zero.
	-> Code

inPlaceAllocDynClosure closure_info head use_cc amodes_with_offsets
  = let	-- do_move IS THE ASSIGNMENT FUNCTION
	 do_move (amode, offset_from_start)
	   = CAssign (CVal (CIndex head (mkIntCLit offset_from_start) WordRep)
		     	(getAmodeRep amode))
		     amode
    in
	-- GENERATE THE CODE
    absC ( mkAbstractCs (
	   [ CInitHdr closure_info head use_cc ]
	   ++ (map do_move amodes_with_offsets)))

-- Avoid hanging on to anything in the CC field when we're not profiling.

cInitHdr closure_info amode cc 
  | opt_SccProfilingOn = CInitHdr closure_info (CAddr amode) cc
  | otherwise          = CInitHdr closure_info (CAddr amode) (panic "absent cc")
	
\end{code}
