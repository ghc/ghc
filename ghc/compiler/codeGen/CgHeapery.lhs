%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CgHeapery]{Heap management functions}

\begin{code}
#include "HsVersions.h"

module CgHeapery (
	heapCheck,
	allocHeap, allocDynClosure

        -- new functions, basically inserting macro calls into Code -- HWL
        , heapCheckOnly, fetchAndReschedule, yield
    ) where

IMP_Ubiq(){-uitous-}

import AbsCSyn
import CgMonad

import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CgUsages		( getVirtAndRealHp, setVirtHp, setRealHp,
			  initHeapUsage
			)
import ClosureInfo	( closureSize, closureHdrSize, closureGoodStuffSize,
			  slopSize, allocProfilingMsg, closureKind
			)
import HeapOffs		( isZeroOff, addOff, intOff,
			  SYN_IE(VirtualHeapOffset)
			)
import PrimRep		( PrimRep(..) )
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

\begin{code}
heapCheck :: [MagicId]          -- Live registers
	  -> Bool               -- Node reqd after GC?
	  -> Code
	  -> Code

heapCheck = heapCheck' False

heapCheckOnly :: [MagicId]          -- Live registers
		 -> Bool               -- Node reqd after GC?
		 -> Code
		 -> Code

heapCheckOnly = heapCheck' False

-- May be emit context switch and emit heap check macro

heapCheck' ::   Bool                    -- context switch here?
		-> [MagicId]            -- Live registers
		-> Bool                 -- Node reqd after GC?
		-> Code
		-> Code

heapCheck' do_context_switch regs node_reqd code
  = initHeapUsage (\ hHw -> do_heap_chk hHw `thenC` code)
  where

    do_heap_chk :: HeapOffset -> Code
    do_heap_chk words_required
      =
	-- HWL:: absC (CComment "Forced heap check --- HWL")  `thenC`
	--absC  (if do_context_switch
	--         then context_switch_code
	--         else AbsCNop)                                 `thenC`

	absC (if do_context_switch && not (isZeroOff words_required)
		then context_switch_code
		else AbsCNop)                                   `thenC`
	absC (if isZeroOff(words_required)
		then  AbsCNop
		else  checking_code)  `thenC`

	-- HWL was here:
	--  For GrAnSim we want heap checks even if no heap is allocated in
	--  the basic block to make context switches possible.
	--  So, the if construct has been replaced by its else branch.

	    -- The test is *inside* the absC, to avoid black holes!

	-- Now we have set up the real heap pointer and checked there is
	-- enough space. It remains only to reflect this in the environment

	setRealHp words_required

	    -- The "word_required" here is a fudge.
	    -- *** IT DEPENDS ON THE DIRECTION ***, and on
	    -- whether the Hp is moved the whole way all
	    -- at once or not.
      where
	all_regs = if node_reqd then node:regs else regs
	liveness_mask = mkLiveRegsMask all_regs

	maybe_context_switch = if do_context_switch
				then context_switch_code
				else AbsCNop

	context_switch_code = CMacroStmt THREAD_CONTEXT_SWITCH [
			      mkIntCLit liveness_mask,
			      mkIntCLit (if node_reqd then 1 else 0)]

	-- Good old heap check (excluding context switch)
	checking_code = CMacroStmt HEAP_CHK [
			mkIntCLit liveness_mask,
			COffset words_required,
			mkIntCLit (if node_reqd then 1 else 0)]

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
	liveness_mask = mkLiveRegsMask all_regs

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
        liveness_mask = mkLiveRegsMask all_regs

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
    let  info_offset = addOff virtHp (intOff 1)

	-- do_move IS THE ASSIGNMENT FUNCTION
	 do_move (amode, offset_from_start)
	   = CAssign (CVal (HpRel realHp
				  (info_offset `addOff` offset_from_start))
			   (getAmodeRep amode))
		     amode
    in
	-- SAY WHAT WE ARE ABOUT TO DO
    profCtrC (allocProfilingMsg closure_info)
			   [COffset   (closureHdrSize closure_info),
			    mkIntCLit (closureGoodStuffSize closure_info),
			    mkIntCLit slop_size,
			    COffset   closure_size]	`thenC`

	-- GENERATE THE CODE
    absC ( mkAbstractCs (
	   [ CInitHdr closure_info (HpRel realHp info_offset) use_cc False ]
	   ++ (map do_move amodes_with_offsets)))	`thenC`

	-- GENERATE CC PROFILING MESSAGES
    costCentresC SLIT("CC_ALLOC") [blame_cc,
			     COffset closure_size,
			     CLitLit (_PK_ (closureKind closure_info)) IntRep]
					 		`thenC`

	-- BUMP THE VIRTUAL HEAP POINTER
    setVirtHp (virtHp `addOff` closure_size)		`thenC`

	-- RETURN PTR TO START OF OBJECT
    returnFC info_offset
  where
    closure_size = closureSize closure_info
    slop_size    = slopSize closure_info
\end{code}

%************************************************************************
%*									*
\subsection{Allocate uninitialized heap space}
%*									*
%************************************************************************

\begin{code}
allocHeap :: HeapOffset		-- Size of the space required
	  -> FCode CAddrMode	-- Addr mode for first word of object

allocHeap space
  = getVirtAndRealHp				`thenFC` \ (virtHp, realHp) ->
    let block_start = addOff virtHp (intOff 1)
    in
    	-- We charge the allocation to "PRIM" (which is probably right)
    profCtrC SLIT("ALLOC_PRIM2") [COffset space]	`thenC`

	-- BUMP THE VIRTUAL HEAP POINTER
    setVirtHp (virtHp `addOff` space)		`thenC`

	-- RETURN PTR TO START OF OBJECT
    returnFC (CAddr (HpRel realHp block_start))
\end{code}
