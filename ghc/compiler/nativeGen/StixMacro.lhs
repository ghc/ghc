%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module StixMacro (
	genMacroCode, doHeapCheck, smStablePtrTable,

	Target, StixTree, SplitUniqSupply, CAddrMode, CExprMacro,
	CStmtMacro
    ) where

import AbsCSyn
import AbsPrel      ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import MachDesc	    {- lots -}
import CgCompInfo   ( sTD_UF_SIZE, uF_RET, uF_SUA, uF_SUB, uF_UPDATEE )
import Stix
import SplitUniq
import Unique
import Util

\end{code}

The @ARGS_CHK_A{_LOAD_NODE}@ macros check for sufficient arguments on
the A stack, and perform a tail call to @UpdatePAP@ if the arguments are
not there.  The @_LOAD_NODE@ version also loads R1 with an appropriate
closure address.

\begin{code}
mkIntCLit_0 = mkIntCLit 0 -- out here to avoid CAF (sigh)
mkIntCLit_3 = mkIntCLit 3

genMacroCode 
    :: Target 
    -> CStmtMacro   	    -- statement macro
    -> [CAddrMode]  	    -- args
    -> SUniqSM StixTreeList

genMacroCode target ARGS_CHK_A_LOAD_NODE args = 
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let [words, lbl] = map (amodeToStix target) args
    	temp = StIndex PtrKind stgSpA words
	test = StPrim AddrGeOp [stgSuA, temp]
	cjmp = StCondJump ulbl test
	assign = StAssign PtrKind stgNode lbl
	join = StLabel ulbl
    in
	returnSUs (\xs -> cjmp : assign : updatePAP : join : xs)

genMacroCode target ARGS_CHK_A [words] = 
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let temp = StIndex PtrKind stgSpA (amodeToStix target words)
	test = StPrim AddrGeOp [stgSuA, temp]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
	returnSUs (\xs -> cjmp : updatePAP : join : xs)

\end{code}

Like the macros above, the @ARGS_CHK_B{_LOAD_NODE}@ macros check for
sufficient arguments on the B stack, and perform a tail call to
@UpdatePAP@ if the arguments are not there.  The @_LOAD_NODE@ version
also loads R1 with an appropriate closure address.  Note that the
directions are swapped relative to the A stack.

\begin{code}

genMacroCode target ARGS_CHK_B_LOAD_NODE args = 
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let [words, lbl] = map (amodeToStix target) args
    	temp = StIndex PtrKind stgSuB (StPrim IntNegOp [words])
	test = StPrim AddrGeOp [stgSpB, temp]
	cjmp = StCondJump ulbl test
	assign = StAssign PtrKind stgNode lbl
	join = StLabel ulbl
    in
	returnSUs (\xs -> cjmp : assign : updatePAP : join : xs)

genMacroCode target ARGS_CHK_B [words] = 
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let	temp = StIndex PtrKind stgSuB (StPrim IntNegOp [amodeToStix target words])
	test = StPrim AddrGeOp [stgSpB, temp]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
	returnSUs (\xs -> cjmp : updatePAP : join : xs)

\end{code}

The @HEAP_CHK@ macro checks to see that there are enough words
available in the heap (before reaching @HpLim@).  When a heap check
fails, it has to call @PerformGC@ via the @PerformGC_wrapper@.  The
call wrapper saves all of our volatile registers so that we don't have to.

Since there are @HEAP_CHK@s buried at unfortunate places in the integer
primOps, this is just a wrapper.

\begin{code}

genMacroCode target HEAP_CHK args =
    let [liveness,words,reenter] = map (amodeToStix target) args
    in
	doHeapCheck target liveness words reenter

\end{code}

The @STK_CHK@ macro checks for enough space on the stack between @SpA@
and @SpB@.  A stack check can be complicated in the parallel world,
but for the sequential case, we just need to ensure that we have
enough space to continue.  Not that @_StackOverflow@ doesn't return,
so we don't have to @callWrapper@ it.

\begin{code}

genMacroCode target STK_CHK [liveness, aWords, bWords, spa, spb, prim, reenter] = 
{- Need to check to see if we are compiling with stack checks
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let words = StPrim IntNegOp 
    	    [StPrim IntAddOp [amodeToStix target aWords, amodeToStix target bWords]]
	temp = StIndex PtrKind stgSpA words
	test = StPrim AddrGtOp [temp, stgSpB]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
	returnSUs (\xs -> cjmp : stackOverflow : join : xs)
-}
    returnSUs id

\end{code}

@UPD_CAF@ involves changing the info pointer of the closure, adding an indirection,
and putting the new CAF on a linked list for the storage manager.

\begin{code}

genMacroCode target UPD_CAF args =
    let [cafptr,bhptr] = map (amodeToStix target) args
    	w0 = StInd PtrKind cafptr
	w1 = StInd PtrKind (StIndex PtrKind cafptr (StInt 1))
	w2 = StInd PtrKind (StIndex PtrKind cafptr (StInt 2))
	a1 = StAssign PtrKind w0 caf_info
	a2 = StAssign PtrKind w1 smCAFlist
	a3 = StAssign PtrKind w2 bhptr
	a4 = StAssign PtrKind smCAFlist cafptr
    in
	returnSUs (\xs -> a1 : a2 : a3 : a4 : xs)

\end{code}

@UPD_IND@ is complicated by the fact that we are supporting the
Appel-style garbage collector by default.  This means some extra work
if we update an old generation object.

\begin{code}

genMacroCode target UPD_IND args = 
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let [updptr, heapptr] = map (amodeToStix target) args
    	test = StPrim AddrGtOp [updptr, smOldLim]
    	cjmp = StCondJump ulbl test
    	updRoots = StAssign PtrKind smOldMutables updptr
	join = StLabel ulbl
    	upd0 = StAssign PtrKind (StInd PtrKind updptr) ind_info
    	upd1 = StAssign PtrKind (StInd PtrKind 
    	    	(StIndex PtrKind updptr (StInt 1))) smOldMutables
    	upd2 = StAssign PtrKind (StInd PtrKind 
    	    	(StIndex PtrKind updptr (StInt 2))) heapptr
    in
    	returnSUs (\xs -> cjmp : upd1 : updRoots : join : upd0 : upd2 : xs)

\end{code}

@UPD_INPLACE_NOPTRS@ is only needed for ticky-ticky profiling.

\begin{code}

genMacroCode target UPD_INPLACE_NOPTRS args = returnSUs id

\end{code}

@UPD_INPLACE_PTRS@ is complicated by the fact that we are supporting
the Appel-style garbage collector by default.  This means some extra work 
if we update an old generation object.

\begin{code}

genMacroCode target UPD_INPLACE_PTRS [liveness] =
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let cjmp = StCondJump ulbl testOldLim
        testOldLim = StPrim AddrGtOp [stgNode, smOldLim]
	join = StLabel ulbl
        updUpd0 = StAssign PtrKind (StInd PtrKind stgNode) ind_info
    	updUpd1 = StAssign PtrKind (StInd PtrKind 
	    	    (StIndex PtrKind stgNode (StInt 1))) smOldMutables
    	updUpd2 = StAssign PtrKind (StInd PtrKind 
    	    	    (StIndex PtrKind stgNode (StInt 2))) hpBack2
    	hpBack2 = StIndex PtrKind stgHp (StInt (-2))
    	updOldMutables = StAssign PtrKind smOldMutables stgNode
    	updUpdReg = StAssign PtrKind stgNode hpBack2
    in
	genMacroCode target HEAP_CHK [liveness, mkIntCLit_3, mkIntCLit_0]
							`thenSUs` \ heap_chk ->
	returnSUs (\xs -> (cjmp : 
    	    	    	    heap_chk (updUpd0 : updUpd1 : updUpd2 : 
    	    	    	    	    	updOldMutables : updUpdReg : join : xs)))

\end{code}

@UPD_BH_UPDATABLE@ is only used when running concurrent threads (in
the sequential case, the GC takes care of this).  However, we do need
to handle @UPD_BH_SINGLE_ENTRY@ in all cases.

\begin{code}

genMacroCode target UPD_BH_UPDATABLE args = returnSUs id

genMacroCode target UPD_BH_SINGLE_ENTRY [arg] =
    let
    	update = StAssign PtrKind (StInd PtrKind (amodeToStix target arg)) bh_info
    in
        returnSUs (\xs -> update : xs)

\end{code}

Push a four word update frame on the stack and slide the Su[AB]
registers to the current Sp[AB] locations.

\begin{code}

genMacroCode target PUSH_STD_UPD_FRAME args =
    let [bhptr, aWords, bWords] = map (amodeToStix target) args
    	frame n = StInd PtrKind 
	    (StIndex PtrKind stgSpB (StPrim IntAddOp 
    	    	[bWords, StInt (toInteger (sTD_UF_SIZE - n))]))

	a1 = StAssign PtrKind (frame uF_RET) stgRetReg
	a2 = StAssign PtrKind (frame uF_SUB) stgSuB
	a3 = StAssign PtrKind (frame uF_SUA) stgSuA
	a4 = StAssign PtrKind (frame uF_UPDATEE) bhptr

	updSuB = StAssign PtrKind
	    stgSuB (StIndex PtrKind stgSpB (StPrim IntAddOp 
    	    	[bWords, StInt (toInteger sTD_UF_SIZE)]))
	updSuA = StAssign PtrKind
	    stgSuA (StIndex PtrKind stgSpA (StPrim IntNegOp [aWords]))
    in
	returnSUs (\xs -> a1 : a2 : a3 : a4 : updSuB : updSuA : xs)

\end{code}

Pop a standard update frame.

\begin{code}

genMacroCode target POP_STD_UPD_FRAME args =
    let frame n = StInd PtrKind (StIndex PtrKind stgSpB (StInt (toInteger (-n))))

	grabRet = StAssign PtrKind stgRetReg (frame uF_RET)
	grabSuB = StAssign PtrKind stgSuB    (frame uF_SUB)
	grabSuA = StAssign PtrKind stgSuA    (frame uF_SUA)

	updSpB = StAssign PtrKind
	    stgSpB (StIndex PtrKind stgSpB (StInt (toInteger (-sTD_UF_SIZE))))
    in
	returnSUs (\xs -> grabRet : grabSuB : grabSuA : updSpB : xs)

\end{code}

@PUSH_CON_UPD_FRAME@ appears to be unused at the moment.

\begin{code}
{- UNUSED:
genMacroCode target PUSH_CON_UPD_FRAME args = 
    panic "genMacroCode:PUSH_CON_UPD_FRAME"
-}
\end{code}

The @SET_ARITY@ and @CHK_ARITY@ macros are disabled for ``normal'' compilation.

\begin{code}

genMacroCode target SET_ARITY args = returnSUs id
genMacroCode target CHK_ARITY args = returnSUs id

\end{code}

This one only applies if we have a machine register devoted to TagReg.

\begin{code}

genMacroCode target SET_TAG [tag] = 
    let set_tag = StAssign IntKind stgTagReg (amodeToStix target tag)
    in
        case stgReg target TagReg of
            Always _ -> returnSUs id
            Save _ -> returnSUs (\xs -> set_tag : xs)

\end{code}

Do the business for a @HEAP_CHK@, having converted the args to Trees
of StixOp.

\begin{code}

doHeapCheck 
    :: Target 
    -> StixTree  	-- liveness
    -> StixTree  	-- words needed
    -> StixTree  	-- always reenter node? (boolean)
    -> SUniqSM StixTreeList

doHeapCheck target liveness words reenter =
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let newHp = StIndex PtrKind stgHp words
	assign = StAssign PtrKind stgHp newHp
	test = StPrim AddrLeOp [stgHp, stgHpLim]
	cjmp = StCondJump ulbl test
        arg = StPrim IntAddOp [StPrim IntMulOp [words, StInt 256], liveness]
	-- ToDo: Overflow?  (JSM)
	gc = StCall SLIT("PerformGC_wrapper") VoidKind [arg]
	join = StLabel ulbl
    in
	returnSUs (\xs -> assign : cjmp : gc : join : xs)

\end{code}

Let's make sure that these CAFs are lifted out, shall we?

\begin{code}

-- Some common labels

bh_info, caf_info, ind_info :: StixTree

bh_info   = sStLitLbl SLIT("BH_SINGLE_info")
caf_info  = sStLitLbl SLIT("Caf_info")
ind_info  = sStLitLbl SLIT("Ind_info")

-- Some common call trees

updatePAP, stackOverflow :: StixTree

updatePAP     = StJump (sStLitLbl SLIT("UpdatePAP"))
stackOverflow = StCall SLIT("StackOverflow") VoidKind []

\end{code}

Storage manager nonsense.  Note that the indices are dependent on 
the definition of the smInfo structure in SMinterface.lh

\begin{code}

#include "../../includes/platform.h"

#if alpha_TARGET_ARCH
#include "../../includes/alpha-dec-osf1.h"
#else
#if sunos4_TARGET_OS
#include "../../includes/sparc-sun-sunos4.h"
#else
#include "../../includes/sparc-sun-solaris2.h"
#endif
#endif

storageMgrInfo, smCAFlist, smOldMutables, smOldLim :: StixTree

storageMgrInfo = sStLitLbl SLIT("StorageMgrInfo")
smCAFlist  = StInd PtrKind (StIndex PtrKind storageMgrInfo (StInt SM_CAFLIST))
smOldMutables = StInd PtrKind (StIndex PtrKind storageMgrInfo (StInt SM_OLDMUTABLES))
smOldLim   = StInd PtrKind (StIndex PtrKind storageMgrInfo (StInt SM_OLDLIM))

smStablePtrTable = StInd PtrKind 
    	    	    	 (StIndex PtrKind storageMgrInfo (StInt SM_STABLEPOINTERTABLE))

\end{code}
