%
% (c) The AQUA Project, Glasgow University, 1993-1996
%

\begin{code}
#include "HsVersions.h"

module StixMacro ( macroCode, heapCheck ) where

import Ubiq{-uitious-}
import NcgLoop		( amodeToStix )

import MachMisc
import MachRegs

import AbsCSyn		( CStmtMacro(..), MagicId(..), mkIntCLit, CAddrMode )
import CgCompInfo	( uF_RET, uF_SUA, uF_SUB, uF_UPDATEE,
			  sTD_UF_SIZE
			)
import OrdList		( OrdList )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Stix
import UniqSupply	( returnUs, thenUs, UniqSM(..) )
\end{code}

The @ARGS_CHK_A{_LOAD_NODE}@ macros check for sufficient arguments on
the A stack, and perform a tail call to @UpdatePAP@ if the arguments are
not there.  The @_LOAD_NODE@ version also loads R1 with an appropriate
closure address.

\begin{code}
mkIntCLit_0 = mkIntCLit 0 -- out here to avoid CAF (sigh)
mkIntCLit_3 = mkIntCLit 3

macroCode
    :: CStmtMacro   	    -- statement macro
    -> [CAddrMode]  	    -- args
    -> UniqSM StixTreeList

macroCode ARGS_CHK_A_LOAD_NODE args
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	  [words, lbl] = map amodeToStix args
	  temp = StIndex PtrRep stgSpA words
	  test = StPrim AddrGeOp [stgSuA, temp]
	  cjmp = StCondJump ulbl test
	  assign = StAssign PtrRep stgNode lbl
	  join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : assign : updatePAP : join : xs)

macroCode ARGS_CHK_A [words]
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let temp = StIndex PtrRep stgSpA (amodeToStix words)
	test = StPrim AddrGeOp [stgSuA, temp]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : updatePAP : join : xs)
\end{code}

Like the macros above, the @ARGS_CHK_B{_LOAD_NODE}@ macros check for
sufficient arguments on the B stack, and perform a tail call to
@UpdatePAP@ if the arguments are not there.  The @_LOAD_NODE@ version
also loads R1 with an appropriate closure address.  Note that the
directions are swapped relative to the A stack.

\begin{code}
macroCode ARGS_CHK_B_LOAD_NODE args
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	[words, lbl] = map amodeToStix args
    	temp = StIndex PtrRep stgSuB (StPrim IntNegOp [words])
	test = StPrim AddrGeOp [stgSpB, temp]
	cjmp = StCondJump ulbl test
	assign = StAssign PtrRep stgNode lbl
	join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : assign : updatePAP : join : xs)

macroCode ARGS_CHK_B [words]
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	temp = StIndex PtrRep stgSuB (StPrim IntNegOp [amodeToStix words])
	test = StPrim AddrGeOp [stgSpB, temp]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : updatePAP : join : xs)
\end{code}

The @HEAP_CHK@ macro checks to see that there are enough words
available in the heap (before reaching @HpLim@).  When a heap check
fails, it has to call @PerformGC@ via the @PerformGC_wrapper@.  The
call wrapper saves all of our volatile registers so that we don't have
to.

Since there are @HEAP_CHK@s buried at unfortunate places in the
integer primOps, this is just a wrapper.

\begin{code}
macroCode HEAP_CHK args
  = let [liveness,words,reenter] = map amodeToStix args
    in
    heapCheck liveness words reenter
\end{code}

The @STK_CHK@ macro checks for enough space on the stack between @SpA@
and @SpB@.  A stack check can be complicated in the parallel world,
but for the sequential case, we just need to ensure that we have
enough space to continue.  Not that @_StackOverflow@ doesn't return,
so we don't have to @callWrapper@ it.

\begin{code}
macroCode STK_CHK [liveness, aWords, bWords, spa, spb, prim, reenter]
  =
{- Need to check to see if we are compiling with stack checks
   getUniqLabelNCG					`thenUs` \ ulbl ->
    let words = StPrim IntNegOp
    	    [StPrim IntAddOp [amodeToStix aWords, amodeToStix bWords]]
	temp = StIndex PtrRep stgSpA words
	test = StPrim AddrGtOp [temp, stgSpB]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
	returnUs (\xs -> cjmp : stackOverflow : join : xs)
-}
    returnUs id
\end{code}

@UPD_CAF@ involves changing the info pointer of the closure, adding an
indirection, and putting the new CAF on a linked list for the storage
manager.

\begin{code}
macroCode UPD_CAF args
  = let
	[cafptr,bhptr] = map amodeToStix args
    	w0 = StInd PtrRep cafptr
	w1 = StInd PtrRep (StIndex PtrRep cafptr (StInt 1))
	w2 = StInd PtrRep (StIndex PtrRep cafptr (StInt 2))
	a1 = StAssign PtrRep w0 caf_info
	a2 = StAssign PtrRep w1 smCAFlist
	a3 = StAssign PtrRep w2 bhptr
	a4 = StAssign PtrRep smCAFlist cafptr
    in
    returnUs (\xs -> a1 : a2 : a3 : a4 : xs)
\end{code}

@UPD_IND@ is complicated by the fact that we are supporting the
Appel-style garbage collector by default.  This means some extra work
if we update an old generation object.

\begin{code}
macroCode UPD_IND args
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	[updptr, heapptr] = map amodeToStix args
    	test = StPrim AddrGtOp [updptr, smOldLim]
    	cjmp = StCondJump ulbl test
    	updRoots = StAssign PtrRep smOldMutables updptr
	join = StLabel ulbl
    	upd0 = StAssign PtrRep (StInd PtrRep updptr) ind_info
    	upd1 = StAssign PtrRep (StInd PtrRep
    	    	(StIndex PtrRep updptr (StInt 1))) smOldMutables
    	upd2 = StAssign PtrRep (StInd PtrRep
    	    	(StIndex PtrRep updptr (StInt 2))) heapptr
    in
    returnUs (\xs -> cjmp : upd1 : updRoots : join : upd0 : upd2 : xs)
\end{code}

@UPD_INPLACE_NOPTRS@ is only needed for ticky-ticky profiling.

\begin{code}
macroCode UPD_INPLACE_NOPTRS args = returnUs id
\end{code}

@UPD_INPLACE_PTRS@ is complicated by the fact that we are supporting
the Appel-style garbage collector by default.  This means some extra
work if we update an old generation object.

\begin{code}
macroCode UPD_INPLACE_PTRS [liveness]
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let cjmp = StCondJump ulbl testOldLim
	testOldLim = StPrim AddrGtOp [stgNode, smOldLim]
	join = StLabel ulbl
	updUpd0 = StAssign PtrRep (StInd PtrRep stgNode) ind_info
    	updUpd1 = StAssign PtrRep (StInd PtrRep
	    	    (StIndex PtrRep stgNode (StInt 1))) smOldMutables
    	updUpd2 = StAssign PtrRep (StInd PtrRep
    	    	    (StIndex PtrRep stgNode (StInt 2))) hpBack2
    	hpBack2 = StIndex PtrRep stgHp (StInt (-2))
    	updOldMutables = StAssign PtrRep smOldMutables stgNode
    	updUpdReg = StAssign PtrRep stgNode hpBack2
    in
    macroCode HEAP_CHK [liveness, mkIntCLit_3, mkIntCLit_0]
						    `thenUs` \ heap_chk ->
    returnUs (\xs -> (cjmp :
			heap_chk (updUpd0 : updUpd1 : updUpd2 :
				    updOldMutables : updUpdReg : join : xs)))
\end{code}

@UPD_BH_UPDATABLE@ is only used when running concurrent threads (in
the sequential case, the GC takes care of this).  However, we do need
to handle @UPD_BH_SINGLE_ENTRY@ in all cases.

\begin{code}
macroCode UPD_BH_UPDATABLE args = returnUs id

macroCode UPD_BH_SINGLE_ENTRY [arg]
  = let
    	update = StAssign PtrRep (StInd PtrRep (amodeToStix arg)) bh_info
    in
    returnUs (\xs -> update : xs)
\end{code}

Push a four word update frame on the stack and slide the Su[AB]
registers to the current Sp[AB] locations.

\begin{code}
macroCode PUSH_STD_UPD_FRAME args
  = let
	[bhptr, aWords, bWords] = map amodeToStix args
    	frame n = StInd PtrRep
	    (StIndex PtrRep stgSpB (StPrim IntAddOp
    	    	[bWords, StInt (toInteger (sTD_UF_SIZE - n))]))

	a1 = StAssign PtrRep (frame uF_RET) stgRetReg
	a2 = StAssign PtrRep (frame uF_SUB) stgSuB
	a3 = StAssign PtrRep (frame uF_SUA) stgSuA
	a4 = StAssign PtrRep (frame uF_UPDATEE) bhptr

	updSuB = StAssign PtrRep
	    stgSuB (StIndex PtrRep stgSpB (StPrim IntAddOp
    	    	[bWords, StInt (toInteger sTD_UF_SIZE)]))
	updSuA = StAssign PtrRep
	    stgSuA (StIndex PtrRep stgSpA (StPrim IntNegOp [aWords]))
    in
    returnUs (\xs -> a1 : a2 : a3 : a4 : updSuB : updSuA : xs)
\end{code}

Pop a standard update frame.

\begin{code}
macroCode POP_STD_UPD_FRAME args
  = let
	frame n = StInd PtrRep (StIndex PtrRep stgSpB (StInt (toInteger (-n))))

	grabRet = StAssign PtrRep stgRetReg (frame uF_RET)
	grabSuB = StAssign PtrRep stgSuB    (frame uF_SUB)
	grabSuA = StAssign PtrRep stgSuA    (frame uF_SUA)

	updSpB = StAssign PtrRep
	    stgSpB (StIndex PtrRep stgSpB (StInt (toInteger (-sTD_UF_SIZE))))
    in
    returnUs (\xs -> grabRet : grabSuB : grabSuA : updSpB : xs)
\end{code}

The @SET_ARITY@ and @CHK_ARITY@ macros are disabled for ``normal''
compilation.
\begin{code}
macroCode SET_ARITY args = returnUs id
macroCode CHK_ARITY args = returnUs id
\end{code}

This one only applies if we have a machine register devoted to TagReg.
\begin{code}
macroCode SET_TAG [tag]
  = let set_tag = StAssign IntRep stgTagReg (amodeToStix tag)
    in
    case stgReg TagReg of
      Always _ -> returnUs id
      Save   _ -> returnUs (\ xs -> set_tag : xs)
\end{code}

Do the business for a @HEAP_CHK@, having converted the args to Trees
of StixOp.

\begin{code}
heapCheck
    :: StixTree  	-- liveness
    -> StixTree  	-- words needed
    -> StixTree  	-- always reenter node? (boolean)
    -> UniqSM StixTreeList

heapCheck liveness words reenter
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let newHp = StIndex PtrRep stgHp words
	assign = StAssign PtrRep stgHp newHp
	test = StPrim AddrLeOp [stgHp, stgHpLim]
	cjmp = StCondJump ulbl test
	arg = StPrim IntAddOp [StPrim IntMulOp [words, StInt 256], liveness]
	-- ToDo: Overflow?  (JSM)
	gc = StCall SLIT("PerformGC_wrapper") VoidRep [arg]
	join = StLabel ulbl
    in
    returnUs (\xs -> assign : cjmp : gc : join : xs)
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
stackOverflow = StCall SLIT("StackOverflow") VoidRep []
\end{code}
