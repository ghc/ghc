%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module StixPrim (
	genPrimCode, amodeCode, amodeCode',

    	Target, CAddrMode, StixTree, PrimOp, SplitUniqSupply
    ) where

IMPORT_Trace	-- ToDo: rm debugging

import AbsCSyn
import AbsPrel		( PrimOp(..), PrimOpResultInfo(..), TyCon,
			  getPrimOpResultInfo, isCompareOp, showPrimOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( cmpTyCon ) -- pragmas only
import CgCompInfo	( spARelToInt, spBRelToInt )
import MachDesc
import Pretty	    
import PrimKind		( isFloatingKind )
import CostCentre
import SMRep		( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import Stix
import StixMacro	( smStablePtrTable )
import StixInteger	{- everything -}
import SplitUniq
import Unique
import Unpretty
import Util

\end{code}

The main honcho here is genPrimCode, which handles the guts of COpStmts.

\begin{code}
arrayOfData_info      = sStLitLbl SLIT("ArrayOfData_info") -- out here to avoid CAF (sigh)
imMutArrayOfPtrs_info = sStLitLbl SLIT("ImMutArrayOfPtrs_info")

genPrimCode
    :: Target 
    -> [CAddrMode]  	-- results
    -> PrimOp 	    	-- op
    -> [CAddrMode]  	-- args
    -> SUniqSM StixTreeList

\end{code}

First, the dreaded @ccall@.  We can't handle @casm@s.

Usually, this compiles to an assignment, but when the left-hand side is
empty, we just perform the call and ignore the result.

ToDo ADR: modify this to handle Malloc Ptrs.

btw Why not let programmer use casm to provide assembly code instead
of C code?  ADR

\begin{code}

genPrimCode target lhs (CCallOp fn is_asm may_gc arg_tys result_ty) rhs 
  | is_asm = error "ERROR: Native code generator can't handle casm"
  | otherwise =
    case lhs of
    	[] -> returnSUs (\xs -> (StCall fn VoidKind args) : xs)
    	[lhs] ->
    	    let lhs' = amodeToStix target lhs
    	    	pk = if isFloatingKind (getAmodeKind lhs) then DoubleKind else IntKind
	    	call = StAssign pk lhs' (StCall fn pk args)
    	    in
	    	returnSUs (\xs -> call : xs)
    where
    	args = map amodeCodeForCCall rhs
        amodeCodeForCCall x = 
    	    let base = amodeToStix' target x
    	    in
    	    	case getAmodeKind x of
    	    	    ArrayKind -> StIndex PtrKind base (mutHS target)
    	    	    ByteArrayKind -> StIndex IntKind base (dataHS target)
		    MallocPtrKind -> error "ERROR: native-code generator can't handle Malloc Ptrs (yet): use -fvia-C!"
    	    	    _ -> base

\end{code}    

The @ErrorIO@ primitive is actually a bit weird...assign a new value to the root
closure, flush stdout and stderr, and jump to the @ErrorIO_innards@.

\begin{code}

genPrimCode target [] ErrorIOPrimOp [rhs] = 
    let changeTop = StAssign PtrKind topClosure (amodeToStix target rhs)
    in
	returnSUs (\xs -> changeTop : flushStdout : flushStderr : errorIO : xs)

\end{code}

The (MP) integer operations are a true nightmare.  Since we don't have a 
convenient abstract way of allocating temporary variables on the (C) stack,
we use the space just below HpLim for the @MP_INT@ structures, and modify our
heap check accordingly.

\begin{code}

genPrimCode target res IntegerAddOp args =
    gmpTake2Return1 target res SLIT("mpz_add") args
genPrimCode target res IntegerSubOp args =
    gmpTake2Return1 target res SLIT("mpz_sub") args
genPrimCode target res IntegerMulOp args =
    gmpTake2Return1 target res SLIT("mpz_mul") args

genPrimCode target res IntegerNegOp arg =
    gmpTake1Return1 target res SLIT("mpz_neg") arg

genPrimCode target res IntegerQuotRemOp arg =
    gmpTake2Return2 target res SLIT("mpz_divmod") arg
genPrimCode target res IntegerDivModOp arg =
    gmpTake2Return2 target res SLIT("mpz_targetivmod") arg

\end{code}

Since we are using the heap for intermediate @MP_INT@ structs, integer comparison
{\em does} require a heap check in the native code implementation.

\begin{code}

genPrimCode target [res] IntegerCmpOp args = gmpCompare target res args

genPrimCode target [res] Integer2IntOp arg = gmpInteger2Int target res arg

genPrimCode target res Int2IntegerOp args = gmpInt2Integer target res args

genPrimCode target res Word2IntegerOp args = panic "genPrimCode:Word2IntegerOp"

genPrimCode target res Addr2IntegerOp args = gmpString2Integer target res args

genPrimCode target res FloatEncodeOp args =
    encodeFloatingKind FloatKind target res args

genPrimCode target res DoubleEncodeOp args =
    encodeFloatingKind DoubleKind target res args

genPrimCode target res FloatDecodeOp args =
    decodeFloatingKind FloatKind target res args

genPrimCode target res DoubleDecodeOp args =
    decodeFloatingKind DoubleKind target res args

genPrimCode target res Int2AddrOp arg =
    simpleCoercion target AddrKind res arg

genPrimCode target res Addr2IntOp arg =
    simpleCoercion target IntKind res arg

genPrimCode target res Int2WordOp arg =
    simpleCoercion target IntKind{-WordKind?-} res arg

genPrimCode target res Word2IntOp arg =
    simpleCoercion target IntKind res arg

\end{code}

@newArray#@ ops allocate heap space.

\begin{code}

genPrimCode target [res] NewArrayOp args =
    let	[liveness, n, initial] = map (amodeToStix target) args
        result = amodeToStix target res
    	space = StPrim IntAddOp [n, mutHS target]
    	loc = StIndex PtrKind stgHp 
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrKind result loc
    	initialise = StCall SLIT("newArrZh_init") VoidKind [result, n, initial]
    in
    	heapCheck target liveness space (StInt 0)
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk . (\xs -> assign : initialise : xs))

genPrimCode target [res] (NewByteArrayOp pk) args =
    let	[liveness, count] = map (amodeToStix target) args
        result = amodeToStix target res
    	n = StPrim IntMulOp [count, StInt (toInteger (sizeof target pk))]
        slop = StPrim IntAddOp [n, StInt (toInteger (sizeof target IntKind - 1))]
        words = StPrim IntDivOp [slop, StInt (toInteger (sizeof target IntKind))]
    	space = StPrim IntAddOp [n, StPrim IntAddOp [words, dataHS target]]
    	loc = StIndex PtrKind stgHp 
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrKind result loc
    	init1 = StAssign PtrKind (StInd PtrKind loc) arrayOfData_info
        init2 = StAssign IntKind 
    	    	    	 (StInd IntKind 
    	    	    	    	(StIndex IntKind loc 
    	    	    	    	    	 (StInt (toInteger (fixedHeaderSize target)))))
                         (StPrim IntAddOp [words, 
    	    	    	    	    	  StInt (toInteger (varHeaderSize target 
    	    	    	    	    	    	    	    	    	  (DataRep 0)))])
    in
    	heapCheck target liveness space (StInt 0)
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk . (\xs -> assign : init1 : init2 : xs))

genPrimCode target [res] SameMutableArrayOp args =
    let compare = StPrim AddrEqOp (map (amodeToStix target) args)
        assign = StAssign IntKind (amodeToStix target res) compare
    in
        returnSUs (\xs -> assign : xs)

genPrimCode target res SameMutableByteArrayOp args =
    genPrimCode target res SameMutableArrayOp args

\end{code}

Freezing an array of pointers is a double assignment.  We fix the header of
the ``new'' closure because the lhs is probably a better addressing mode for
the indirection (most likely, it's a VanillaReg).

\begin{code}

genPrimCode target [lhs] UnsafeFreezeArrayOp [rhs] =
    let lhs' = amodeToStix target lhs
    	rhs' = amodeToStix target rhs
    	header = StInd PtrKind lhs'
	assign = StAssign PtrKind lhs' rhs'
	freeze = StAssign PtrKind header imMutArrayOfPtrs_info
    in
	returnSUs (\xs -> assign : freeze : xs)

genPrimCode target lhs UnsafeFreezeByteArrayOp rhs =
    simpleCoercion target PtrKind lhs rhs

\end{code}

Most other array primitives translate to simple indexing.

\begin{code}

genPrimCode target lhs IndexArrayOp args =
    genPrimCode target lhs ReadArrayOp args

genPrimCode target [lhs] ReadArrayOp [obj, ix] =
    let lhs' = amodeToStix target lhs
    	obj' = amodeToStix target obj
    	ix' = amodeToStix target ix
    	base = StIndex IntKind obj' (mutHS target)
    	assign = StAssign PtrKind lhs' (StInd PtrKind (StIndex PtrKind base ix'))
    in
    	returnSUs (\xs -> assign : xs)

genPrimCode target [lhs] WriteArrayOp [obj, ix, v] =
    let	obj' = amodeToStix target obj
    	ix' = amodeToStix target ix
    	v' = amodeToStix target v
    	base = StIndex IntKind obj' (mutHS target)
    	assign = StAssign PtrKind (StInd PtrKind (StIndex PtrKind base ix')) v'
    in
    	returnSUs (\xs -> assign : xs)

genPrimCode target lhs (IndexByteArrayOp pk) args =
    genPrimCode target lhs (ReadByteArrayOp pk) args

genPrimCode target [lhs] (ReadByteArrayOp pk) [obj, ix] =
    let lhs' = amodeToStix target lhs
    	obj' = amodeToStix target obj
    	ix' = amodeToStix target ix
    	base = StIndex IntKind obj' (dataHS target)
    	assign = StAssign pk lhs' (StInd pk (StIndex CharKind base ix'))
    in
    	returnSUs (\xs -> assign : xs)

genPrimCode target [] (WriteByteArrayOp pk) [obj, ix, v] =
    let	obj' = amodeToStix target obj
    	ix' = amodeToStix target ix
    	v' = amodeToStix target v
    	base = StIndex IntKind obj' (dataHS target)
    	assign = StAssign pk (StInd pk (StIndex CharKind base ix')) v'
    in
    	returnSUs (\xs -> assign : xs)

genPrimCode target [lhs] (IndexOffAddrOp pk) [obj, ix] =
    let lhs' = amodeToStix target lhs
    	obj' = amodeToStix target obj
    	ix' = amodeToStix target ix
    	assign = StAssign pk lhs' (StInd pk (StIndex CharKind obj' ix'))
    in
    	returnSUs (\xs -> assign : xs)

\end{code}

Stable pointer operations.

First the easy one.

\begin{code}

genPrimCode target [lhs] DeRefStablePtrOp [sp] =
    let lhs' = amodeToStix target lhs
    	pk = getAmodeKind lhs
    	sp' = amodeToStix target sp
	call = StCall SLIT("deRefStablePointer") pk [sp', smStablePtrTable]
    	assign = StAssign pk lhs' call
    in
    	returnSUs (\xs -> assign : xs)

\end{code}

Now the hard one.  For comparison, here's the code from StgMacros:

\begin{verbatim}
#define makeStablePtrZh(stablePtr,liveness,unstablePtr)              \
do {                                                                 \
  EXTDATA(MK_INFO_LBL(StablePointerTable));                          \
  EXTDATA(UnusedSP);                                                 \
  StgStablePtr newSP;                                                \
                                                                     \
  if (SPT_EMPTY(StorageMgrInfo.StablePointerTable)) { /* free stack is empty */ \
    I_ OldNoPtrs = SPT_NoPTRS(StorageMgrInfo.StablePointerTable);    \
                                                                     \
    /* any strictly increasing expression will do here */            \
    I_ NewNoPtrs = OldNoPtrs * 2 + 100;                              \
                                                                     \
    I_ NewSize = DYN_VHS + NewNoPtrs + 1 + NewNoPtrs;                \
    P_ SPTable;                                                      \
                                                                     \
    HEAP_CHK(NO_LIVENESS, _FHS+NewSize, 0);                          \
    CC_ALLOC(CCC, _FHS+NewSize, SPT_K); /* cc prof */                \
                                                                     \
    SPTable = Hp + 1 - (_FHS + NewSize);                             \
    SET_DYN_HDR(SPTable,StablePointerTable,CCC,NewSize,NewNoPtrs);   \
    SAFESTGCALL2(void, (void *, P_, P_), enlargeSPTable, SPTable, StorageMgrInfo.StablePointerTable);      \
    StorageMgrInfo.StablePointerTable = SPTable;                     \
  }                                                                  \
                                                                     \
  newSP = SPT_POP(StorageMgrInfo.StablePointerTable);                \
  SPT_SPTR(StorageMgrInfo.StablePointerTable, newSP) = unstablePtr; \
  stablePtr = newSP;                                                 \
} while (0)
\end{verbatim}

ToDo ADR: finish this.  (Boy, this is hard work!)

Notes for ADR:
    trMumbles are now just StMumbles.
    StInt 1 is how to write ``1''
    temporaries are allocated at the end of the heap (see notes in StixInteger)
    Good luck!

    --JSM

\begin{pseudocode}
genPrimCode sty md [lhs] MakeStablePtrOp args =
    let 
	-- some useful abbreviations (I'm sure these must exist already)
	add = trPrim . IntAddOp 
	sub = trPrim . IntSubOp
	one = trInt [1]
	dec x = trAssign IntKind [x, sub [x, one]]
	inc x = trAssign IntKind [x, add [x, one]]

	-- tedious hardwiring in of closure layout offsets (from SMClosures)
	dynHS = 2 + fixedHeaderSize md sty + varHeaderSize md sty DynamicRep
	spt_SIZE c   = trIndex PtrKind [c, trInt [fhs + gc_reserved] ]
	spt_NoPTRS c = trIndex PtrKind [c, trInt [fhs + gc_reserved + 1] ]
	spt_SPTR c i = trIndex PtrKind [c, add [trInt [dynHS], i]]
	spt_TOP c    = trIndex PtrKind [c, add [trInt [dynHS], spt_NoPTRS c]]
	spt_FREE c i = trIndex PtrKind [c, add [trInt [dynHS], spt_NoPTRS c]]

	-- tedious hardwiring in of stack manipulation macros (from SMClosures)
	spt_FULL c lbl =
		trCondJump lbl [trPrim IntEqOp [spt_TOP c, spt_NoPTRS c]]
	spt_EMPTY c lbl =
		trCondJump lbl [trPrim IntEqOp [spt_TOP c, trInt [0]]]
	spt_PUSH c f = [ 
		trAssign PtrKind [spt_FREE c (spt_TOP c), f],
		inc (spt_TOP c),
	spt_POP c x  = [ 
		dec (spt_TOP c), 
		trAssign PtrKind [x, spt_FREE c (spt_TOP c)]
	]

	-- now to get down to business
	lhs' = amodeCode sty md lhs
    	[liveness, unstable] = map (amodeCode sty md) args

	spt = smStablePtrTable

	newSPT = -- a temporary (don't know how to allocate it)
	newSP = -- another temporary

	allocNewTable = -- some sort fo heap allocation needed
	copyOldTable = trCall "enlargeSPTable" PtrKind [newSPT, spt]

	enlarge = 
		allocNewTable ++ [
		copyOldTable,
		trAssign PtrKind [spt, newSPT]
	allocate = [
		spt_POP spt newSP,
		trAssign PtrKind [spt_SPTR spt newSP, unstable],
		trAssign StablePtrKind [lhs', newSP]
	]
		
    in
    getUniqLabelCTS 	    	    	    	   `thenCTS` \ oklbl ->
    returnCodes sty md 
	(spt_EMPTY spt oklbl : (enlarge ++ (trLabel [oklbl] : allocate)))
\end{pseudocode}


Now the more mundane operations.

\begin{code}

genPrimCode target lhs op rhs = 
    let lhs' = map (amodeToStix target) lhs
    	rhs' = map (amodeToStix' target) rhs
    in
        returnSUs (\ xs -> simplePrim target lhs' op rhs' : xs)

simpleCoercion 
    :: Target 
    -> PrimKind 
    -> [CAddrMode] 
    -> [CAddrMode] 
    -> SUniqSM StixTreeList

simpleCoercion target pk [lhs] [rhs] =
    returnSUs (\xs -> StAssign pk (amodeToStix target lhs) (amodeToStix target rhs) : xs)

\end{code}

Here we try to rewrite primitives into a form the code generator
can understand.	 Any primitives not handled here must be handled 
at the level of the specific code generator.

\begin{code}

simplePrim 
    :: Target 
    -> [StixTree] 
    -> PrimOp 
    -> [StixTree] 
    -> StixTree

\end{code}

Now look for something more conventional.

\begin{code}

simplePrim target [lhs] op rest = StAssign pk lhs (StPrim op rest)
    where pk = if isCompareOp op then IntKind 
               else case getPrimOpResultInfo op of
		 ReturnsPrim pk -> pk
		 _ -> simplePrim_error op

simplePrim target _ op _ = simplePrim_error op

simplePrim_error op
  = error ("ERROR: primitive operation `"++showPrimOp PprDebug op++"'cannot be handled\nby the native-code generator.  Workaround: use -fvia-C.\n(Perhaps you should report it as a GHC bug, also.)\n")
\end{code}

%---------------------------------------------------------------------

Here we generate the Stix code for CAddrModes.

When a character is fetched from a mixed type location, we have to
do an extra cast.  This is reflected in amodeCode', which is for rhs
amodes that might possibly need the extra cast.

\begin{code}

amodeCode, amodeCode' 
    :: Target 
    -> CAddrMode 
    -> StixTree

amodeCode' target am@(CVal rr CharKind) 
    | mixedTypeLocn am = StPrim ChrOp [amodeToStix target am]
    | otherwise = amodeToStix target am

amodeCode' target am = amodeToStix target am

amodeCode target am@(CVal rr CharKind) | mixedTypeLocn am =
    	StInd IntKind (amodeCode target (CAddr rr))

amodeCode target (CVal rr pk) = StInd pk (amodeCode target (CAddr rr))

amodeCode target (CAddr r@(SpARel spA off)) =
    StIndex PtrKind stgSpA (StInt (toInteger (spARelToInt r)))

amodeCode target (CAddr r@(SpBRel spB off)) =
    StIndex IntKind stgSpB (StInt (toInteger (spBRelToInt r)))

amodeCode target (CAddr (HpRel hp off)) =
    StIndex IntKind stgHp (StInt (toInteger (-(hpRel target (hp `subOff` off)))))

amodeCode target (CAddr (NodeRel off)) =
    StIndex IntKind stgNode (StInt (toInteger (hpRel target off)))

amodeCode target (CReg magic) = StReg (StixMagicId magic)
amodeCode target (CTemp uniq pk) = StReg (StixTemp uniq pk)

amodeCode target (CLbl lbl _) = StCLbl lbl

amodeCode target (CUnVecLbl dir _) = StCLbl dir

amodeCode target (CTableEntry base off pk) = 
    StInd pk (StIndex pk (amodeCode target base) (amodeCode target off))

-- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeCode target (CCharLike (CLit (MachChar c))) = 
    StLitLbl (uppBeside (uppPStr SLIT("CHARLIKE_closures+")) (uppInt off))
    where off = charLikeClosureSize target * ord c

amodeCode target (CCharLike x) = 
    StPrim IntAddOp [charLike, off]
    where off = StPrim IntMulOp [amodeCode target x, 
            StInt (toInteger (charLikeClosureSize target))]

amodeCode target (CIntLike (CLit (MachInt i _))) = 
    StPrim IntAddOp [intLikePtr, StInt off]
    where off = toInteger (intLikeClosureSize target) * i

amodeCode target (CIntLike x) = 
    StPrim IntAddOp [intLikePtr, off]
    where off = StPrim IntMulOp [amodeCode target x,
    	    StInt (toInteger (intLikeClosureSize target))]

-- A CString is just a (CLit . MachStr)
amodeCode target (CString s) = StString s

amodeCode target (CLit core) = case core of
    (MachChar c) -> StInt (toInteger (ord c))
    (MachStr s) -> StString s
    (MachAddr a) -> StInt a
    (MachInt i _) -> StInt i
    (MachLitLit s _) -> StLitLit s
    (MachFloat d) -> StDouble d
    (MachDouble d) -> StDouble d
    _ -> panic "amodeCode:core literal"

-- A CLitLit is just a (CLit . MachLitLit)
amodeCode target (CLitLit s _) = StLitLit s

-- COffsets are in words, not bytes!
amodeCode target (COffset off) = StInt (toInteger (hpRel target off))

amodeCode target (CMacroExpr _ macro [arg]) = 
    case macro of
    	INFO_PTR -> StInd PtrKind (amodeToStix target arg)
    	ENTRY_CODE -> amodeToStix target arg
    	INFO_TAG -> tag
    	EVAL_TAG -> StPrim IntGeOp [tag, StInt 0]
  where
    tag = StInd IntKind (StIndex IntKind (amodeToStix target arg) (StInt (-2)))
    -- That ``-2'' really bothers me. (JSM)

amodeCode target (CCostCentre cc print_as_string)
  = if noCostCentreAttached cc
    then StComment SLIT("") -- sigh
    else panic "amodeCode:CCostCentre"
\end{code}

Sizes of the CharLike and IntLike closures that are arranged as arrays in the
data segment.  (These are in bytes.)

\begin{code}

-- The INTLIKE base pointer

intLikePtr :: StixTree

intLikePtr = StInd PtrKind (sStLitLbl SLIT("INTLIKE_closures"))

-- The CHARLIKE base

charLike :: StixTree

charLike = sStLitLbl SLIT("CHARLIKE_closures")

-- Trees for the ErrorIOPrimOp

topClosure, flushStdout, flushStderr, errorIO :: StixTree

topClosure = StInd PtrKind (sStLitLbl SLIT("TopClosure"))
flushStdout = StCall SLIT("fflush") VoidKind [StLitLit SLIT("stdout")]
flushStderr = StCall SLIT("fflush") VoidKind [StLitLit SLIT("stderr")]
errorIO = StJump (StInd PtrKind (sStLitLbl SLIT("ErrorIO_innards")))

\end{code}

