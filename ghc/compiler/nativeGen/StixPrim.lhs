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
-- hacking with Uncle Will:
#define target_STRICT target@(Target _ _ _ _ _ _ _ _)

genPrimCode target_STRICT res op args
 = genprim res op args
 where
  a2stix    = amodeToStix target
  a2stix'   = amodeToStix' target
  mut_hs    = mutHS target
  data_hs   = dataHS target
  heap_chkr = heapCheck target
  size_of   = sizeof target
  fixed_hs  = fixedHeaderSize target
  var_hs    = varHeaderSize target 

  --- real code will follow... -------------
\end{code}

The (MP) integer operations are a true nightmare.  Since we don't have a 
convenient abstract way of allocating temporary variables on the (C) stack,
we use the space just below HpLim for the @MP_INT@ structures, and modify our
heap check accordingly.

\begin{code}
  -- NB: ordering of clauses somewhere driven by
  -- the desire to getting sane patt-matching behavior

  genprim res@[ar1,sr1,dr1, ar2,sr2,dr2]
	  IntegerQuotRemOp
	  args@[liveness, aa1,sa1,da1, aa2,sa2,da2] =
    gmpTake2Return2 target (ar1,sr1,dr1, ar2,sr2,dr2) SLIT("mpz_divmod") (liveness, aa1,sa1,da1, aa2,sa2,da2)

  genprim res@[ar1,sr1,dr1, ar2,sr2,dr2]
	  IntegerDivModOp
	  args@[liveness, aa1,sa1,da1, aa2,sa2,da2] =
    gmpTake2Return2 target (ar1,sr1,dr1, ar2,sr2,dr2) SLIT("mpz_targetivmod") (liveness, aa1,sa1,da1, aa2,sa2,da2)

  genprim res@[ar,sr,dr] IntegerAddOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2] =
    gmpTake2Return1 target (ar,sr,dr) SLIT("mpz_add") (liveness, aa1,sa1,da1, aa2,sa2,da2)
  genprim res@[ar,sr,dr] IntegerSubOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2] =
    gmpTake2Return1 target (ar,sr,dr) SLIT("mpz_sub") (liveness, aa1,sa1,da1, aa2,sa2,da2)
  genprim res@[ar,sr,dr] IntegerMulOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2] =
    gmpTake2Return1 target (ar,sr,dr) SLIT("mpz_mul") (liveness, aa1,sa1,da1, aa2,sa2,da2)

  genprim res@[ar,sr,dr] IntegerNegOp arg@[liveness,aa,sa,da] =
    gmpTake1Return1 target (ar,sr,dr) SLIT("mpz_neg") (liveness,aa,sa,da)
\end{code}

Since we are using the heap for intermediate @MP_INT@ structs, integer comparison
{\em does} require a heap check in the native code implementation.

\begin{code}
  genprim res@[exponr,ar,sr,dr] FloatDecodeOp args@[hp, arg] =
    decodeFloatingKind FloatKind target (exponr,ar,sr,dr) (hp, arg)

  genprim res@[exponr,ar,sr,dr] DoubleDecodeOp args@[hp, arg] =
    decodeFloatingKind DoubleKind target (exponr,ar,sr,dr) (hp, arg)

  genprim res@[ar,sr,dr] Int2IntegerOp args@[hp, n]
    = gmpInt2Integer target (ar,sr,dr) (hp, n)

  genprim res@[ar,sr,dr] Addr2IntegerOp args@[liveness,str]
    = gmpString2Integer target (ar,sr,dr) (liveness,str)

  genprim [res] IntegerCmpOp args@[hp, aa1,sa1,da1, aa2,sa2,da2]
    = gmpCompare target res (hp, aa1,sa1,da1, aa2,sa2,da2)

  genprim [res] Integer2IntOp arg@[hp, aa,sa,da]
    = gmpInteger2Int target res (hp, aa,sa,da)

  genprim [res] FloatEncodeOp args@[hp, aa,sa,da, expon] =
    encodeFloatingKind FloatKind target res (hp, aa,sa,da, expon)

  genprim [res] DoubleEncodeOp args@[hp, aa,sa,da, expon] =
    encodeFloatingKind DoubleKind target res (hp, aa,sa,da, expon)

  genprim [res] Int2AddrOp [arg] =
    simpleCoercion AddrKind res arg

  genprim [res] Addr2IntOp [arg] =
    simpleCoercion IntKind res arg

  genprim [res] Int2WordOp [arg] =
    simpleCoercion IntKind{-WordKind?-} res arg

  genprim [res] Word2IntOp [arg] =
    simpleCoercion IntKind res arg

\end{code}

The @ErrorIO@ primitive is actually a bit weird...assign a new value to the root
closure, flush stdout and stderr, and jump to the @ErrorIO_innards@.

\begin{code}

  genprim [] ErrorIOPrimOp [rhs] = 
    let changeTop = StAssign PtrKind topClosure (a2stix rhs)
    in
	returnSUs (\xs -> changeTop : flushStdout : flushStderr : errorIO : xs)

\end{code}

@newArray#@ ops allocate heap space.

\begin{code}
  genprim [res] NewArrayOp args =
    let	[liveness, n, initial] = map a2stix args
        result = a2stix res
    	space = StPrim IntAddOp [n, mut_hs]
    	loc = StIndex PtrKind stgHp 
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrKind result loc
    	initialise = StCall SLIT("newArrZh_init") VoidKind [result, n, initial]
    in
    	heap_chkr liveness space (StInt 0)	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk . (\xs -> assign : initialise : xs))

  genprim [res] (NewByteArrayOp pk) args =
    let	[liveness, count] = map a2stix args
        result = a2stix res
    	n = StPrim IntMulOp [count, StInt (toInteger (size_of pk))]
        slop = StPrim IntAddOp [n, StInt (toInteger (size_of IntKind - 1))]
        words = StPrim IntQuotOp [slop, StInt (toInteger (size_of IntKind))]
    	space = StPrim IntAddOp [n, StPrim IntAddOp [words, data_hs]]
    	loc = StIndex PtrKind stgHp 
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrKind result loc
    	init1 = StAssign PtrKind (StInd PtrKind loc) arrayOfData_info
        init2 = StAssign IntKind 
    	    	    	 (StInd IntKind 
    	    	    	    	(StIndex IntKind loc 
    	    	    	    	    	 (StInt (toInteger fixed_hs))))
                         (StPrim IntAddOp [words, 
    	    	    	    	    	  StInt (toInteger (var_hs (DataRep 0)))])
    in
    	heap_chkr liveness space (StInt 0)	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk . (\xs -> assign : init1 : init2 : xs))

  genprim [res] SameMutableArrayOp args =
    let compare = StPrim AddrEqOp (map a2stix args)
        assign = StAssign IntKind (a2stix res) compare
    in
        returnSUs (\xs -> assign : xs)

  genprim res@[_] SameMutableByteArrayOp args =
    genprim res SameMutableArrayOp args

\end{code}

Freezing an array of pointers is a double assignment.  We fix the header of
the ``new'' closure because the lhs is probably a better addressing mode for
the indirection (most likely, it's a VanillaReg).

\begin{code}

  genprim [lhs] UnsafeFreezeArrayOp [rhs] =
    let lhs' = a2stix lhs
    	rhs' = a2stix rhs
    	header = StInd PtrKind lhs'
	assign = StAssign PtrKind lhs' rhs'
	freeze = StAssign PtrKind header imMutArrayOfPtrs_info
    in
	returnSUs (\xs -> assign : freeze : xs)

  genprim [lhs] UnsafeFreezeByteArrayOp [rhs] =
    simpleCoercion PtrKind lhs rhs

\end{code}

Most other array primitives translate to simple indexing.

\begin{code}

  genprim lhs@[_] IndexArrayOp args =
    genprim lhs ReadArrayOp args

  genprim [lhs] ReadArrayOp [obj, ix] =
    let lhs' = a2stix lhs
    	obj' = a2stix obj
    	ix' = a2stix ix
    	base = StIndex IntKind obj' mut_hs
    	assign = StAssign PtrKind lhs' (StInd PtrKind (StIndex PtrKind base ix'))
    in
    	returnSUs (\xs -> assign : xs)

  genprim [lhs] WriteArrayOp [obj, ix, v] =
    let	obj' = a2stix obj
    	ix' = a2stix ix
    	v' = a2stix v
    	base = StIndex IntKind obj' mut_hs
    	assign = StAssign PtrKind (StInd PtrKind (StIndex PtrKind base ix')) v'
    in
    	returnSUs (\xs -> assign : xs)

  genprim lhs@[_] (IndexByteArrayOp pk) args =
    genprim lhs (ReadByteArrayOp pk) args

-- NB: indexing in "pk" units, *not* in bytes (WDP 95/09)

  genprim [lhs] (ReadByteArrayOp pk) [obj, ix] =
    let lhs' = a2stix lhs
    	obj' = a2stix obj
    	ix' = a2stix ix
    	base = StIndex IntKind obj' data_hs
    	assign = StAssign pk lhs' (StInd pk (StIndex pk base ix'))
    in
    	returnSUs (\xs -> assign : xs)

  genprim [lhs] (IndexOffAddrOp pk) [obj, ix] =
    let lhs' = a2stix lhs
    	obj' = a2stix obj
    	ix' = a2stix ix
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj' ix'))
    in
    	returnSUs (\xs -> assign : xs)

  genprim [] (WriteByteArrayOp pk) [obj, ix, v] =
    let	obj' = a2stix obj
    	ix' = a2stix ix
    	v' = a2stix v
    	base = StIndex IntKind obj' data_hs
    	assign = StAssign pk (StInd pk (StIndex pk base ix')) v'
    in
    	returnSUs (\xs -> assign : xs)
\end{code}

Stable pointer operations.

First the easy one.

\begin{code}

  genprim [lhs] DeRefStablePtrOp [sp] =
    let lhs' = a2stix lhs
    	pk = getAmodeKind lhs
    	sp' = a2stix sp
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
  genprim [lhs] MakeStablePtrOp args =
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

\begin{code}
  genprim res Word2IntegerOp args = panic "genPrimCode:Word2IntegerOp"

  genprim lhs (CCallOp fn is_asm may_gc arg_tys result_ty) rhs 
   | is_asm = error "ERROR: Native code generator can't handle casm"
   | otherwise =
    case lhs of
    	[] -> returnSUs (\xs -> (StCall fn VoidKind args) : xs)
    	[lhs] ->
    	    let lhs' = a2stix lhs
    	    	pk = if isFloatingKind (getAmodeKind lhs) then DoubleKind else IntKind
	    	call = StAssign pk lhs' (StCall fn pk args)
    	    in
	    	returnSUs (\xs -> call : xs)
    where
    	args = map amodeCodeForCCall rhs
        amodeCodeForCCall x = 
    	    let base = a2stix' x
    	    in
    	    	case getAmodeKind x of
    	    	    ArrayKind -> StIndex PtrKind base mut_hs
    	    	    ByteArrayKind -> StIndex IntKind base data_hs
		    MallocPtrKind -> error "ERROR: native-code generator can't handle Malloc Ptrs (yet): use -fvia-C!"
    	    	    _ -> base
\end{code}    

Now the more mundane operations.

\begin{code}
  genprim lhs op rhs = 
    let lhs' = map a2stix  lhs
    	rhs' = map a2stix' rhs
    in
        returnSUs (\ xs -> simplePrim lhs' op rhs' : xs)

  {-
  simpleCoercion 
      :: Target 
      -> PrimKind 
      -> [CAddrMode] 
      -> [CAddrMode] 
      -> SUniqSM StixTreeList
  -}
  simpleCoercion pk lhs rhs =
      returnSUs (\xs -> StAssign pk (a2stix lhs) (a2stix rhs) : xs)

\end{code}

Here we try to rewrite primitives into a form the code generator
can understand.	 Any primitives not handled here must be handled 
at the level of the specific code generator.

\begin{code}
  {-
  simplePrim 
    :: Target 
    -> [StixTree] 
    -> PrimOp 
    -> [StixTree] 
    -> StixTree
  -}
\end{code}

Now look for something more conventional.

\begin{code}

  simplePrim [lhs] op rest = StAssign pk lhs (StPrim op rest)
    where pk = if isCompareOp op then IntKind 
               else case getPrimOpResultInfo op of
		 ReturnsPrim pk -> pk
		 _ -> simplePrim_error op

  simplePrim _ op _ = simplePrim_error op

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

amodeCode'{-'-} target_STRICT am@(CVal rr CharKind) 
    | mixedTypeLocn am = StPrim ChrOp [amodeToStix target am]
    | otherwise = amodeToStix target am

amodeCode' target am = amodeToStix target am

amodeCode target_STRICT am
 = acode am
 where
 -- grab "target" things:
 hp_rel    = hpRel target
 char_like = charLikeClosureSize target
 int_like  = intLikeClosureSize target
 a2stix    = amodeToStix target

 -- real code: ----------------------------------
 acode am@(CVal rr CharKind) | mixedTypeLocn am =
	 StInd IntKind (acode (CAddr rr))

 acode (CVal rr pk) = StInd pk (acode (CAddr rr))

 acode (CAddr r@(SpARel spA off)) =
     StIndex PtrKind stgSpA (StInt (toInteger (spARelToInt r)))

 acode (CAddr r@(SpBRel spB off)) =
     StIndex IntKind stgSpB (StInt (toInteger (spBRelToInt r)))

 acode (CAddr (HpRel hp off)) =
     StIndex IntKind stgHp (StInt (toInteger (-(hp_rel (hp `subOff` off)))))

 acode (CAddr (NodeRel off)) =
     StIndex IntKind stgNode (StInt (toInteger (hp_rel off)))

 acode (CReg magic) = StReg (StixMagicId magic)
 acode (CTemp uniq pk) = StReg (StixTemp uniq pk)

 acode (CLbl lbl _) = StCLbl lbl

 acode (CUnVecLbl dir _) = StCLbl dir

 acode (CTableEntry base off pk) = 
     StInd pk (StIndex pk (acode base) (acode off))

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

 acode (CCharLike (CLit (MachChar c))) = 
     StLitLbl (uppBeside (uppPStr SLIT("CHARLIKE_closures+")) (uppInt off))
     where off = char_like * ord c

 acode (CCharLike x) = 
     StPrim IntAddOp [charLike, off]
     where off = StPrim IntMulOp [acode x, 
	     StInt (toInteger (char_like))]

 acode (CIntLike (CLit (MachInt i _))) = 
     StPrim IntAddOp [intLikePtr, StInt off]
     where off = toInteger int_like * i

 acode (CIntLike x) = 
     StPrim IntAddOp [intLikePtr, off]
     where off = StPrim IntMulOp [acode x,
	     StInt (toInteger int_like)]

 -- A CString is just a (CLit . MachStr)
 acode (CString s) = StString s

 acode (CLit core) = case core of
     (MachChar c) -> StInt (toInteger (ord c))
     (MachStr s) -> StString s
     (MachAddr a) -> StInt a
     (MachInt i _) -> StInt i
     (MachLitLit s _) -> StLitLit s
     (MachFloat d) -> StDouble d
     (MachDouble d) -> StDouble d
     _ -> panic "amodeCode:core literal"

 -- A CLitLit is just a (CLit . MachLitLit)
 acode (CLitLit s _) = StLitLit s

 -- COffsets are in words, not bytes!
 acode (COffset off) = StInt (toInteger (hp_rel off))

 acode (CMacroExpr _ macro [arg]) = 
     case macro of
	 INFO_PTR -> StInd PtrKind (a2stix arg)
	 ENTRY_CODE -> a2stix arg
	 INFO_TAG -> tag
	 EVAL_TAG -> StPrim IntGeOp [tag, StInt 0]
   where
     tag = StInd IntKind (StIndex IntKind (a2stix arg) (StInt (-2)))
     -- That ``-2'' really bothers me. (JSM)

 acode (CCostCentre cc print_as_string)
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

