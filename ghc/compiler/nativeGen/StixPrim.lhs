%
% (c) The AQUA Project, Glasgow University, 1993-1996
%

\begin{code}
#include "HsVersions.h"

module StixPrim ( primCode, amodeToStix, amodeToStix' ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(NcgLoop)		-- paranoia checking only

import MachMisc
import MachRegs

import AbsCSyn
import AbsCUtils	( getAmodeRep, mixedTypeLocn )
import CgCompInfo	( spARelToInt, spBRelToInt )
import CostCentre	( noCostCentreAttached )
import HeapOffs		( hpRelToInt, subOff )
import Literal		( Literal(..) )
import PrimOp		( PrimOp(..), isCompareOp, showPrimOp,
			  getPrimOpResultInfo, PrimOpResultInfo(..)
			)
import PrimRep		( PrimRep(..), isFloatingRep )
import OrdList		( OrdList )
import PprStyle		( PprStyle(..) )
import SMRep		( SMRep(..), SMSpecRepKind, SMUpdateKind )
import Stix
import StixMacro	( heapCheck )
import StixInteger	{- everything -}
import UniqSupply	( returnUs, thenUs, SYN_IE(UniqSM) )
import Unpretty		( uppBeside, uppPStr, uppInt )
import Util		( panic )

#ifdef REALLY_HASKELL_1_3
ord = fromEnum :: Char -> Int
#endif
\end{code}

The main honcho here is primCode, which handles the guts of COpStmts.

\begin{code}
arrayOfData_info      = sStLitLbl SLIT("ArrayOfData_info") -- out here to avoid CAF (sigh)
imMutArrayOfPtrs_info = sStLitLbl SLIT("ImMutArrayOfPtrs_info")

primCode
    :: [CAddrMode]  	-- results
    -> PrimOp 	    	-- op
    -> [CAddrMode]  	-- args
    -> UniqSM StixTreeList
\end{code}

First, the dreaded @ccall@.  We can't handle @casm@s.

Usually, this compiles to an assignment, but when the left-hand side
is empty, we just perform the call and ignore the result.

ToDo ADR: modify this to handle ForeignObjs.

btw Why not let programmer use casm to provide assembly code instead
of C code?  ADR

The (MP) integer operations are a true nightmare.  Since we don't have
a convenient abstract way of allocating temporary variables on the (C)
stack, we use the space just below HpLim for the @MP_INT@ structures,
and modify our heap check accordingly.

\begin{code}
-- NB: ordering of clauses somewhere driven by
-- the desire to getting sane patt-matching behavior

primCode res@[ar1,sr1,dr1, ar2,sr2,dr2]
	 IntegerQuotRemOp
	 args@[liveness, aa1,sa1,da1, aa2,sa2,da2]
  = gmpTake2Return2 (ar1,sr1,dr1, ar2,sr2,dr2) SLIT("mpz_divmod") (liveness, aa1,sa1,da1, aa2,sa2,da2)

primCode res@[ar1,sr1,dr1, ar2,sr2,dr2]
	 IntegerDivModOp
	 args@[liveness, aa1,sa1,da1, aa2,sa2,da2]
  = gmpTake2Return2 (ar1,sr1,dr1, ar2,sr2,dr2) SLIT("mpz_targetivmod") (liveness, aa1,sa1,da1, aa2,sa2,da2)

primCode res@[ar,sr,dr] IntegerAddOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2]
  = gmpTake2Return1 (ar,sr,dr) SLIT("mpz_add") (liveness, aa1,sa1,da1, aa2,sa2,da2)
primCode res@[ar,sr,dr] IntegerSubOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2]
  = gmpTake2Return1 (ar,sr,dr) SLIT("mpz_sub") (liveness, aa1,sa1,da1, aa2,sa2,da2)
primCode res@[ar,sr,dr] IntegerMulOp args@[liveness, aa1,sa1,da1, aa2,sa2,da2]
  = gmpTake2Return1 (ar,sr,dr) SLIT("mpz_mul") (liveness, aa1,sa1,da1, aa2,sa2,da2)

primCode res@[ar,sr,dr] IntegerNegOp arg@[liveness,aa,sa,da]
  = gmpTake1Return1 (ar,sr,dr) SLIT("mpz_neg") (liveness,aa,sa,da)
\end{code}

Since we are using the heap for intermediate @MP_INT@ structs, integer
comparison {\em does} require a heap check in the native code
implementation.

\begin{code}
primCode res@[exponr,ar,sr,dr] FloatDecodeOp args@[hp, arg]
  = decodeFloatingKind FloatRep (exponr,ar,sr,dr) (hp, arg)

primCode res@[exponr,ar,sr,dr] DoubleDecodeOp args@[hp, arg]
  = decodeFloatingKind DoubleRep (exponr,ar,sr,dr) (hp, arg)

primCode res@[ar,sr,dr] Int2IntegerOp args@[hp, n]
  = gmpInt2Integer (ar,sr,dr) (hp, n)

primCode res@[ar,sr,dr] Addr2IntegerOp args@[liveness,str]
  = gmpString2Integer (ar,sr,dr) (liveness,str)

primCode [res] IntegerCmpOp args@[hp, aa1,sa1,da1, aa2,sa2,da2]
  = gmpCompare res (hp, aa1,sa1,da1, aa2,sa2,da2)

primCode [res] Integer2IntOp arg@[hp, aa,sa,da]
  = gmpInteger2Int res (hp, aa,sa,da)

primCode [res] FloatEncodeOp args@[hp, aa,sa,da, expon]
  = encodeFloatingKind FloatRep res (hp, aa,sa,da, expon)

primCode [res] DoubleEncodeOp args@[hp, aa,sa,da, expon]
  = encodeFloatingKind DoubleRep res (hp, aa,sa,da, expon)

primCode [res] Int2AddrOp [arg]
  = simpleCoercion AddrRep res arg

primCode [res] Addr2IntOp [arg]
  = simpleCoercion IntRep res arg

primCode [res] Int2WordOp [arg]
  = simpleCoercion IntRep{-WordRep?-} res arg

primCode [res] Word2IntOp [arg]
  = simpleCoercion IntRep res arg
\end{code}

The @ErrorIO@ primitive is actually a bit weird...assign a new value
to the root closure, flush stdout and stderr, and jump to the
@ErrorIO_innards@.

\begin{code}
primCode [] ErrorIOPrimOp [rhs]
  = let
	changeTop = StAssign PtrRep topClosure (amodeToStix rhs)
    in
    returnUs (\xs -> changeTop : flushStdout : flushStderr : errorIO : xs)
\end{code}

@newArray#@ ops allocate heap space.

\begin{code}
primCode [res] NewArrayOp args
  = let
	[liveness, n, initial] = map amodeToStix args
	result = amodeToStix res
    	space = StPrim IntAddOp [n, mutHS]
    	loc = StIndex PtrRep stgHp
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrRep result loc
    	initialise = StCall SLIT("newArrZh_init") VoidRep [result, n, initial]
    in
    heapCheck liveness space (StInt 0)	`thenUs` \ heap_chk ->

    returnUs (heap_chk . (\xs -> assign : initialise : xs))

primCode [res] (NewByteArrayOp pk) args
  = let
	[liveness, count] = map amodeToStix args
	result = amodeToStix res
    	n = StPrim IntMulOp [count, StInt (sizeOf pk)]
	slop = StPrim IntAddOp [n, StInt (sizeOf IntRep - 1)]
	words = StPrim IntQuotOp [slop, StInt (sizeOf IntRep)]
    	space = StPrim IntAddOp [n, StPrim IntAddOp [words, dataHS]]
    	loc = StIndex PtrRep stgHp
    	      (StPrim IntNegOp [StPrim IntSubOp [space, StInt 1]])
    	assign = StAssign PtrRep result loc
    	init1 = StAssign PtrRep (StInd PtrRep loc) arrayOfData_info
	init2 = StAssign IntRep
    	    	    	 (StInd IntRep
    	    	    	    	(StIndex IntRep loc
    	    	    	    	    	 (StInt (toInteger fixedHdrSizeInWords))))
			 (StPrim IntAddOp [words,
    	    	    	    	    	  StInt (toInteger (varHdrSizeInWords (DataRep 0)))])
    in
    heapCheck liveness space (StInt 0)	`thenUs` \ heap_chk ->

    returnUs (heap_chk . (\xs -> assign : init1 : init2 : xs))

primCode [res] SameMutableArrayOp args
  = let
	compare = StPrim AddrEqOp (map amodeToStix args)
	assign = StAssign IntRep (amodeToStix res) compare
    in
    returnUs (\xs -> assign : xs)

primCode res@[_] SameMutableByteArrayOp args
  = primCode res SameMutableArrayOp args
\end{code}

Freezing an array of pointers is a double assignment.  We fix the
header of the ``new'' closure because the lhs is probably a better
addressing mode for the indirection (most likely, it's a VanillaReg).

\begin{code}

primCode [lhs] UnsafeFreezeArrayOp [rhs]
  = let
	lhs' = amodeToStix lhs
    	rhs' = amodeToStix rhs
    	header = StInd PtrRep lhs'
	assign = StAssign PtrRep lhs' rhs'
	freeze = StAssign PtrRep header imMutArrayOfPtrs_info
    in
    returnUs (\xs -> assign : freeze : xs)

primCode [lhs] UnsafeFreezeByteArrayOp [rhs]
  = simpleCoercion PtrRep lhs rhs
\end{code}

Most other array primitives translate to simple indexing.

\begin{code}

primCode lhs@[_] IndexArrayOp args
  = primCode lhs ReadArrayOp args

primCode [lhs] ReadArrayOp [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	base = StIndex IntRep obj' mutHS
    	assign = StAssign PtrRep lhs' (StInd PtrRep (StIndex PtrRep base ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [lhs] WriteArrayOp [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' mutHS
    	assign = StAssign PtrRep (StInd PtrRep (StIndex PtrRep base ix')) v'
    in
    returnUs (\xs -> assign : xs)

primCode lhs@[_] (IndexByteArrayOp pk) args
  = primCode lhs (ReadByteArrayOp pk) args

-- NB: indexing in "pk" units, *not* in bytes (WDP 95/09)

primCode [lhs] (ReadByteArrayOp pk) [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	base = StIndex IntRep obj' dataHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk base ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [lhs] (IndexOffAddrOp pk) [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj' ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [] (WriteByteArrayOp pk) [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' dataHS
    	assign = StAssign pk (StInd pk (StIndex pk base ix')) v'
    in
    returnUs (\xs -> assign : xs)
\end{code}

Stable pointer operations.

First the easy one.
\begin{code}

primCode [lhs] DeRefStablePtrOp [sp]
  = let
	lhs' = amodeToStix lhs
    	pk = getAmodeRep lhs
    	sp' = amodeToStix sp
	call = StCall SLIT("deRefStablePointer") pk [sp', smStablePtrTable]
    	assign = StAssign pk lhs' call
    in
    returnUs (\xs -> assign : xs)
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
primCode [lhs] MakeStablePtrOp args
  = let
	-- some useful abbreviations (I'm sure these must exist already)
	add = trPrim . IntAddOp
	sub = trPrim . IntSubOp
	one = trInt [1]
	dec x = trAssign IntRep [x, sub [x, one]]
	inc x = trAssign IntRep [x, add [x, one]]

	-- tedious hardwiring in of closure layout offsets (from SMClosures)
	dynHS = 2 + fixedHdrSizeInWords + varHdrSizeInWords DynamicRep
	spt_SIZE c   = trIndex PtrRep [c, trInt [fhs + gc_reserved] ]
	spt_NoPTRS c = trIndex PtrRep [c, trInt [fhs + gc_reserved + 1] ]
	spt_SPTR c i = trIndex PtrRep [c, add [trInt [dynHS], i]]
	spt_TOP c    = trIndex PtrRep [c, add [trInt [dynHS], spt_NoPTRS c]]
	spt_FREE c i = trIndex PtrRep [c, add [trInt [dynHS], spt_NoPTRS c]]

	-- tedious hardwiring in of stack manipulation macros (from SMClosures)
	spt_FULL c lbl =
		trCondJump lbl [trPrim IntEqOp [spt_TOP c, spt_NoPTRS c]]
	spt_EMPTY c lbl =
		trCondJump lbl [trPrim IntEqOp [spt_TOP c, trInt [0]]]
	spt_PUSH c f = [
		trAssign PtrRep [spt_FREE c (spt_TOP c), f],
		inc (spt_TOP c),
	spt_POP c x  = [
		dec (spt_TOP c),
		trAssign PtrRep [x, spt_FREE c (spt_TOP c)]
	]

	-- now to get down to business
	lhs' = amodeCode lhs
    	[liveness, unstable] = map amodeCode args

	spt = smStablePtrTable

	newSPT = -- a temporary (don't know how to allocate it)
	newSP = -- another temporary

	allocNewTable = -- some sort fo heap allocation needed
	copyOldTable = trCall "enlargeSPTable" PtrRep [newSPT, spt]

	enlarge =
		allocNewTable ++ [
		copyOldTable,
		trAssign PtrRep [spt, newSPT]
	allocate = [
		spt_POP spt newSP,
		trAssign PtrRep [spt_SPTR spt newSP, unstable],
		trAssign StablePtrRep [lhs', newSP]
	]

    in
    getUniqLabelCTS 	    	    	    	   `thenCTS` \ oklbl ->
    returnCodes sty md
	(spt_EMPTY spt oklbl : (enlarge ++ (trLabel [oklbl] : allocate)))
\end{pseudocode}

\begin{code}
primCode res Word2IntegerOp args = panic "primCode:Word2IntegerOp"

primCode lhs (CCallOp fn is_asm may_gc arg_tys result_ty) rhs
  | is_asm = error "ERROR: Native code generator can't handle casm"
  | otherwise
  = case lhs of
      [] -> returnUs (\xs -> (StCall fn VoidRep args) : xs)
      [lhs] ->
	  let lhs' = amodeToStix lhs
	      pk = if isFloatingRep (getAmodeRep lhs) then DoubleRep else IntRep
	      call = StAssign pk lhs' (StCall fn pk args)
	  in
	      returnUs (\xs -> call : xs)
  where
    args = map amodeCodeForCCall rhs
    amodeCodeForCCall x =
	let base = amodeToStix' x
	in
	    case getAmodeRep x of
	      ArrayRep -> StIndex PtrRep base mutHS
	      ByteArrayRep -> StIndex IntRep base dataHS
	      ForeignObjRep -> error "ERROR: native-code generator can't handle ForeignObjs (yet): use -fvia-C!"
	      _ -> base
\end{code}

Now the more mundane operations.

\begin{code}
primCode lhs op rhs
  = let
	lhs' = map amodeToStix  lhs
    	rhs' = map amodeToStix' rhs
    in
    returnUs (\ xs -> simplePrim lhs' op rhs' : xs)
\end{code}

\begin{code}
simpleCoercion
      :: PrimRep
      -> CAddrMode
      -> CAddrMode
      -> UniqSM StixTreeList

simpleCoercion pk lhs rhs
  = returnUs (\xs -> StAssign pk (amodeToStix lhs) (amodeToStix rhs) : xs)
\end{code}

Here we try to rewrite primitives into a form the code generator can
understand.  Any primitives not handled here must be handled at the
level of the specific code generator.

\begin{code}
simplePrim
    :: [StixTree]
    -> PrimOp
    -> [StixTree]
    -> StixTree
\end{code}

Now look for something more conventional.

\begin{code}
simplePrim [lhs] op rest
  = StAssign pk lhs (StPrim op rest)
  where
    pk = if isCompareOp op then
	    IntRep
	 else
	    case getPrimOpResultInfo op of
	       ReturnsPrim pk -> pk
	       _ -> simplePrim_error op

simplePrim _ op _ = simplePrim_error op

simplePrim_error op
    = error ("ERROR: primitive operation `"++showPrimOp PprDebug op++"'cannot be handled\nby the native-code generator.  Workaround: use -fvia-C.\n(Perhaps you should report it as a GHC bug, also.)\n")
\end{code}

%---------------------------------------------------------------------

Here we generate the Stix code for CAddrModes.

When a character is fetched from a mixed type location, we have to do
an extra cast.  This is reflected in amodeCode', which is for rhs
amodes that might possibly need the extra cast.

\begin{code}
amodeToStix, amodeToStix' :: CAddrMode -> StixTree

amodeToStix'{-'-} am@(CVal rr CharRep)
    | mixedTypeLocn am = StPrim ChrOp [amodeToStix am]
    | otherwise = amodeToStix am

amodeToStix' am = amodeToStix am

-----------
amodeToStix am@(CVal rr CharRep)
  | mixedTypeLocn am
  = StInd IntRep (amodeToStix (CAddr rr))

amodeToStix (CVal rr pk) = StInd pk (amodeToStix (CAddr rr))

amodeToStix (CAddr (SpARel spA off))
  = StIndex PtrRep stgSpA (StInt (toInteger (spARelToInt spA off)))

amodeToStix (CAddr (SpBRel spB off))
  = StIndex IntRep stgSpB (StInt (toInteger (spBRelToInt spB off)))

amodeToStix (CAddr (HpRel hp off))
  = StIndex IntRep stgHp (StInt (toInteger (-(hpRelToInt (hp `subOff` off)))))

amodeToStix (CAddr (NodeRel off))
  = StIndex IntRep stgNode (StInt (toInteger (hpRelToInt off)))

amodeToStix (CReg magic)    = StReg (StixMagicId magic)
amodeToStix (CTemp uniq pk) = StReg (StixTemp uniq pk)

amodeToStix (CLbl      lbl _) = StCLbl lbl
amodeToStix (CUnVecLbl dir _) = StCLbl dir

amodeToStix (CTableEntry base off pk)
  = StInd pk (StIndex pk (amodeToStix base) (amodeToStix off))

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeToStix (CCharLike (CLit (MachChar c)))
  = StLitLbl (uppBeside (uppPStr SLIT("CHARLIKE_closures+")) (uppInt off))
  where
    off = charLikeSize * ord c

amodeToStix (CCharLike x)
  = StPrim IntAddOp [charLike, off]
  where
    off = StPrim IntMulOp [amodeToStix x, StInt (toInteger charLikeSize)]

amodeToStix (CIntLike (CLit (MachInt i _)))
  = StPrim IntAddOp [intLikePtr, StInt off]
  where
    off = toInteger intLikeSize * i

amodeToStix (CIntLike x)
  = StPrim IntAddOp [intLikePtr, off]
  where
    off = StPrim IntMulOp [amodeToStix x, StInt (toInteger intLikeSize)]

 -- A CString is just a (CLit . MachStr)
amodeToStix (CString s) = StString s

amodeToStix (CLit core)
  = case core of
      MachChar c     -> StInt (toInteger (ord c))
      MachStr s	     -> StString s
      MachAddr a     -> StInt a
      MachInt i _    -> StInt i
      MachLitLit s _ -> StLitLit s
      MachFloat d    -> StDouble d
      MachDouble d   -> StDouble d
      _ -> panic "amodeToStix:core literal"

 -- A CLitLit is just a (CLit . MachLitLit)
amodeToStix (CLitLit s _) = StLitLit s

 -- COffsets are in words, not bytes!
amodeToStix (COffset off) = StInt (toInteger (hpRelToInt off))

amodeToStix (CMacroExpr _ macro [arg])
  = case macro of
      INFO_PTR   -> StInd PtrRep (amodeToStix arg)
      ENTRY_CODE -> amodeToStix arg
      INFO_TAG   -> tag
      EVAL_TAG   -> StPrim IntGeOp [tag, StInt 0]
   where
     tag = StInd IntRep (StIndex IntRep (amodeToStix arg) (StInt (-2)))
     -- That ``-2'' really bothers me. (JSM) (Replace w/ oTHER_TAG? [WDP])

amodeToStix (CCostCentre cc print_as_string)
  = if noCostCentreAttached cc
    then StComment SLIT("") -- sigh
    else panic "amodeToStix:CCostCentre"
\end{code}

Sizes of the CharLike and IntLike closures that are arranged as arrays
in the data segment.  (These are in bytes.)

\begin{code}
-- The INTLIKE base pointer

intLikePtr :: StixTree

intLikePtr = StInd PtrRep (sStLitLbl SLIT("INTLIKE_closures"))

-- The CHARLIKE base

charLike :: StixTree

charLike = sStLitLbl SLIT("CHARLIKE_closures")

-- Trees for the ErrorIOPrimOp

topClosure, flushStdout, flushStderr, errorIO :: StixTree

topClosure = StInd PtrRep (sStLitLbl SLIT("TopClosure"))
flushStdout = StCall SLIT("fflush") VoidRep [StLitLit SLIT("stdout")]
flushStderr = StCall SLIT("fflush") VoidRep [StLitLit SLIT("stderr")]
errorIO = StJump (StInd PtrRep (sStLitLbl SLIT("ErrorIO_innards")))
\end{code}

