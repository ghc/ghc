%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module SparcGen (
	sparcCodeGen,

	-- and, for self-sufficiency
	PprStyle, StixTree, CSeq
    ) where

IMPORT_Trace

import AbsCSyn	    ( AbstractC, MagicId(..), kindFromMagicId )
import PrelInfo	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import AsmRegAlloc  ( runRegAllocate, mkReg, extractMappedRegNos,
		      Reg(..), RegLiveness(..), RegUsage(..),
    	    	      FutureLive(..), MachineRegisters(..), MachineCode(..)
    	    	    )
import CLabel   ( CLabel, isAsmTemp )
import SparcCode    {- everything -}
import MachDesc
import Maybes	    ( maybeToBool, Maybe(..) )
import OrdList	    -- ( mkEmptyList, mkUnitList, mkSeqList, mkParList, OrdList )
import Outputable
import SparcDesc
import Stix
import UniqSupply
import Pretty
import Unpretty
import Util

type CodeBlock a = (OrdList a -> OrdList a)
\end{code}

%************************************************************************
%*									*
\subsection[SparcCodeGen]{Generating Sparc Code}
%*									*
%************************************************************************

This is the top-level code-generation function for the Sparc.

\begin{code}

sparcCodeGen :: PprStyle -> [[StixTree]] -> UniqSM Unpretty
sparcCodeGen sty trees =
    mapUs genSparcCode trees	    	`thenUs` \ dynamicCodes ->
    let
    	staticCodes = scheduleSparcCode dynamicCodes
    	pretty = printLabeledCodes sty staticCodes
    in
    	returnUs pretty

\end{code}

This bit does the code scheduling.  The scheduler must also deal with
register allocation of temporaries.  Much parallelism can be exposed via
the OrdList, but more might occur, so further analysis might be needed.

\begin{code}

scheduleSparcCode :: [SparcCode] -> [SparcInstr]
scheduleSparcCode = concat . map (runRegAllocate freeSparcRegs reservedRegs)
  where
    freeSparcRegs :: SparcRegs
    freeSparcRegs = mkMRegs (extractMappedRegNos freeRegs)


\end{code}

Registers passed up the tree.  If the stix code forces the register
to live in a pre-decided machine register, it comes out as @Fixed@;
otherwise, it comes out as @Any@, and the parent can decide which
register to put it in.

\begin{code}

data Register
  = Fixed Reg PrimRep (CodeBlock SparcInstr)
  | Any PrimRep (Reg -> (CodeBlock SparcInstr))

registerCode :: Register -> Reg -> CodeBlock SparcInstr
registerCode (Fixed _ _ code) reg = code
registerCode (Any _ code) reg = code reg

registerName :: Register -> Reg -> Reg
registerName (Fixed reg _ _) _ = reg
registerName (Any _ _) reg = reg

registerKind :: Register -> PrimRep
registerKind (Fixed _ pk _) = pk
registerKind (Any pk _) = pk

isFixed :: Register -> Bool
isFixed (Fixed _ _ _) = True
isFixed (Any _ _)     = False

\end{code}

Memory addressing modes passed up the tree.

\begin{code}

data Amode = Amode Addr (CodeBlock SparcInstr)

amodeAddr (Amode addr _) = addr
amodeCode (Amode _ code) = code

\end{code}

Condition codes passed up the tree.

\begin{code}

data Condition = Condition Bool Cond (CodeBlock SparcInstr)

condName (Condition _ cond _) = cond
condFloat (Condition float _ _) = float
condCode (Condition _ _ code) = code

\end{code}

General things for putting together code sequences.

\begin{code}

asmVoid :: OrdList SparcInstr
asmVoid = mkEmptyList

asmInstr :: SparcInstr -> SparcCode
asmInstr i = mkUnitList i

asmSeq :: [SparcInstr] -> SparcCode
asmSeq is = foldr (mkSeqList . asmInstr) asmVoid is

asmParThen :: [SparcCode] -> (CodeBlock SparcInstr)
asmParThen others code = mkSeqList (foldr mkParList mkEmptyList others) code

returnInstr :: SparcInstr -> UniqSM (CodeBlock SparcInstr)
returnInstr instr = returnUs (\xs -> mkSeqList (asmInstr instr) xs)

returnInstrs :: [SparcInstr] -> UniqSM (CodeBlock SparcInstr)
returnInstrs instrs = returnUs (\xs -> mkSeqList (asmSeq instrs) xs)

returnSeq :: (CodeBlock SparcInstr) -> [SparcInstr] -> UniqSM (CodeBlock SparcInstr)
returnSeq code instrs = returnUs (\xs -> code (mkSeqList (asmSeq instrs) xs))

mkSeqInstr :: SparcInstr -> (CodeBlock SparcInstr)
mkSeqInstr instr code = mkSeqList (asmInstr instr) code

mkSeqInstrs :: [SparcInstr] -> (CodeBlock SparcInstr)
mkSeqInstrs instrs code = mkSeqList (asmSeq instrs) code

\end{code}

Top level sparc code generator for a chunk of stix code.

\begin{code}

genSparcCode :: [StixTree] -> UniqSM (SparcCode)

genSparcCode trees =
    mapUs getCode trees    	    	`thenUs` \ blocks ->
    returnUs (foldr (.) id blocks asmVoid)

\end{code}

Code extractor for an entire stix tree---stix statement level.

\begin{code}

getCode
    :: StixTree     -- a stix statement
    -> UniqSM (CodeBlock SparcInstr)

getCode (StSegment seg) = returnInstr (SEGMENT seg)

getCode (StAssign pk dst src)
  | isFloatingRep pk = assignFltCode pk dst src
  | otherwise = assignIntCode pk dst src

getCode (StLabel lab) = returnInstr (LABEL lab)

getCode (StFunBegin lab) = returnInstr (LABEL lab)

getCode (StFunEnd lab) = returnUs id

getCode (StJump arg) = genJump arg

getCode (StFallThrough lbl) = returnUs id

getCode (StCondJump lbl arg) = genCondJump lbl arg

getCode (StData kind args) =
    mapAndUnzipUs getData args		    `thenUs` \ (codes, imms) ->
    returnUs (\xs -> mkSeqList (asmInstr (DATA (kindToSize kind) imms))
				(foldr1 (.) codes xs))
  where
    getData :: StixTree -> UniqSM (CodeBlock SparcInstr, Imm)
    getData (StInt i) = returnUs (id, ImmInteger i)
    getData (StDouble d) = returnUs (id, strImmLit ('0' : 'r' : ppShow 80 (ppRational d)))
    getData (StLitLbl s) = returnUs (id, ImmLab s)
    getData (StLitLit s) = returnUs (id, strImmLit (cvtLitLit (_UNPK_ s)))
    getData (StString s) =
	getUniqLabelNCG 	    	    `thenUs` \ lbl ->
	returnUs (mkSeqInstrs [LABEL lbl, ASCII True (_UNPK_ s)], ImmCLbl lbl)
    getData (StCLbl l)   = returnUs (id, ImmCLbl l)

getCode (StCall fn VoidRep args) = genCCall fn VoidRep args

getCode (StComment s) = returnInstr (COMMENT s)

\end{code}

Generate code to get a subtree into a register.

\begin{code}

getReg :: StixTree -> UniqSM Register

getReg (StReg (StixMagicId stgreg)) =
    case stgRegMap stgreg of
    	Just reg -> returnUs (Fixed reg (kindFromMagicId stgreg) id)
    	-- cannae be Nothing

getReg (StReg (StixTemp u pk)) = returnUs (Fixed (UnmappedReg u pk) pk id)

getReg (StDouble d) =
    getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
	    DATA DF [strImmLit ('0' : 'r' : ppShow  80 (ppRational d))],
	    SEGMENT TextSegment,
	    SETHI (HI (ImmCLbl lbl)) tmp,
	    LD DF (AddrRegImm tmp (LO (ImmCLbl lbl))) dst]
    in
    	returnUs (Any DoubleRep code)

getReg (StString s) =
    getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII True (_UNPK_ s),
	    SEGMENT TextSegment,
	    SETHI (HI (ImmCLbl lbl)) dst,
	    OR False dst (RIImm (LO (ImmCLbl lbl))) dst]
    in
    	returnUs (Any PtrRep code)

getReg (StLitLit s) | _HEAD_ s == '"' && last xs == '"' =
    getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII False (init xs),
	    SEGMENT TextSegment,
	    SETHI (HI (ImmCLbl lbl)) dst,
	    OR False dst (RIImm (LO (ImmCLbl lbl))) dst]
    in
    	returnUs (Any PtrRep code)
  where
    xs = _UNPK_ (_TAIL_ s)

getReg tree@(StIndex _ _ _) = getReg (mangleIndexTree tree)

getReg (StCall fn kind args) =
    genCCall fn kind args   	    `thenUs` \ call ->
    returnUs (Fixed reg kind call)
  where
    reg = if isFloatingRep kind then f0 else o0

getReg (StPrim primop args) =
    case primop of

    	CharGtOp -> condIntReg GT args
    	CharGeOp -> condIntReg GE args
    	CharEqOp -> condIntReg EQ args
    	CharNeOp -> condIntReg NE args
    	CharLtOp -> condIntReg LT args
    	CharLeOp -> condIntReg LE args

    	IntAddOp -> trivialCode (ADD False False) args

    	IntSubOp -> trivialCode (SUB False False) args
    	IntMulOp -> call SLIT(".umul") IntRep
    	IntQuotOp -> call SLIT(".div") IntRep
    	IntRemOp -> call SLIT(".rem") IntRep
    	IntNegOp -> trivialUCode (SUB False False g0) args
    	IntAbsOp -> absIntCode args

    	AndOp -> trivialCode (AND False) args
    	OrOp  -> trivialCode (OR False) args
    	NotOp -> trivialUCode (XNOR False g0) args
    	SllOp -> trivialCode SLL args
    	SraOp -> trivialCode SRA args
    	SrlOp -> trivialCode SRL args
    	ISllOp -> panic "SparcGen:isll"
    	ISraOp -> panic "SparcGen:isra"
    	ISrlOp -> panic "SparcGen:isrl"

    	IntGtOp -> condIntReg GT args
    	IntGeOp -> condIntReg GE args
    	IntEqOp -> condIntReg EQ args
    	IntNeOp -> condIntReg NE args
    	IntLtOp -> condIntReg LT args
    	IntLeOp -> condIntReg LE args

    	WordGtOp -> condIntReg GU args
    	WordGeOp -> condIntReg GEU args
    	WordEqOp -> condIntReg EQ args
    	WordNeOp -> condIntReg NE args
    	WordLtOp -> condIntReg LU args
    	WordLeOp -> condIntReg LEU args

    	AddrGtOp -> condIntReg GU args
    	AddrGeOp -> condIntReg GEU args
    	AddrEqOp -> condIntReg EQ args
    	AddrNeOp -> condIntReg NE args
    	AddrLtOp -> condIntReg LU args
    	AddrLeOp -> condIntReg LEU args

    	FloatAddOp -> trivialFCode FloatRep FADD args
    	FloatSubOp -> trivialFCode FloatRep FSUB args
    	FloatMulOp -> trivialFCode FloatRep FMUL args
    	FloatDivOp -> trivialFCode FloatRep FDIV args
    	FloatNegOp -> trivialUFCode FloatRep (FNEG F) args

    	FloatGtOp -> condFltReg GT args
    	FloatGeOp -> condFltReg GE args
    	FloatEqOp -> condFltReg EQ args
    	FloatNeOp -> condFltReg NE args
    	FloatLtOp -> condFltReg LT args
    	FloatLeOp -> condFltReg LE args

    	FloatExpOp -> promoteAndCall SLIT("exp") DoubleRep
    	FloatLogOp -> promoteAndCall SLIT("log") DoubleRep
    	FloatSqrtOp -> promoteAndCall SLIT("sqrt") DoubleRep

    	FloatSinOp -> promoteAndCall SLIT("sin") DoubleRep
    	FloatCosOp -> promoteAndCall SLIT("cos") DoubleRep
    	FloatTanOp -> promoteAndCall SLIT("tan") DoubleRep

    	FloatAsinOp -> promoteAndCall SLIT("asin") DoubleRep
    	FloatAcosOp -> promoteAndCall SLIT("acos") DoubleRep
    	FloatAtanOp -> promoteAndCall SLIT("atan") DoubleRep

    	FloatSinhOp -> promoteAndCall SLIT("sinh") DoubleRep
    	FloatCoshOp -> promoteAndCall SLIT("cosh") DoubleRep
    	FloatTanhOp -> promoteAndCall SLIT("tanh") DoubleRep

    	FloatPowerOp -> promoteAndCall SLIT("pow") DoubleRep

    	DoubleAddOp -> trivialFCode DoubleRep FADD args
    	DoubleSubOp -> trivialFCode DoubleRep FSUB args
    	DoubleMulOp -> trivialFCode DoubleRep FMUL args
   	DoubleDivOp -> trivialFCode DoubleRep FDIV args
    	DoubleNegOp -> trivialUFCode DoubleRep (FNEG DF) args

    	DoubleGtOp -> condFltReg GT args
    	DoubleGeOp -> condFltReg GE args
    	DoubleEqOp -> condFltReg EQ args
    	DoubleNeOp -> condFltReg NE args
    	DoubleLtOp -> condFltReg LT args
    	DoubleLeOp -> condFltReg LE args

    	DoubleExpOp -> call SLIT("exp") DoubleRep
    	DoubleLogOp -> call SLIT("log") DoubleRep
    	DoubleSqrtOp -> call SLIT("sqrt") DoubleRep

    	DoubleSinOp -> call SLIT("sin") DoubleRep
    	DoubleCosOp -> call SLIT("cos") DoubleRep
    	DoubleTanOp -> call SLIT("tan") DoubleRep

    	DoubleAsinOp -> call SLIT("asin") DoubleRep
    	DoubleAcosOp -> call SLIT("acos") DoubleRep
    	DoubleAtanOp -> call SLIT("atan") DoubleRep

    	DoubleSinhOp -> call SLIT("sinh") DoubleRep
    	DoubleCoshOp -> call SLIT("cosh") DoubleRep
    	DoubleTanhOp -> call SLIT("tanh") DoubleRep

    	DoublePowerOp -> call SLIT("pow") DoubleRep

    	OrdOp -> coerceIntCode IntRep args
    	ChrOp -> chrCode args

    	Float2IntOp -> coerceFP2Int args
    	Int2FloatOp -> coerceInt2FP FloatRep args
    	Double2IntOp -> coerceFP2Int args
    	Int2DoubleOp -> coerceInt2FP DoubleRep args

    	Double2FloatOp -> trivialUFCode FloatRep (FxTOy DF F) args
    	Float2DoubleOp -> trivialUFCode DoubleRep (FxTOy F DF) args

  where
    call fn pk = getReg (StCall fn pk args)
    promoteAndCall fn pk = getReg (StCall fn pk (map promote args))
      where
	promote x = StPrim Float2DoubleOp [x]

getReg (StInd pk mem) =
    getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = kindToSize pk
    	code__2 dst = code . mkSeqInstr (LD size src dst)
    in
    	returnUs (Any pk code__2)

getReg (StInt i)
  | is13Bits i =
    let
    	src = ImmInt (fromInteger i)
    	code dst = mkSeqInstr (OR False g0 (RIImm src) dst)
    in
    	returnUs (Any IntRep code)

getReg leaf
  | maybeToBool imm =
    let
    	code dst = mkSeqInstrs [
    	    SETHI (HI imm__2) dst,
    	    OR False dst (RIImm (LO imm__2)) dst]
    in
    	returnUs (Any PtrRep code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

\end{code}

Now, given a tree (the argument to an StInd) that references memory,
produce a suitable addressing mode.

\begin{code}

getAmode :: StixTree -> UniqSM Amode

getAmode tree@(StIndex _ _ _) = getAmode (mangleIndexTree tree)

getAmode (StPrim IntSubOp [x, StInt i])
  | is13Bits (-i) =
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    getReg x    	    	    `thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    	returnUs (Amode (AddrRegImm reg off) code)


getAmode (StPrim IntAddOp [x, StInt i])
  | is13Bits i =
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    getReg x    	    	    `thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    	returnUs (Amode (AddrRegImm reg off) code)

getAmode (StPrim IntAddOp [x, y]) =
    getNewRegNCG PtrRep    	    `thenUs` \ tmp1 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp2 ->
    getReg x    	    	    `thenUs` \ register1 ->
    getReg y    	    	    `thenUs` \ register2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	reg1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	reg2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2]
    in
    	returnUs (Amode (AddrRegReg reg1 reg2) code__2)

getAmode leaf
  | maybeToBool imm =
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	code = mkSeqInstr (SETHI (HI imm__2) tmp)
    in
    	returnUs (Amode (AddrRegImm tmp (LO imm__2)) code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other =
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    getReg other    	    	    `thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt 0
    in
    	returnUs (Amode (AddrRegImm reg off) code)

\end{code}

Try to get a value into a specific register (or registers) for a call.  The Sparc
calling convention is an absolute nightmare.  The first 6x32 bits of arguments are
mapped into %o0 through %o5, and the remaining arguments are dumped to the stack,
beginning at [%sp+92].  (Note that %o6 == %sp.)  Our first argument is a pair of
the list of remaining argument registers to be assigned for this call and the next
stack offset to use for overflowing arguments.  This way, @getCallArg@ can be applied
to all of a call's arguments using @mapAccumL@.

\begin{code}

getCallArg
    :: ([Reg],Int)   	    -- Argument registers and stack offset (accumulator)
    -> StixTree 	    -- Current argument
    -> UniqSM (([Reg],Int), CodeBlock SparcInstr)    -- Updated accumulator and code

-- We have to use up all of our argument registers first.

getCallArg (dst:dsts, offset) arg =
    getReg arg	    	    	    `thenUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenUs` \ tmp ->
    let
    	reg = if isFloatingRep pk then tmp else dst
    	code = registerCode register reg
    	src = registerName register reg
    	pk = registerKind register
    in
    	returnUs (case pk of
    	    DoubleRep ->
    	    	case dsts of
    	    	    [] -> (([], offset + 1), code . mkSeqInstrs [
    	    	    	    -- conveniently put the second part in the right stack
    	    	    	    -- location, and load the first part into %o5
    	    	    	    ST DF src (spRel (offset - 1)),
    	    	    	    LD W (spRel (offset - 1)) dst])
    	    	    (dst__2:dsts__2) -> ((dsts__2, offset), code . mkSeqInstrs [
    	    	    	    ST DF src (spRel (-2)),
    	    	    	    LD W (spRel (-2)) dst,
    	    	    	    LD W (spRel (-1)) dst__2])
    	    FloatRep -> ((dsts, offset), code . mkSeqInstrs [
    	    	    	    ST F src (spRel (-2)),
    	    	    	    LD W (spRel (-2)) dst])
    	    _ -> ((dsts, offset), if isFixed register then
    	    	    	    	  code . mkSeqInstr (OR False g0 (RIReg src) dst)
    	    	    	    	  else code))

-- Once we have run out of argument registers, we move to the stack

getCallArg ([], offset) arg =
    getReg arg	    	    	    `thenUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src = registerName register tmp
    	pk = registerKind register
    	sz = kindToSize pk
    	words = if pk == DoubleRep then 2 else 1
    in
    	returnUs (([], offset + words), code . mkSeqInstr (ST sz src (spRel offset)))

\end{code}

Set up a condition code for a conditional branch.

\begin{code}

getCondition :: StixTree -> UniqSM Condition

getCondition (StPrim primop args) =
    case primop of

    	CharGtOp -> condIntCode GT args
    	CharGeOp -> condIntCode GE args
    	CharEqOp -> condIntCode EQ args
    	CharNeOp -> condIntCode NE args
    	CharLtOp -> condIntCode LT args
    	CharLeOp -> condIntCode LE args

    	IntGtOp -> condIntCode GT args
    	IntGeOp -> condIntCode GE args
    	IntEqOp -> condIntCode EQ args
    	IntNeOp -> condIntCode NE args
   	IntLtOp -> condIntCode LT args
    	IntLeOp -> condIntCode LE args

    	WordGtOp -> condIntCode GU args
    	WordGeOp -> condIntCode GEU args
    	WordEqOp -> condIntCode EQ args
    	WordNeOp -> condIntCode NE args
    	WordLtOp -> condIntCode LU args
    	WordLeOp -> condIntCode LEU args

    	AddrGtOp -> condIntCode GU args
    	AddrGeOp -> condIntCode GEU args
    	AddrEqOp -> condIntCode EQ args
    	AddrNeOp -> condIntCode NE args
    	AddrLtOp -> condIntCode LU args
    	AddrLeOp -> condIntCode LEU args

    	FloatGtOp -> condFltCode GT args
    	FloatGeOp -> condFltCode GE args
    	FloatEqOp -> condFltCode EQ args
    	FloatNeOp -> condFltCode NE args
    	FloatLtOp -> condFltCode LT args
    	FloatLeOp -> condFltCode LE args

    	DoubleGtOp -> condFltCode GT args
    	DoubleGeOp -> condFltCode GE args
    	DoubleEqOp -> condFltCode EQ args
    	DoubleNeOp -> condFltCode NE args
    	DoubleLtOp -> condFltCode LT args
    	DoubleLeOp -> condFltCode LE args

\end{code}

Turn a boolean expression into a condition, to be passed
back up the tree.

\begin{code}

condIntCode, condFltCode :: Cond -> [StixTree] -> UniqSM Condition

condIntCode cond [x, StInt y]
  | is13Bits y =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    let
	code = registerCode register tmp
	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
	code__2 = code . mkSeqInstr (SUB False True src1 (RIImm src2) g0)
    in
	returnUs (Condition False cond code__2)

condIntCode cond [x, y] =
    getReg x	    	    	    `thenUs` \ register1 ->
    getReg y	    	    	    `thenUs` \ register2 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp1 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp2 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
	code__2 = asmParThen [code1, code2] .
    	    	mkSeqInstr (SUB False True src1 (RIReg src2) g0)
    in
	returnUs (Condition False cond code__2)

condFltCode cond [x, y] =
    getReg x	    	    	    `thenUs` \ register1 ->
    getReg y	    	    	    `thenUs` \ register2 ->
    getNewRegNCG (registerKind register1)
      	    	        	    `thenUs` \ tmp1 ->
    getNewRegNCG (registerKind register2)
     	    	        	    `thenUs` \ tmp2 ->
    getNewRegNCG DoubleRep   	    `thenUs` \ tmp ->
    let
    	promote x = asmInstr (FxTOy F DF x tmp)

    	pk1   = registerKind register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	pk2   = registerKind register2
    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 =
    	    	if pk1 == pk2 then
    	            asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	    mkSeqInstr (FCMP True (kindToSize pk1) src1 src2)
    	    	else if pk1 == FloatRep then
    	    	    asmParThen [code1 (promote src1), code2 asmVoid] .
    	    	    mkSeqInstr (FCMP True DF tmp src2)
    	    	else
    	    	    asmParThen [code1 asmVoid, code2 (promote src2)] .
    	    	    mkSeqInstr (FCMP True DF src1 tmp)
    in
    	returnUs (Condition True cond code__2)

\end{code}

Turn those condition codes into integers now (when they appear on
the right hand side of an assignment).

Do not fill the delay slots here; you will confuse the register allocator.

\begin{code}

condIntReg :: Cond -> [StixTree] -> UniqSM Register

condIntReg EQ [x, StInt 0] =
    getReg x    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep   	    `thenUs` \ tmp ->
    let
	code = registerCode register tmp
	src  = registerName register tmp
	code__2 dst = code . mkSeqInstrs [
    	    SUB False True g0 (RIReg src) g0,
    	    SUB True False g0 (RIImm (ImmInt (-1))) dst]
    in
	returnUs (Any IntRep code__2)

condIntReg EQ [x, y] =
    getReg x	    	    `thenUs` \ register1 ->
    getReg y	    	    `thenUs` \ register2 ->
    getNewRegNCG IntRep        `thenUs` \ tmp1 ->
    getNewRegNCG IntRep        `thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = asmParThen [code1, code2] . mkSeqInstrs [
    	    XOR False src1 (RIReg src2) dst,
    	    SUB False True g0 (RIReg dst) g0,
    	    SUB True False g0 (RIImm (ImmInt (-1))) dst]
    in
	returnUs (Any IntRep code__2)

condIntReg NE [x, StInt 0] =
    getReg x    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep   	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstrs [
    	    SUB False True g0 (RIReg src) g0,
    	    ADD True False g0 (RIImm (ImmInt 0)) dst]
    in
	returnUs (Any IntRep code__2)

condIntReg NE [x, y] =
    getReg x	    	    `thenUs` \ register1 ->
    getReg y	    	    `thenUs` \ register2 ->
    getNewRegNCG IntRep        `thenUs` \ tmp1 ->
    getNewRegNCG IntRep        `thenUs` \ tmp2 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
	code__2 dst = asmParThen [code1, code2] . mkSeqInstrs [
    	    XOR False src1 (RIReg src2) dst,
    	    SUB False True g0 (RIReg dst) g0,
    	    ADD True False g0 (RIImm (ImmInt 0)) dst]
    in
	returnUs (Any IntRep code__2)

condIntReg cond args =
    getUniqLabelNCG	    	    `thenUs` \ lbl1 ->
    getUniqLabelNCG	    	    `thenUs` \ lbl2 ->
    condIntCode cond args 	    `thenUs` \ condition ->
    let
	code = condCode condition
	cond = condName condition
	code__2 dst = code . mkSeqInstrs [
	    BI cond False (ImmCLbl lbl1), NOP,
	    OR False g0 (RIImm (ImmInt 0)) dst,
	    BI ALWAYS False (ImmCLbl lbl2), NOP,
	    LABEL lbl1,
	    OR False g0 (RIImm (ImmInt 1)) dst,
	    LABEL lbl2]
    in
	returnUs (Any IntRep code__2)

condFltReg :: Cond -> [StixTree] -> UniqSM Register

condFltReg cond args =
    getUniqLabelNCG	    	    `thenUs` \ lbl1 ->
    getUniqLabelNCG	    	    `thenUs` \ lbl2 ->
    condFltCode cond args 	    `thenUs` \ condition ->
    let
    	code = condCode condition
    	cond = condName condition
    	code__2 dst = code . mkSeqInstrs [
    	    NOP,
	    BF cond False (ImmCLbl lbl1), NOP,
	    OR False g0 (RIImm (ImmInt 0)) dst,
	    BI ALWAYS False (ImmCLbl lbl2), NOP,
	    LABEL lbl1,
	    OR False g0 (RIImm (ImmInt 1)) dst,
	    LABEL lbl2]
    in
	returnUs (Any IntRep code__2)

\end{code}

Assignments are really at the heart of the whole code generation business.
Almost all top-level nodes of any real importance are assignments, which
correspond to loads, stores, or register transfers.  If we're really lucky,
some of the register transfers will go away, because we can use the destination
register to complete the code generation for the right hand side.  This only
fails when the right hand side is forced into a fixed register (e.g. the result
of a call).

\begin{code}

assignIntCode :: PrimRep -> StixTree -> StixTree -> UniqSM (CodeBlock SparcInstr)

assignIntCode pk (StInd _ dst) src =
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getReg src	    	    	    `thenUs` \ register ->
    let
    	code1 = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2 = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz    = kindToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    	returnUs code__2

assignIntCode pk dst src =
    getReg dst	    	    	    `thenUs` \ register1 ->
    getReg src	    	    	    `thenUs` \ register2 ->
    let
    	dst__2 = registerName register1 g0
    	code = registerCode register2 dst__2
    	src__2 = registerName register2 dst__2
    	code__2 = if isFixed register2 then
    	    	    code . mkSeqInstr (OR False g0 (RIReg src__2) dst__2)
    	    	else code
    in
    	returnUs code__2

assignFltCode :: PrimRep -> StixTree -> StixTree -> UniqSM (CodeBlock SparcInstr)

assignFltCode pk (StInd _ dst) src =
    getNewRegNCG pk        	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getReg src	    	    	    `thenUs` \ register ->
    let
    	sz    = kindToSize pk
    	dst__2  = amodeAddr amode

    	code1 = amodeCode amode asmVoid
    	code2 = registerCode register tmp asmVoid

    	src__2  = registerName register tmp
    	pk__2  = registerKind register
    	sz__2 = kindToSize pk__2

    	code__2 = asmParThen [code1, code2] .
	    if pk == pk__2 then
		mkSeqInstr (ST sz src__2 dst__2)
	    else
		mkSeqInstrs [FxTOy sz__2 sz src__2 tmp, ST sz tmp dst__2]
    in
	returnUs code__2

assignFltCode pk dst src =
    getReg dst	    	    	    `thenUs` \ register1 ->
    getReg src	    	    	    `thenUs` \ register2 ->
    getNewRegNCG (registerKind register2)
    	    	        	    `thenUs` \ tmp ->
    let
    	sz = kindToSize pk
    	dst__2 = registerName register1 g0    -- must be Fixed

    	reg__2 = if pk /= pk__2 then tmp else dst__2

    	code = registerCode register2 reg__2
    	src__2 = registerName register2 reg__2
    	pk__2  = registerKind register2
    	sz__2 = kindToSize pk__2

    	code__2 = if pk /= pk__2 then code . mkSeqInstr (FxTOy sz__2 sz src__2 dst__2)
    	    	else if isFixed register2 then code . mkSeqInstr (FMOV sz src__2 dst__2)
    	    	else code
    in
    	returnUs code__2

\end{code}

Generating an unconditional branch.  We accept two types of targets:
an immediate CLabel or a tree that gets evaluated into a register.
Any CLabels which are AsmTemporaries are assumed to be in the local
block of code, close enough for a branch instruction.  Other CLabels
are assumed to be far away, so we use call.

Do not fill the delay slots here; you will confuse the register allocator.

\begin{code}

genJump
    :: StixTree     -- the branch target
    -> UniqSM (CodeBlock SparcInstr)

genJump (StCLbl lbl)
  | isAsmTemp lbl = returnInstrs [BI ALWAYS False target, NOP]
  | otherwise     = returnInstrs [CALL target 0 True, NOP]
  where
    target = ImmCLbl lbl

genJump tree =
    getReg tree	    	    	    `thenUs` \ register ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	target = registerName register tmp
    in
    	returnSeq code [JMP (AddrRegReg target g0), NOP]

\end{code}

Conditional jumps are always to local labels, so we can use
branch instructions.  First, we have to ensure that the condition
codes are set according to the supplied comparison operation.
We generate slightly different code for floating point comparisons,
because a floating point operation cannot directly precede a @BF@.
We assume the worst and fill that slot with a @NOP@.

Do not fill the delay slots here; you will confuse the register allocator.

\begin{code}

genCondJump
    :: CLabel	    -- the branch target
    -> StixTree     -- the condition on which to branch
    -> UniqSM (CodeBlock SparcInstr)

genCondJump lbl bool =
    getCondition bool  	    	    `thenUs` \ condition ->
    let
    	code = condCode condition
    	cond = condName condition
	target = ImmCLbl lbl
    in
    	if condFloat condition then
    	    returnSeq code [NOP, BF cond False target, NOP]
    	else
    	    returnSeq code [BI cond False target, NOP]

\end{code}

Now the biggest nightmare---calls.  Most of the nastiness is buried in
getCallArg, which moves the arguments to the correct registers/stack
locations.  Apart from that, the code is easy.

Do not fill the delay slots here; you will confuse the register allocator.

\begin{code}

genCCall
    :: FAST_STRING  -- function to call
    -> PrimRep	    -- type of the result
    -> [StixTree]   -- arguments (of mixed type)
    -> UniqSM (CodeBlock SparcInstr)

genCCall fn kind args =
    mapAccumLNCG getCallArg (argRegs,stackArgLoc) args
    	    	    	    	    `thenUs` \ ((unused,_), argCode) ->
    let
    	nRegs = length argRegs - length unused
    	call = CALL fn__2 nRegs False
    	code = asmParThen (map ($ asmVoid) argCode)
    in
    	returnSeq code [call, NOP]
  where
    -- function names that begin with '.' are assumed to be special internally
    -- generated names like '.mul,' which don't get an underscore prefix
    fn__2 = case (_HEAD_ fn) of
	      '.' -> ImmLit (uppPStr fn)
	      _   -> ImmLab (uppPStr fn)

    mapAccumLNCG f b []     = returnUs (b, [])
    mapAccumLNCG f b (x:xs) =
    	f b x   	        	    `thenUs` \ (b__2, x__2) ->
    	mapAccumLNCG f b__2 xs   	    `thenUs` \ (b__3, xs__2) ->
    	returnUs (b__3, x__2:xs__2)

\end{code}

Trivial (dyadic) instructions.  Only look for constants on the right hand
side, because that's where the generic optimizer will have put them.

\begin{code}

trivialCode
    :: (Reg -> RI -> Reg -> SparcInstr)
    -> [StixTree]
    -> UniqSM Register

trivialCode instr [x, StInt y]
  | is13Bits y =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . mkSeqInstr (instr src1 (RIImm src2) dst)
    in
    	returnUs (Any IntRep code__2)

trivialCode instr [x, y] =
    getReg x	    	    	    `thenUs` \ register1 ->
    getReg y	    	    	    `thenUs` \ register2 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp1 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = asmParThen [code1, code2] .
    	    	     mkSeqInstr (instr src1 (RIReg src2) dst)
    in
    	returnUs (Any IntRep code__2)

trivialFCode
    :: PrimRep
    -> (Size -> Reg -> Reg -> Reg -> SparcInstr)
    -> [StixTree]
    -> UniqSM Register

trivialFCode pk instr [x, y] =
    getReg x	    	    	    `thenUs` \ register1 ->
    getReg y	    	    	    `thenUs` \ register2 ->
    getNewRegNCG (registerKind register1)
      	    	        	    `thenUs` \ tmp1 ->
    getNewRegNCG (registerKind register2)
     	    	        	    `thenUs` \ tmp2 ->
    getNewRegNCG DoubleRep   	    `thenUs` \ tmp ->
    let
    	promote x = asmInstr (FxTOy F DF x tmp)

    	pk1   = registerKind register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	pk2   = registerKind register2
    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst =
    	    	if pk1 == pk2 then
    	            asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	    mkSeqInstr (instr (kindToSize pk) src1 src2 dst)
    	    	else if pk1 == FloatRep then
    	    	    asmParThen [code1 (promote src1), code2 asmVoid] .
    	    	    mkSeqInstr (instr DF tmp src2 dst)
    	    	else
    	    	    asmParThen [code1 asmVoid, code2 (promote src2)] .
    	    	    mkSeqInstr (instr DF src1 tmp dst)
    in
    	returnUs (Any (if pk1 == pk2 then pk1 else DoubleRep) code__2)

\end{code}

Trivial unary instructions.  Note that we don't have to worry about
matching an StInt as the argument, because genericOpt will already
have handled the constant-folding.

\begin{code}

trivialUCode
    :: (RI -> Reg -> SparcInstr)
    -> [StixTree]
    -> UniqSM Register

trivialUCode instr [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr (RIReg src) dst)
    in
    	returnUs (Any IntRep code__2)

trivialUFCode
    :: PrimRep
    -> (Reg -> Reg -> SparcInstr)
    -> [StixTree]
    -> UniqSM Register

trivialUFCode pk instr [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG pk        	    `thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    	returnUs (Any pk code__2)

\end{code}

Absolute value on integers, mostly for gmp size check macros.  Again,
the argument cannot be an StInt, because genericOpt already folded
constants.

Do not fill the delay slots here; you will confuse the register allocator.

\begin{code}

absIntCode :: [StixTree] -> UniqSM Register
absIntCode [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ reg ->
    getUniqLabelNCG    	    	    `thenUs` \ lbl ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	code__2 dst = code . mkSeqInstrs [
	    SUB False True g0 (RIReg src) dst,
	    BI GE False (ImmCLbl lbl), NOP,
	    OR False g0 (RIReg src) dst,
	    LABEL lbl]
    in
    	returnUs (Any IntRep code__2)

\end{code}

Simple integer coercions that don't require any code to be generated.
Here we just change the type on the register passed on up

\begin{code}

coerceIntCode :: PrimRep -> [StixTree] -> UniqSM Register
coerceIntCode pk [x] =
    getReg x	    	    	    `thenUs` \ register ->
    case register of
    	Fixed reg _ code -> returnUs (Fixed reg pk code)
    	Any _ code       -> returnUs (Any pk code)

\end{code}

Integer to character conversion.  We try to do this in one step if
the original object is in memory.

\begin{code}

chrCode :: [StixTree] -> UniqSM Register
chrCode [StInd pk mem] =
    getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code = amodeCode amode
    	src  = amodeAddr amode
    	srcOff = offset src 3
    	src__2 = case srcOff of Just x -> x
    	code__2 dst = if maybeToBool srcOff then
    	    	    	code . mkSeqInstr (LD UB src__2 dst)
    	    	    else
    	    	    	code . mkSeqInstrs [
    	    	    	    LD (kindToSize pk) src dst,
    	    	    	    AND False dst (RIImm (ImmInt 255)) dst]
    in
    	returnUs (Any pk code__2)

chrCode [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	code__2 dst = code . mkSeqInstr (AND False src (RIImm (ImmInt 255)) dst)
    in
    	returnUs (Any IntRep code__2)

\end{code}

More complicated integer/float conversions.  Here we have to store
temporaries in memory to move between the integer and the floating
point register sets.

\begin{code}

coerceInt2FP :: PrimRep -> [StixTree] -> UniqSM Register
coerceInt2FP pk [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep      	    `thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
    	    ST W src (spRel (-2)),
    	    LD W (spRel (-2)) dst,
    	    FxTOy W (kindToSize pk) dst dst]
    in
    	returnUs (Any pk code__2)

coerceFP2Int :: [StixTree] -> UniqSM Register
coerceFP2Int [x] =
    getReg x	    	    	    `thenUs` \ register ->
    getNewRegNCG IntRep      	    `thenUs` \ reg ->
    getNewRegNCG FloatRep     	    `thenUs` \ tmp ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	pk   = registerKind register

    	code__2 dst = code . mkSeqInstrs [
    	    FxTOy (kindToSize pk) W src tmp,
    	    ST W tmp (spRel (-2)),
    	    LD W (spRel (-2)) dst]
    in
    	returnUs (Any IntRep code__2)

\end{code}

Some random little helpers.

\begin{code}

maybeImm :: StixTree -> Maybe Imm
maybeImm (StInt i)
  | i >= toInteger minInt && i <= toInteger maxInt = Just (ImmInt (fromInteger i))
  | otherwise = Just (ImmInteger i)
maybeImm (StLitLbl s)  = Just (ImmLab s)
maybeImm (StLitLit s)  = Just (strImmLit (cvtLitLit (_UNPK_ s)))
maybeImm (StCLbl l) = Just (ImmCLbl l)
maybeImm _          = Nothing

mangleIndexTree :: StixTree -> StixTree

mangleIndexTree (StIndex pk base (StInt i)) =
    StPrim IntAddOp [base, off]
  where
    off = StInt (i * size pk)
    size :: PrimRep -> Integer
    size pk = case kindToSize pk of
    	{SB -> 1; UB -> 1; HW -> 2; UHW -> 2; W -> 4; D -> 8; F -> 4; DF -> 8}

mangleIndexTree (StIndex pk base off) =
    case pk of
    	CharRep -> StPrim IntAddOp [base, off]
    	_   	 -> StPrim IntAddOp [base, off__2]
  where
    off__2 = StPrim SllOp [off, StInt (shift pk)]
    shift :: PrimRep -> Integer
    shift DoubleRep 	= 3
    shift _ 	       	= 2

cvtLitLit :: String -> String
cvtLitLit "stdin" = "__iob+0x0"   -- This one is probably okay...
cvtLitLit "stdout" = "__iob+0x14" -- but these next two are dodgy at best
cvtLitLit "stderr" = "__iob+0x28"
cvtLitLit s
  | isHex s = s
  | otherwise = error ("Native code generator can't handle ``" ++ s ++ "''")
  where
    isHex ('0':'x':xs) = all isHexDigit xs
    isHex _ = False
    -- Now, where have I seen this before?
    isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'


\end{code}

spRel gives us a stack relative addressing mode for volatile temporaries
and for excess call arguments.

\begin{code}

spRel
    :: Int  	-- desired stack offset in words, positive or negative
    -> Addr
spRel n = AddrRegImm sp (ImmInt (n * 4))

stackArgLoc = 23 :: Int	    -- where to stack extra call arguments (beyond 6x32 bits)

\end{code}

\begin{code}

getNewRegNCG :: PrimRep -> UniqSM Reg
getNewRegNCG pk =
      getUnique          `thenUs` \ u ->
      returnUs (mkReg u pk)

\end{code}
