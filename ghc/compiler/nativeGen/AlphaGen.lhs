%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module AlphaGen (
	alphaCodeGen,

	-- and, for self-sufficiency
	PprStyle, StixTree, CSeq
    ) where

IMPORT_Trace

import AbsCSyn	    ( AbstractC, MagicId(..), kindFromMagicId )
import AbsPrel	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import AsmRegAlloc  ( runRegAllocate, extractMappedRegNos, mkReg,
		      Reg(..), RegLiveness(..), RegUsage(..), FutureLive(..),
		      MachineRegisters(..), MachineCode(..)
    	    	    )
import CLabelInfo   ( CLabel, isAsmTemp )
import AlphaCode    {- everything -}
import MachDesc
import Maybes	    ( maybeToBool, Maybe(..) )
import OrdList	    -- ( mkEmptyList, mkUnitList, mkSeqList, mkParList, OrdList )
import Outputable
import PrimKind	    ( PrimKind(..), isFloatingKind )
import AlphaDesc
import Stix
import SplitUniq
import Unique
import Pretty
import Unpretty
import Util

type CodeBlock a = (OrdList a -> OrdList a)

\end{code}

%************************************************************************
%*									*
\subsection[AlphaCodeGen]{Generating Alpha Code}
%*									*
%************************************************************************

This is the top-level code-generation function for the Alpha.

\begin{code}

alphaCodeGen :: PprStyle -> [[StixTree]] -> SUniqSM Unpretty
alphaCodeGen sty trees = 
    mapSUs genAlphaCode trees	    	`thenSUs` \ dynamicCodes ->
    let
    	staticCodes = scheduleAlphaCode dynamicCodes
    	pretty = printLabeledCodes sty staticCodes
    in
    	returnSUs pretty

\end{code}

This bit does the code scheduling.  The scheduler must also deal with
register allocation of temporaries.  Much parallelism can be exposed via
the OrdList, but more might occur, so further analysis might be needed.

\begin{code}

scheduleAlphaCode :: [AlphaCode] -> [AlphaInstr]
scheduleAlphaCode = concat . map (runRegAllocate freeAlphaRegs reservedRegs)
  where
    freeAlphaRegs :: AlphaRegs
    freeAlphaRegs = mkMRegs (extractMappedRegNos freeRegs)

\end{code}

Registers passed up the tree.  If the stix code forces the register
to live in a pre-decided machine register, it comes out as @Fixed@;
otherwise, it comes out as @Any@, and the parent can decide which
register to put it in.

\begin{code}

data Register 
  = Fixed Reg PrimKind (CodeBlock AlphaInstr) 
  | Any PrimKind (Reg -> (CodeBlock AlphaInstr))

registerCode :: Register -> Reg -> CodeBlock AlphaInstr
registerCode (Fixed _ _ code) reg = code
registerCode (Any _ code) reg = code reg

registerName :: Register -> Reg -> Reg
registerName (Fixed reg _ _) _ = reg
registerName (Any _ _) reg = reg

registerKind :: Register -> PrimKind
registerKind (Fixed _ pk _) = pk
registerKind (Any pk _) = pk

isFixed :: Register -> Bool
isFixed (Fixed _ _ _) = True
isFixed (Any _ _)     = False

\end{code}

Memory addressing modes passed up the tree.

\begin{code}

data Amode = Amode Addr (CodeBlock AlphaInstr)

amodeAddr (Amode addr _) = addr
amodeCode (Amode _ code) = code

\end{code}

General things for putting together code sequences.

\begin{code}

asmVoid :: OrdList AlphaInstr
asmVoid = mkEmptyList

asmInstr :: AlphaInstr -> AlphaCode
asmInstr i = mkUnitList i

asmSeq :: [AlphaInstr] -> AlphaCode
asmSeq is = foldr (mkSeqList . asmInstr) asmVoid is

asmParThen :: [AlphaCode] -> CodeBlock AlphaInstr
asmParThen others code = mkSeqList (foldr mkParList mkEmptyList others) code

returnInstr :: AlphaInstr -> SUniqSM (CodeBlock AlphaInstr)
returnInstr instr = returnSUs (\xs -> mkSeqList (asmInstr instr) xs)

returnInstrs :: [AlphaInstr] -> SUniqSM (CodeBlock AlphaInstr)
returnInstrs instrs = returnSUs (\xs -> mkSeqList (asmSeq instrs) xs)

returnSeq :: (CodeBlock AlphaInstr) -> [AlphaInstr] -> SUniqSM (CodeBlock AlphaInstr)
returnSeq code instrs = returnSUs (\xs -> code (mkSeqList (asmSeq instrs) xs))

mkSeqInstr :: AlphaInstr -> (CodeBlock AlphaInstr)
mkSeqInstr instr code = mkSeqList (asmInstr instr) code

mkSeqInstrs :: [AlphaInstr] -> (CodeBlock AlphaInstr)
mkSeqInstrs instrs code = mkSeqList (asmSeq instrs) code

\end{code}

Top level alpha code generator for a chunk of stix code.

\begin{code}

genAlphaCode :: [StixTree] -> SUniqSM (AlphaCode)

genAlphaCode trees =
    mapSUs getCode trees    	    	`thenSUs` \ blocks ->
    returnSUs (foldr (.) id blocks asmVoid)

\end{code}

Code extractor for an entire stix tree---stix statement level.

\begin{code}

getCode 
    :: StixTree     -- a stix statement
    -> SUniqSM (CodeBlock AlphaInstr)

getCode (StSegment seg) = returnInstr (SEGMENT seg)

getCode (StAssign pk dst src)
  | isFloatingKind pk = assignFltCode pk dst src
  | otherwise = assignIntCode pk dst src

getCode (StLabel lab) = returnInstr (LABEL lab)

getCode (StFunBegin lab) = returnInstr (FUNBEGIN lab)

getCode (StFunEnd lab) = returnInstr (FUNEND lab)

getCode (StJump arg) = genJump arg

-- When falling through on the alpha, we still have to load pv with the
-- address of the next routine, so that it can load gp
getCode (StFallThrough lbl) = returnInstr (LDA pv (AddrImm (ImmCLbl lbl)))

getCode (StCondJump lbl arg) = genCondJump lbl arg

getCode (StData kind args) = 
    mapAndUnzipSUs getData args		    `thenSUs` \ (codes, imms) ->
    returnSUs (\xs -> mkSeqList (asmInstr (DATA (kindToSize kind) imms))
                                (foldr1 (.) codes xs))
  where
    getData :: StixTree -> SUniqSM (CodeBlock AlphaInstr, Imm)
    getData (StInt i) = returnSUs (id, ImmInteger i)
#if __GLASGOW_HASKELL__ >= 23
--  getData (StDouble d) = returnSUs (id, strImmLab (_showRational 30 d))
    getData (StDouble d) = returnSUs (id, ImmLab (prettyToUn (ppRational d)))
#else
    getData (StDouble d) = returnSUs (id, strImmLab (show d))
#endif
    getData (StLitLbl s) = returnSUs (id, ImmLab s)
    getData (StLitLit s) = returnSUs (id, strImmLab (cvtLitLit (_UNPK_ s)))
    getData (StString s) = 
        getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
	returnSUs (mkSeqInstrs [LABEL lbl, ASCII True (_UNPK_ s)], ImmCLbl lbl)
    getData (StCLbl l)   = returnSUs (id, ImmCLbl l)

getCode (StCall fn VoidKind args) = genCCall fn VoidKind args

getCode (StComment s) = returnInstr (COMMENT s)

\end{code}

Generate code to get a subtree into a register.

\begin{code}

getReg :: StixTree -> SUniqSM Register

getReg (StReg (StixMagicId stgreg)) =
    case stgRegMap stgreg of
    	Just reg -> returnSUs (Fixed reg (kindFromMagicId stgreg) id)
    	-- cannae be Nothing

getReg (StReg (StixTemp u pk)) = returnSUs (Fixed (UnmappedReg u pk) pk id)

getReg (StDouble d) =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
#if __GLASGOW_HASKELL__ >= 23
--	    DATA TF [strImmLab (_showRational 30 d)],
	    DATA TF [ImmLab (prettyToUn (ppRational d))],
#else
	    DATA TF [strImmLab (show d)],
#endif
	    SEGMENT TextSegment,
	    LDA tmp (AddrImm (ImmCLbl lbl)),
	    LD TF dst (AddrReg tmp)]
    in
    	returnSUs (Any DoubleKind code)

getReg (StString s) =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII True (_UNPK_ s),
	    SEGMENT TextSegment,
	    LDA dst (AddrImm (ImmCLbl lbl))]
    in
    	returnSUs (Any PtrKind code)

getReg (StLitLit s) | _HEAD_ s == '"' && last xs == '"' =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII False (init xs),
	    SEGMENT TextSegment,
	    LDA dst (AddrImm (ImmCLbl lbl))]
    in
    	returnSUs (Any PtrKind code)
  where
    xs = _UNPK_ (_TAIL_ s)

getReg tree@(StIndex _ _ _) = getReg (mangleIndexTree tree)

getReg (StCall fn kind args) = 
    genCCall fn kind args   	    `thenSUs` \ call ->
    returnSUs (Fixed reg kind call)
  where
    reg = if isFloatingKind kind then f0 else v0

getReg (StPrim primop args) = 
    case primop of

    	CharGtOp -> case args of [x,y] -> trivialCode (CMP LT) [y,x]
    	CharGeOp -> case args of [x,y] -> trivialCode (CMP LE) [y,x]
    	CharEqOp -> trivialCode (CMP EQ) args
    	CharNeOp -> intNECode args
    	CharLtOp -> trivialCode (CMP LT) args
    	CharLeOp -> trivialCode (CMP LE) args

    	IntAddOp -> trivialCode (ADD Q False) args

    	IntSubOp -> trivialCode (SUB Q False) args
    	IntMulOp -> trivialCode (MUL Q False) args
    	IntQuotOp -> trivialCode (DIV Q False) args
    	IntRemOp -> trivialCode (REM Q False) args
    	IntNegOp -> trivialUCode (NEG Q False) args
    	IntAbsOp -> trivialUCode (ABS Q) args
   
    	AndOp -> trivialCode AND args
    	OrOp  -> trivialCode OR args
    	NotOp -> trivialUCode NOT args
    	SllOp -> trivialCode SLL args
    	SraOp -> trivialCode SRA args
    	SrlOp -> trivialCode SRL args
    	ISllOp -> panic "AlphaGen:isll"
    	ISraOp -> panic "AlphaGen:isra"
    	ISrlOp -> panic "AlphaGen:isrl"
   
    	IntGtOp -> case args of [x,y] -> trivialCode (CMP LT) [y,x]
    	IntGeOp -> case args of [x,y] -> trivialCode (CMP LE) [y,x]
    	IntEqOp -> trivialCode (CMP EQ) args
    	IntNeOp -> intNECode args
    	IntLtOp -> trivialCode (CMP LT) args
    	IntLeOp -> trivialCode (CMP LE) args

    	WordGtOp -> case args of [x,y] -> trivialCode (CMP ULT) [y,x]
    	WordGeOp -> case args of [x,y] -> trivialCode (CMP ULE) [y,x]
    	WordEqOp -> trivialCode (CMP EQ) args
    	WordNeOp -> intNECode args
    	WordLtOp -> trivialCode (CMP ULT) args
    	WordLeOp -> trivialCode (CMP ULE) args

    	AddrGtOp -> case args of [x,y] -> trivialCode (CMP ULT) [y,x]
    	AddrGeOp -> case args of [x,y] -> trivialCode (CMP ULE) [y,x]
    	AddrEqOp -> trivialCode (CMP EQ) args
    	AddrNeOp -> intNECode args
    	AddrLtOp -> trivialCode (CMP ULT) args
    	AddrLeOp -> trivialCode (CMP ULE) args

    	FloatAddOp -> trivialFCode (FADD TF) args
    	FloatSubOp -> trivialFCode (FSUB TF) args
    	FloatMulOp -> trivialFCode (FMUL TF) args
    	FloatDivOp -> trivialFCode (FDIV TF) args
    	FloatNegOp -> trivialUFCode (FNEG TF) args

    	FloatGtOp -> cmpFCode (FCMP TF LE) EQ args
    	FloatGeOp -> cmpFCode (FCMP TF LT) EQ args
    	FloatEqOp -> cmpFCode (FCMP TF EQ) NE args
    	FloatNeOp -> cmpFCode (FCMP TF EQ) EQ args
    	FloatLtOp -> cmpFCode (FCMP TF LT) NE args
    	FloatLeOp -> cmpFCode (FCMP TF LE) NE args

    	FloatExpOp -> call SLIT("exp") DoubleKind
    	FloatLogOp -> call SLIT("log") DoubleKind
    	FloatSqrtOp -> call SLIT("sqrt") DoubleKind
       
    	FloatSinOp -> call SLIT("sin") DoubleKind
    	FloatCosOp -> call SLIT("cos") DoubleKind
    	FloatTanOp -> call SLIT("tan") DoubleKind
       
    	FloatAsinOp -> call SLIT("asin") DoubleKind
    	FloatAcosOp -> call SLIT("acos") DoubleKind
    	FloatAtanOp -> call SLIT("atan") DoubleKind
       
    	FloatSinhOp -> call SLIT("sinh") DoubleKind
    	FloatCoshOp -> call SLIT("cosh") DoubleKind
    	FloatTanhOp -> call SLIT("tanh") DoubleKind
       
    	FloatPowerOp -> call SLIT("pow") DoubleKind

    	DoubleAddOp -> trivialFCode (FADD TF) args
    	DoubleSubOp -> trivialFCode (FSUB TF) args
    	DoubleMulOp -> trivialFCode (FMUL TF) args
   	DoubleDivOp -> trivialFCode (FDIV TF) args
    	DoubleNegOp -> trivialUFCode (FNEG TF) args
   
    	DoubleGtOp -> cmpFCode (FCMP TF LE) EQ args
    	DoubleGeOp -> cmpFCode (FCMP TF LT) EQ args
    	DoubleEqOp -> cmpFCode (FCMP TF EQ) NE args
    	DoubleNeOp -> cmpFCode (FCMP TF EQ) EQ args
    	DoubleLtOp -> cmpFCode (FCMP TF LT) NE args
    	DoubleLeOp -> cmpFCode (FCMP TF LE) NE args

    	DoubleExpOp -> call SLIT("exp") DoubleKind
    	DoubleLogOp -> call SLIT("log") DoubleKind
    	DoubleSqrtOp -> call SLIT("sqrt") DoubleKind

    	DoubleSinOp -> call SLIT("sin") DoubleKind
    	DoubleCosOp -> call SLIT("cos") DoubleKind
    	DoubleTanOp -> call SLIT("tan") DoubleKind
       
    	DoubleAsinOp -> call SLIT("asin") DoubleKind
    	DoubleAcosOp -> call SLIT("acos") DoubleKind
    	DoubleAtanOp -> call SLIT("atan") DoubleKind
       
    	DoubleSinhOp -> call SLIT("sinh") DoubleKind
    	DoubleCoshOp -> call SLIT("cosh") DoubleKind
    	DoubleTanhOp -> call SLIT("tanh") DoubleKind
       
    	DoublePowerOp -> call SLIT("pow") DoubleKind

    	OrdOp -> coerceIntCode IntKind args
    	ChrOp -> chrCode args
       
    	Float2IntOp -> coerceFP2Int args
    	Int2FloatOp -> coerceInt2FP args
    	Double2IntOp -> coerceFP2Int args
    	Int2DoubleOp -> coerceInt2FP args
       
    	Double2FloatOp -> coerceFltCode args
    	Float2DoubleOp -> coerceFltCode args

  where
    call fn pk = getReg (StCall fn pk args)

getReg (StInd pk mem) =
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = kindToSize pk
    	code__2 dst = code . mkSeqInstr (LD size dst src)
    in
    	returnSUs (Any pk code__2)

getReg (StInt i)
  | is8Bits i =
    let
    	code dst = mkSeqInstr (OR zero (RIImm src) dst)
    in
    	returnSUs (Any IntKind code)
  | otherwise =
    let
    	code dst = mkSeqInstr (LDI Q dst src)
    in
    	returnSUs (Any IntKind code)
  where
    src = ImmInt (fromInteger i)

getReg leaf
  | maybeToBool imm =
    let
    	code dst = mkSeqInstr (LDA dst (AddrImm imm__2))
    in
    	returnSUs (Any PtrKind code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

\end{code}

Now, given a tree (the argument to an StInd) that references memory,
produce a suitable addressing mode.

\begin{code}

getAmode :: StixTree -> SUniqSM Amode

getAmode tree@(StIndex _ _ _) = getAmode (mangleIndexTree tree)

getAmode (StPrim IntSubOp [x, StInt i]) =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg x    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    	returnSUs (Amode (AddrRegImm reg off) code)


getAmode (StPrim IntAddOp [x, StInt i]) =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg x    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    	returnSUs (Amode (AddrRegImm reg off) code)

getAmode leaf
  | maybeToBool imm =
    	returnSUs (Amode (AddrImm imm__2) id)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg other    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    in
    	returnSUs (Amode (AddrReg reg) code)

\end{code}

Try to get a value into a specific register (or registers) for a call.
The first 6 arguments go into the appropriate argument register
(separate registers for integer and floating point arguments, but used
in lock-step), and the remaining arguments are dumped to the stack,
beginning at 0(sp).  Our first argument is a pair of the list of
remaining argument registers to be assigned for this call and the next
stack offset to use for overflowing arguments.  This way, @getCallArg@
can be applied to all of a call's arguments using @mapAccumL@.

\begin{code}

getCallArg 
    :: ([(Reg,Reg)],Int)    -- Argument registers and stack offset (accumulator)
    -> StixTree 	    -- Current argument
    -> SUniqSM (([(Reg,Reg)],Int), CodeBlock AlphaInstr) -- Updated accumulator and code

-- We have to use up all of our argument registers first.

getCallArg ((iDst,fDst):dsts, offset) arg = 
    getReg arg	    	    	    `thenSUs` \ register ->
    let
    	reg = if isFloatingKind pk then fDst else iDst
    	code = registerCode register reg
    	src = registerName register reg
    	pk = registerKind register
    in
    	returnSUs (
            if isFloatingKind pk then
    	        ((dsts, offset), if isFixed register then 
    	    	    code . mkSeqInstr (FMOV src fDst)
    	    	    else code)
    	    else 
                ((dsts, offset), if isFixed register then 
    	    	    code . mkSeqInstr (OR src (RIReg src) iDst)
    	    	    else code))

-- Once we have run out of argument registers, we move to the stack

getCallArg ([], offset) arg = 
    getReg arg	    	    	    `thenSUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenSUs` \ tmp ->
    let 
    	code = registerCode register tmp
    	src = registerName register tmp
    	pk = registerKind register
    	sz = kindToSize pk
    in
    	returnSUs (([], offset + 1), code . mkSeqInstr (ST sz src (spRel offset)))

\end{code}

Assignments are really at the heart of the whole code generation business.
Almost all top-level nodes of any real importance are assignments, which
correspond to loads, stores, or register transfers.  If we're really lucky,
some of the register transfers will go away, because we can use the destination
register to complete the code generation for the right hand side.  This only
fails when the right hand side is forced into a fixed register (e.g. the result
of a call).  

\begin{code}

assignIntCode :: PrimKind -> StixTree -> StixTree -> SUniqSM (CodeBlock AlphaInstr)

assignIntCode pk (StInd _ dst) src =
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    getAmode dst    	    	    `thenSUs` \ amode ->
    getReg src	    	    	    `thenSUs` \ register ->
    let 
    	code1 = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2 = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz    = kindToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    	returnSUs code__2

assignIntCode pk dst src =
    getReg dst	    	    	    `thenSUs` \ register1 ->
    getReg src	    	    	    `thenSUs` \ register2 ->
    let 
    	dst__2 = registerName register1 zero
    	code = registerCode register2 dst__2
    	src__2 = registerName register2 dst__2
    	code__2 = if isFixed register2 then 
    	    	    code . mkSeqInstr (OR src__2 (RIReg src__2) dst__2)
    	    	else code
    in
    	returnSUs code__2

assignFltCode :: PrimKind -> StixTree -> StixTree -> SUniqSM (CodeBlock AlphaInstr)

assignFltCode pk (StInd _ dst) src =
    getNewRegNCG pk        	    `thenSUs` \ tmp ->
    getAmode dst    	    	    `thenSUs` \ amode ->
    getReg src	    	    	    `thenSUs` \ register ->
    let 
    	code1 = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2 = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz    = kindToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
        returnSUs code__2

assignFltCode pk dst src =
    getReg dst	    	    	    `thenSUs` \ register1 ->
    getReg src	    	    	    `thenSUs` \ register2 ->
    let 
    	dst__2 = registerName register1 zero
    	code = registerCode register2 dst__2
    	src__2 = registerName register2 dst__2
    	code__2 = if isFixed register2 then 
    	    	    code . mkSeqInstr (FMOV src__2 dst__2)
    	    	else code
    in
    	returnSUs code__2

\end{code} 

Generating an unconditional branch.  We accept two types of targets:
an immediate CLabel or a tree that gets evaluated into a register.
Any CLabels which are AsmTemporaries are assumed to be in the local
block of code, close enough for a branch instruction.  Other CLabels
are assumed to be far away, so we use jmp.

\begin{code}

genJump 
    :: StixTree     -- the branch target
    -> SUniqSM (CodeBlock AlphaInstr)

genJump (StCLbl lbl) 
  | isAsmTemp lbl = returnInstr (BR target)
  | otherwise     = returnInstrs [LDA pv (AddrImm target), JMP zero (AddrReg pv) 0]
  where
    target = ImmCLbl lbl

genJump tree =
    getReg tree	    	    	    `thenSUs` \ register ->
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    let
    	dst = registerName register pv
    	code = registerCode register pv
    	target = registerName register pv
    in
    	if isFixed register then
    	    returnSeq code [OR dst (RIReg dst) pv, JMP zero (AddrReg pv) 0]
    	else
    	    returnSUs (code . mkSeqInstr (JMP zero (AddrReg pv) 0))

\end{code}

Conditional jumps are always to local labels, so we can use
branch instructions.  We peek at the arguments to decide what kind 
of comparison to do.  For comparisons with 0, we're laughing, because 
we can just do the desired conditional branch.  

\begin{code}

genCondJump 
    :: CLabel	    -- the branch target
    -> StixTree     -- the condition on which to branch
    -> SUniqSM (CodeBlock AlphaInstr)

genCondJump lbl (StPrim op [x, StInt 0]) =
    getReg x	  	    	    `thenSUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	value = registerName register tmp
    	pk = registerKind register
        target = ImmCLbl lbl    
    in
    	    returnSeq code [BI (cmpOp op) value target]
  where
    cmpOp CharGtOp = GT
    cmpOp CharGeOp = GE
    cmpOp CharEqOp = EQ
    cmpOp CharNeOp = NE
    cmpOp CharLtOp = LT
    cmpOp CharLeOp = LE
    cmpOp IntGtOp = GT
    cmpOp IntGeOp = GE
    cmpOp IntEqOp = EQ
    cmpOp IntNeOp = NE
    cmpOp IntLtOp = LT
    cmpOp IntLeOp = LE
    cmpOp WordGtOp = NE
    cmpOp WordGeOp = ALWAYS
    cmpOp WordEqOp = EQ
    cmpOp WordNeOp = NE
    cmpOp WordLtOp = NEVER
    cmpOp WordLeOp = EQ
    cmpOp AddrGtOp = NE
    cmpOp AddrGeOp = ALWAYS
    cmpOp AddrEqOp = EQ
    cmpOp AddrNeOp = NE
    cmpOp AddrLtOp = NEVER
    cmpOp AddrLeOp = EQ

genCondJump lbl (StPrim op [x, StDouble 0.0]) =
    getReg x	  	    	    `thenSUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	value = registerName register tmp
    	pk = registerKind register
        target = ImmCLbl lbl    
    in
    	    returnSUs (code . mkSeqInstr (BF (cmpOp op) value target))
  where
    cmpOp FloatGtOp = GT
    cmpOp FloatGeOp = GE
    cmpOp FloatEqOp = EQ
    cmpOp FloatNeOp = NE
    cmpOp FloatLtOp = LT
    cmpOp FloatLeOp = LE
    cmpOp DoubleGtOp = GT
    cmpOp DoubleGeOp = GE
    cmpOp DoubleEqOp = EQ
    cmpOp DoubleNeOp = NE
    cmpOp DoubleLtOp = LT
    cmpOp DoubleLeOp = LE

genCondJump lbl (StPrim op args) 
  | fltCmpOp op =
    trivialFCode instr args    	    `thenSUs` \ register ->
    getNewRegNCG DoubleKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	result = registerName register tmp
        target = ImmCLbl lbl    
    in
        returnSUs (code . mkSeqInstr (BF cond result target))
  where
    fltCmpOp op = case op of
        FloatGtOp -> True
        FloatGeOp -> True
        FloatEqOp -> True
        FloatNeOp -> True
        FloatLtOp -> True
        FloatLeOp -> True
        DoubleGtOp -> True
        DoubleGeOp -> True
        DoubleEqOp -> True
        DoubleNeOp -> True
        DoubleLtOp -> True
        DoubleLeOp -> True
        _ -> False
    (instr, cond) = case op of
        FloatGtOp -> (FCMP TF LE, EQ)
        FloatGeOp -> (FCMP TF LT, EQ)
        FloatEqOp -> (FCMP TF EQ, NE)
        FloatNeOp -> (FCMP TF EQ, EQ)
        FloatLtOp -> (FCMP TF LT, NE)
        FloatLeOp -> (FCMP TF LE, NE)
        DoubleGtOp -> (FCMP TF LE, EQ)
        DoubleGeOp -> (FCMP TF LT, EQ)
        DoubleEqOp -> (FCMP TF EQ, NE)
        DoubleNeOp -> (FCMP TF EQ, EQ)
        DoubleLtOp -> (FCMP TF LT, NE)
        DoubleLeOp -> (FCMP TF LE, NE)

genCondJump lbl (StPrim op args) =
    trivialCode instr args    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	result = registerName register tmp
        target = ImmCLbl lbl    
    in
        returnSUs (code . mkSeqInstr (BI cond result target))
  where
    (instr, cond) = case op of
        CharGtOp -> (CMP LE, EQ)
        CharGeOp -> (CMP LT, EQ)
        CharEqOp -> (CMP EQ, NE)
        CharNeOp -> (CMP EQ, EQ)
        CharLtOp -> (CMP LT, NE)
        CharLeOp -> (CMP LE, NE)
        IntGtOp -> (CMP LE, EQ)
        IntGeOp -> (CMP LT, EQ)
        IntEqOp -> (CMP EQ, NE)
        IntNeOp -> (CMP EQ, EQ)
        IntLtOp -> (CMP LT, NE)
        IntLeOp -> (CMP LE, NE)
        WordGtOp -> (CMP ULE, EQ)
        WordGeOp -> (CMP ULT, EQ)
        WordEqOp -> (CMP EQ, NE)
        WordNeOp -> (CMP EQ, EQ)
        WordLtOp -> (CMP ULT, NE)
        WordLeOp -> (CMP ULE, NE)
        AddrGtOp -> (CMP ULE, EQ)
        AddrGeOp -> (CMP ULT, EQ)
        AddrEqOp -> (CMP EQ, NE)
        AddrNeOp -> (CMP EQ, EQ)
        AddrLtOp -> (CMP ULT, NE)
        AddrLeOp -> (CMP ULE, NE)

\end{code}

Now the biggest nightmare---calls.  Most of the nastiness is buried in
getCallArg, which moves the arguments to the correct registers/stack
locations.  Apart from that, the code is easy.

\begin{code}

genCCall
    :: FAST_STRING    -- function to call
    -> PrimKind	    -- type of the result
    -> [StixTree]   -- arguments (of mixed type)
    -> SUniqSM (CodeBlock AlphaInstr)

genCCall fn kind args =
    mapAccumLNCG getCallArg (argRegs,stackArgLoc) args 
    	    	    	    	    `thenSUs` \ ((unused,_), argCode) ->
    let
    	nRegs = length argRegs - length unused
    	code = asmParThen (map ($ asmVoid) argCode)
    in
    	returnSeq code [
    	    LDA pv (AddrImm (ImmLab (uppPStr fn))),
    	    JSR ra (AddrReg pv) nRegs, 
    	    LDGP gp (AddrReg ra)]
  where
    mapAccumLNCG f b []     = returnSUs (b, [])
    mapAccumLNCG f b (x:xs) = 
    	f b x   	        	    `thenSUs` \ (b__2, x__2) ->
    	mapAccumLNCG f b__2 xs   	    `thenSUs` \ (b__3, xs__2) ->
    	returnSUs (b__3, x__2:xs__2)

\end{code}

Trivial (dyadic) instructions.  Only look for constants on the right hand
side, because that's where the generic optimizer will have put them.

\begin{code}

trivialCode 
    :: (Reg -> RI -> Reg -> AlphaInstr) 
    -> [StixTree]
    -> SUniqSM Register

trivialCode instr [x, StInt y]
  | is8Bits y =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . mkSeqInstr (instr src1 (RIImm src2) dst)
    in
    	returnSUs (Any IntKind code__2)

trivialCode instr [x, y] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = asmParThen [code1, code2] .
    	    	     mkSeqInstr (instr src1 (RIReg src2) dst)
    in
    	returnSUs (Any IntKind code__2)

trivialFCode 
    :: (Reg -> Reg -> Reg -> AlphaInstr) 
    -> [StixTree] 
    -> SUniqSM Register

trivialFCode instr [x, y] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG DoubleKind   	    `thenSUs` \ tmp1 ->
    getNewRegNCG DoubleKind   	    `thenSUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst = asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	      mkSeqInstr (instr src1 src2 dst)
    in
    	returnSUs (Any DoubleKind code__2)

\end{code}

Some bizarre special code for getting condition codes into registers.
Integer non-equality is a test for equality followed by an XOR with 1.
(Integer comparisons always set the result register to 0 or 1.)  Floating
point comparisons of any kind leave the result in a floating point register, 
so we need to wrangle an integer register out of things.

\begin{code}
intNECode
    :: [StixTree]
    -> SUniqSM Register

intNECode args =
    trivialCode (CMP EQ) args  	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (XOR src (RIImm (ImmInt 1)) dst)
    in
    	returnSUs (Any IntKind code__2)

cmpFCode 
    :: (Reg -> Reg -> Reg -> AlphaInstr) 
    -> Cond
    -> [StixTree] 
    -> SUniqSM Register

cmpFCode instr cond args =
    trivialFCode instr args    	    `thenSUs` \ register ->
    getNewRegNCG DoubleKind    	    `thenSUs` \ tmp ->
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    let
    	code = registerCode register tmp
    	result  = registerName register tmp

    	code__2 dst = code . mkSeqInstrs [
    	    OR zero (RIImm (ImmInt 1)) dst,
    	    BF cond result (ImmCLbl lbl),
    	    OR zero (RIReg zero) dst,
            LABEL lbl]
    in
    	returnSUs (Any IntKind code__2)

\end{code}

Trivial unary instructions.  Note that we don't have to worry about
matching an StInt as the argument, because genericOpt will already
have handled the constant-folding.

\begin{code}

trivialUCode 
    :: (RI -> Reg -> AlphaInstr) 
    -> [StixTree]
    -> SUniqSM Register

trivialUCode instr [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr (RIReg src) dst)
    in
    	returnSUs (Any IntKind code__2)

trivialUFCode 
    :: (Reg -> Reg -> AlphaInstr) 
    -> [StixTree]
    -> SUniqSM Register

trivialUFCode instr [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG DoubleKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    	returnSUs (Any DoubleKind code__2)

\end{code}

Simple coercions that don't require any code to be generated.
Here we just change the type on the register passed on up

\begin{code}

coerceIntCode :: PrimKind -> [StixTree] -> SUniqSM Register
coerceIntCode pk [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    case register of
    	Fixed reg _ code -> returnSUs (Fixed reg pk code)
    	Any _ code       -> returnSUs (Any pk code)

coerceFltCode :: [StixTree] -> SUniqSM Register
coerceFltCode [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    case register of
    	Fixed reg _ code -> returnSUs (Fixed reg DoubleKind code)
    	Any _ code       -> returnSUs (Any DoubleKind code)

\end{code}

Integer to character conversion.  

\begin{code}

chrCode [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	code__2 dst = code . mkSeqInstr (ZAPNOT src (RIImm (ImmInt 1)) dst)
    in
    	returnSUs (Any IntKind code__2)

\end{code}

More complicated integer/float conversions.  Here we have to store
temporaries in memory to move between the integer and the floating
point register sets.

\begin{code}

coerceInt2FP :: [StixTree] -> SUniqSM Register
coerceInt2FP [x] = 
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind      	    `thenSUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
    	    ST Q src (spRel 0),
    	    LD TF dst (spRel 0),
    	    CVTxy Q TF dst dst]
    in
    	returnSUs (Any DoubleKind code__2)

coerceFP2Int :: [StixTree] -> SUniqSM Register
coerceFP2Int [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG DoubleKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp

    	code__2 dst = code . mkSeqInstrs [
    	    CVTxy TF Q src tmp,
    	    ST TF tmp (spRel 0),
    	    LD Q dst (spRel 0)]
    in
    	returnSUs (Any IntKind code__2)

\end{code}

Some random little helpers.

\begin{code}

is8Bits :: Integer -> Bool
is8Bits i = i >= -256 && i < 256

maybeImm :: StixTree -> Maybe Imm
maybeImm (StInt i) 
  | i >= toInteger minInt && i <= toInteger maxInt = Just (ImmInt (fromInteger i))
  | otherwise = Just (ImmInteger i)
maybeImm (StLitLbl s)  = Just (ImmLab s)
maybeImm (StLitLit s)  = Just (strImmLab (cvtLitLit (_UNPK_ s)))
maybeImm (StCLbl l) = Just (ImmCLbl l)
maybeImm _          = Nothing

mangleIndexTree :: StixTree -> StixTree

mangleIndexTree (StIndex pk base (StInt i)) = 
    StPrim IntAddOp [base, off]
  where
    off = StInt (i * size pk)
    size :: PrimKind -> Integer
    size pk = case kindToSize pk of
    	{B -> 1; BU -> 1; W -> 2; WU -> 2; L -> 4; FF -> 4; SF -> 4; _ -> 8}

mangleIndexTree (StIndex pk base off) = 
    case pk of
    	CharKind -> StPrim IntAddOp [base, off]
    	_   	 -> StPrim IntAddOp [base, off__2]
  where
    off__2 = StPrim SllOp [off, StInt 3]

cvtLitLit :: String -> String
cvtLitLit "stdin" = "_iob+0"   -- This one is probably okay...
cvtLitLit "stdout" = "_iob+56" -- but these next two are dodgy at best
cvtLitLit "stderr" = "_iob+112"
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
spRel n = AddrRegImm sp (ImmInt (n * 8))

stackArgLoc = 0 :: Int	    -- where to stack extra call arguments (beyond 6)

\end{code}

\begin{code}

getNewRegNCG :: PrimKind -> SUniqSM Reg
getNewRegNCG pk = 
      getSUnique          `thenSUs` \ u ->
      returnSUs (mkReg u pk)

\end{code}
