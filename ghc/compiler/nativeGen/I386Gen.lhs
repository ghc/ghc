%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"
#include "../includes/i386-unknown-linuxaout.h"

module I386Gen (
	i386CodeGen,

	-- and, for self-sufficiency
	PprStyle, StixTree, CSeq
    ) where

IMPORT_Trace

import AbsCSyn	    ( AbstractC, MagicId(..), kindFromMagicId )
import AbsPrel	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import AsmRegAlloc  ( runRegAllocate, mkReg, extractMappedRegNos,
		      Reg(..), RegLiveness(..), RegUsage(..), 
    	    	      FutureLive(..), MachineRegisters(..), MachineCode(..)
    	    	    )
import CLabelInfo   ( CLabel, isAsmTemp )
import I386Code    {- everything -}
import MachDesc
import Maybes	    ( maybeToBool, Maybe(..) )
import OrdList	    -- ( mkEmptyList, mkUnitList, mkSeqList, mkParList, OrdList )
import Outputable
import PrimKind	    ( PrimKind(..), isFloatingKind )
import I386Desc
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
\subsection[I386CodeGen]{Generating I386 Code}
%*									*
%************************************************************************

This is the top-level code-generation function for the I386.

\begin{code}

i386CodeGen :: PprStyle -> [[StixTree]] -> SUniqSM Unpretty
i386CodeGen sty trees = 
    mapSUs genI386Code trees	    	`thenSUs` \ dynamicCodes ->
    let
    	staticCodes = scheduleI386Code dynamicCodes
    	pretty = printLabeledCodes sty staticCodes
    in
    	returnSUs pretty

\end{code}

This bit does the code scheduling.  The scheduler must also deal with
register allocation of temporaries.  Much parallelism can be exposed via
the OrdList, but more might occur, so further analysis might be needed.

\begin{code}

scheduleI386Code :: [I386Code] -> [I386Instr]
scheduleI386Code = concat . map (runRegAllocate freeI386Regs reservedRegs)
  where
    freeI386Regs :: I386Regs
    freeI386Regs = mkMRegs (extractMappedRegNos freeRegs)


\end{code}

Registers passed up the tree.  If the stix code forces the register
to live in a pre-decided machine register, it comes out as @Fixed@;
otherwise, it comes out as @Any@, and the parent can decide which
register to put it in.

\begin{code}

data Register 
  = Fixed Reg PrimKind (CodeBlock I386Instr) 
  | Any PrimKind (Reg -> (CodeBlock I386Instr))

registerCode :: Register -> Reg -> CodeBlock I386Instr
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

data Amode = Amode Addr (CodeBlock I386Instr)

amodeAddr (Amode addr _) = addr
amodeCode (Amode _ code) = code

\end{code}

Condition codes passed up the tree.

\begin{code}

data Condition = Condition Bool Cond (CodeBlock I386Instr)

condName (Condition _ cond _) = cond
condFloat (Condition float _ _) = float
condCode (Condition _ _ code) = code

\end{code}

General things for putting together code sequences.

\begin{code}

asmVoid :: OrdList I386Instr
asmVoid = mkEmptyList

asmInstr :: I386Instr -> I386Code
asmInstr i = mkUnitList i

asmSeq :: [I386Instr] -> I386Code
asmSeq is = foldr (mkSeqList . asmInstr) asmVoid is

asmParThen :: [I386Code] -> (CodeBlock I386Instr)
asmParThen others code = mkSeqList (foldr mkParList mkEmptyList others) code

returnInstr :: I386Instr -> SUniqSM (CodeBlock I386Instr)
returnInstr instr = returnSUs (\xs -> mkSeqList (asmInstr instr) xs)

returnInstrs :: [I386Instr] -> SUniqSM (CodeBlock I386Instr)
returnInstrs instrs = returnSUs (\xs -> mkSeqList (asmSeq instrs) xs)

returnSeq :: (CodeBlock I386Instr) -> [I386Instr] -> SUniqSM (CodeBlock I386Instr)
returnSeq code instrs = returnSUs (\xs -> code (mkSeqList (asmSeq instrs) xs))

mkSeqInstr :: I386Instr -> (CodeBlock I386Instr)
mkSeqInstr instr code = mkSeqList (asmInstr instr) code

mkSeqInstrs :: [I386Instr] -> (CodeBlock I386Instr)
mkSeqInstrs instrs code = mkSeqList (asmSeq instrs) code

\end{code}

Top level i386 code generator for a chunk of stix code.

\begin{code}

genI386Code :: [StixTree] -> SUniqSM (I386Code)

genI386Code trees =
    mapSUs getCode trees    	    	`thenSUs` \ blocks ->
    returnSUs (foldr (.) id blocks asmVoid)

\end{code}

Code extractor for an entire stix tree---stix statement level.

\begin{code}

getCode 
    :: StixTree     -- a stix statement
    -> SUniqSM (CodeBlock I386Instr)

getCode (StSegment seg) = returnInstr (SEGMENT seg)

getCode (StAssign pk dst src)
  | isFloatingKind pk = assignFltCode pk dst src
  | otherwise = assignIntCode pk dst src

getCode (StLabel lab) = returnInstr (LABEL lab)

getCode (StFunBegin lab) = returnInstr (LABEL lab)

getCode (StFunEnd lab) = returnSUs id

getCode (StJump arg) = genJump arg

getCode (StFallThrough lbl) = returnSUs id

getCode (StCondJump lbl arg) = genCondJump lbl arg

getCode (StData kind args) = 
    mapAndUnzipSUs getData args		    `thenSUs` \ (codes, imms) ->
    returnSUs (\xs -> mkSeqList (asmInstr (DATA (kindToSize kind) imms))
                                (foldr1 (.) codes xs))
  where
    getData :: StixTree -> SUniqSM (CodeBlock I386Instr, Imm)
    getData (StInt i) = returnSUs (id, ImmInteger i)
#if __GLASGOW_HASKELL__ >= 23
--  getData (StDouble d) = returnSUs (id, strImmLit ('0' : 'd' : _showRational 30 d))
    -- yurgh (WDP 94/12)
    getData (StDouble d) = returnSUs (id, strImmLit ('0' : 'd' : ppShow 80 (ppRational d)))
#else
    getData (StDouble d) = returnSUs (id, strImmLit ('0' : 'd' : show d))
#endif
    getData (StLitLbl s) = returnSUs (id, ImmLit (uppBeside (uppChar '_') s))
    getData (StLitLit s) = returnSUs (id, strImmLit (cvtLitLit (_UNPK_ s)))
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
    	-- cannot be Nothing

getReg (StReg (StixTemp u pk)) = returnSUs (Fixed (UnmappedReg u pk) pk id)

getReg (StDouble 0.0)
  = let
    	code dst = mkSeqInstrs [FLDZ]
    in
    	returnSUs (Any DoubleKind code)

getReg (StDouble 1.0)
  = let
    	code dst = mkSeqInstrs [FLD1]
    in
    	returnSUs (Any DoubleKind code)

getReg (StDouble d) =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    --getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
#if __GLASGOW_HASKELL__ >= 23
--	    DATA D [strImmLit ('0' : 'd' :_showRational 30 d)],
	    DATA D [strImmLit ('0' : 'd' :ppShow 80 (ppRational d))],
#else
	    DATA D [strImmLit ('0' : 'd' :show d)],
#endif
	    SEGMENT TextSegment,
	    FLD D (OpImm (ImmCLbl lbl)) 
            ]
    in
    	returnSUs (Any DoubleKind code)

getReg (StString s) =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII True (_UNPK_ s),
	    SEGMENT TextSegment,
	    MOV L (OpImm (ImmCLbl lbl)) (OpReg dst)]
    in
    	returnSUs (Any PtrKind code)

getReg (StLitLit s) | _HEAD_ s == '"' && last xs == '"' =
    getUniqLabelNCG 	    	    `thenSUs` \ lbl ->
    let code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII False (init xs),
	    SEGMENT TextSegment,
	    MOV L (OpImm (ImmCLbl lbl)) (OpReg dst)]
    in
    	returnSUs (Any PtrKind code)
  where
    xs = _UNPK_ (_TAIL_ s)


getReg tree@(StIndex _ _ _) = getReg (mangleIndexTree tree)

getReg (StCall fn kind args) = 
    genCCall fn kind args   	    `thenSUs` \ call ->
    returnSUs (Fixed reg kind call)
  where
    reg = if isFloatingKind kind then st0 else eax

getReg (StPrim primop args) = 
    case primop of

    	CharGtOp -> condIntReg GT args
    	CharGeOp -> condIntReg GE args
    	CharEqOp -> condIntReg EQ args
    	CharNeOp -> condIntReg NE args
    	CharLtOp -> condIntReg LT args
    	CharLeOp -> condIntReg LE args

    	IntAddOp -> -- this should be optimised by the generic Opts, 
                    -- I don't know why it is not (sometimes)!
                    case args of 
                      [x, StInt 0] -> getReg x
                      _ -> addCode L args

    	IntSubOp -> subCode L args
    	IntMulOp -> trivialCode (IMUL L) args True
    	IntQuotOp -> divCode L args True -- division
    	IntRemOp -> divCode L args False -- remainder
    	IntNegOp -> trivialUCode (NEGI L) args
    	IntAbsOp -> absIntCode args
   
    	AndOp -> trivialCode (AND L) args True
    	OrOp  -> trivialCode (OR L) args True
    	NotOp -> trivialUCode (NOT L) args
    	SllOp -> trivialCode (SHL L) args False
    	SraOp -> trivialCode (SAR L) args False
    	SrlOp -> trivialCode (SHR L) args False
    	ISllOp -> panic "I386Gen:isll"
    	ISraOp -> panic "I386Gen:isra"
    	ISrlOp -> panic "I386Gen:isrl"
   
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

    	FloatAddOp -> trivialFCode FloatKind FADD FADD FADDP FADDP args
    	FloatSubOp -> trivialFCode FloatKind FSUB FSUBR FSUBP FSUBRP args
    	FloatMulOp -> trivialFCode FloatKind FMUL FMUL FMULP FMULP args
    	FloatDivOp -> trivialFCode FloatKind FDIV FDIVR FDIVP FDIVRP args
    	FloatNegOp -> trivialUFCode FloatKind FCHS args

    	FloatGtOp -> condFltReg GT args
    	FloatGeOp -> condFltReg GE args
    	FloatEqOp -> condFltReg EQ args
    	FloatNeOp -> condFltReg NE args
    	FloatLtOp -> condFltReg LT args
    	FloatLeOp -> condFltReg LE args

    	FloatExpOp -> promoteAndCall SLIT("exp") DoubleKind
    	FloatLogOp -> promoteAndCall SLIT("log") DoubleKind
    	FloatSqrtOp -> trivialUFCode FloatKind FSQRT args
       
    	FloatSinOp -> promoteAndCall SLIT("sin") DoubleKind 
                      --trivialUFCode FloatKind FSIN args
    	FloatCosOp -> promoteAndCall SLIT("cos") DoubleKind 
                      --trivialUFCode FloatKind FCOS args
    	FloatTanOp -> promoteAndCall SLIT("tan") DoubleKind
       
    	FloatAsinOp -> promoteAndCall SLIT("asin") DoubleKind
    	FloatAcosOp -> promoteAndCall SLIT("acos") DoubleKind
    	FloatAtanOp -> promoteAndCall SLIT("atan") DoubleKind
       
    	FloatSinhOp -> promoteAndCall SLIT("sinh") DoubleKind
    	FloatCoshOp -> promoteAndCall SLIT("cosh") DoubleKind
    	FloatTanhOp -> promoteAndCall SLIT("tanh") DoubleKind
       
    	FloatPowerOp -> promoteAndCall SLIT("pow") DoubleKind

    	DoubleAddOp -> trivialFCode DoubleKind FADD FADD FADDP FADDP args
    	DoubleSubOp -> trivialFCode DoubleKind FSUB FSUBR FSUBP FSUBRP args
    	DoubleMulOp -> trivialFCode DoubleKind FMUL FMUL FMULP FMULP args
   	DoubleDivOp -> trivialFCode DoubleKind FDIV FDIVR FDIVP FDIVRP args
    	DoubleNegOp -> trivialUFCode DoubleKind FCHS args
   
    	DoubleGtOp -> condFltReg GT args
    	DoubleGeOp -> condFltReg GE args
    	DoubleEqOp -> condFltReg EQ args
    	DoubleNeOp -> condFltReg NE args
    	DoubleLtOp -> condFltReg LT args
    	DoubleLeOp -> condFltReg LE args

    	DoubleExpOp -> call SLIT("exp") DoubleKind
    	DoubleLogOp -> call SLIT("log") DoubleKind
    	DoubleSqrtOp -> trivialUFCode DoubleKind FSQRT args

    	DoubleSinOp -> call SLIT("sin") DoubleKind
                       --trivialUFCode DoubleKind FSIN args
    	DoubleCosOp -> call SLIT("cos") DoubleKind
                       --trivialUFCode DoubleKind FCOS args
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
    	Int2FloatOp -> coerceInt2FP FloatKind args
    	Double2IntOp -> coerceFP2Int args
    	Int2DoubleOp -> coerceInt2FP DoubleKind args

    	Double2FloatOp -> coerceFltCode args
    	Float2DoubleOp -> coerceFltCode args

  where
    call fn pk = getReg (StCall fn pk args)
    promoteAndCall fn pk = getReg (StCall fn pk (map promote args))
      where
        promote x = StPrim Float2DoubleOp [x]

getReg (StInd pk mem) =
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = kindToSize pk
    	code__2 dst = code . 
                      if pk == DoubleKind || pk == FloatKind
                      then mkSeqInstr (FLD {-D-} size (OpAddr src))
                      else mkSeqInstr (MOV size (OpAddr src) (OpReg dst))
    in
    	returnSUs (Any pk code__2)


getReg (StInt i)
  = let
    	src = ImmInt (fromInteger i)
    	code dst = mkSeqInstr (MOV L (OpImm src) (OpReg dst))
    in
    	returnSUs (Any IntKind code)

getReg leaf
  | maybeToBool imm =
    let
    	code dst = mkSeqInstr (MOV L (OpImm imm__2) (OpReg dst)) 
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

getAmode (StPrim IntSubOp [x, StInt i])
  =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg x    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    	returnSUs (Amode (Addr (Just reg) Nothing off) code)

getAmode (StPrim IntAddOp [x, StInt i])
  | maybeToBool imm 
  = let
        code = mkSeqInstrs []
    in
    	returnSUs (Amode (ImmAddr imm__2 (fromInteger i)) code)
  where
    imm = maybeImm x
    imm__2 = case imm of Just x -> x

getAmode (StPrim IntAddOp [x, StInt i])
  =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg x    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    	returnSUs (Amode (Addr (Just reg) Nothing off) code)

getAmode (StPrim IntAddOp [x, y]) =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    getReg x    	    	    `thenSUs` \ register1 ->
    getReg y    	    	    `thenSUs` \ register2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	reg1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	reg2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2]
    in
    	returnSUs (Amode (Addr (Just reg1) (Just (reg2,4)) (ImmInt 0)) code__2)

getAmode leaf
  | maybeToBool imm =
    let code = mkSeqInstrs []
    in
        returnSUs (Amode (ImmAddr imm__2 0) code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other =
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    getReg other    	    	    `thenSUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = Nothing
    in
    	returnSUs (Amode (Addr (Just reg) Nothing (ImmInt 0)) code)

\end{code}

\begin{code}
getOp
    :: StixTree	
    -> SUniqSM (CodeBlock I386Instr,Operand, Size)	-- code, operator, size
getOp (StInt i)
  = returnSUs (asmParThen [], OpImm (ImmInt (fromInteger i)), L)

getOp (StInd pk mem)
  = getAmode mem    	    	    `thenSUs` \ amode ->
    let
    	code = amodeCode amode --asmVoid
    	addr  = amodeAddr amode
    	sz = kindToSize pk
    in returnSUs (code, OpAddr addr, sz)

getOp op
  = getReg op	    	    	    `thenSUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenSUs` \ tmp ->
    let 
    	code = registerCode register tmp
    	reg = registerName register tmp
    	pk = registerKind register
    	sz = kindToSize pk
    in
    	returnSUs (code, OpReg reg, sz)

getOpRI
    :: StixTree	
    -> SUniqSM (CodeBlock I386Instr,Operand, Size)	-- code, operator, size
getOpRI op
  | maybeToBool imm
  = returnSUs (asmParThen [], OpImm imm_op, L)
  where
    imm = maybeImm op
    imm_op = case imm of Just x -> x

getOpRI op
  = getReg op	    	    	    `thenSUs` \ register ->
    getNewRegNCG (registerKind register)
    	    	        	    `thenSUs` \ tmp ->
    let 
    	code = registerCode register tmp
    	reg = registerName register tmp
    	pk = registerKind register
    	sz = kindToSize pk
    in
    	returnSUs (code, OpReg reg, sz)

\end{code}

Set up a condition code for a conditional branch.

\begin{code}

getCondition :: StixTree -> SUniqSM Condition

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

condIntCode, condFltCode :: Cond -> [StixTree] -> SUniqSM Condition
condIntCode cond [StInd _ x, y] 
  | maybeToBool imm
  = getAmode x    	    	    `thenSUs` \ amode ->
    let
    	code1 = amodeCode amode asmVoid
    	y__2  = amodeAddr amode
    	code__2 = asmParThen [code1] . 
    	    	  mkSeqInstr (CMP L (OpImm imm__2) (OpAddr y__2))
    in
        returnSUs (Condition False cond code__2)
  where
    imm = maybeImm y
    imm__2 = case imm of Just x -> x

condIntCode cond [x, StInt 0] 
  = getReg x	    	    	    `thenSUs` \ register1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    let
        code1 = registerCode register1 tmp1 asmVoid
        src1  = registerName register1 tmp1
        code__2 = asmParThen [code1] . 
    	    	mkSeqInstr (TEST L (OpReg src1) (OpReg src1))
    in
        returnSUs (Condition False cond code__2)

condIntCode cond [x, y] 
  | maybeToBool imm
  = getReg x	    	    	    `thenSUs` \ register1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    let
        code1 = registerCode register1 tmp1 asmVoid
        src1  = registerName register1 tmp1
        code__2 = asmParThen [code1] . 
    	    	mkSeqInstr (CMP L (OpImm imm__2) (OpReg src1))
    in
        returnSUs (Condition False cond code__2)
  where
    imm = maybeImm y
    imm__2 = case imm of Just x -> x

condIntCode cond [StInd _ x, y] 
  = getAmode x    	    	    `thenSUs` \ amode ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
    	code1 = amodeCode amode asmVoid
    	src1  = amodeAddr amode
        code2 = registerCode register2 tmp2 asmVoid
        src2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2] . 
    	    	  mkSeqInstr (CMP L (OpReg src2) (OpAddr src1))
    in
        returnSUs (Condition False cond code__2)

condIntCode cond [y, StInd _ x] 
  = getAmode x    	    	    `thenSUs` \ amode ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
    	code1 = amodeCode amode asmVoid
    	src1  = amodeAddr amode
        code2 = registerCode register2 tmp2 asmVoid
        src2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2] . 
    	    	  mkSeqInstr (CMP L (OpAddr src1) (OpReg src2))
    in
        returnSUs (Condition False cond code__2)

condIntCode cond [x, y] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
        code1 = registerCode register1 tmp1 asmVoid
        src1  = registerName register1 tmp1
        code2 = registerCode register2 tmp2 asmVoid
        src2  = registerName register2 tmp2
        code__2 = asmParThen [code1, code2] . 
    	    	mkSeqInstr (CMP L (OpReg src2) (OpReg src1))
    in
        returnSUs (Condition False cond code__2)

condFltCode cond [x, StDouble 0.0] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getNewRegNCG (registerKind register1)
      	    	        	    `thenSUs` \ tmp1 ->
    let
    	pk1   = registerKind register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code__2 = asmParThen [code1 asmVoid] .
    	    	  mkSeqInstrs [FTST, FSTP D (OpReg st0), -- or FLDZ, FUCOMPP ?
                               FNSTSW,
                               --AND HB (OpImm (ImmInt 68)) (OpReg eax),
                               --XOR HB (OpImm (ImmInt 64)) (OpReg eax)
                               SAHF
                              ]
    in
    	returnSUs (Condition True (fixFPCond cond) code__2)

condFltCode cond [x, y] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG (registerKind register1)
      	    	        	    `thenSUs` \ tmp1 ->
    getNewRegNCG (registerKind register2)
     	    	        	    `thenSUs` \ tmp2 ->
    let
    	pk1   = registerKind register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 = asmParThen [code2 asmVoid, code1 asmVoid] .
    	    	  mkSeqInstrs [FUCOMPP,
                               FNSTSW,
                               --AND HB (OpImm (ImmInt 68)) (OpReg eax),
                               --XOR HB (OpImm (ImmInt 64)) (OpReg eax)
                               SAHF
                              ]
    in
    	returnSUs (Condition True (fixFPCond cond) code__2)

\end{code}

Turn those condition codes into integers now (when they appear on
the right hand side of an assignment).

\begin{code}

condIntReg :: Cond -> [StixTree] -> SUniqSM Register
condIntReg cond args =
    condIntCode cond args 	    `thenSUs` \ condition ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    --getReg dst	    	    	    `thenSUs` \ register ->
    let 
    	--code2 = registerCode register tmp asmVoid
    	--dst__2  = registerName register tmp
        code = condCode condition
        cond = condName condition
-- ToDo: if dst is eax, ebx, ecx, or edx we would not need the move.
        code__2 dst = code . mkSeqInstrs [
	    SETCC cond (OpReg tmp),
	    AND L (OpImm (ImmInt 1)) (OpReg tmp),
	    MOV L (OpReg tmp) (OpReg dst)] 
    in
        returnSUs (Any IntKind code__2)

condFltReg :: Cond -> [StixTree] -> SUniqSM Register

condFltReg cond args =
    getUniqLabelNCG	    	    `thenSUs` \ lbl1 ->
    getUniqLabelNCG	    	    `thenSUs` \ lbl2 ->
    condFltCode cond args 	    `thenSUs` \ condition ->
    let
    	code = condCode condition
    	cond = condName condition
    	code__2 dst = code . mkSeqInstrs [
	    JXX cond lbl1, 
	    MOV L (OpImm (ImmInt 0)) (OpReg dst),
	    JXX ALWAYS lbl2,
	    LABEL lbl1,
	    MOV L (OpImm (ImmInt 1)) (OpReg dst),
	    LABEL lbl2]
    in
        returnSUs (Any IntKind code__2)

\end{code}

Assignments are really at the heart of the whole code generation business.
Almost all top-level nodes of any real importance are assignments, which
correspond to loads, stores, or register transfers.  If we're really lucky,
some of the register transfers will go away, because we can use the destination
register to complete the code generation for the right hand side.  This only
fails when the right hand side is forced into a fixed register (e.g. the result
of a call).  

\begin{code}

assignIntCode :: PrimKind -> StixTree -> StixTree -> SUniqSM (CodeBlock I386Instr)
assignIntCode pk (StInd _ dst) src 
  = getAmode dst    	    	    `thenSUs` \ amode ->
    getOpRI src                     `thenSUs` \ (codesrc, opsrc, sz) ->
    let 
    	code1 = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code__2 = asmParThen [code1, codesrc asmVoid] . 
                  mkSeqInstr (MOV sz opsrc (OpAddr dst__2))
    in
    	returnSUs code__2

assignIntCode pk dst (StInd _ src) =
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    getAmode src    	    	    `thenSUs` \ amode ->
    getReg dst	    	    	    `thenSUs` \ register ->
    let 
    	code1 = amodeCode amode asmVoid
    	src__2  = amodeAddr amode
    	code2 = registerCode register tmp asmVoid
    	dst__2  = registerName register tmp
    	sz    = kindToSize pk
    	code__2 = asmParThen [code1, code2] . 
                  mkSeqInstr (MOV sz (OpAddr src__2) (OpReg dst__2))
    in
    	returnSUs code__2

assignIntCode pk dst src =
    getReg dst	    	    	    `thenSUs` \ register1 ->
    getReg src	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let 
    	dst__2 = registerName register1 tmp
    	code = registerCode register2 dst__2
    	src__2 = registerName register2 dst__2
    	code__2 = if isFixed register2 && dst__2 /= src__2
    	    	  then code . mkSeqInstr (MOV L (OpReg src__2) (OpReg dst__2))
    	    	  else 
                       code
    in
    	returnSUs code__2

assignFltCode :: PrimKind -> StixTree -> StixTree -> SUniqSM (CodeBlock I386Instr)
assignFltCode pk (StInd pk_dst dst) (StInd pk_src src) 
  = getNewRegNCG IntKind       	    `thenSUs` \ tmp ->
    getAmode src    	    	    `thenSUs` \ amodesrc ->
    getAmode dst    	    	    `thenSUs` \ amodedst ->
    --getReg src	    	    	    `thenSUs` \ register ->
    let 
    	codesrc1 = amodeCode amodesrc asmVoid
    	addrsrc1 = amodeAddr amodesrc
    	codedst1 = amodeCode amodedst asmVoid
    	addrdst1 = amodeAddr amodedst
    	addrsrc2 = case (offset addrsrc1 4) of Just x -> x
    	addrdst2 = case (offset addrdst1 4) of Just x -> x

    	code__2 = asmParThen [codesrc1, codedst1] . 
		  mkSeqInstrs ([MOV L (OpAddr addrsrc1) (OpReg tmp),
		                MOV L (OpReg tmp) (OpAddr addrdst1)]
                               ++
		               if pk == DoubleKind 
                               then [MOV L (OpAddr addrsrc2) (OpReg tmp),
		                     MOV L (OpReg tmp) (OpAddr addrdst2)]
                               else [])
    in
        returnSUs code__2

assignFltCode pk (StInd _ dst) src =
    --getNewRegNCG pk        	    `thenSUs` \ tmp ->
    getAmode dst    	    	    `thenSUs` \ amode ->
    getReg src	    	    	    `thenSUs` \ register ->
    let 
    	sz    = kindToSize pk
    	dst__2  = amodeAddr amode

    	code1 = amodeCode amode asmVoid
    	code2 = registerCode register {-tmp-}st0 asmVoid

    	--src__2  = registerName register tmp
    	pk__2  = registerKind register
    	sz__2 = kindToSize pk__2

    	code__2 = asmParThen [code1, code2] . 
		  mkSeqInstr (FSTP sz (OpAddr dst__2))
    in
        returnSUs code__2

assignFltCode pk dst src =
    getReg dst	    	    	    `thenSUs` \ register1 ->
    getReg src	    	    	    `thenSUs` \ register2 ->
    --getNewRegNCG (registerKind register2)
    --	    	        	    `thenSUs` \ tmp ->
    let 
    	sz = kindToSize pk
    	dst__2 = registerName register1 st0 --tmp

    	code = registerCode register2 dst__2
    	src__2 = registerName register2 dst__2

    	code__2 = code 
    in
    	returnSUs code__2

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
    -> SUniqSM (CodeBlock I386Instr)

{-
genJump (StCLbl lbl) 
  | isAsmTemp lbl = returnInstrs [JXX ALWAYS lbl]
  | otherwise     = returnInstrs [JMP (OpImm target)]
  where
    target = ImmCLbl lbl
-}

genJump (StInd pk mem) =
    getAmode mem    	    	    `thenSUs` \ amode ->
    let
    	code = amodeCode amode
    	target  = amodeAddr amode
    in
    	returnSeq code [JMP (OpAddr target)]

genJump tree 
  | maybeToBool imm
  = returnInstr (JMP (OpImm target))
  where
    imm = maybeImm tree
    target = case imm of Just x -> x


genJump tree =
    getReg tree	    	    	    `thenSUs` \ register ->
    getNewRegNCG PtrKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	target = registerName register tmp
    in
    	returnSeq code [JMP (OpReg target)]

\end{code}

Conditional jumps are always to local labels, so we can use
branch instructions.  First, we have to ensure that the condition
codes are set according to the supplied comparison operation.

\begin{code}

genCondJump 
    :: CLabel	    -- the branch target
    -> StixTree     -- the condition on which to branch
    -> SUniqSM (CodeBlock I386Instr)

genCondJump lbl bool = 
    getCondition bool  	    	    `thenSUs` \ condition ->
    let
    	code = condCode condition
    	cond = condName condition
        target = ImmCLbl lbl    
    in
        returnSeq code [JXX cond lbl]

\end{code}

\begin{code}

genCCall
    :: FAST_STRING  -- function to call
    -> PrimKind	    -- type of the result
    -> [StixTree]   -- arguments (of mixed type)
    -> SUniqSM (CodeBlock I386Instr)

genCCall fn kind [StInt i] 
  | fn == SLIT ("PerformGC_wrapper")
  = getUniqLabelNCG    	    	    `thenSUs` \ lbl ->
    let
        call = [MOV L (OpImm (ImmInt (fromInteger i))) (OpReg eax),
                MOV L (OpImm (ImmCLbl lbl)) 
                      -- this is hardwired
                      (OpAddr (Addr (Just ebx) Nothing (ImmInt 104))),
                JMP (OpImm (ImmLit (uppPStr (SLIT ("_PerformGC_wrapper"))))),
                LABEL lbl]
    in
    	returnInstrs call

genCCall fn kind args =
    mapSUs getCallArg args `thenSUs` \ argCode ->
    let
        nargs = length args
        code1 = asmParThen [asmSeq [ -- MOV L (OpReg esp) (OpAddr (Addr (Just ebx) Nothing (ImmInt 80))),
                        MOV L (OpAddr (Addr (Just ebx) Nothing (ImmInt 100))) (OpReg esp)
                                   ]
                           ]
        code2 = asmParThen (map ($ asmVoid) (reverse argCode)) 
        call = [CALL (ImmLit fn__2) -- ,
                -- ADD L (OpImm (ImmInt (nargs * 4))) (OpReg esp),
                -- MOV L (OpAddr (Addr (Just ebx) Nothing (ImmInt 80))) (OpReg esp)
                ]
    in
    	returnSeq (code1 . code2) call
  where
    -- function names that begin with '.' are assumed to be special internally
    -- generated names like '.mul,' which don't get an underscore prefix
    fn__2 = case (_HEAD_ fn) of
	      '.' -> uppPStr fn
	      _   -> uppBeside (uppChar '_') (uppPStr fn)

    getCallArg 
        :: StixTree				-- Current argument
        -> SUniqSM (CodeBlock I386Instr)	-- code
    getCallArg arg = 
        getOp arg	    	    	    `thenSUs` \ (code, op, sz) ->
        returnSUs (code . mkSeqInstr (PUSH sz op))
\end{code}

Trivial (dyadic) instructions.  Only look for constants on the right hand
side, because that's where the generic optimizer will have put them.

\begin{code}

trivialCode 
    :: (Operand -> Operand -> I386Instr) 
    -> [StixTree]
    -> Bool	-- is the instr commutative?
    -> SUniqSM Register

trivialCode instr [x, y] _
  | maybeToBool imm
  = getReg x	    	    	    `thenSUs` \ register1 ->
    --getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    let
    	fixedname  = registerName register1 eax
    	code__2 dst = let code1 = registerCode register1 dst 
    	                  src1  = registerName register1 dst
                      in code1 .
                         if isFixed register1 && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           instr (OpImm imm__2) (OpReg dst)]
                         else 
                                mkSeqInstrs [instr (OpImm imm__2) (OpReg src1)]
    in
    	returnSUs (Any IntKind code__2)
  where
    imm = maybeImm y
    imm__2 = case imm of Just x -> x

trivialCode instr [x, y] _
  | maybeToBool imm
  = getReg y	    	    	    `thenSUs` \ register1 ->
    --getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    let
    	fixedname  = registerName register1 eax
    	code__2 dst = let code1 = registerCode register1 dst
                          src1  = registerName register1 dst
                      in code1 .
                         if isFixed register1 && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           instr (OpImm imm__2) (OpReg dst)]
                         else 
                                mkSeqInstr (instr (OpImm imm__2) (OpReg src1))
    in
    	returnSUs (Any IntKind code__2)
  where
    imm = maybeImm x
    imm__2 = case imm of Just x -> x

trivialCode instr [x, StInd pk mem] _
  = getReg x	    	    	    `thenSUs` \ register ->
    --getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let
    	fixedname  = registerName register eax
    	code2 = amodeCode amode asmVoid
    	src2  = amodeAddr amode
    	code__2 dst = let code1 = registerCode register dst asmVoid
                          src1  = registerName register dst
                      in asmParThen [code1, code2] .
                         if isFixed register && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           instr (OpAddr src2)  (OpReg dst)]
                         else 
                                mkSeqInstr (instr (OpAddr src2) (OpReg src1))
    in
    	returnSUs (Any pk code__2)

trivialCode instr [StInd pk mem, y] _
  = getReg y	    	    	    `thenSUs` \ register ->
    --getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let
    	fixedname  = registerName register eax
    	code2 = amodeCode amode asmVoid
    	src2  = amodeAddr amode
    	code__2 dst = let 
    	                  code1 = registerCode register dst asmVoid
    	                  src1  = registerName register dst
                      in asmParThen [code1, code2] .
                         if isFixed register && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           instr (OpAddr src2)  (OpReg dst)]
                         else 
                                mkSeqInstr (instr (OpAddr src2) (OpReg src1))
    in
    	returnSUs (Any pk code__2)

trivialCode instr [x, y] is_comm_op 
  = getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    --getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
    	fixedname  = registerName register1 eax
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = let
    	                  code1 = registerCode register1 dst asmVoid
    	                  src1  = registerName register1 dst
                      in asmParThen [code1, code2] .
                         if isFixed register1 && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           instr (OpReg src2)  (OpReg dst)]
                         else 
                                mkSeqInstr (instr (OpReg src2) (OpReg src1))
    in
    	returnSUs (Any IntKind code__2)

addCode 
    :: Size
    -> [StixTree]
    -> SUniqSM Register
addCode sz [x, StInt y]
  =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . 
                      mkSeqInstr (LEA sz (OpAddr (Addr (Just src1) Nothing src2)) (OpReg dst))
    in
    	returnSUs (Any IntKind code__2)

addCode sz [x, StInd _ mem]
  = getReg x	    	    	    `thenSUs` \ register1 ->
    --getNewRegNCG (registerKind register1)
    --  	    	        	    `thenSUs` \ tmp1 ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code2 = amodeCode amode
    	src2  = amodeAddr amode

    	fixedname  = registerName register1 eax
    	code__2 dst = let code1 = registerCode register1 dst
    	                  src1  = registerName register1 dst
    	              in asmParThen [code2 asmVoid,code1 asmVoid] .
                         if isFixed register1 && src1 /= dst
                         then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
                                           ADD sz (OpAddr src2)  (OpReg dst)]
                         else 
                                mkSeqInstrs [ADD sz (OpAddr src2) (OpReg src1)]
    in
    	returnSUs (Any IntKind code__2)

addCode sz [StInd _ mem, y]
  = getReg y	    	    	    `thenSUs` \ register2 ->
    --getNewRegNCG (registerKind register2)
    --  	    	        	    `thenSUs` \ tmp2 ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code1 = amodeCode amode
    	src1  = amodeAddr amode

    	fixedname  = registerName register2 eax
    	code__2 dst = let code2 = registerCode register2 dst
                          src2  = registerName register2 dst
                      in asmParThen [code1 asmVoid,code2 asmVoid] .
                         if isFixed register2 && src2 /= dst
                         then mkSeqInstrs [MOV L (OpReg src2) (OpReg dst),
                                           ADD sz (OpAddr src1)  (OpReg dst)]
                         else 
                                mkSeqInstrs [ADD sz (OpAddr src1) (OpReg src2)]
    in
    	returnSUs (Any IntKind code__2)

addCode sz [x, y] =
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
                      mkSeqInstr (LEA sz (OpAddr (Addr (Just src1) (Just (src2,1)) (ImmInt 0))) (OpReg dst))
    in
    	returnSUs (Any IntKind code__2)

subCode 
    :: Size
    -> [StixTree]
    -> SUniqSM Register
subCode sz [x, StInt y]
  = getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (-(fromInteger y))
    	code__2 dst = code . 
                      mkSeqInstr (LEA sz (OpAddr (Addr (Just src1) Nothing src2)) (OpReg dst))
    in
    	returnSUs (Any IntKind code__2)

subCode sz args = trivialCode (SUB sz) args False

divCode 
    :: Size
    -> [StixTree]
    -> Bool -- True => division, False => remainder operation
    -> SUniqSM Register

-- x must go into eax, edx must be a sign-extension of eax, 
-- and y should go in some other register (or memory),
-- so that we get edx:eax / reg -> eax (remainder in edx)
-- Currently we chose to put y in memory (if it is not there already)
divCode sz [x, StInd pk mem] is_division
  = getReg x		    	    `thenSUs` \ register1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code1 = registerCode register1 tmp1 asmVoid
    	src1 = registerName register1 tmp1
    	code2 = amodeCode amode asmVoid
    	src2  = amodeAddr amode
    	code__2 = asmParThen [code1, code2] .
                  mkSeqInstrs [MOV L (OpReg src1) (OpReg eax),
                               CLTD,
                               IDIV sz (OpAddr src2)]
    in
        returnSUs (Fixed (if is_division then eax else edx) IntKind code__2)

divCode sz [x, StInt i] is_division
  = getReg x		    	    `thenSUs` \ register1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1 = registerName register1 tmp1
    	src2 = ImmInt (fromInteger i)
    	code__2 = asmParThen [code1] .
                  mkSeqInstrs [-- we put src2 in (ebx)
                               MOV L (OpImm src2) (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1))),
                               MOV L (OpReg src1) (OpReg eax),
                               CLTD,
                               IDIV sz (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1)))]
    in
        returnSUs (Fixed (if is_division then eax else edx) IntKind code__2)

divCode sz [x, y] is_division
  = getReg x		    	    `thenSUs` \ register1 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    getNewRegNCG IntKind    	    `thenSUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1 = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2 = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2] .
                  if src2 == ecx || src2 == esi
                  then mkSeqInstrs [ MOV L (OpReg src1) (OpReg eax),
                                     CLTD,
                                     IDIV sz (OpReg src2)]
                  else mkSeqInstrs [ -- we put src2 in (ebx)
                                     MOV L (OpReg src2) (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1))),
                                     MOV L (OpReg src1) (OpReg eax),
                                     CLTD,
                                     IDIV sz (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1)))]
    in
        returnSUs (Fixed (if is_division then eax else edx) IntKind code__2)

trivialFCode 
    :: PrimKind
    -> (Size -> Operand -> I386Instr) 
    -> (Size -> Operand -> I386Instr) -- reversed instr
    -> I386Instr -- pop
    -> I386Instr -- reversed instr, pop
    -> [StixTree] 
    -> SUniqSM Register
trivialFCode pk _ instrr _ _ [StInd pk' mem, y]
  = getReg y	    	    	    `thenSUs` \ register2 ->
    --getNewRegNCG (registerKind register2)
    --  	    	        	    `thenSUs` \ tmp2 ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code1 = amodeCode amode
    	src1  = amodeAddr amode

    	code__2 dst = let 
    	                  code2 = registerCode register2 dst
                      	  src2  = registerName register2 dst
                      in asmParThen [code1 asmVoid,code2 asmVoid] .
    	    	         mkSeqInstrs [instrr (kindToSize pk) (OpAddr src1)]
    in
    	returnSUs (Any pk code__2)

trivialFCode pk instr _ _ _ [x, StInd pk' mem]
  = getReg x	    	    	    `thenSUs` \ register1 ->
    --getNewRegNCG (registerKind register1)
    --  	    	        	    `thenSUs` \ tmp1 ->
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code2 = amodeCode amode
    	src2  = amodeAddr amode

    	code__2 dst = let 
    	                  code1 = registerCode register1 dst
    	                  src1  = registerName register1 dst
                      in asmParThen [code2 asmVoid,code1 asmVoid] .
    	    	         mkSeqInstrs [instr (kindToSize pk) (OpAddr src2)]
    in
    	returnSUs (Any pk code__2)

trivialFCode pk _ _ _ instrpr [x, y] =
    getReg x	    	    	    `thenSUs` \ register1 ->
    getReg y	    	    	    `thenSUs` \ register2 ->
    --getNewRegNCG (registerKind register1)
    --  	    	        	    `thenSUs` \ tmp1 ->
    --getNewRegNCG (registerKind register2)
    -- 	    	        	    `thenSUs` \ tmp2 ->
    getNewRegNCG DoubleKind   	    `thenSUs` \ tmp ->
    let
    	pk1   = registerKind register1
    	code1 = registerCode register1 st0 --tmp1
    	src1  = registerName register1 st0 --tmp1

    	pk2   = registerKind register2

    	code__2 dst = let 
    	                  code2 = registerCode register2 dst
    	                  src2  = registerName register2 dst
    	              in asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	         mkSeqInstr instrpr 
    in
    	returnSUs (Any pk1 code__2)

\end{code}

Trivial unary instructions.  Note that we don't have to worry about
matching an StInt as the argument, because genericOpt will already
have handled the constant-folding.

\begin{code}

trivialUCode 
    :: (Operand -> I386Instr) 
    -> [StixTree]
    -> SUniqSM Register

trivialUCode instr [x] =
    getReg x	    	    	    `thenSUs` \ register ->
--    getNewRegNCG IntKind    	    `thenSUs` \ tmp ->
    let
--    	fixedname = registerName register eax
    	code__2 dst = let
    	                  code = registerCode register dst
                      	  src  = registerName register dst
                      in code . if isFixed register && dst /= src
                                then mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
                                                  instr (OpReg dst)]
                                else mkSeqInstr (instr (OpReg src))
    in
        returnSUs (Any IntKind code__2)

trivialUFCode 
    :: PrimKind
    -> I386Instr
    -> [StixTree]
    -> SUniqSM Register

trivialUFCode pk instr [StInd pk' mem] =
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code = amodeCode amode
    	src  = amodeAddr amode
    	code__2 dst = code . mkSeqInstrs [FLD (kindToSize pk) (OpAddr src),
                                          instr]
    in
    	returnSUs (Any pk code__2)

trivialUFCode pk instr [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    --getNewRegNCG pk        	    `thenSUs` \ tmp ->
    let
    	code__2 dst = let
    	                  code = registerCode register dst
    	                  src  = registerName register dst
                      in code . mkSeqInstrs [instr]
    in
    	returnSUs (Any pk code__2)
\end{code}

Absolute value on integers, mostly for gmp size check macros.  Again,
the argument cannot be an StInt, because genericOpt already folded
constants.

\begin{code}

absIntCode :: [StixTree] -> SUniqSM Register
absIntCode [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    --getNewRegNCG IntKind    	    `thenSUs` \ reg ->
    getUniqLabelNCG    	    	    `thenSUs` \ lbl ->
    let
    	code__2 dst = let code = registerCode register dst
    	                  src  = registerName register dst
                      in code . if isFixed register && dst /= src
                                then mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
                                                  TEST L (OpReg dst) (OpReg dst),
                                                  JXX GE lbl,
                                                  NEGI L (OpReg dst),
                                                  LABEL lbl]
                                else mkSeqInstrs [TEST L (OpReg src) (OpReg src),
                                                  JXX GE lbl,
                                                  NEGI L (OpReg src),
                                                  LABEL lbl]
    in
    	returnSUs (Any IntKind code__2)

\end{code}
                      
Simple integer coercions that don't require any code to be generated.
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

Integer to character conversion.  We try to do this in one step if
the original object is in memory.

\begin{code}
chrCode :: [StixTree] -> SUniqSM Register
{-
chrCode [StInd pk mem] =
    getAmode mem    	    	    `thenSUs` \ amode ->
    let 
    	code = amodeCode amode
    	src  = amodeAddr amode
    	code__2 dst = code . mkSeqInstr (MOVZX L (OpAddr src) (OpReg dst))
    in
    	returnSUs (Any pk code__2)
-}
chrCode [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    --getNewRegNCG IntKind    	    `thenSUs` \ reg ->
    let
    	fixedname = registerName register eax
    	code__2 dst = let
    	                  code = registerCode register dst
    	                  src  = registerName register dst
                      in code . 
                         if isFixed register && src /= dst
                         then mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
                                           AND L (OpImm (ImmInt 255)) (OpReg dst)]
                         else mkSeqInstr (AND L (OpImm (ImmInt 255)) (OpReg src))
    in
        returnSUs (Any IntKind code__2)

\end{code}

More complicated integer/float conversions.  Here we have to store
temporaries in memory to move between the integer and the floating
point register sets.

\begin{code}
coerceInt2FP :: PrimKind -> [StixTree] -> SUniqSM Register
coerceInt2FP pk [x] = 
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG IntKind      	    `thenSUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
        -- to fix: should spill instead of using R1
    	              MOV L (OpReg src) (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1))),
    	              FILD (kindToSize pk) (Addr (Just ebx) Nothing (ImmInt OFFSET_R1)) dst]
    in
    	returnSUs (Any pk code__2)

coerceFP2Int :: [StixTree] -> SUniqSM Register
coerceFP2Int [x] =
    getReg x	    	    	    `thenSUs` \ register ->
    getNewRegNCG DoubleKind    	    `thenSUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	pk   = registerKind register

    	code__2 dst = let 
                      in code . mkSeqInstrs [
    	                        FRNDINT,
    	                        FIST L (Addr (Just ebx) Nothing (ImmInt OFFSET_R1)),
    	                        MOV L (OpAddr (Addr (Just ebx) Nothing (ImmInt OFFSET_R1))) (OpReg dst)]
    in
    	returnSUs (Any IntKind code__2)
\end{code}

Some random little helpers.

\begin{code}

maybeImm :: StixTree -> Maybe Imm
maybeImm (StInt i) 
  | i >= toInteger minInt && i <= toInteger maxInt = Just (ImmInt (fromInteger i))
  | otherwise = Just (ImmInteger i)
maybeImm (StLitLbl s)  = Just (ImmLit (uppBeside (uppChar '_') s))
maybeImm (StLitLit s)  = Just (strImmLit (cvtLitLit (_UNPK_ s)))
maybeImm (StCLbl l) = Just (ImmCLbl l)
maybeImm _          = Nothing

mangleIndexTree :: StixTree -> StixTree

mangleIndexTree (StIndex pk base (StInt i)) = 
    StPrim IntAddOp [base, off]
  where
    off = StInt (i * size pk)
    size :: PrimKind -> Integer
    size pk = case kindToSize pk of
    	{B -> 1; S -> 2; L -> 4; F -> 4; D -> 8 }

mangleIndexTree (StIndex pk base off) = 
    case pk of
    	CharKind -> StPrim IntAddOp [base, off]
    	_   	 -> StPrim IntAddOp [base, off__2]
  where
    off__2 = StPrim SllOp [off, StInt (shift pk)]
    shift :: PrimKind -> Integer
    shift DoubleKind 	= 3
    shift _ 	       	= 2

cvtLitLit :: String -> String
cvtLitLit "stdin"  = "_IO_stdin_"   
cvtLitLit "stdout" = "_IO_stdout_" 
cvtLitLit "stderr" = "_IO_stderr_"
cvtLitLit s 
  | isHex s = s
  | otherwise = error ("Native code generator can't handle ``" ++ s ++ "''")
  where 
    isHex ('0':'x':xs) = all isHexDigit xs
    isHex _ = False
    -- Now, where have I seen this before?
    isHexDigit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'


\end{code}

\begin{code}

stackArgLoc = 23 :: Int	-- where to stack call arguments 

\end{code}

\begin{code}

getNewRegNCG :: PrimKind -> SUniqSM Reg
getNewRegNCG pk = 
      getSUnique          `thenSUs` \ u ->
      returnSUs (mkReg u pk)

fixFPCond :: Cond -> Cond
-- on the 486 the flags set by FP compare are the unsigned ones!
fixFPCond GE  = GEU
fixFPCond GT  = GU
fixFPCond LT  = LU
fixFPCond LE  = LEU
fixFPCond any = any
\end{code}
