%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[MachCode]{Generating machine code}

This is a big module, but, if you pay attention to
(a) the sectioning, (b) the type signatures, and
(c) the \tr{#if blah_TARGET_ARCH} things, the
structure should not be too overwhelming.

\begin{code}
module MachCode ( stmt2Instrs, asmVoid, InstrList ) where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import MachMisc		-- may differ per-platform
import MachRegs

import AbsCSyn		( MagicId )
import AbsCUtils	( magicIdPrimRep )
import CallConv		( CallConv )
import CLabel		( isAsmTemp, CLabel, pprCLabel_asm )
import Maybes		( maybeToBool, expectJust )
import OrdList		-- quite a bit of it
import PrimRep		( isFloatingRep, PrimRep(..) )
import PrimOp		( PrimOp(..) )
import CallConv		( cCallConv )
import Stix		( getUniqLabelNCG, StixTree(..),
			  StixReg(..), CodeSegment(..), pprStixTrees
			)
import UniqSupply	( returnUs, thenUs, mapUs, mapAndUnzipUs,
			  mapAccumLUs, UniqSM
			)
import Outputable
\end{code}

Code extractor for an entire stix tree---stix statement level.

\begin{code}
stmt2Instrs :: StixTree {- a stix statement -} -> UniqSM InstrBlock

stmt2Instrs stmt = case stmt of
    StComment s    -> returnInstr (COMMENT s)
    StSegment seg  -> returnInstr (SEGMENT seg)

#if 1
    -- StFunBegin, normal non-debugging code for all architectures
    StFunBegin lab -> returnInstr (IF_ARCH_alpha(FUNBEGIN lab,LABEL lab))
#else
    -- StFunBegin, special tracing code for x86-Linux only
    -- requires you to supply
    -- void native_trace ( char* str )
    StFunBegin lab -> getUniqLabelNCG `thenUs` \ str_lbl ->
                      returnUs (mkSeqInstrs [
                         LABEL lab,
                         COMMENT SLIT("begin trace sequence"),
                         SEGMENT DataSegment,
                         LABEL str_lbl,
                         ASCII True (showSDoc (pprCLabel_asm lab)),
                         SEGMENT TextSegment,
                         PUSHA,
                         PUSH L (OpImm (ImmCLbl str_lbl)),
                         CALL (ImmLit (text "native_trace")),
		         ADD L (OpImm (ImmInt 4)) (OpReg esp),
                         POPA,
                         COMMENT SLIT("end trace sequence")
                      ])
#endif

    StFunEnd lab   -> IF_ARCH_alpha(returnInstr (FUNEND lab),returnUs id)
    StLabel lab	   -> returnInstr (LABEL lab)

    StJump arg		   -> genJump arg
    StCondJump lab arg	   -> genCondJump lab arg
    StCall fn cconv VoidRep args -> genCCall fn cconv VoidRep args

    StAssign pk dst src
      | isFloatingRep pk -> assignFltCode pk dst src
      | otherwise	 -> assignIntCode pk dst src

    StFallThrough lbl
	-- When falling through on the Alpha, we still have to load pv
	-- with the address of the next routine, so that it can load gp.
      -> IF_ARCH_alpha(returnInstr (LDA pv (AddrImm (ImmCLbl lbl)))
	,returnUs id)

    StData kind args
      -> mapAndUnzipUs getData args	`thenUs` \ (codes, imms) ->
	 returnUs (\xs -> mkSeqList (asmInstr (DATA (primRepToSize kind) imms))
				    (foldr (.) id codes xs))
      where
	getData :: StixTree -> UniqSM (InstrBlock, Imm)

	getData (StInt i)    = returnUs (id, ImmInteger i)
	getData (StDouble d) = returnUs (id, ImmDouble d)
	getData (StLitLbl s) = returnUs (id, ImmLab s)
	getData (StCLbl l)   = returnUs (id, ImmCLbl l)
	getData (StString s) =
	    getUniqLabelNCG 	    	    `thenUs` \ lbl ->
	    returnUs (mkSeqInstrs [LABEL lbl,
				   ASCII True (_UNPK_ s)],
				   ImmCLbl lbl)
	-- the linker can handle simple arithmetic...
	getData (StIndex rep (StCLbl lbl) (StInt off)) =
		returnUs (id, ImmIndex lbl (fromInteger (off * sizeOf rep)))
\end{code}

%************************************************************************
%*									*
\subsection{General things for putting together code sequences}
%*									*
%************************************************************************

\begin{code}
type InstrList  = OrdList Instr
type InstrBlock = InstrList -> InstrList

asmVoid :: InstrList
asmVoid = mkEmptyList

asmInstr :: Instr -> InstrList
asmInstr i = mkUnitList i

asmSeq :: [Instr] -> InstrList
asmSeq is = foldr (mkSeqList . asmInstr) asmVoid is

asmParThen :: [InstrList] -> InstrBlock
asmParThen others code = mkSeqList (foldr mkParList mkEmptyList others) code

returnInstr :: Instr -> UniqSM InstrBlock
returnInstr instr = returnUs (\xs -> mkSeqList (asmInstr instr) xs)

returnInstrs :: [Instr] -> UniqSM InstrBlock
returnInstrs instrs = returnUs (\xs -> mkSeqList (asmSeq instrs) xs)

returnSeq :: InstrBlock -> [Instr] -> UniqSM InstrBlock
returnSeq code instrs = returnUs (\xs -> code (mkSeqList (asmSeq instrs) xs))

mkSeqInstr :: Instr -> InstrBlock
mkSeqInstr instr code = mkSeqList (asmInstr instr) code

mkSeqInstrs :: [Instr] -> InstrBlock
mkSeqInstrs instrs code = mkSeqList (asmSeq instrs) code
\end{code}

\begin{code}
mangleIndexTree :: StixTree -> StixTree

mangleIndexTree (StIndex pk base (StInt i))
  = StPrim IntAddOp [base, off]
  where
    off = StInt (i * sizeOf pk)

#ifndef i386_TARGET_ARCH
mangleIndexTree (StIndex pk base off)
  = StPrim IntAddOp [base,
      case pk of
    	CharRep -> off
    	_	-> let
			s = shift pk
		   in
		   ASSERT(toInteger s == expectJust "MachCode" (exactLog2 (sizeOf pk)))
		   StPrim SllOp [off, StInt s]
    ]
  where
    shift DoubleRep 	= 3::Integer
    shift _ 	       	= IF_ARCH_alpha(3,2)
#else
-- Hmm..this is an attempt at fixing Adress amodes (i.e., *[disp](base,index,<n>),
-- that do include the size of the primitive kind we're addressing. When StIndex
-- is expanded to actual code, the index (in units) is by the above code approp.
-- shifted to get the no. of bytes. Since Address amodes do contain size info
-- explicitly, we disable the shifting for x86s.
mangleIndexTree (StIndex pk base off) = StPrim IntAddOp [base, off]
#endif

\end{code}

\begin{code}
maybeImm :: StixTree -> Maybe Imm

maybeImm (StLitLbl s) = Just (ImmLab s)
maybeImm (StCLbl   l) = Just (ImmCLbl l)

maybeImm (StIndex rep (StCLbl l) (StInt off)) = 
	Just (ImmIndex l (fromInteger (off * sizeOf rep)))

maybeImm (StInt i)
  | i >= toInteger minInt && i <= toInteger maxInt
  = Just (ImmInt (fromInteger i))
  | otherwise
  = Just (ImmInteger i)

maybeImm _ = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{The @Register@ type}
%*									*
%************************************************************************

@Register@s passed up the tree.  If the stix code forces the register
to live in a pre-decided machine register, it comes out as @Fixed@;
otherwise, it comes out as @Any@, and the parent can decide which
register to put it in.

\begin{code}
data Register
  = Fixed   PrimRep Reg InstrBlock
  | Any	    PrimRep (Reg -> InstrBlock)

registerCode :: Register -> Reg -> InstrBlock
registerCode (Fixed _ _ code) reg = code
registerCode (Any _ code) reg = code reg

registerName :: Register -> Reg -> Reg
registerName (Fixed _ reg _) _ = reg
registerName (Any   _ _)   reg = reg

registerRep :: Register -> PrimRep
registerRep (Fixed pk _ _) = pk
registerRep (Any   pk _) = pk

isFixed :: Register -> Bool
isFixed (Fixed _ _ _) = True
isFixed (Any _ _)     = False
\end{code}

Generate code to get a subtree into a @Register@:
\begin{code}
getRegister :: StixTree -> UniqSM Register

getRegister (StReg (StixMagicId stgreg))
  = case (magicIdRegMaybe stgreg) of
      Just reg -> returnUs (Fixed (magicIdPrimRep stgreg) reg id)
                  -- cannae be Nothing

getRegister (StReg (StixTemp u pk))
  = returnUs (Fixed pk (UnmappedReg u pk) id)

getRegister tree@(StIndex _ _ _) = getRegister (mangleIndexTree tree)

getRegister (StCall fn cconv kind args)
  = genCCall fn cconv kind args   	    `thenUs` \ call ->
    returnUs (Fixed kind reg call)
  where
    reg = if isFloatingRep kind
	  then IF_ARCH_alpha( f0, IF_ARCH_i386( fake0, IF_ARCH_sparc( f0,)))
	  else IF_ARCH_alpha( v0, IF_ARCH_i386( eax, IF_ARCH_sparc( o0,)))

getRegister (StString s)
  = getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    let
	imm_lbl = ImmCLbl lbl

	code dst = mkSeqInstrs [
	    SEGMENT DataSegment,
	    LABEL lbl,
	    ASCII True (_UNPK_ s),
	    SEGMENT TextSegment,
#if alpha_TARGET_ARCH
	    LDA dst (AddrImm imm_lbl)
#endif
#if i386_TARGET_ARCH
	    MOV L (OpImm imm_lbl) (OpReg dst)
#endif
#if sparc_TARGET_ARCH
	    SETHI (HI imm_lbl) dst,
	    OR False dst (RIImm (LO imm_lbl)) dst
#endif
	    ]
    in
    returnUs (Any PtrRep code)



-- end of machine-"independent" bit; here we go on the rest...

#if alpha_TARGET_ARCH

getRegister (StDouble d)
  = getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
	    DATA TF [ImmLab (rational d)],
	    SEGMENT TextSegment,
	    LDA tmp (AddrImm (ImmCLbl lbl)),
	    LD TF dst (AddrReg tmp)]
    in
    	returnUs (Any DoubleRep code)

getRegister (StPrim primop [x]) -- unary PrimOps
  = case primop of
      IntNegOp -> trivialUCode (NEG Q False) x

      NotOp    -> trivialUCode NOT x

      FloatNegOp  -> trivialUFCode FloatRep  (FNEG TF) x
      DoubleNegOp -> trivialUFCode DoubleRep (FNEG TF) x

      OrdOp -> coerceIntCode IntRep x
      ChrOp -> chrCode x

      Float2IntOp  -> coerceFP2Int    x
      Int2FloatOp  -> coerceInt2FP pr x
      Double2IntOp -> coerceFP2Int    x
      Int2DoubleOp -> coerceInt2FP pr x

      Double2FloatOp -> coerceFltCode x
      Float2DoubleOp -> coerceFltCode x

      other_op -> getRegister (StCall fn cCallConv DoubleRep [x])
	where
	  fn = case other_op of
		 FloatExpOp    -> SLIT("exp")
		 FloatLogOp    -> SLIT("log")
		 FloatSqrtOp   -> SLIT("sqrt")
		 FloatSinOp    -> SLIT("sin")
		 FloatCosOp    -> SLIT("cos")
		 FloatTanOp    -> SLIT("tan")
		 FloatAsinOp   -> SLIT("asin")
		 FloatAcosOp   -> SLIT("acos")
		 FloatAtanOp   -> SLIT("atan")
		 FloatSinhOp   -> SLIT("sinh")
		 FloatCoshOp   -> SLIT("cosh")
		 FloatTanhOp   -> SLIT("tanh")
		 DoubleExpOp   -> SLIT("exp")
		 DoubleLogOp   -> SLIT("log")
		 DoubleSqrtOp  -> SLIT("sqrt")
		 DoubleSinOp   -> SLIT("sin")
		 DoubleCosOp   -> SLIT("cos")
		 DoubleTanOp   -> SLIT("tan")
		 DoubleAsinOp  -> SLIT("asin")
		 DoubleAcosOp  -> SLIT("acos")
		 DoubleAtanOp  -> SLIT("atan")
		 DoubleSinhOp  -> SLIT("sinh")
		 DoubleCoshOp  -> SLIT("cosh")
		 DoubleTanhOp  -> SLIT("tanh")
  where
    pr = panic "MachCode.getRegister: no primrep needed for Alpha"

getRegister (StPrim primop [x, y]) -- dyadic PrimOps
  = case primop of
      CharGtOp -> trivialCode (CMP LTT) y x
      CharGeOp -> trivialCode (CMP LE) y x
      CharEqOp -> trivialCode (CMP EQQ) x y
      CharNeOp -> int_NE_code x y
      CharLtOp -> trivialCode (CMP LTT) x y
      CharLeOp -> trivialCode (CMP LE) x y

      IntGtOp  -> trivialCode (CMP LTT) y x
      IntGeOp  -> trivialCode (CMP LE) y x
      IntEqOp  -> trivialCode (CMP EQQ) x y
      IntNeOp  -> int_NE_code x y
      IntLtOp  -> trivialCode (CMP LTT) x y
      IntLeOp  -> trivialCode (CMP LE) x y

      WordGtOp -> trivialCode (CMP ULT) y x
      WordGeOp -> trivialCode (CMP ULE) x y
      WordEqOp -> trivialCode (CMP EQQ)  x y
      WordNeOp -> int_NE_code x y
      WordLtOp -> trivialCode (CMP ULT) x y
      WordLeOp -> trivialCode (CMP ULE) x y

      AddrGtOp -> trivialCode (CMP ULT) y x
      AddrGeOp -> trivialCode (CMP ULE) y x
      AddrEqOp -> trivialCode (CMP EQQ)  x y
      AddrNeOp -> int_NE_code x y
      AddrLtOp -> trivialCode (CMP ULT) x y
      AddrLeOp -> trivialCode (CMP ULE) x y

      FloatGtOp -> cmpF_code (FCMP TF LE) EQQ x y
      FloatGeOp -> cmpF_code (FCMP TF LTT) EQQ x y
      FloatEqOp -> cmpF_code (FCMP TF EQQ) NE x y
      FloatNeOp -> cmpF_code (FCMP TF EQQ) EQQ x y
      FloatLtOp -> cmpF_code (FCMP TF LTT) NE x y
      FloatLeOp -> cmpF_code (FCMP TF LE) NE x y

      DoubleGtOp -> cmpF_code (FCMP TF LE) EQQ x y
      DoubleGeOp -> cmpF_code (FCMP TF LTT) EQQ x y
      DoubleEqOp -> cmpF_code (FCMP TF EQQ) NE x y
      DoubleNeOp -> cmpF_code (FCMP TF EQQ) EQQ x y
      DoubleLtOp -> cmpF_code (FCMP TF LTT) NE x y
      DoubleLeOp -> cmpF_code (FCMP TF LE) NE x y

      IntAddOp  -> trivialCode (ADD Q False) x y
      IntSubOp  -> trivialCode (SUB Q False) x y
      IntMulOp  -> trivialCode (MUL Q False) x y
      IntQuotOp -> trivialCode (DIV Q False) x y
      IntRemOp  -> trivialCode (REM Q False) x y

      WordQuotOp -> trivialCode (DIV Q True) x y
      WordRemOp  -> trivialCode (REM Q True) x y

      FloatAddOp -> trivialFCode  FloatRep (FADD TF) x y
      FloatSubOp -> trivialFCode  FloatRep (FSUB TF) x y
      FloatMulOp -> trivialFCode  FloatRep (FMUL TF) x y
      FloatDivOp -> trivialFCode  FloatRep (FDIV TF) x y

      DoubleAddOp -> trivialFCode  DoubleRep (FADD TF) x y
      DoubleSubOp -> trivialFCode  DoubleRep (FSUB TF) x y
      DoubleMulOp -> trivialFCode  DoubleRep (FMUL TF) x y
      DoubleDivOp -> trivialFCode  DoubleRep (FDIV TF) x y

      AndOp  -> trivialCode AND x y
      OrOp   -> trivialCode OR  x y
      XorOp  -> trivialCode XOR x y
      SllOp  -> trivialCode SLL x y
      SrlOp  -> trivialCode SRL x y

      ISllOp -> trivialCode SLL x y -- was: panic "AlphaGen:isll"
      ISraOp -> trivialCode SRA x y -- was: panic "AlphaGen:isra"
      ISrlOp -> trivialCode SRL x y -- was: panic "AlphaGen:isrl"

      FloatPowerOp  -> getRegister (StCall SLIT("pow") cCallConv DoubleRep [x,y])
      DoublePowerOp -> getRegister (StCall SLIT("pow") cCallConv DoubleRep [x,y])
  where
    {- ------------------------------------------------------------
	Some bizarre special code for getting condition codes into
	registers.  Integer non-equality is a test for equality
	followed by an XOR with 1.  (Integer comparisons always set
	the result register to 0 or 1.)  Floating point comparisons of
	any kind leave the result in a floating point register, so we
	need to wrangle an integer register out of things.
    -}
    int_NE_code :: StixTree -> StixTree -> UniqSM Register

    int_NE_code x y
      = trivialCode (CMP EQQ) x y	`thenUs` \ register ->
	getNewRegNCG IntRep		`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    src  = registerName register tmp
	    code__2 dst = code . mkSeqInstr (XOR src (RIImm (ImmInt 1)) dst)
	in
	returnUs (Any IntRep code__2)

    {- ------------------------------------------------------------
	Comments for int_NE_code also apply to cmpF_code
    -}
    cmpF_code
	:: (Reg -> Reg -> Reg -> Instr)
	-> Cond
	-> StixTree -> StixTree
	-> UniqSM Register

    cmpF_code instr cond x y
      = trivialFCode pr instr x y	`thenUs` \ register ->
	getNewRegNCG DoubleRep		`thenUs` \ tmp ->
	getUniqLabelNCG			`thenUs` \ lbl ->
	let
	    code = registerCode register tmp
	    result  = registerName register tmp

	    code__2 dst = code . mkSeqInstrs [
		OR zeroh (RIImm (ImmInt 1)) dst,
		BF cond  result (ImmCLbl lbl),
		OR zeroh (RIReg zeroh) dst,
		LABEL lbl]
	in
	returnUs (Any IntRep code__2)
      where
	pr = panic "trivialU?FCode: does not use PrimRep on Alpha"
      ------------------------------------------------------------

getRegister (StInd pk mem)
  = getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = primRepToSize pk
    	code__2 dst = code . mkSeqInstr (LD size dst src)
    in
    returnUs (Any pk code__2)

getRegister (StInt i)
  | fits8Bits i
  = let
    	code dst = mkSeqInstr (OR zeroh (RIImm src) dst)
    in
    returnUs (Any IntRep code)
  | otherwise
  = let
    	code dst = mkSeqInstr (LDI Q dst src)
    in
    returnUs (Any IntRep code)
  where
    src = ImmInt (fromInteger i)

getRegister leaf
  | maybeToBool imm
  = let
    	code dst = mkSeqInstr (LDA dst (AddrImm imm__2))
    in
    returnUs (Any PtrRep code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

getRegister (StDouble d)
  = getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
	    DATA DF [ImmDouble d],
	    SEGMENT TextSegment,
	    GLD DF (ImmAddr (ImmCLbl lbl) 0) dst
	    ]
    in
    returnUs (Any DoubleRep code)


getRegister (StPrim primop [x]) -- unary PrimOps
  = case primop of
      IntNegOp  -> trivialUCode (NEGI L) x
      NotOp	-> trivialUCode (NOT L) x

      FloatNegOp  -> trivialUFCode FloatRep  (GNEG F) x
      DoubleNegOp -> trivialUFCode DoubleRep (GNEG DF) x

      FloatSqrtOp  -> trivialUFCode FloatRep  (GSQRT F) x
      DoubleSqrtOp -> trivialUFCode DoubleRep (GSQRT DF) x

      Double2FloatOp -> trivialUFCode FloatRep  GDTOF x
      Float2DoubleOp -> trivialUFCode DoubleRep GFTOD x

      OrdOp -> coerceIntCode IntRep x
      ChrOp -> chrCode x

      Float2IntOp  -> coerceFP2Int x
      Int2FloatOp  -> coerceInt2FP FloatRep x
      Double2IntOp -> coerceFP2Int x
      Int2DoubleOp -> coerceInt2FP DoubleRep x

      other_op ->
        let
	    fixed_x = if   is_float_op  -- promote to double
		      then StPrim Float2DoubleOp [x]
		      else x
	in
	getRegister (StCall fn cCallConv DoubleRep [x])
       where
	(is_float_op, fn)
	  = case primop of
	      FloatExpOp    -> (True,  SLIT("exp"))
	      FloatLogOp    -> (True,  SLIT("log"))

	      FloatSinOp    -> (True,  SLIT("sin"))
	      FloatCosOp    -> (True,  SLIT("cos"))
	      FloatTanOp    -> (True,  SLIT("tan"))

	      FloatAsinOp   -> (True,  SLIT("asin"))
	      FloatAcosOp   -> (True,  SLIT("acos"))
	      FloatAtanOp   -> (True,  SLIT("atan"))

	      FloatSinhOp   -> (True,  SLIT("sinh"))
	      FloatCoshOp   -> (True,  SLIT("cosh"))
	      FloatTanhOp   -> (True,  SLIT("tanh"))

	      DoubleExpOp   -> (False, SLIT("exp"))
	      DoubleLogOp   -> (False, SLIT("log"))

	      DoubleSinOp   -> (False, SLIT("sin"))
	      DoubleCosOp   -> (False, SLIT("cos"))
	      DoubleTanOp   -> (False, SLIT("tan"))

	      DoubleAsinOp  -> (False, SLIT("asin"))
	      DoubleAcosOp  -> (False, SLIT("acos"))
	      DoubleAtanOp  -> (False, SLIT("atan"))

	      DoubleSinhOp  -> (False, SLIT("sinh"))
	      DoubleCoshOp  -> (False, SLIT("cosh"))
	      DoubleTanhOp  -> (False, SLIT("tanh"))

getRegister (StPrim primop [x, y]) -- dyadic PrimOps
  = case primop of
      CharGtOp -> condIntReg GTT x y
      CharGeOp -> condIntReg GE x y
      CharEqOp -> condIntReg EQQ x y
      CharNeOp -> condIntReg NE x y
      CharLtOp -> condIntReg LTT x y
      CharLeOp -> condIntReg LE x y

      IntGtOp  -> condIntReg GTT x y
      IntGeOp  -> condIntReg GE x y
      IntEqOp  -> condIntReg EQQ x y
      IntNeOp  -> condIntReg NE x y
      IntLtOp  -> condIntReg LTT x y
      IntLeOp  -> condIntReg LE x y

      WordGtOp -> condIntReg GU  x y
      WordGeOp -> condIntReg GEU x y
      WordEqOp -> condIntReg EQQ  x y
      WordNeOp -> condIntReg NE  x y
      WordLtOp -> condIntReg LU  x y
      WordLeOp -> condIntReg LEU x y

      AddrGtOp -> condIntReg GU  x y
      AddrGeOp -> condIntReg GEU x y
      AddrEqOp -> condIntReg EQQ  x y
      AddrNeOp -> condIntReg NE  x y
      AddrLtOp -> condIntReg LU  x y
      AddrLeOp -> condIntReg LEU x y

      FloatGtOp -> condFltReg GTT x y
      FloatGeOp -> condFltReg GE x y
      FloatEqOp -> condFltReg EQQ x y
      FloatNeOp -> condFltReg NE x y
      FloatLtOp -> condFltReg LTT x y
      FloatLeOp -> condFltReg LE x y

      DoubleGtOp -> condFltReg GTT x y
      DoubleGeOp -> condFltReg GE x y
      DoubleEqOp -> condFltReg EQQ x y
      DoubleNeOp -> condFltReg NE x y
      DoubleLtOp -> condFltReg LTT x y
      DoubleLeOp -> condFltReg LE x y

      IntAddOp  -> {- ToDo: fix this, whatever it is (WDP 96/04)...
		   -- this should be optimised by the generic Opts,
		   -- I don't know why it is not (sometimes)!
		   case args of
		    [x, StInt 0] -> getRegister x
		    _ -> add_code L x y
		   -}
		   add_code  L x y

      IntSubOp  -> sub_code  L x y
      IntQuotOp -> quot_code L x y True{-division-}
      IntRemOp  -> quot_code L x y False{-remainder-}
      IntMulOp  -> trivialCode (IMUL L) x y {-True-}

      FloatAddOp -> trivialFCode  FloatRep  GADD x y
      FloatSubOp -> trivialFCode  FloatRep  GSUB x y
      FloatMulOp -> trivialFCode  FloatRep  GMUL x y
      FloatDivOp -> trivialFCode  FloatRep  GDIV x y

      DoubleAddOp -> trivialFCode DoubleRep GADD x y
      DoubleSubOp -> trivialFCode DoubleRep GSUB x y
      DoubleMulOp -> trivialFCode DoubleRep GMUL x y
      DoubleDivOp -> trivialFCode DoubleRep GDIV x y

      AndOp -> trivialCode (AND L) x y {-True-}
      OrOp  -> trivialCode (OR L)  x y {-True-}
      XorOp -> trivialCode (XOR L) x y {-True-}

	{- Shift ops on x86s have constraints on their source, it
	   either has to be Imm, CL or 1
	    => trivialCode's is not restrictive enough (sigh.)
	-}
	   
      SllOp -> shift_code (SHL L) x y {-False-}
      SrlOp -> shift_code (SHR L) x y {-False-}

      ISllOp -> shift_code (SHL L) x y {-False-}
      ISraOp -> shift_code (SAR L) x y {-False-}
      ISrlOp -> shift_code (SHR L) x y {-False-}

      FloatPowerOp  -> getRegister (StCall SLIT("pow") cCallConv DoubleRep 
                                           [promote x, promote y])
		       where promote x = StPrim Float2DoubleOp [x]
      DoublePowerOp -> getRegister (StCall SLIT("pow") cCallConv DoubleRep 
                                           [x, y])
  where

    --------------------
    shift_code :: (Operand -> Operand -> Instr)
	       -> StixTree
	       -> StixTree
	       -> UniqSM Register

      {- Case1: shift length as immediate -}
      -- Code is the same as the first eq. for trivialCode -- sigh.
    shift_code instr x y{-amount-}
      | maybeToBool imm
      = getRegister x		`thenUs` \ register ->
	let
	    op_imm = OpImm imm__2
	    code__2 dst = 
		let
		 code  = registerCode  register dst
		 src   = registerName  register dst
	        in
		mkSeqInstr (COMMENT SLIT("shift_code")) . 
		code .
		if isFixed register && src /= dst
		then
		   mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
				instr op_imm  (OpReg dst)]
		else
		   mkSeqInstr (instr op_imm (OpReg src)) 
	in
        returnUs (Any IntRep code__2)
      where
       imm = maybeImm y
       imm__2 = case imm of Just x -> x

      {- Case2: shift length is complex (non-immediate) -}
    shift_code instr x y{-amount-}
     = getRegister y		`thenUs` \ register1 ->  
       getRegister x		`thenUs` \ register2 ->
       let
	-- Note: we force the shift length to be loaded
	-- into ECX, so that we can use CL when shifting.
	-- (only register location we are allowed
	-- to put shift amounts.)
	-- 
	-- The shift instruction is fed ECX as src reg,
	-- but we coerce this into CL when printing out.
	src1	= registerName register1 ecx
    	code1   = if src1 /= ecx then -- if it is not in ecx already, force it!
    		    registerCode register1 ecx .
		    mkSeqInstr (MOV L (OpReg src1) (OpReg ecx))
		  else 
    		    registerCode register1 ecx
    	code__2 = 
		      let
	               code2 = registerCode register2 eax
	               src2  = registerName register2 eax
		      in
		      code1 . code2 .
		      mkSeqInstr (instr (OpReg ecx) (OpReg eax))
       in
       returnUs (Fixed IntRep eax code__2)

    --------------------
    add_code :: Size -> StixTree -> StixTree -> UniqSM Register

    add_code sz x (StInt y)
      = getRegister x		`thenUs` \ register ->
	getNewRegNCG IntRep	`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    src1 = registerName register tmp
	    src2 = ImmInt (fromInteger y)
	    code__2 dst 
               = code .
		 mkSeqInstr (LEA sz (OpAddr (AddrBaseIndex (Just src1) Nothing src2)) 
                                    (OpReg dst))
	in
	returnUs (Any IntRep code__2)

    add_code sz x y
      = getRegister x		`thenUs` \ register1 ->
	getRegister y		`thenUs` \ register2 ->
	getNewRegNCG IntRep	`thenUs` \ tmp1 ->
	getNewRegNCG IntRep	`thenUs` \ tmp2 ->
	let
	    code1 = registerCode register1 tmp1 asmVoid
	    src1  = registerName register1 tmp1
	    code2 = registerCode register2 tmp2 asmVoid
	    src2  = registerName register2 tmp2
	    code__2 dst 
               = asmParThen [code1, code2] .
		 mkSeqInstr (LEA sz (OpAddr (AddrBaseIndex (Just src1) (Just (src2,1)) 
                                                           (ImmInt 0))) 
                                    (OpReg dst))
	in
	returnUs (Any IntRep code__2)

    --------------------
    sub_code :: Size -> StixTree -> StixTree -> UniqSM Register

    sub_code sz x (StInt y)
      = getRegister x		`thenUs` \ register ->
	getNewRegNCG IntRep	`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    src1 = registerName register tmp
	    src2 = ImmInt (-(fromInteger y))
	    code__2 dst 
               = code .
		 mkSeqInstr (LEA sz (OpAddr (AddrBaseIndex (Just src1) Nothing src2))
                                    (OpReg dst))
	in
	returnUs (Any IntRep code__2)

    sub_code sz x y = trivialCode (SUB sz) x y {-False-}

    --------------------
    quot_code
	:: Size
	-> StixTree -> StixTree
	-> Bool -- True => division, False => remainder operation
	-> UniqSM Register

    -- x must go into eax, edx must be a sign-extension of eax, and y
    -- should go in some other register (or memory), so that we get
    -- edx:eax / reg -> eax (remainder in edx) Currently we chose to
    -- put y in memory (if it is not there already)

    quot_code sz x (StInd pk mem) is_division
      = getRegister x		`thenUs` \ register1 ->
	getNewRegNCG IntRep	`thenUs` \ tmp1 ->
	getAmode mem		`thenUs` \ amode ->
	let
	    code1   = registerCode register1 tmp1 asmVoid
	    src1    = registerName register1 tmp1
	    code2   = amodeCode amode asmVoid
	    src2    = amodeAddr amode
	    code__2 = asmParThen [code1, code2] .
		      mkSeqInstrs [MOV L (OpReg src1) (OpReg eax),
				   CLTD,
				   IDIV sz (OpAddr src2)]
	in
	returnUs (Fixed IntRep (if is_division then eax else edx) code__2)

    quot_code sz x (StInt i) is_division
      = getRegister x		`thenUs` \ register1 ->
	getNewRegNCG IntRep	`thenUs` \ tmp1 ->
	let
	    code1   = registerCode register1 tmp1 asmVoid
	    src1    = registerName register1 tmp1
	    src2    = ImmInt (fromInteger i)
	    code__2 = asmParThen [code1] .
		      mkSeqInstrs [-- we put src2 in (ebx)
		         MOV L (OpImm src2) 
                               (OpAddr (AddrBaseIndex (Just ebx) Nothing 
                                                      (ImmInt OFFSET_R1))),
			 MOV L (OpReg src1) (OpReg eax),
			 CLTD,
			 IDIV sz (OpAddr (AddrBaseIndex (Just ebx) Nothing 
                                         (ImmInt OFFSET_R1)))
                      ]
	in
	returnUs (Fixed IntRep (if is_division then eax else edx) code__2)

    quot_code sz x y is_division
      = getRegister x		`thenUs` \ register1 ->
	getNewRegNCG IntRep	`thenUs` \ tmp1 ->
	getRegister y		`thenUs` \ register2 ->
	getNewRegNCG IntRep	`thenUs` \ tmp2 ->
	let
	    code1   = registerCode register1 tmp1 asmVoid
	    src1    = registerName register1 tmp1
	    code2   = registerCode register2 tmp2 asmVoid
	    src2    = registerName register2 tmp2
	    code__2 = asmParThen [code1, code2] .
		      if src2 == ecx || src2 == esi
		      then mkSeqInstrs [ 
                              MOV L (OpReg src1) (OpReg eax),
			      CLTD,
			      IDIV sz (OpReg src2)
                           ]
		      else mkSeqInstrs [ -- we put src2 in (ebx)
			      MOV L (OpReg src2) 
                                    (OpAddr (AddrBaseIndex (Just ebx) Nothing 
                                                           (ImmInt OFFSET_R1))),
			      MOV L (OpReg src1) (OpReg eax),
			      CLTD,
			      IDIV sz (OpAddr (AddrBaseIndex (Just ebx) Nothing 
                                                             (ImmInt OFFSET_R1)))
                           ]
	in
	returnUs (Fixed IntRep (if is_division then eax else edx) code__2)
	-----------------------

getRegister (StInd pk mem)
  = getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code = amodeCode amode
    	src  = amodeAddr amode
    	size = primRepToSize pk
    	code__2 dst = code .
		      if pk == DoubleRep || pk == FloatRep
		      then mkSeqInstr (GLD size src dst)
		      else mkSeqInstr (MOV size (OpAddr src) (OpReg dst))
    in
    	returnUs (Any pk code__2)

getRegister (StInt i)
  = let
    	src = ImmInt (fromInteger i)
    	code dst = mkSeqInstr (MOV L (OpImm src) (OpReg dst))
    in
    	returnUs (Any IntRep code)

getRegister leaf
  | maybeToBool imm
  = let
    	code dst = mkSeqInstr (MOV L (OpImm imm__2) (OpReg dst))
    in
    	returnUs (Any PtrRep code)
  | otherwise
  = pprPanic "getRegister(x86)" (pprStixTrees [leaf])

  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

getRegister (StDouble d)
  = getUniqLabelNCG 	    	    `thenUs` \ lbl ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let code dst = mkSeqInstrs [
    	    SEGMENT DataSegment,
	    LABEL lbl,
	    DATA DF [ImmDouble d],
	    SEGMENT TextSegment,
	    SETHI (HI (ImmCLbl lbl)) tmp,
	    LD DF (AddrRegImm tmp (LO (ImmCLbl lbl))) dst]
    in
    	returnUs (Any DoubleRep code)

getRegister (StPrim primop [x]) -- unary PrimOps
  = case primop of
      IntNegOp -> trivialUCode (SUB False False g0) x
      NotOp    -> trivialUCode (XNOR False g0) x

      FloatNegOp  -> trivialUFCode FloatRep (FNEG F) x

      DoubleNegOp -> trivialUFCode DoubleRep (FNEG DF) x

      Double2FloatOp -> trivialUFCode FloatRep  (FxTOy DF F) x
      Float2DoubleOp -> trivialUFCode DoubleRep (FxTOy F DF) x

      OrdOp -> coerceIntCode IntRep x
      ChrOp -> chrCode x

      Float2IntOp  -> coerceFP2Int x
      Int2FloatOp  -> coerceInt2FP FloatRep x
      Double2IntOp -> coerceFP2Int x
      Int2DoubleOp -> coerceInt2FP DoubleRep x

      other_op ->
        let
	    fixed_x = if is_float_op  -- promote to double
			  then StPrim Float2DoubleOp [x]
			  else x
	in
	getRegister (StCall fn cCallConv DoubleRep [x])
       where
	(is_float_op, fn)
	  = case primop of
	      FloatExpOp    -> (True,  SLIT("exp"))
	      FloatLogOp    -> (True,  SLIT("log"))
	      FloatSqrtOp   -> (True,  SLIT("sqrt"))

	      FloatSinOp    -> (True,  SLIT("sin"))
	      FloatCosOp    -> (True,  SLIT("cos"))
	      FloatTanOp    -> (True,  SLIT("tan"))

	      FloatAsinOp   -> (True,  SLIT("asin"))
	      FloatAcosOp   -> (True,  SLIT("acos"))
	      FloatAtanOp   -> (True,  SLIT("atan"))

	      FloatSinhOp   -> (True,  SLIT("sinh"))
	      FloatCoshOp   -> (True,  SLIT("cosh"))
	      FloatTanhOp   -> (True,  SLIT("tanh"))

	      DoubleExpOp   -> (False, SLIT("exp"))
	      DoubleLogOp   -> (False, SLIT("log"))
	      DoubleSqrtOp  -> (True,  SLIT("sqrt"))

	      DoubleSinOp   -> (False, SLIT("sin"))
	      DoubleCosOp   -> (False, SLIT("cos"))
	      DoubleTanOp   -> (False, SLIT("tan"))

	      DoubleAsinOp  -> (False, SLIT("asin"))
	      DoubleAcosOp  -> (False, SLIT("acos"))
	      DoubleAtanOp  -> (False, SLIT("atan"))

	      DoubleSinhOp  -> (False, SLIT("sinh"))
	      DoubleCoshOp  -> (False, SLIT("cosh"))
	      DoubleTanhOp  -> (False, SLIT("tanh"))
	      _             -> panic ("Monadic PrimOp not handled: " ++ show primop)

getRegister (StPrim primop [x, y]) -- dyadic PrimOps
  = case primop of
      CharGtOp -> condIntReg GTT x y
      CharGeOp -> condIntReg GE x y
      CharEqOp -> condIntReg EQQ x y
      CharNeOp -> condIntReg NE x y
      CharLtOp -> condIntReg LTT x y
      CharLeOp -> condIntReg LE x y

      IntGtOp  -> condIntReg GTT x y
      IntGeOp  -> condIntReg GE x y
      IntEqOp  -> condIntReg EQQ x y
      IntNeOp  -> condIntReg NE x y
      IntLtOp  -> condIntReg LTT x y
      IntLeOp  -> condIntReg LE x y

      WordGtOp -> condIntReg GU  x y
      WordGeOp -> condIntReg GEU x y
      WordEqOp -> condIntReg EQQ  x y
      WordNeOp -> condIntReg NE  x y
      WordLtOp -> condIntReg LU  x y
      WordLeOp -> condIntReg LEU x y

      AddrGtOp -> condIntReg GU  x y
      AddrGeOp -> condIntReg GEU x y
      AddrEqOp -> condIntReg EQQ  x y
      AddrNeOp -> condIntReg NE  x y
      AddrLtOp -> condIntReg LU  x y
      AddrLeOp -> condIntReg LEU x y

      FloatGtOp -> condFltReg GTT x y
      FloatGeOp -> condFltReg GE x y
      FloatEqOp -> condFltReg EQQ x y
      FloatNeOp -> condFltReg NE x y
      FloatLtOp -> condFltReg LTT x y
      FloatLeOp -> condFltReg LE x y

      DoubleGtOp -> condFltReg GTT x y
      DoubleGeOp -> condFltReg GE x y
      DoubleEqOp -> condFltReg EQQ x y
      DoubleNeOp -> condFltReg NE x y
      DoubleLtOp -> condFltReg LTT x y
      DoubleLeOp -> condFltReg LE x y

      IntAddOp -> trivialCode (ADD False False) x y
      IntSubOp -> trivialCode (SUB False False) x y

	-- ToDo: teach about V8+ SPARC mul/div instructions
      IntMulOp    -> imul_div SLIT(".umul") x y
      IntQuotOp   -> imul_div SLIT(".div")  x y
      IntRemOp    -> imul_div SLIT(".rem")  x y

      FloatAddOp  -> trivialFCode FloatRep  FADD x y
      FloatSubOp  -> trivialFCode FloatRep  FSUB x y
      FloatMulOp  -> trivialFCode FloatRep  FMUL x y
      FloatDivOp  -> trivialFCode FloatRep  FDIV x y

      DoubleAddOp -> trivialFCode DoubleRep FADD x y
      DoubleSubOp -> trivialFCode DoubleRep FSUB x y
      DoubleMulOp -> trivialFCode DoubleRep FMUL x y
      DoubleDivOp -> trivialFCode DoubleRep FDIV x y

      AndOp -> trivialCode (AND False) x y
      OrOp  -> trivialCode (OR  False) x y
      XorOp -> trivialCode (XOR False) x y
      SllOp -> trivialCode SLL x y
      SrlOp -> trivialCode SRL x y

      ISllOp -> trivialCode SLL x y  --was: panic "SparcGen:isll"
      ISraOp -> trivialCode SRA x y  --was: panic "SparcGen:isra"
      ISrlOp -> trivialCode SRL x y  --was: panic "SparcGen:isrl"

      FloatPowerOp  -> getRegister (StCall SLIT("pow") cCallConv DoubleRep [promote x, promote y])
		       where promote x = StPrim Float2DoubleOp [x]
      DoublePowerOp -> getRegister (StCall SLIT("pow") cCallConv DoubleRep [x, y])
--      _ -> panic "Prim op " ++ (showPrimOp primop) ++ " not handled!"
  where
    imul_div fn x y = getRegister (StCall fn cCallConv IntRep [x, y])

getRegister (StInd pk mem)
  = getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = primRepToSize pk
    	code__2 dst = code . mkSeqInstr (LD size src dst)
    in
    	returnUs (Any pk code__2)

getRegister (StInt i)
  | fits13Bits i
  = let
    	src = ImmInt (fromInteger i)
    	code dst = mkSeqInstr (OR False g0 (RIImm src) dst)
    in
    	returnUs (Any IntRep code)

getRegister leaf
  | maybeToBool imm
  = let
    	code dst = mkSeqInstrs [
    	    SETHI (HI imm__2) dst,
    	    OR False dst (RIImm (LO imm__2)) dst]
    in
    	returnUs (Any PtrRep code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{The @Amode@ type}
%*									*
%************************************************************************

@Amode@s: Memory addressing modes passed up the tree.
\begin{code}
data Amode = Amode MachRegsAddr InstrBlock

amodeAddr (Amode addr _) = addr
amodeCode (Amode _ code) = code
\end{code}

Now, given a tree (the argument to an StInd) that references memory,
produce a suitable addressing mode.

\begin{code}
getAmode :: StixTree -> UniqSM Amode

getAmode tree@(StIndex _ _ _) = getAmode (mangleIndexTree tree)

#if alpha_TARGET_ARCH

getAmode (StPrim IntSubOp [x, StInt i])
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    returnUs (Amode (AddrRegImm reg off) code)

getAmode (StPrim IntAddOp [x, StInt i])
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    returnUs (Amode (AddrRegImm reg off) code)

getAmode leaf
  | maybeToBool imm
  = returnUs (Amode (AddrImm imm__2) id)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister other		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    in
    returnUs (Amode (AddrReg reg) code)

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

getAmode (StPrim IntSubOp [x, StInt i])
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    returnUs (Amode (AddrBaseIndex (Just reg) Nothing off) code)

getAmode (StPrim IntAddOp [x, StInt i])
  | maybeToBool imm
  = let
	code = mkSeqInstrs []
    in
    returnUs (Amode (ImmAddr imm__2 (fromInteger i)) code)
  where
    imm    = maybeImm x
    imm__2 = case imm of Just x -> x

getAmode (StPrim IntAddOp [x, StInt i])
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    returnUs (Amode (AddrBaseIndex (Just reg) Nothing off) code)

getAmode (StPrim IntAddOp [x, y])
  = getNewRegNCG PtrRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep    	`thenUs` \ tmp2 ->
    getRegister x    	    	`thenUs` \ register1 ->
    getRegister y    	    	`thenUs` \ register2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	reg1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	reg2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2]
    in
    returnUs (Amode (AddrBaseIndex (Just reg1) (Just (reg2,4)) (ImmInt 0)) code__2)

getAmode leaf
  | maybeToBool imm
  = let
	code = mkSeqInstrs []
    in
    returnUs (Amode (ImmAddr imm__2 0) code)
  where
    imm    = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister other		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = Nothing
    in
    returnUs (Amode (AddrBaseIndex (Just reg) Nothing (ImmInt 0)) code)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

getAmode (StPrim IntSubOp [x, StInt i])
  | fits13Bits (-i)
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    returnUs (Amode (AddrRegImm reg off) code)


getAmode (StPrim IntAddOp [x, StInt i])
  | fits13Bits i
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister x		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    returnUs (Amode (AddrRegImm reg off) code)

getAmode (StPrim IntAddOp [x, y])
  = getNewRegNCG PtrRep    	`thenUs` \ tmp1 ->
    getNewRegNCG IntRep    	`thenUs` \ tmp2 ->
    getRegister x    	    	`thenUs` \ register1 ->
    getRegister y    	    	`thenUs` \ register2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	reg1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	reg2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2]
    in
    returnUs (Amode (AddrRegReg reg1 reg2) code__2)

getAmode leaf
  | maybeToBool imm
  = getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	code = mkSeqInstr (SETHI (HI imm__2) tmp)
    in
    returnUs (Amode (AddrRegImm tmp (LO imm__2)) code)
  where
    imm    = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other
  = getNewRegNCG PtrRep		`thenUs` \ tmp ->
    getRegister other		`thenUs` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt 0
    in
    returnUs (Amode (AddrRegImm reg off) code)

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{The @CondCode@ type}
%*									*
%************************************************************************

Condition codes passed up the tree.
\begin{code}
data CondCode = CondCode Bool Cond InstrBlock

condName  (CondCode _ cond _)	   = cond
condFloat (CondCode is_float _ _) = is_float
condCode  (CondCode _ _ code)	   = code
\end{code}

Set up a condition code for a conditional branch.

\begin{code}
getCondCode :: StixTree -> UniqSM CondCode

#if alpha_TARGET_ARCH
getCondCode = panic "MachCode.getCondCode: not on Alphas"
#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#if i386_TARGET_ARCH || sparc_TARGET_ARCH
-- yes, they really do seem to want exactly the same!

getCondCode (StPrim primop [x, y])
  = case primop of
      CharGtOp -> condIntCode GTT  x y
      CharGeOp -> condIntCode GE  x y
      CharEqOp -> condIntCode EQQ  x y
      CharNeOp -> condIntCode NE  x y
      CharLtOp -> condIntCode LTT  x y
      CharLeOp -> condIntCode LE  x y
 
      IntGtOp  -> condIntCode GTT  x y
      IntGeOp  -> condIntCode GE  x y
      IntEqOp  -> condIntCode EQQ  x y
      IntNeOp  -> condIntCode NE  x y
      IntLtOp  -> condIntCode LTT  x y
      IntLeOp  -> condIntCode LE  x y

      WordGtOp -> condIntCode GU  x y
      WordGeOp -> condIntCode GEU x y
      WordEqOp -> condIntCode EQQ  x y
      WordNeOp -> condIntCode NE  x y
      WordLtOp -> condIntCode LU  x y
      WordLeOp -> condIntCode LEU x y

      AddrGtOp -> condIntCode GU  x y
      AddrGeOp -> condIntCode GEU x y
      AddrEqOp -> condIntCode EQQ  x y
      AddrNeOp -> condIntCode NE  x y
      AddrLtOp -> condIntCode LU  x y
      AddrLeOp -> condIntCode LEU x y

      FloatGtOp -> condFltCode GTT x y
      FloatGeOp -> condFltCode GE x y
      FloatEqOp -> condFltCode EQQ x y
      FloatNeOp -> condFltCode NE x y
      FloatLtOp -> condFltCode LTT x y
      FloatLeOp -> condFltCode LE x y

      DoubleGtOp -> condFltCode GTT x y
      DoubleGeOp -> condFltCode GE x y
      DoubleEqOp -> condFltCode EQQ x y
      DoubleNeOp -> condFltCode NE x y
      DoubleLtOp -> condFltCode LTT x y
      DoubleLeOp -> condFltCode LE x y

#endif {- i386_TARGET_ARCH || sparc_TARGET_ARCH -}
\end{code}

% -----------------

@cond(Int|Flt)Code@: Turn a boolean expression into a condition, to be
passed back up the tree.

\begin{code}
condIntCode, condFltCode :: Cond -> StixTree -> StixTree -> UniqSM CondCode

#if alpha_TARGET_ARCH
condIntCode = panic "MachCode.condIntCode: not on Alphas"
condFltCode = panic "MachCode.condFltCode: not on Alphas"
#endif {- alpha_TARGET_ARCH -}

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

condIntCode cond (StInd _ x) y
  | maybeToBool imm
  = getAmode x			`thenUs` \ amode ->
    let
    	code1 = amodeCode amode asmVoid
    	y__2  = amodeAddr amode
    	code__2 = asmParThen [code1] .
    	    	  mkSeqInstr (CMP L (OpImm imm__2) (OpAddr y__2))
    in
    returnUs (CondCode False cond code__2)
  where
    imm    = maybeImm y
    imm__2 = case imm of Just x -> x

condIntCode cond x (StInt 0)
  = getRegister x		`thenUs` \ register1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code__2 = asmParThen [code1] .
    	    	mkSeqInstr (TEST L (OpReg src1) (OpReg src1))
    in
    returnUs (CondCode False cond code__2)

condIntCode cond x y
  | maybeToBool imm
  = getRegister x		`thenUs` \ register1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code__2 = asmParThen [code1] .
    	    	mkSeqInstr (CMP L (OpImm imm__2) (OpReg src1))
    in
    returnUs (CondCode False cond code__2)
  where
    imm    = maybeImm y
    imm__2 = case imm of Just x -> x

condIntCode cond (StInd _ x) y
  = getAmode x			`thenUs` \ amode ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
    	code1 = amodeCode amode asmVoid
    	src1  = amodeAddr amode
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2] .
    	    	  mkSeqInstr (CMP L (OpReg src2) (OpAddr src1))
    in
    returnUs (CondCode False cond code__2)

condIntCode cond y (StInd _ x)
  = getAmode x			`thenUs` \ amode ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
    	code1 = amodeCode amode asmVoid
    	src1  = amodeAddr amode
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
    	code__2 = asmParThen [code1, code2] .
    	    	  mkSeqInstr (CMP L (OpAddr src1) (OpReg src2))
    in
    returnUs (CondCode False cond code__2)

condIntCode cond x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
	code__2 = asmParThen [code1, code2] .
    	    	mkSeqInstr (CMP L (OpReg src2) (OpReg src1))
    in
    returnUs (CondCode False cond code__2)

-----------
condFltCode cond x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG (registerRep register1)
      	    	        	`thenUs` \ tmp1 ->
    getNewRegNCG (registerRep register2)
     	    	        	`thenUs` \ tmp2 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp ->
    let
    	pk1   = registerRep register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	pk2   = registerRep register2
    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 =   asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	    mkSeqInstr (GCMP (primRepToSize pk1) src1 src2)

        {- On the 486, the flags set by FP compare are the unsigned ones!
           (This looks like a HACK to me.  WDP 96/03)
        -}
        fix_FP_cond :: Cond -> Cond

        fix_FP_cond GE  = GEU
        fix_FP_cond GTT  = GU
        fix_FP_cond LTT  = LU
        fix_FP_cond LE  = LEU
        fix_FP_cond any = any
    in
    returnUs (CondCode True (fix_FP_cond cond) code__2)



#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

condIntCode cond x (StInt y)
  | fits13Bits y
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
	code = registerCode register tmp
	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
	code__2 = code . mkSeqInstr (SUB False True src1 (RIImm src2) g0)
    in
    returnUs (CondCode False cond code__2)

condIntCode cond x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
	code1 = registerCode register1 tmp1 asmVoid
	src1  = registerName register1 tmp1
	code2 = registerCode register2 tmp2 asmVoid
	src2  = registerName register2 tmp2
	code__2 = asmParThen [code1, code2] .
    	    	mkSeqInstr (SUB False True src1 (RIReg src2) g0)
    in
    returnUs (CondCode False cond code__2)

-----------
condFltCode cond x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG (registerRep register1)
      	    	        	`thenUs` \ tmp1 ->
    getNewRegNCG (registerRep register2)
     	    	        	`thenUs` \ tmp2 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp ->
    let
    	promote x = asmInstr (FxTOy F DF x tmp)

    	pk1   = registerRep register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	pk2   = registerRep register2
    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 =
		if pk1 == pk2 then
    	            asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	    mkSeqInstr (FCMP True (primRepToSize pk1) src1 src2)
    	    	else if pk1 == FloatRep then
    	    	    asmParThen [code1 (promote src1), code2 asmVoid] .
    	    	    mkSeqInstr (FCMP True DF tmp src2)
    	    	else
    	    	    asmParThen [code1 asmVoid, code2 (promote src2)] .
    	    	    mkSeqInstr (FCMP True DF src1 tmp)
    in
    returnUs (CondCode True cond code__2)

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{Generating assignments}
%*									*
%************************************************************************

Assignments are really at the heart of the whole code generation
business.  Almost all top-level nodes of any real importance are
assignments, which correspond to loads, stores, or register transfers.
If we're really lucky, some of the register transfers will go away,
because we can use the destination register to complete the code
generation for the right hand side.  This only fails when the right
hand side is forced into a fixed register (e.g. the result of a call).

\begin{code}
assignIntCode, assignFltCode
	:: PrimRep -> StixTree -> StixTree -> UniqSM InstrBlock

#if alpha_TARGET_ARCH

assignIntCode pk (StInd _ dst) src
  = getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getRegister src	     	    `thenUs` \ register ->
    let
    	code1   = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2   = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    returnUs code__2

assignIntCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    let
    	dst__2  = registerName register1 zeroh
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2
		  then code . mkSeqInstr (OR src__2 (RIReg src__2) dst__2)
    	    	  else code
    in
    returnUs code__2

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

assignIntCode pk (StInd _ dst) src
  = getAmode dst		`thenUs` \ amode ->
    get_op_RI src		`thenUs` \ (codesrc, opsrc, sz) ->
    let
    	code1   = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code__2 = asmParThen [code1, codesrc asmVoid] .
		  mkSeqInstr (MOV sz opsrc (OpAddr dst__2))
    in
    returnUs code__2
  where
    get_op_RI
	:: StixTree
	-> UniqSM (InstrBlock,Operand, Size)	-- code, operator, size

    get_op_RI op
      | maybeToBool imm
      = returnUs (asmParThen [], OpImm imm_op, L)
      where
	imm    = maybeImm op
	imm_op = case imm of Just x -> x

    get_op_RI op
      = getRegister op			`thenUs` \ register ->
	getNewRegNCG (registerRep register)
					`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    reg  = registerName register tmp
	    pk   = registerRep  register
	    sz   = primRepToSize pk
	in
	returnUs (code, OpReg reg, sz)

assignIntCode pk dst (StInd _ src)
  = getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    getAmode src    	    	    `thenUs` \ amode ->
    getRegister dst	    	    	    `thenUs` \ register ->
    let
    	code1   = amodeCode amode asmVoid
    	src__2  = amodeAddr amode
    	code2   = registerCode register tmp asmVoid
    	dst__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmParThen [code1, code2] .
		  mkSeqInstr (MOV sz (OpAddr src__2) (OpReg dst__2))
    in
    returnUs code__2

assignIntCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    let
    	dst__2  = registerName register1 tmp
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2 && dst__2 /= src__2
    	    	  then code . mkSeqInstr (MOV L (OpReg src__2) (OpReg dst__2))
    	    	  else code
    in
    returnUs code__2

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

assignIntCode pk (StInd _ dst) src
  = getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getRegister src	    	    	    `thenUs` \ register ->
    let
    	code1   = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2   = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    returnUs code__2

assignIntCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    let
    	dst__2  = registerName register1 g0
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2
		  then code . mkSeqInstr (OR False g0 (RIReg src__2) dst__2)
    	    	  else code
    in
    returnUs code__2

#endif {- sparc_TARGET_ARCH -}
\end{code}

% --------------------------------
Floating-point assignments:
% --------------------------------
\begin{code}
#if alpha_TARGET_ARCH

assignFltCode pk (StInd _ dst) src
  = getNewRegNCG pk        	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getRegister src	    	    	    `thenUs` \ register ->
    let
    	code1   = amodeCode amode asmVoid
    	dst__2  = amodeAddr amode
    	code2   = registerCode register tmp asmVoid
    	src__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmParThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    returnUs code__2

assignFltCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    let
    	dst__2  = registerName register1 zeroh
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2
		  then code . mkSeqInstr (FMOV src__2 dst__2)
		  else code
    in
    returnUs code__2

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

assignFltCode pk (StInd pk_dst dst) (StInd pk_src src)
  = getNewRegNCG IntRep       	    `thenUs` \ tmp ->
    getAmode src    	    	    `thenUs` \ amodesrc ->
    getAmode dst    	    	    `thenUs` \ amodedst ->
    let
    	codesrc1 = amodeCode amodesrc asmVoid
    	addrsrc1 = amodeAddr amodesrc
    	codedst1 = amodeCode amodedst asmVoid
    	addrdst1 = amodeAddr amodedst
    	addrsrc2 = case (addrOffset addrsrc1 4) of Just x -> x
    	addrdst2 = case (addrOffset addrdst1 4) of Just x -> x

    	code__2 = asmParThen [codesrc1, codedst1] .
		  mkSeqInstrs ([MOV L (OpAddr addrsrc1) (OpReg tmp),
				MOV L (OpReg tmp) (OpAddr addrdst1)]
			       ++
			       if pk == DoubleRep
			       then [MOV L (OpAddr addrsrc2) (OpReg tmp),
				     MOV L (OpReg tmp) (OpAddr addrdst2)]
			       else [])
    in
    returnUs code__2

assignFltCode pk (StInd _ dst) src
  = getNewRegNCG pk        	    `thenUs` \ tmp ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getRegister src	    	    `thenUs` \ register ->
    let
    	sz      = primRepToSize pk
    	dst__2  = amodeAddr amode

    	code1   = amodeCode amode asmVoid
    	code2   = registerCode register tmp asmVoid

    	src__2  = registerName register tmp

    	code__2 = asmParThen [code1, code2] .
		  mkSeqInstr (GST sz src__2 dst__2)
    in
    returnUs code__2

assignFltCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    getNewRegNCG pk                         `thenUs` \ tmp ->
    let
        -- the register which is dst
    	dst__2  = registerName register1 tmp
        -- the register into which src is computed, preferably dst__2
    	src__2  = registerName register2 dst__2
        -- code to compute src into src__2
    	code    = registerCode register2 dst__2

    	code__2 = if isFixed register2
                  then code . mkSeqInstr (GMOV src__2 dst__2)
                  else code
    in
    returnUs code__2

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

assignFltCode pk (StInd _ dst) src
  = getNewRegNCG pk        	    `thenUs` \ tmp1 ->
    getAmode dst    	    	    `thenUs` \ amode ->
    getRegister src	      	    `thenUs` \ register ->
    let
    	sz      = primRepToSize pk
    	dst__2  = amodeAddr amode

    	code1   = amodeCode amode asmVoid
    	code2   = registerCode register tmp1 asmVoid

    	src__2  = registerName register tmp1
    	pk__2   = registerRep register
    	sz__2   = primRepToSize pk__2

    	code__2 = asmParThen [code1, code2] .
	    if pk == pk__2 then
		    mkSeqInstr (ST sz src__2 dst__2)
	    else
		mkSeqInstrs [FxTOy sz__2 sz src__2 tmp1, ST sz tmp1 dst__2]
    in
    returnUs code__2

assignFltCode pk dst src
  = getRegister dst	    	    	    `thenUs` \ register1 ->
    getRegister src	    	    	    `thenUs` \ register2 ->
    let 
        pk__2   = registerRep register2 
        sz__2   = primRepToSize pk__2
    in
    getNewRegNCG pk__2                      `thenUs` \ tmp ->
    let
    	sz     	= primRepToSize pk
    	dst__2 	= registerName register1 g0    -- must be Fixed
 

    	reg__2 	= if pk /= pk__2 then tmp else dst__2
 
    	code   	= registerCode register2 reg__2

    	src__2 	= registerName register2 reg__2

	code__2 = 
	        if pk /= pk__2 then
		     code . mkSeqInstr (FxTOy sz__2 sz src__2 dst__2)
    	    	else if isFixed register2 then
		     code . mkSeqInstr (FMOV sz src__2 dst__2)
    	    	else
		     code
    in
    returnUs code__2

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{Generating an unconditional branch}
%*									*
%************************************************************************

We accept two types of targets: an immediate CLabel or a tree that
gets evaluated into a register.  Any CLabels which are AsmTemporaries
are assumed to be in the local block of code, close enough for a
branch instruction.  Other CLabels are assumed to be far away.

(If applicable) Do not fill the delay slots here; you will confuse the
register allocator.

\begin{code}
genJump :: StixTree{-the branch target-} -> UniqSM InstrBlock

#if alpha_TARGET_ARCH

genJump (StCLbl lbl)
  | isAsmTemp lbl = returnInstr (BR target)
  | otherwise     = returnInstrs [LDA pv (AddrImm target), JMP zeroh (AddrReg pv) 0]
  where
    target = ImmCLbl lbl

genJump tree
  = getRegister tree	    	    	    `thenUs` \ register ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	dst    = registerName register pv
    	code   = registerCode register pv
    	target = registerName register pv
    in
    if isFixed register then
	returnSeq code [OR dst (RIReg dst) pv, JMP zeroh (AddrReg pv) 0]
    else
    returnUs (code . mkSeqInstr (JMP zeroh (AddrReg pv) 0))

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

{-
genJump (StCLbl lbl)
  | isAsmTemp lbl = returnInstrs [JXX ALWAYS lbl]
  | otherwise     = returnInstrs [JMP (OpImm target)]
  where
    target = ImmCLbl lbl
-}

genJump (StInd pk mem)
  = getAmode mem    	    	    `thenUs` \ amode ->
    let
    	code   = amodeCode amode
    	target = amodeAddr amode
    in
    returnSeq code [JMP (OpAddr target)]

genJump tree
  | maybeToBool imm
  = returnInstr (JMP (OpImm target))

  | otherwise
  = getRegister tree	    	    	    `thenUs` \ register ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	target = registerName register tmp
    in
    returnSeq code [JMP (OpReg target)]
  where
    imm    = maybeImm tree
    target = case imm of Just x -> x

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

genJump (StCLbl lbl)
  | isAsmTemp lbl = returnInstrs [BI ALWAYS False target, NOP]
  | otherwise     = returnInstrs [CALL target 0 True, NOP]
  where
    target = ImmCLbl lbl

genJump tree
  = getRegister tree	    	    	    `thenUs` \ register ->
    getNewRegNCG PtrRep    	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	target = registerName register tmp
    in
    returnSeq code [JMP (AddrRegReg target g0), NOP]

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{Conditional jumps}
%*									*
%************************************************************************

Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

ALPHA: For comparisons with 0, we're laughing, because we can just do
the desired conditional branch.

I386: First, we have to ensure that the condition
codes are set according to the supplied comparison operation.

SPARC: First, we have to ensure that the condition codes are set
according to the supplied comparison operation.  We generate slightly
different code for floating point comparisons, because a floating
point operation cannot directly precede a @BF@.  We assume the worst
and fill that slot with a @NOP@.

SPARC: Do not fill the delay slots here; you will confuse the register
allocator.

\begin{code}
genCondJump
    :: CLabel	    -- the branch target
    -> StixTree     -- the condition on which to branch
    -> UniqSM InstrBlock

#if alpha_TARGET_ARCH

genCondJump lbl (StPrim op [x, StInt 0])
  = getRegister x	  	    	    `thenUs` \ register ->
    getNewRegNCG (registerRep register)
    	    	        	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	value  = registerName register tmp
    	pk     = registerRep register
	target = ImmCLbl lbl
    in
    returnSeq code [BI (cmpOp op) value target]
  where
    cmpOp CharGtOp = GTT
    cmpOp CharGeOp = GE
    cmpOp CharEqOp = EQQ
    cmpOp CharNeOp = NE
    cmpOp CharLtOp = LTT
    cmpOp CharLeOp = LE
    cmpOp IntGtOp = GTT
    cmpOp IntGeOp = GE
    cmpOp IntEqOp = EQQ
    cmpOp IntNeOp = NE
    cmpOp IntLtOp = LTT
    cmpOp IntLeOp = LE
    cmpOp WordGtOp = NE
    cmpOp WordGeOp = ALWAYS
    cmpOp WordEqOp = EQQ
    cmpOp WordNeOp = NE
    cmpOp WordLtOp = NEVER
    cmpOp WordLeOp = EQQ
    cmpOp AddrGtOp = NE
    cmpOp AddrGeOp = ALWAYS
    cmpOp AddrEqOp = EQQ
    cmpOp AddrNeOp = NE
    cmpOp AddrLtOp = NEVER
    cmpOp AddrLeOp = EQQ

genCondJump lbl (StPrim op [x, StDouble 0.0])
  = getRegister x	  	    	    `thenUs` \ register ->
    getNewRegNCG (registerRep register)
    	    	        	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	value  = registerName register tmp
    	pk     = registerRep register
	target = ImmCLbl lbl
    in
    returnUs (code . mkSeqInstr (BF (cmpOp op) value target))
  where
    cmpOp FloatGtOp = GTT
    cmpOp FloatGeOp = GE
    cmpOp FloatEqOp = EQQ
    cmpOp FloatNeOp = NE
    cmpOp FloatLtOp = LTT
    cmpOp FloatLeOp = LE
    cmpOp DoubleGtOp = GTT
    cmpOp DoubleGeOp = GE
    cmpOp DoubleEqOp = EQQ
    cmpOp DoubleNeOp = NE
    cmpOp DoubleLtOp = LTT
    cmpOp DoubleLeOp = LE

genCondJump lbl (StPrim op [x, y])
  | fltCmpOp op
  = trivialFCode pr instr x y 	    `thenUs` \ register ->
    getNewRegNCG DoubleRep    	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	result = registerName register tmp
	target = ImmCLbl lbl
    in
    returnUs (code . mkSeqInstr (BF cond result target))
  where
    pr = panic "trivialU?FCode: does not use PrimRep on Alpha"

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
	FloatGtOp -> (FCMP TF LE, EQQ)
	FloatGeOp -> (FCMP TF LTT, EQQ)
	FloatEqOp -> (FCMP TF EQQ, NE)
	FloatNeOp -> (FCMP TF EQQ, EQQ)
	FloatLtOp -> (FCMP TF LTT, NE)
	FloatLeOp -> (FCMP TF LE, NE)
	DoubleGtOp -> (FCMP TF LE, EQQ)
	DoubleGeOp -> (FCMP TF LTT, EQQ)
	DoubleEqOp -> (FCMP TF EQQ, NE)
	DoubleNeOp -> (FCMP TF EQQ, EQQ)
	DoubleLtOp -> (FCMP TF LTT, NE)
	DoubleLeOp -> (FCMP TF LE, NE)

genCondJump lbl (StPrim op [x, y])
  = trivialCode instr x y    	    `thenUs` \ register ->
    getNewRegNCG IntRep    	    `thenUs` \ tmp ->
    let
    	code   = registerCode register tmp
    	result = registerName register tmp
	target = ImmCLbl lbl
    in
    returnUs (code . mkSeqInstr (BI cond result target))
  where
    (instr, cond) = case op of
	CharGtOp -> (CMP LE, EQQ)
	CharGeOp -> (CMP LTT, EQQ)
	CharEqOp -> (CMP EQQ, NE)
	CharNeOp -> (CMP EQQ, EQQ)
	CharLtOp -> (CMP LTT, NE)
	CharLeOp -> (CMP LE, NE)
	IntGtOp -> (CMP LE, EQQ)
	IntGeOp -> (CMP LTT, EQQ)
	IntEqOp -> (CMP EQQ, NE)
	IntNeOp -> (CMP EQQ, EQQ)
	IntLtOp -> (CMP LTT, NE)
	IntLeOp -> (CMP LE, NE)
	WordGtOp -> (CMP ULE, EQQ)
	WordGeOp -> (CMP ULT, EQQ)
	WordEqOp -> (CMP EQQ, NE)
	WordNeOp -> (CMP EQQ, EQQ)
	WordLtOp -> (CMP ULT, NE)
	WordLeOp -> (CMP ULE, NE)
	AddrGtOp -> (CMP ULE, EQQ)
	AddrGeOp -> (CMP ULT, EQQ)
	AddrEqOp -> (CMP EQQ, NE)
	AddrNeOp -> (CMP EQQ, EQQ)
	AddrLtOp -> (CMP ULT, NE)
	AddrLeOp -> (CMP ULE, NE)

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

genCondJump lbl bool
  = getCondCode bool  	    	    `thenUs` \ condition ->
    let
    	code   = condCode condition
    	cond   = condName condition
	target = ImmCLbl lbl
    in
    returnSeq code [JXX cond lbl]

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

genCondJump lbl bool
  = getCondCode bool  	    	    `thenUs` \ condition ->
    let
    	code   = condCode condition
    	cond   = condName condition
	target = ImmCLbl lbl
    in
    returnSeq code (
    if condFloat condition then
	[NOP, BF cond False target, NOP]
    else
	[BI cond False target, NOP]
    )

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{Generating C calls}
%*									*
%************************************************************************

Now the biggest nightmare---calls.  Most of the nastiness is buried in
@get_arg@, which moves the arguments to the correct registers/stack
locations.  Apart from that, the code is easy.

(If applicable) Do not fill the delay slots here; you will confuse the
register allocator.

\begin{code}
genCCall
    :: FAST_STRING	-- function to call
    -> CallConv
    -> PrimRep		-- type of the result
    -> [StixTree]	-- arguments (of mixed type)
    -> UniqSM InstrBlock

#if alpha_TARGET_ARCH

genCCall fn cconv kind args
  = mapAccumLUs get_arg (allArgRegs, eXTRA_STK_ARGS_HERE) args
    	    	    	    	    `thenUs` \ ((unused,_), argCode) ->
    let
    	nRegs = length allArgRegs - length unused
    	code = asmParThen (map ($ asmVoid) argCode)
    in
    	returnSeq code [
    	    LDA pv (AddrImm (ImmLab (ptext fn))),
    	    JSR ra (AddrReg pv) nRegs,
    	    LDGP gp (AddrReg ra)]
  where
    ------------------------
    {-	Try to get a value into a specific register (or registers) for
	a call.  The first 6 arguments go into the appropriate
	argument register (separate registers for integer and floating
	point arguments, but used in lock-step), and the remaining
	arguments are dumped to the stack, beginning at 0(sp).  Our
	first argument is a pair of the list of remaining argument
	registers to be assigned for this call and the next stack
	offset to use for overflowing arguments.  This way,
	@get_Arg@ can be applied to all of a call's arguments using
	@mapAccumLUs@.
    -}
    get_arg
	:: ([(Reg,Reg)], Int)	-- Argument registers and stack offset (accumulator)
	-> StixTree		-- Current argument
	-> UniqSM (([(Reg,Reg)],Int), InstrBlock) -- Updated accumulator and code

    -- We have to use up all of our argument registers first...

    get_arg ((iDst,fDst):dsts, offset) arg
      = getRegister arg	    	    	    `thenUs` \ register ->
	let
	    reg  = if isFloatingRep pk then fDst else iDst
	    code = registerCode register reg
	    src  = registerName register reg
	    pk   = registerRep register
	in
	returnUs (
	    if isFloatingRep pk then
		((dsts, offset), if isFixed register then
		    code . mkSeqInstr (FMOV src fDst)
		    else code)
	    else
		((dsts, offset), if isFixed register then
		    code . mkSeqInstr (OR src (RIReg src) iDst)
		    else code))

    -- Once we have run out of argument registers, we move to the
    -- stack...

    get_arg ([], offset) arg
      = getRegister arg			`thenUs` \ register ->
	getNewRegNCG (registerRep register)
					`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    src  = registerName register tmp
	    pk   = registerRep register
	    sz   = primRepToSize pk
	in
	returnUs (([], offset + 1), code . mkSeqInstr (ST sz src (spRel offset)))

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

genCCall fn cconv kind [StInt i]
  | fn == SLIT ("PerformGC_wrapper")
  = let call = [MOV L (OpImm (ImmInt (fromInteger i))) (OpReg eax),
	        CALL (ImmLit (ptext (if   underscorePrefix 
                                     then (SLIT ("_PerformGC_wrapper"))
                                     else (SLIT ("PerformGC_wrapper")))))]
    in
    returnInstrs call


genCCall fn cconv kind args
  = get_call_args args `thenUs` \ (tot_arg_size, argCode) ->
    let
	code2 = asmParThen (map ($ asmVoid) argCode)
	call = [SUB L (OpImm (ImmInt tot_arg_size)) (OpReg esp),
                CALL fn__2 ,
		ADD L (OpImm (ImmInt tot_arg_size)) (OpReg esp)
               ]
    in
    returnSeq code2 call

  where
    -- function names that begin with '.' are assumed to be special
    -- internally generated names like '.mul,' which don't get an
    -- underscore prefix
    -- ToDo:needed (WDP 96/03) ???
    fn__2 = case (_HEAD_ fn) of
	      '.' -> ImmLit (ptext fn)
	      _   -> ImmLab (ptext fn)

    arg_size DF = 8
    arg_size F  = 8
    arg_size _  = 4

    ------------
    -- do get_call_arg on each arg, threading the total arg size along
    -- process the args right-to-left
    get_call_args :: [StixTree] -> UniqSM (Int, [InstrBlock])
    get_call_args args
       = f 0 args
         where
            f curr_sz [] 
               = returnUs (curr_sz, [])
            f curr_sz (arg:args)             
               = f curr_sz args          `thenUs` \ (new_sz, iblocks) ->
                 get_call_arg arg new_sz `thenUs` \ (new_sz2, iblock) ->
                 returnUs (new_sz2, iblock:iblocks)


    ------------
    get_call_arg :: StixTree{-current argument-}
                    -> Int{-running total of arg sizes seen so far-}
                    -> UniqSM (Int, InstrBlock)  -- updated tot argsz, code

    get_call_arg arg old_sz
      = get_op arg		`thenUs` \ (code, reg, sz) ->
        let new_sz = old_sz + arg_size sz
        in  if   (case sz of DF -> True; F -> True; _ -> False)
            then returnUs (new_sz,
                           code .
                           mkSeqInstr (GST DF reg
                                              (AddrBaseIndex (Just esp) 
                                                  Nothing (ImmInt (- new_sz))))
                          )
	    else returnUs (new_sz,
                           code . 
                           mkSeqInstr (MOV L (OpReg reg)
                                             (OpAddr 
                                                 (AddrBaseIndex (Just esp) 
                                                    Nothing (ImmInt (- new_sz)))))
                          )
    ------------
    get_op
	:: StixTree
	-> UniqSM (InstrBlock, Reg, Size) -- code, reg, size

    get_op op
      = getRegister op		`thenUs` \ register ->
	getNewRegNCG (registerRep register)
				`thenUs` \ tmp ->
	let
	    code = registerCode register tmp
	    reg  = registerName register tmp
	    pk   = registerRep  register
	    sz   = primRepToSize pk
	in
	returnUs (code, reg, sz)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

genCCall fn cconv kind args
  = mapAccumLUs get_arg (allArgRegs, eXTRA_STK_ARGS_HERE) args
    	    	    	    	    `thenUs` \ ((unused,_), argCode) ->
    let
    	nRegs = length allArgRegs - length unused
    	call = CALL fn__2 nRegs False
    	code = asmParThen (map ($ asmVoid) argCode)
    in
    	returnSeq code [call, NOP]
  where
    -- function names that begin with '.' are assumed to be special
    -- internally generated names like '.mul,' which don't get an
    -- underscore prefix
    -- ToDo:needed (WDP 96/03) ???
    fn__2 = case (_HEAD_ fn) of
	      '.' -> ImmLit (ptext fn)
	      _   -> ImmLab (ptext fn)

    ------------------------------------
    {-  Try to get a value into a specific register (or registers) for
	a call.  The SPARC calling convention is an absolute
	nightmare.  The first 6x32 bits of arguments are mapped into
	%o0 through %o5, and the remaining arguments are dumped to the
	stack, beginning at [%sp+92].  (Note that %o6 == %sp.)  Our
	first argument is a pair of the list of remaining argument
	registers to be assigned for this call and the next stack
	offset to use for overflowing arguments.  This way,
	@get_arg@ can be applied to all of a call's arguments using
	@mapAccumL@.
    -}
    get_arg
	:: ([Reg],Int)	-- Argument registers and stack offset (accumulator)
	-> StixTree	-- Current argument
	-> UniqSM (([Reg],Int), InstrBlock) -- Updated accumulator and code

    -- We have to use up all of our argument registers first...

    get_arg (dst:dsts, offset) arg
      = getRegister arg			`thenUs` \ register ->
	getNewRegNCG (registerRep register)
					`thenUs` \ tmp ->
	let
	    reg  = if isFloatingRep pk then tmp else dst
	    code = registerCode register reg
	    src  = registerName register reg
	    pk   = registerRep register
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

    -- Once we have run out of argument registers, we move to the
    -- stack...

    get_arg ([], offset) arg
      = getRegister arg			`thenUs` \ register ->
	getNewRegNCG (registerRep register)
					`thenUs` \ tmp ->
	let
	    code  = registerCode register tmp
	    src   = registerName register tmp
	    pk    = registerRep register
	    sz    = primRepToSize pk
	    words = if pk == DoubleRep then 2 else 1
	in
	returnUs (([], offset + words), code . mkSeqInstr (ST sz src (spRel offset)))

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsection{Support bits}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection{@condIntReg@ and @condFltReg@: condition codes into registers}
%*									*
%************************************************************************

Turn those condition codes into integers now (when they appear on
the right hand side of an assignment).

(If applicable) Do not fill the delay slots here; you will confuse the
register allocator.

\begin{code}
condIntReg, condFltReg :: Cond -> StixTree -> StixTree -> UniqSM Register

#if alpha_TARGET_ARCH
condIntReg = panic "MachCode.condIntReg (not on Alpha)"
condFltReg = panic "MachCode.condFltReg (not on Alpha)"
#endif {- alpha_TARGET_ARCH -}

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

condIntReg cond x y
  = condIntCode cond x y	`thenUs` \ condition ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    --getRegister dst		`thenUs` \ register ->
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
    returnUs (Any IntRep code__2)

condFltReg cond x y
  = getUniqLabelNCG		`thenUs` \ lbl1 ->
    getUniqLabelNCG	    	`thenUs` \ lbl2 ->
    condFltCode cond x y 	`thenUs` \ condition ->
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
    returnUs (Any IntRep code__2)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

condIntReg EQQ x (StInt 0)
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
	code = registerCode register tmp
	src  = registerName register tmp
	code__2 dst = code . mkSeqInstrs [
    	    SUB False True g0 (RIReg src) g0,
    	    SUB True False g0 (RIImm (ImmInt (-1))) dst]
    in
    returnUs (Any IntRep code__2)

condIntReg EQQ x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
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

condIntReg NE x (StInt 0)
  = getRegister x    	    	`thenUs` \ register ->
    getNewRegNCG IntRep   	`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstrs [
    	    SUB False True g0 (RIReg src) g0,
    	    ADD True False g0 (RIImm (ImmInt 0)) dst]
    in
    returnUs (Any IntRep code__2)

condIntReg NE x y
  = getRegister x	    	`thenUs` \ register1 ->
    getRegister y	    	`thenUs` \ register2 ->
    getNewRegNCG IntRep        	`thenUs` \ tmp1 ->
    getNewRegNCG IntRep        	`thenUs` \ tmp2 ->
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

condIntReg cond x y
  = getUniqLabelNCG		`thenUs` \ lbl1 ->
    getUniqLabelNCG	    	`thenUs` \ lbl2 ->
    condIntCode cond x y 	`thenUs` \ condition ->
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

condFltReg cond x y
  = getUniqLabelNCG		`thenUs` \ lbl1 ->
    getUniqLabelNCG	    	`thenUs` \ lbl2 ->
    condFltCode cond x y 	`thenUs` \ condition ->
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

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsubsection{@trivial*Code@: deal with trivial instructions}
%*									*
%************************************************************************

Trivial (dyadic: @trivialCode@, floating-point: @trivialFCode@, unary:
@trivialUCode@, unary fl-pt:@trivialUFCode@) instructions.  Only look
for constants on the right hand side, because that's where the generic
optimizer will have put them.

Similarly, for unary instructions, we don't have to worry about
matching an StInt as the argument, because genericOpt will already
have handled the constant-folding.

\begin{code}
trivialCode
    :: IF_ARCH_alpha((Reg -> RI -> Reg -> Instr)
      ,IF_ARCH_i386 ((Operand -> Operand -> Instr)
      ,IF_ARCH_sparc((Reg -> RI -> Reg -> Instr)
      ,)))
    -> StixTree -> StixTree -- the two arguments
    -> UniqSM Register

trivialFCode
    :: PrimRep
    -> IF_ARCH_alpha((Reg -> Reg -> Reg -> Instr)
      ,IF_ARCH_sparc((Size -> Reg -> Reg -> Reg -> Instr)
      ,IF_ARCH_i386 ((Size -> Reg -> Reg -> Reg -> Instr)
      ,)))
    -> StixTree -> StixTree -- the two arguments
    -> UniqSM Register

trivialUCode
    :: IF_ARCH_alpha((RI -> Reg -> Instr)
      ,IF_ARCH_i386 ((Operand -> Instr)
      ,IF_ARCH_sparc((RI -> Reg -> Instr)
      ,)))
    -> StixTree	-- the one argument
    -> UniqSM Register

trivialUFCode
    :: PrimRep
    -> IF_ARCH_alpha((Reg -> Reg -> Instr)
      ,IF_ARCH_i386 ((Reg -> Reg -> Instr)
      ,IF_ARCH_sparc((Reg -> Reg -> Instr)
      ,)))
    -> StixTree -- the one argument
    -> UniqSM Register

#if alpha_TARGET_ARCH

trivialCode instr x (StInt y)
  | fits8Bits y
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . mkSeqInstr (instr src1 (RIImm src2) dst)
    in
    returnUs (Any IntRep code__2)

trivialCode instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = asmParThen [code1, code2] .
    	    	     mkSeqInstr (instr src1 (RIReg src2) dst)
    in
    returnUs (Any IntRep code__2)

------------
trivialUCode instr x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr (RIReg src) dst)
    in
    returnUs (Any IntRep code__2)

------------
trivialFCode _ instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp1 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst = asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	      mkSeqInstr (instr src1 src2 dst)
    in
    returnUs (Any DoubleRep code__2)

trivialUFCode _ instr x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    returnUs (Any DoubleRep code__2)

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

trivialCode instr x y
  | maybeToBool imm
  = getRegister x		`thenUs` \ register1 ->
    let
    	code__2 dst = let code1 = registerCode register1 dst
    	                  src1  = registerName register1 dst
		      in code1 .
			 if isFixed register1 && src1 /= dst
			 then mkSeqInstrs [MOV L (OpReg src1) (OpReg dst),
					   instr (OpImm imm__2) (OpReg dst)]
			 else
				mkSeqInstrs [instr (OpImm imm__2) (OpReg src1)]
    in
    returnUs (Any IntRep code__2)
  where
    imm = maybeImm y
    imm__2 = case imm of Just x -> x

trivialCode instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
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
    returnUs (Any IntRep code__2)

-----------
trivialUCode instr x
  = getRegister x		`thenUs` \ register ->
    let
    	code__2 dst = let
    	                  code = registerCode register dst
		      	  src  = registerName register dst
		      in code . if isFixed register && dst /= src
				then mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
						  instr (OpReg dst)]
				else mkSeqInstr (instr (OpReg src))
    in
    returnUs (Any IntRep code__2)

-----------
trivialFCode pk instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp1 ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst = asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	      mkSeqInstr (instr (primRepToSize pk) src1 src2 dst)
    in
    returnUs (Any DoubleRep code__2)


-------------
trivialUFCode pk instr x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG pk		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    returnUs (Any pk code__2)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

trivialCode instr x (StInt y)
  | fits13Bits y
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . mkSeqInstr (instr src1 (RIImm src2) dst)
    in
    returnUs (Any IntRep code__2)

trivialCode instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG IntRep		`thenUs` \ tmp1 ->
    getNewRegNCG IntRep		`thenUs` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 asmVoid
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 asmVoid
    	src2  = registerName register2 tmp2
    	code__2 dst = asmParThen [code1, code2] .
    	    	     mkSeqInstr (instr src1 (RIReg src2) dst)
    in
    returnUs (Any IntRep code__2)

------------
trivialFCode pk instr x y
  = getRegister x		`thenUs` \ register1 ->
    getRegister y		`thenUs` \ register2 ->
    getNewRegNCG (registerRep register1)
      	    	        	`thenUs` \ tmp1 ->
    getNewRegNCG (registerRep register2)
     	    	        	`thenUs` \ tmp2 ->
    getNewRegNCG DoubleRep   	`thenUs` \ tmp ->
    let
    	promote x = asmInstr (FxTOy F DF x tmp)

    	pk1   = registerRep register1
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	pk2   = registerRep register2
    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst =
    	    	if pk1 == pk2 then
    	            asmParThen [code1 asmVoid, code2 asmVoid] .
    	    	    mkSeqInstr (instr (primRepToSize pk) src1 src2 dst)
    	    	else if pk1 == FloatRep then
    	    	    asmParThen [code1 (promote src1), code2 asmVoid] .
    	    	    mkSeqInstr (instr DF tmp src2 dst)
    	    	else
    	    	    asmParThen [code1 asmVoid, code2 (promote src2)] .
    	    	    mkSeqInstr (instr DF src1 tmp dst)
    in
    returnUs (Any (if pk1 == pk2 then pk1 else DoubleRep) code__2)

------------
trivialUCode instr x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr (RIReg src) dst)
    in
    returnUs (Any IntRep code__2)

-------------
trivialUFCode pk instr x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG pk		`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    returnUs (Any pk code__2)

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsubsection{Coercing to/from integer/floating-point...}
%*									*
%************************************************************************

@coerce(Int|Flt)Code@ are simple coercions that don't require any code
to be generated.  Here we just change the type on the Register passed
on up.  The code is machine-independent.

@coerce(Int2FP|FP2Int)@ are more complicated integer/float
conversions.  We have to store temporaries in memory to move
between the integer and the floating point register sets.

\begin{code}
coerceIntCode :: PrimRep -> StixTree -> UniqSM Register
coerceFltCode ::	    StixTree -> UniqSM Register

coerceInt2FP :: PrimRep -> StixTree -> UniqSM Register
coerceFP2Int :: 	   StixTree -> UniqSM Register

coerceIntCode pk x
  = getRegister x		`thenUs` \ register ->
    returnUs (
    case register of
    	Fixed _ reg code -> Fixed pk reg code
    	Any   _ code     -> Any   pk code
    )

-------------
coerceFltCode x
  = getRegister x		`thenUs` \ register ->
    returnUs (
    case register of
    	Fixed _ reg code -> Fixed DoubleRep reg code
    	Any   _ code     -> Any   DoubleRep code
    )
\end{code}

\begin{code}
#if alpha_TARGET_ARCH

coerceInt2FP _ x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
    	    ST Q src (spRel 0),
    	    LD TF dst (spRel 0),
    	    CVTxy Q TF dst dst]
    in
    returnUs (Any DoubleRep code__2)

-------------
coerceFP2Int x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp

    	code__2 dst = code . mkSeqInstrs [
    	    CVTxy TF Q src tmp,
    	    ST TF tmp (spRel 0),
    	    LD Q dst (spRel 0)]
    in
    returnUs (Any IntRep code__2)

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

coerceInt2FP pk x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
        opc  = case pk of FloatRep -> GITOF ; DoubleRep -> GITOD
        code__2 dst = code . 
                      mkSeqInstr (opc src dst)
    in
    returnUs (Any pk code__2)

------------
coerceFP2Int x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG DoubleRep	`thenUs` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	pk   = registerRep register

        opc  = case pk of FloatRep -> GFTOI ; DoubleRep -> GDTOI
        code__2 dst = code . 
                      mkSeqInstr (opc src dst)
    in
    returnUs (Any IntRep code__2)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

coerceInt2FP pk x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
    	    ST W src (spRel (-2)),
    	    LD W (spRel (-2)) dst,
    	    FxTOy W (primRepToSize pk) dst dst]
    in
    returnUs (Any pk code__2)

------------
coerceFP2Int x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    getNewRegNCG FloatRep	`thenUs` \ tmp ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	pk   = registerRep  register

    	code__2 dst = code . mkSeqInstrs [
    	    FxTOy (primRepToSize pk) W src tmp,
    	    ST W tmp (spRel (-2)),
    	    LD W (spRel (-2)) dst]
    in
    returnUs (Any IntRep code__2)

#endif {- sparc_TARGET_ARCH -}
\end{code}

%************************************************************************
%*									*
\subsubsection{Coercing integer to @Char@...}
%*									*
%************************************************************************

Integer to character conversion.  Where applicable, we try to do this
in one step if the original object is in memory.

\begin{code}
chrCode :: StixTree -> UniqSM Register

#if alpha_TARGET_ARCH

chrCode x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	code__2 dst = code . mkSeqInstr (ZAPNOT src (RIImm (ImmInt 1)) dst)
    in
    returnUs (Any IntRep code__2)

#endif {- alpha_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH

chrCode x
  = getRegister x		`thenUs` \ register ->
    --getNewRegNCG IntRep	`thenUs` \ reg ->
    let
    	code__2 dst = let
    	                  code = registerCode register dst
    	                  src  = registerName register dst
		      in code .
			 if isFixed register && src /= dst
			 then mkSeqInstrs [MOV L (OpReg src) (OpReg dst),
					   AND L (OpImm (ImmInt 255)) (OpReg dst)]
			 else mkSeqInstr (AND L (OpImm (ImmInt 255)) (OpReg src))
    in
    returnUs (Any IntRep code__2)

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if sparc_TARGET_ARCH

chrCode (StInd pk mem)
  = getAmode mem		`thenUs` \ amode ->
    let
    	code    = amodeCode amode
    	src     = amodeAddr amode
    	src_off = addrOffset src 3
    	src__2  = case src_off of Just x -> x
    	code__2 dst = if maybeToBool src_off then
    	    	    	code . mkSeqInstr (LD BU src__2 dst)
    	    	    else
    	    	    	code . mkSeqInstrs [
    	    	    	    LD (primRepToSize pk) src dst,
    	    	    	    AND False dst (RIImm (ImmInt 255)) dst]
    in
    returnUs (Any pk code__2)

chrCode x
  = getRegister x		`thenUs` \ register ->
    getNewRegNCG IntRep		`thenUs` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg
    	code__2 dst = code . mkSeqInstr (AND False src (RIImm (ImmInt 255)) dst)
    in
    returnUs (Any IntRep code__2)

#endif {- sparc_TARGET_ARCH -}
\end{code}
