%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[SparcCode]{The Native (Sparc) Machine Code}

\begin{code}
#define ILIT2(x) ILIT(x)
#include "HsVersions.h"

module SparcCode (
	Addr(..),Cond(..),Imm(..),RI(..),Size(..),
	SparcCode(..),SparcInstr(..),SparcRegs,
	strImmLit,

    	printLabeledCodes,

	baseRegOffset, stgRegMap, callerSaves,

	is13Bits, offset,

    	kindToSize,

    	g0, o0, f0, fp, sp, argRegs,

    	freeRegs, reservedRegs

	-- and, for self-sufficiency ...
    ) where

IMPORT_Trace

import AbsCSyn	    	( MagicId(..) )
import AsmRegAlloc  	( MachineCode(..), MachineRegisters(..), FutureLive(..),
    	    	    	  Reg(..), RegUsage(..), RegLiveness(..)
    	    	    	)
import BitSet
import CgCompInfo   	( mAX_Double_REG, mAX_Float_REG, mAX_Vanilla_REG )
import CLabel   	( CLabel, pprCLabel, externallyVisibleCLabel, charToC )
import FiniteMap
import Maybes	    	( Maybe(..), maybeToBool )
import OrdList	    	( OrdList, mkUnitList, flattenOrdList )
import Outputable
import UniqSet
import Stix
import Unpretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection[SparcReg]{The Native (Sparc) Machine Register Table}
%*									*
%************************************************************************

The sparc has 64 registers of interest; 32 integer registers and 32 floating
point registers.  The mapping of STG registers to sparc machine registers
is defined in StgRegs.h.  We are, of course, prepared for any eventuality.

ToDo: Deal with stg registers that live as offsets from BaseReg!(JSM)

\begin{code}

gReg,lReg,iReg,oReg,fReg :: Int -> Int
gReg x = x
oReg x = (8 + x)
lReg x = (16 + x)
iReg x = (24 + x)
fReg x = (32 + x)

fPair :: Reg -> Reg
fPair (FixedReg i) = FixedReg (i _ADD_ ILIT(1))
fPair (MappedReg i) = MappedReg (i _ADD_ ILIT(1))

g0, fp, sp, o0, f0 :: Reg
g0 = case (gReg 0) of { IBOX(g0) -> FixedReg g0 }
fp = case (iReg 6) of { IBOX(i6) -> FixedReg i6 }
sp = case (oReg 6) of { IBOX(o6) -> FixedReg o6 }
o0 = realReg  (oReg 0)
f0 = realReg  (fReg 0)

argRegs :: [Reg]
argRegs = map realReg [oReg i | i <- [0..5]]

realReg n@IBOX(i) = if _IS_TRUE_(freeReg i) then MappedReg i else FixedReg i

\end{code}

%************************************************************************
%*									*
\subsection[TheSparcCode]{The datatype for sparc assembly language}
%*									*
%************************************************************************

Here is a definition of the Sparc assembly language.

\begin{code}

data Imm = ImmInt Int
    	 | ImmInteger Integer	      -- Sigh.
	 | ImmCLbl CLabel	      -- AbstractC Label (with baggage)
	 | ImmLab  Unpretty	      -- Simple string label (underscored)
	 | ImmLit Unpretty	      -- Simple string
	 | LO Imm		      -- Possible restrictions
	 | HI Imm
	 deriving ()

strImmLit s = ImmLit (uppStr s)

data Addr = AddrRegReg Reg Reg
	  | AddrRegImm Reg Imm
	  deriving ()

data Cond = ALWAYS
	  | NEVER
	  | GEU
	  | LU
	  | EQ
	  | GT
	  | GE
	  | GU
	  | LT
	  | LE
	  | LEU
	  | NE
	  | NEG
	  | POS
	  | VC
	  | VS
	  deriving ()

data RI = RIReg Reg
	| RIImm Imm
	deriving ()

riZero :: RI -> Bool
riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (FixedReg ILIT(0)))   = True
riZero _			    = False

data Size = SB
	  | HW
	  | UB
	  | UHW
	  | W
	  | D
	  | F
	  | DF
	  deriving ()

data SparcInstr =

-- Loads and stores.

		LD	      Size Addr Reg -- size, src, dst
	      | ST	      Size Reg Addr -- size, src, dst

-- Int Arithmetic.

	      | ADD	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst
	      | SUB	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst

-- Simple bit-twiddling.

	      | AND	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ANDN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | OR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | ORN	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | XNOR	      Bool Reg RI Reg -- cc?, src1, src2, dst
	      | SLL	      Reg RI Reg -- src1, src2, dst
	      | SRL	      Reg RI Reg -- src1, src2, dst
	      | SRA	      Reg RI Reg -- src1, src2, dst
	      | SETHI	      Imm Reg -- src, dst
	      | NOP	      -- Really SETHI 0, %g0, but worth an alias

-- Float Arithmetic.

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

    	      | FABS	      Size Reg Reg -- src dst
	      | FADD	      Size Reg Reg Reg -- src1, src2, dst
    	      | FCMP	      Bool Size Reg Reg -- exception?, src1, src2, dst
	      | FDIV	      Size Reg Reg Reg -- src1, src2, dst
    	      | FMOV	      Size Reg Reg -- src, dst
	      | FMUL	      Size Reg Reg Reg -- src1, src2, dst
    	      | FNEG	      Size Reg Reg -- src, dst
    	      | FSQRT	      Size Reg Reg -- src, dst
	      | FSUB	      Size Reg Reg Reg -- src1, src2, dst
    	      | FxTOy	      Size Size Reg Reg -- src, dst

-- Jumping around.

	      | BI	      Cond Bool Imm -- cond, annul?, target
    	      | BF  	      Cond Bool Imm -- cond, annul?, target

	      | JMP	      Addr -- target
	      | CALL	      Imm Int Bool -- target, args, terminal

-- Pseudo-ops.

	      | LABEL CLabel
	      | COMMENT FAST_STRING
	      | SEGMENT CodeSegment
	      | ASCII Bool String   -- needs backslash conversion?
	      | DATA Size [Imm]

type SparcCode	= OrdList SparcInstr

\end{code}

%************************************************************************
%*									*
\subsection[TheSparcPretty]{Pretty-printing the Sparc Assembly Language}
%*									*
%************************************************************************

\begin{code}

printLabeledCodes :: PprStyle -> [SparcInstr] -> Unpretty
printLabeledCodes sty codes = uppAboves (map (pprSparcInstr sty) codes)

\end{code}

Printing the pieces...

\begin{code}

pprReg :: Reg -> Unpretty

pprReg (FixedReg i) = pprSparcReg i
pprReg (MappedReg i) = pprSparcReg i
pprReg other = uppStr (show other)   -- should only happen when debugging

pprSparcReg :: FAST_INT -> Unpretty
pprSparcReg i = uppPStr
    (case i of {
	ILIT( 0) -> SLIT("%g0");  ILIT( 1) -> SLIT("%g1");
	ILIT( 2) -> SLIT("%g2");  ILIT( 3) -> SLIT("%g3");
	ILIT( 4) -> SLIT("%g4");  ILIT( 5) -> SLIT("%g5");
	ILIT( 6) -> SLIT("%g6");  ILIT( 7) -> SLIT("%g7");
	ILIT( 8) -> SLIT("%o0");  ILIT( 9) -> SLIT("%o1");
	ILIT(10) -> SLIT("%o2");  ILIT(11) -> SLIT("%o3");
	ILIT(12) -> SLIT("%o4");  ILIT(13) -> SLIT("%o5");
	ILIT(14) -> SLIT("%o6");  ILIT(15) -> SLIT("%o7");
	ILIT(16) -> SLIT("%l0");  ILIT(17) -> SLIT("%l1");
	ILIT(18) -> SLIT("%l2");  ILIT(19) -> SLIT("%l3");
	ILIT(20) -> SLIT("%l4");  ILIT(21) -> SLIT("%l5");
	ILIT(22) -> SLIT("%l6");  ILIT(23) -> SLIT("%l7");
	ILIT(24) -> SLIT("%i0");  ILIT(25) -> SLIT("%i1");
	ILIT(26) -> SLIT("%i2");  ILIT(27) -> SLIT("%i3");
	ILIT(28) -> SLIT("%i4");  ILIT(29) -> SLIT("%i5");
	ILIT(30) -> SLIT("%i6");  ILIT(31) -> SLIT("%i7");
	ILIT(32) -> SLIT("%f0");  ILIT(33) -> SLIT("%f1");
	ILIT(34) -> SLIT("%f2");  ILIT(35) -> SLIT("%f3");
	ILIT(36) -> SLIT("%f4");  ILIT(37) -> SLIT("%f5");
	ILIT(38) -> SLIT("%f6");  ILIT(39) -> SLIT("%f7");
	ILIT(40) -> SLIT("%f8");  ILIT(41) -> SLIT("%f9");
	ILIT(42) -> SLIT("%f10"); ILIT(43) -> SLIT("%f11");
	ILIT(44) -> SLIT("%f12"); ILIT(45) -> SLIT("%f13");
	ILIT(46) -> SLIT("%f14"); ILIT(47) -> SLIT("%f15");
	ILIT(48) -> SLIT("%f16"); ILIT(49) -> SLIT("%f17");
	ILIT(50) -> SLIT("%f18"); ILIT(51) -> SLIT("%f19");
	ILIT(52) -> SLIT("%f20"); ILIT(53) -> SLIT("%f21");
	ILIT(54) -> SLIT("%f22"); ILIT(55) -> SLIT("%f23");
	ILIT(56) -> SLIT("%f24"); ILIT(57) -> SLIT("%f25");
	ILIT(58) -> SLIT("%f26"); ILIT(59) -> SLIT("%f27");
	ILIT(60) -> SLIT("%f28"); ILIT(61) -> SLIT("%f29");
	ILIT(62) -> SLIT("%f30"); ILIT(63) -> SLIT("%f31");
	_ -> SLIT("very naughty sparc register")
    })

pprCond :: Cond -> Unpretty
pprCond x = uppPStr
    (case x of {
	ALWAYS	-> SLIT("");	NEVER -> SLIT("n");
	GEU	-> SLIT("geu");	LU    -> SLIT("lu");
	EQ	-> SLIT("e");	GT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("gu");
	LT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("leu");	NE    -> SLIT("ne");
	NEG	-> SLIT("neg");	POS   -> SLIT("pos");
	VC	-> SLIT("vc");	VS    -> SLIT("vs")
    })

pprImm :: PprStyle -> Imm -> Unpretty

pprImm sty (ImmInt i) = uppInt i
pprImm sty (ImmInteger i) = uppInteger i

pprImm sty (LO i) =
    uppBesides [
	  pp_lo,
	  pprImm sty i,
	  uppRparen
    ]
  where
#ifdef USE_FAST_STRINGS
    pp_lo = uppPStr (_packCString (A# "%lo("#))
#else
    pp_lo = uppStr "%lo("
#endif

pprImm sty (HI i) =
    uppBesides [
	  pp_hi,
	  pprImm sty i,
	  uppRparen
    ]
  where
#ifdef USE_FAST_STRINGS
    pp_hi = uppPStr (_packCString (A# "%hi("#))
#else
    pp_hi = uppStr "%hi("
#endif

pprImm sty (ImmCLbl l) = pprCLabel sty l

pprImm (PprForAsm _ False _) (ImmLab s) = s
pprImm _                     (ImmLab s) = uppBeside (uppChar '_') s

pprImm sty (ImmLit s) = s

pprAddr :: PprStyle -> Addr -> Unpretty
pprAddr sty (AddrRegReg r1 (FixedReg ILIT(0))) = pprReg r1

pprAddr sty (AddrRegReg r1 r2) =
    uppBesides [
	pprReg r1,
	uppChar '+',
	pprReg r2
    ]

pprAddr sty (AddrRegImm r1 (ImmInt i))
    | i == 0 = pprReg r1
    | i < -4096 || i > 4095 = large_offset_error i
    | i < 0  =
	uppBesides [
	    pprReg r1,
	    uppChar '-',
	    uppInt (-i)
	]

pprAddr sty (AddrRegImm r1 (ImmInteger i))
    | i == 0 = pprReg r1
    | i < -4096 || i > 4095 = large_offset_error i
    | i < 0  =
	uppBesides [
	    pprReg r1,
	    uppChar '-',
	    uppInteger (-i)
	]

pprAddr sty (AddrRegImm r1 imm) =
    uppBesides [
	pprReg r1,
	uppChar '+',
	pprImm sty imm
    ]

large_offset_error i
  = error ("ERROR: SPARC native-code generator cannot handle large offset ("++show i++");\nprobably because of large constant data structures;\nworkaround: use -fvia-C on this module.\n")

pprRI :: PprStyle -> RI -> Unpretty
pprRI sty (RIReg r) = pprReg r
pprRI sty (RIImm r) = pprImm sty r

pprSizeRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Unpretty
pprSizeRegReg name size reg1 reg2 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	(case size of
    	    F  -> uppPStr SLIT("s\t")
    	    DF -> uppPStr SLIT("d\t")),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> Unpretty
pprSizeRegRegReg name size reg1 reg2 reg3 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	(case size of
    	    F  -> uppPStr SLIT("s\t")
    	    DF -> uppPStr SLIT("d\t")),
	pprReg reg1,
	uppComma,
	pprReg reg2,
	uppComma,
	pprReg reg3
    ]

pprRegRIReg :: PprStyle -> FAST_STRING -> Bool -> Reg -> RI -> Reg -> Unpretty
pprRegRIReg sty name b reg1 ri reg2 =
    uppBesides [
	uppChar '\t',
	uppPStr name,
	if b then uppPStr SLIT("cc\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprRIReg :: PprStyle -> FAST_STRING -> Bool -> RI -> Reg -> Unpretty
pprRIReg sty name b ri reg1 =
    uppBesides [
	uppChar '\t',
	uppPStr name,
	if b then uppPStr SLIT("cc\t") else uppChar '\t',
	pprRI sty ri,
	uppComma,
	pprReg reg1
    ]

pprSize :: Size -> Unpretty
pprSize x = uppPStr
    (case x of
	SB  -> SLIT("sb")
	HW  -> SLIT("hw")
	UB  -> SLIT("ub")
	UHW -> SLIT("uhw")
	W   -> SLIT("")
	F   -> SLIT("")
	D   -> SLIT("d")
	DF  -> SLIT("d")
    )

#ifdef USE_FAST_STRINGS
pp_ld_lbracket    = uppPStr (_packCString (A# "\tld\t["#))
pp_rbracket_comma = uppPStr (_packCString (A# "],"#))
pp_comma_lbracket = uppPStr (_packCString (A# ",["#))
pp_comma_a	  = uppPStr (_packCString (A# ",a"#))
#else
pp_ld_lbracket    = uppStr "\tld\t["
pp_rbracket_comma = uppStr "],"
pp_comma_lbracket = uppStr ",["
pp_comma_a	  = uppStr ",a"
#endif

pprSparcInstr :: PprStyle -> SparcInstr -> Unpretty

-- a clumsy hack for now, to handle possible alignment problems

pprSparcInstr sty (LD DF addr reg) | maybeToBool addrOff =
    uppBesides [
	pp_ld_lbracket,
	pprAddr sty addr,
	pp_rbracket_comma,
	pprReg reg,

	uppChar '\n',
	pp_ld_lbracket,
	pprAddr sty addr2,
	pp_rbracket_comma,
	pprReg (fPair reg)
    ]
  where
    addrOff = offset addr 4
    addr2 = case addrOff of Just x -> x

pprSparcInstr sty (LD size addr reg) =
    uppBesides [
	uppPStr SLIT("\tld"),
	pprSize size,
	uppChar '\t',
	uppLbrack,
	pprAddr sty addr,
	pp_rbracket_comma,
	pprReg reg
    ]

-- The same clumsy hack as above

pprSparcInstr sty (ST DF reg addr) | maybeToBool addrOff =
    uppBesides [
	uppPStr SLIT("\tst\t"),
	pprReg reg,
	pp_comma_lbracket,
	pprAddr sty addr,

	uppPStr SLIT("]\n\tst\t"),
	pprReg (fPair reg),
	pp_comma_lbracket,
	pprAddr sty addr2,
	uppRbrack
    ]
  where
    addrOff = offset addr 4
    addr2 = case addrOff of Just x -> x

pprSparcInstr sty (ST size reg addr) =
    uppBesides [
	uppPStr SLIT("\tst"),
	pprSize size,
	uppChar '\t',
	pprReg reg,
	pp_comma_lbracket,
	pprAddr sty addr,
	uppRbrack
    ]

pprSparcInstr sty (ADD x cc reg1 ri reg2)
 | not x && not cc && riZero ri =
    uppBesides [
	uppPStr SLIT("\tmov\t"),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]
 | otherwise = pprRegRIReg sty (if x then SLIT("addx") else SLIT("add")) cc reg1 ri reg2

pprSparcInstr sty (SUB x cc reg1 ri reg2)
 | not x && cc && reg2 == g0 =
    uppBesides [
	uppPStr SLIT("\tcmp\t"),
	pprReg reg1,
	uppComma,
	pprRI sty ri
    ]
 | not x && not cc && riZero ri =
    uppBesides [
	uppPStr SLIT("\tmov\t"),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]
 | otherwise = pprRegRIReg sty (if x then SLIT("subx") else SLIT("sub")) cc reg1 ri reg2

pprSparcInstr sty (AND b reg1 ri reg2) = pprRegRIReg sty SLIT("and") b reg1 ri reg2
pprSparcInstr sty (ANDN b reg1 ri reg2) = pprRegRIReg sty SLIT("andn") b reg1 ri reg2

pprSparcInstr sty (OR b reg1 ri reg2)
 | not b && reg1 == g0 =
    uppBesides [
	uppPStr SLIT("\tmov\t"),
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]
 | otherwise = pprRegRIReg sty SLIT("or") b reg1 ri reg2

pprSparcInstr sty (ORN b reg1 ri reg2) = pprRegRIReg sty SLIT("orn") b reg1 ri reg2

pprSparcInstr sty (XOR b reg1 ri reg2) = pprRegRIReg sty SLIT("xor") b reg1 ri reg2
pprSparcInstr sty (XNOR b reg1 ri reg2) = pprRegRIReg sty SLIT("xnor") b reg1 ri reg2

pprSparcInstr sty (SLL reg1 ri reg2) = pprRegRIReg sty SLIT("sll") False reg1 ri reg2
pprSparcInstr sty (SRL reg1 ri reg2) = pprRegRIReg sty SLIT("srl") False reg1 ri reg2
pprSparcInstr sty (SRA reg1 ri reg2) = pprRegRIReg sty SLIT("sra") False reg1 ri reg2

pprSparcInstr sty (SETHI imm reg) =
    uppBesides [
	uppPStr SLIT("\tsethi\t"),
	pprImm sty imm,
	uppComma,
	pprReg reg
    ]

pprSparcInstr sty (NOP) = uppPStr SLIT("\tnop")

pprSparcInstr sty (FABS F reg1 reg2) = pprSizeRegReg SLIT("fabs") F reg1 reg2
pprSparcInstr sty (FABS DF reg1 reg2) =
    uppBeside (pprSizeRegReg SLIT("fabs") F reg1 reg2)
    (if (reg1 == reg2) then uppNil
     else uppBeside (uppChar '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprSparcInstr sty (FADD size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fadd") size reg1 reg2 reg3
pprSparcInstr sty (FCMP e size reg1 reg2) =
    pprSizeRegReg (if e then SLIT("fcmpe") else SLIT("fcmp")) size reg1 reg2
pprSparcInstr sty (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fdiv") size reg1 reg2 reg3

pprSparcInstr sty (FMOV F reg1 reg2) = pprSizeRegReg SLIT("fmov") F reg1 reg2
pprSparcInstr sty (FMOV DF reg1 reg2) =
    uppBeside (pprSizeRegReg SLIT("fmov") F reg1 reg2)
    (if (reg1 == reg2) then uppNil
     else uppBeside (uppChar '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprSparcInstr sty (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fmul") size reg1 reg2 reg3

pprSparcInstr sty (FNEG F reg1 reg2) = pprSizeRegReg SLIT("fneg") F reg1 reg2
pprSparcInstr sty (FNEG DF reg1 reg2) =
    uppBeside (pprSizeRegReg SLIT("fneg") F reg1 reg2)
    (if (reg1 == reg2) then uppNil
     else uppBeside (uppChar '\n')
    	  (pprSizeRegReg SLIT("fmov") F (fPair reg1) (fPair reg2)))

pprSparcInstr sty (FSQRT size reg1 reg2) = pprSizeRegReg SLIT("fsqrt") size reg1 reg2
pprSparcInstr sty (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("fsub") size reg1 reg2 reg3
pprSparcInstr sty (FxTOy size1 size2 reg1 reg2) =
    uppBesides [
    	uppPStr SLIT("\tf"),
	uppPStr
    	(case size1 of
    	    W  -> SLIT("ito")
    	    F  -> SLIT("sto")
    	    DF -> SLIT("dto")),
	uppPStr
    	(case size2 of
    	    W  -> SLIT("i\t")
    	    F  -> SLIT("s\t")
    	    DF -> SLIT("d\t")),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]


pprSparcInstr sty (BI cond b lab) =
    uppBesides [
	uppPStr SLIT("\tb"), pprCond cond,
	if b then pp_comma_a else uppNil,
	uppChar '\t',
	pprImm sty lab
    ]

pprSparcInstr sty (BF cond b lab) =
    uppBesides [
	uppPStr SLIT("\tfb"), pprCond cond,
	if b then pp_comma_a else uppNil,
	uppChar '\t',
	pprImm sty lab
    ]

pprSparcInstr sty (JMP addr) = uppBeside (uppPStr SLIT("\tjmp\t")) (pprAddr sty addr)

pprSparcInstr sty (CALL imm n _) =
    uppBesides [
	uppPStr SLIT("\tcall\t"),
	pprImm sty imm,
	uppComma,
	uppInt n
    ]

pprSparcInstr sty (LABEL clab) =
    uppBesides [
	if (externallyVisibleCLabel clab) then
	    uppBesides [uppPStr SLIT("\t.global\t"), pprLab, uppChar '\n']
	else
	    uppNil,
    	pprLab,
	uppChar ':'
    ]
    where pprLab = pprCLabel sty clab

pprSparcInstr sty (COMMENT s) = uppBeside (uppPStr SLIT("! ")) (uppPStr s)

pprSparcInstr sty (SEGMENT TextSegment)
    = uppPStr SLIT("\t.text\n\t.align 4")

pprSparcInstr sty (SEGMENT DataSegment)
    = uppPStr SLIT("\t.data\n\t.align 8")   -- Less than 8 will break double constants

pprSparcInstr sty (ASCII False str) =
    uppBesides [
    	uppStr "\t.asciz \"",
    	uppStr str,
    	uppChar '"'
    ]

pprSparcInstr sty (ASCII True str) = uppBeside (uppStr "\t.ascii \"") (asciify str 60)
    where
    	asciify :: String -> Int -> Unpretty
    	asciify [] _ = uppStr ("\\0\"")
    	asciify s n | n <= 0 = uppBeside (uppStr "\"\n\t.ascii \"") (asciify s 60)
	asciify ('\\':cs) n = uppBeside (uppStr "\\\\") (asciify cs (n-1))
	asciify ('\"':cs) n = uppBeside (uppStr "\\\"") (asciify cs (n-1))
	asciify (c:cs) n | isPrint c = uppBeside (uppChar c) (asciify cs (n-1))
    	asciify [c] _ = uppBeside (uppStr (charToC c)) (uppStr ("\\0\""))
    	asciify (c:(cs@(d:_))) n | isDigit d =
    	    	    	    	    	uppBeside (uppStr (charToC c)) (asciify cs 0)
    	    	    	    	 | otherwise =
    	    	    	    	    	uppBeside (uppStr (charToC c)) (asciify cs (n-1))

pprSparcInstr sty (DATA s xs) = uppInterleave (uppChar '\n') (map pp_item xs)
    where pp_item x = case s of
	    SB -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    UB -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    W  -> uppBeside (uppPStr SLIT("\t.word\t")) (pprImm sty x)
    	    DF -> uppBeside (uppPStr SLIT("\t.double\t")) (pprImm sty x)

\end{code}

%************************************************************************
%*									*
\subsection[Schedule]{Register allocation information}
%*									*
%************************************************************************

Getting the conflicts right is a bit tedious for doubles.  We'd have to
add a conflict function to the MachineRegisters class, and we'd have to
put a PrimRep in the MappedReg datatype, or use some kludge (e.g. register
64 + n is really the same as 32 + n, except that it's used for a double,
so it also conflicts with 33 + n) to deal with it.  It's just not worth the
bother, so we just partition the free floating point registers into two
sets: one for single precision and one for double precision.  We never seem
to run out of floating point registers anyway.

\begin{code}

data SparcRegs = SRegs BitSet BitSet BitSet

instance MachineRegisters SparcRegs where
    mkMRegs xs = SRegs (mkBS ints) (mkBS singles') (mkBS doubles')
      where
    	(ints, floats) = partition (< 32) xs
    	(singles, doubles) = partition (< 48) floats
    	singles' = map (subtract 32) singles
	doubles' = map (subtract 32) (filter even doubles)

    possibleMRegs FloatRep (SRegs _ singles _) = [ x + 32 | x <- listBS singles]
    possibleMRegs DoubleRep (SRegs _ _ doubles) = [ x + 32 | x <- listBS doubles]
    possibleMRegs _ (SRegs ints _ _) = listBS ints

    useMReg (SRegs ints singles doubles) n =
    	if n _LT_ ILIT(32) then SRegs (ints `minusBS` singletonBS IBOX(n)) singles doubles
    	else if n _LT_ ILIT(48) then SRegs ints (singles `minusBS` singletonBS (IBOX(n _SUB_ ILIT(32)))) doubles
    	else SRegs ints singles (doubles `minusBS` singletonBS (IBOX(n _SUB_ ILIT(32))))

    useMRegs (SRegs ints singles doubles) xs =
    	SRegs (ints `minusBS` ints')
    	      (singles `minusBS` singles')
    	      (doubles `minusBS` doubles')
      where
	SRegs ints' singles' doubles' = mkMRegs xs

    freeMReg (SRegs ints singles doubles) n =
    	if n _LT_ ILIT(32) then SRegs (ints `unionBS` singletonBS IBOX(n)) singles doubles
    	else if n _LT_ ILIT(48) then SRegs ints (singles `unionBS` singletonBS (IBOX(n _SUB_ ILIT(32)))) doubles
    	else SRegs ints singles (doubles `unionBS` singletonBS (IBOX(n _SUB_ ILIT(32))))

    freeMRegs (SRegs ints singles doubles) xs =
	SRegs (ints `unionBS` ints')
    	      (singles `unionBS` singles')
    	      (doubles `unionBS` doubles')
      where
	SRegs ints' singles' doubles' = mkMRegs xs

instance MachineCode SparcInstr where
    regUsage = sparcRegUsage
    regLiveness = sparcRegLiveness
    patchRegs = sparcPatchRegs

    -- We spill just below the frame pointer, leaving two words per spill location.
    spillReg dyn (MemoryReg i pk) = mkUnitList (ST (kindToSize pk) dyn (fpRel (-2 * i)))
    loadReg (MemoryReg i pk) dyn = mkUnitList (LD (kindToSize pk) (fpRel (-2 * i)) dyn)

-- Duznae work for offsets greater than 13 bits; we just hope for the best
fpRel :: Int -> Addr
fpRel n = AddrRegImm fp (ImmInt (n * 4))

kindToSize :: PrimRep -> Size
kindToSize PtrRep	    = W
kindToSize CodePtrRep	    = W
kindToSize DataPtrRep	    = W
kindToSize RetRep	    = W
kindToSize CostCentreRep   = W
kindToSize CharRep	    = UB
kindToSize IntRep	    = W
kindToSize WordRep	    = W
kindToSize AddrRep	    = W
kindToSize FloatRep	    = F
kindToSize DoubleRep	    = DF
kindToSize ArrayRep	    = W
kindToSize ByteArrayRep    = W
kindToSize StablePtrRep    = W
kindToSize MallocPtrRep    = W

\end{code}

@sparcRegUsage@ returns the sets of src and destination registers used by
a particular instruction.  Machine registers that are pre-allocated
to stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.  (We wouldn't want them to end up on
the free list!)

\begin{code}

sparcRegUsage :: SparcInstr -> RegUsage
sparcRegUsage instr = case instr of
    LD sz addr reg  	-> usage (regAddr addr, [reg])
    ST sz reg addr  	-> usage (reg : regAddr addr, [])
    ADD x cc r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SUB x cc r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    AND b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    ANDN b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    OR b r1 ar r2   	-> usage (r1 : regRI ar, [r2])
    ORN b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XOR b r1 ar r2  	-> usage (r1 : regRI ar, [r2])
    XNOR b r1 ar r2 	-> usage (r1 : regRI ar, [r2])
    SLL r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRL r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SRA r1 ar r2    	-> usage (r1 : regRI ar, [r2])
    SETHI imm reg   	-> usage ([], [reg])
    FABS s r1 r2    	-> usage ([r1], [r2])
    FADD s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FCMP e s r1 r2  	-> usage ([r1, r2], [])
    FDIV s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FMOV s r1 r2    	-> usage ([r1], [r2])
    FMUL s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FNEG s r1 r2    	-> usage ([r1], [r2])
    FSQRT s r1 r2   	-> usage ([r1], [r2])
    FSUB s r1 r2 r3 	-> usage ([r1, r2], [r3])
    FxTOy s1 s2 r1 r2 	-> usage ([r1], [r2])

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.
    JMP addr 	    	-> RU (mkUniqSet (filter interesting (regAddr addr))) freeSet

    CALL _ n True   	-> endUsage
    CALL _ n False  	-> RU (argSet n) callClobberedSet

    _ 	    	    	-> noUsage

  where
    usage (src, dst) = RU (mkUniqSet (filter interesting src))
    	    	    	  (mkUniqSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _	= []

freeRegs :: [Reg]
freeRegs = freeMappedRegs (\ x -> x) [0..63]

freeMappedRegs :: (Int -> Int) -> [Int] -> [Reg]

freeMappedRegs modify nums
  = foldr free [] nums
  where
    free n acc
      = let
	    modified_i = case (modify n) of { IBOX(x) -> x }
	in
	if _IS_TRUE_(freeReg modified_i) then (MappedReg modified_i) : acc else acc

freeSet :: UniqSet Reg
freeSet = mkUniqSet freeRegs

noUsage :: RegUsage
noUsage = RU emptyUniqSet emptyUniqSet

endUsage :: RegUsage
endUsage = RU emptyUniqSet freeSet

-- Color me CAF-like
argSet :: Int -> UniqSet Reg
argSet 0 = emptyUniqSet
argSet 1 = mkUniqSet (freeMappedRegs oReg [0])
argSet 2 = mkUniqSet (freeMappedRegs oReg [0,1])
argSet 3 = mkUniqSet (freeMappedRegs oReg [0,1,2])
argSet 4 = mkUniqSet (freeMappedRegs oReg [0,1,2,3])
argSet 5 = mkUniqSet (freeMappedRegs oReg [0,1,2,3,4])
argSet 6 = mkUniqSet (freeMappedRegs oReg [0,1,2,3,4,5])

callClobberedSet :: UniqSet Reg
callClobberedSet = mkUniqSet callClobberedRegs
  where
    callClobberedRegs = freeMappedRegs (\x -> x)
      ( oReg 7 :
    	[oReg i | i <- [0..5]] ++
    	[gReg i | i <- [1..7]] ++
    	[fReg i | i <- [0..31]] )

\end{code}

@sparcRegLiveness@ takes future liveness information and modifies it according to
the semantics of branches and labels.  (An out-of-line branch clobbers the liveness
passed back by the following instruction; a forward local branch passes back the
liveness from the target label; a conditional branch merges the liveness from the
target and the liveness from its successor; a label stashes away the current liveness
in the future liveness environment).

\begin{code}
sparcRegLiveness :: SparcInstr -> RegLiveness -> RegLiveness
sparcRegLiveness instr info@(RL live future@(FL all env)) = case instr of

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.

    BI ALWAYS _ (ImmCLbl lbl)	-> RL (lookup lbl) future
    BI _ _ (ImmCLbl lbl)	-> RL (lookup lbl `unionUniqSets` live) future
    BF ALWAYS _ (ImmCLbl lbl)	-> RL (lookup lbl) future
    BF _ _ (ImmCLbl lbl)	-> RL (lookup lbl `unionUniqSets` live) future
    JMP _			-> RL emptyUniqSet future
    CALL _ i True   -> RL emptyUniqSet future
    CALL _ i False  -> RL live future
    LABEL lbl	    -> RL live (FL (all `unionUniqSets` live) (addToFM env lbl live))
    _		    -> info

  where
    lookup lbl = case lookupFM env lbl of
	Just regs -> regs
	Nothing -> trace ("Missing " ++ (uppShow 80 (pprCLabel (PprForAsm (\_->False) False id) lbl)) ++
			  " in future?") emptyUniqSet

\end{code}

@sparcPatchRegs@ takes an instruction (possibly with MemoryReg/UnmappedReg registers) and
changes all register references according to the supplied environment.

\begin{code}

sparcPatchRegs :: SparcInstr -> (Reg -> Reg) -> SparcInstr
sparcPatchRegs instr env = case instr of
    LD sz addr reg -> LD sz (fixAddr addr) (env reg)
    ST sz reg addr -> ST sz (env reg) (fixAddr addr)
    ADD x cc r1 ar r2 -> ADD x cc (env r1) (fixRI ar) (env r2)
    SUB x cc r1 ar r2 -> SUB x cc (env r1) (fixRI ar) (env r2)
    AND b r1 ar r2 -> AND b (env r1) (fixRI ar) (env r2)
    ANDN b r1 ar r2 -> ANDN b (env r1) (fixRI ar) (env r2)
    OR b r1 ar r2 -> OR b (env r1) (fixRI ar) (env r2)
    ORN b r1 ar r2 -> ORN b (env r1) (fixRI ar) (env r2)
    XOR b r1 ar r2 -> XOR b (env r1) (fixRI ar) (env r2)
    XNOR b r1 ar r2 -> XNOR b (env r1) (fixRI ar) (env r2)
    SLL r1 ar r2 -> SLL (env r1) (fixRI ar) (env r2)
    SRL r1 ar r2 -> SRL (env r1) (fixRI ar) (env r2)
    SRA r1 ar r2 -> SRA (env r1) (fixRI ar) (env r2)
    SETHI imm reg -> SETHI imm (env reg)
    FABS s r1 r2 -> FABS s (env r1) (env r2)
    FADD s r1 r2 r3 -> FADD s (env r1) (env r2) (env r3)
    FCMP e s r1 r2 -> FCMP e s (env r1) (env r2)
    FDIV s r1 r2 r3 -> FDIV s (env r1) (env r2) (env r3)
    FMOV s r1 r2 -> FMOV s (env r1) (env r2)
    FMUL s r1 r2 r3 -> FMUL s (env r1) (env r2) (env r3)
    FNEG s r1 r2 -> FNEG s (env r1) (env r2)
    FSQRT s r1 r2 -> FSQRT s (env r1) (env r2)
    FSUB s r1 r2 r3 -> FSUB s (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2 -> FxTOy s1 s2 (env r1) (env r2)
    JMP addr -> JMP (fixAddr addr)
    _ -> instr

  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other
\end{code}

Sometimes, we want to be able to modify addresses at compile time.
(Okay, just for chrCode of a fetch.)

\begin{code}
{-# SPECIALIZE
    is13Bits :: Int -> Bool
  #-}
{-# SPECIALIZE
    is13Bits :: Integer -> Bool
  #-}

is13Bits :: Integral a => a -> Bool
is13Bits x = x >= -4096 && x < 4096

offset :: Addr -> Int -> Maybe Addr

offset (AddrRegImm reg (ImmInt n)) off
  | is13Bits n2 = Just (AddrRegImm reg (ImmInt n2))
  | otherwise = Nothing
  where n2 = n + off

offset (AddrRegImm reg (ImmInteger n)) off
  | is13Bits n2 = Just (AddrRegImm reg (ImmInt (fromInteger n2)))
  | otherwise = Nothing
  where n2 = n + toInteger off

offset (AddrRegReg reg (FixedReg ILIT(0))) off
  | is13Bits off = Just (AddrRegImm reg (ImmInt off))
  | otherwise = Nothing

offset _ _ = Nothing

\end{code}

If you value your sanity, do not venture below this line.

\begin{code}

-- platform.h is generate and tells us what the target architecture is
#include "../../includes/platform.h"
#include "../../includes/MachRegs.h"
#if sunos4_TARGET_OS
#include "../../includes/sparc-sun-sunos4.h"
#else
#include "../../includes/sparc-sun-solaris2.h"
#endif

-- Redefine the literals used for Sparc register names in the header
-- files.  Gag me with a spoon, eh?

#define g0 0
#define g1 1
#define g2 2
#define g3 3
#define g4 4
#define g5 5
#define g6 6
#define g7 7
#define o0 8
#define o1 9
#define o2 10
#define o3 11
#define o4 12
#define o5 13
#define o6 14
#define o7 15
#define l0 16
#define l1 17
#define l2 18
#define l3 19
#define l4 20
#define l5 21
#define l6 22
#define l7 23
#define i0 24
#define i1 25
#define i2 26
#define i3 27
#define i4 28
#define i5 29
#define i6 30
#define i7 31
#define f0 32
#define f1 33
#define f2 34
#define f3 35
#define f4 36
#define f5 37
#define f6 38
#define f7 39
#define f8 40
#define f9 41
#define f10 42
#define f11 43
#define f12 44
#define f13 45
#define f14 46
#define f15 47
#define f16 48
#define f17 49
#define f18 50
#define f19 51
#define f20 52
#define f21 53
#define f22 54
#define f23 55
#define f24 56
#define f25 57
#define f26 58
#define f27 59
#define f28 60
#define f29 61
#define f30 62
#define f31 63

baseRegOffset :: MagicId -> Int
baseRegOffset StkOReg			= OFFSET_StkO
baseRegOffset (VanillaReg _ ILIT2(1))	= OFFSET_R1
baseRegOffset (VanillaReg _ ILIT2(2))	= OFFSET_R2
baseRegOffset (VanillaReg _ ILIT2(3))	= OFFSET_R3
baseRegOffset (VanillaReg _ ILIT2(4))	= OFFSET_R4
baseRegOffset (VanillaReg _ ILIT2(5))	= OFFSET_R5
baseRegOffset (VanillaReg _ ILIT2(6))	= OFFSET_R6
baseRegOffset (VanillaReg _ ILIT2(7))	= OFFSET_R7
baseRegOffset (VanillaReg _ ILIT2(8))	= OFFSET_R8
baseRegOffset (FloatReg ILIT2(1))	= OFFSET_Flt1
baseRegOffset (FloatReg ILIT2(2))	= OFFSET_Flt2
baseRegOffset (FloatReg ILIT2(3))	= OFFSET_Flt3
baseRegOffset (FloatReg ILIT2(4))	= OFFSET_Flt4
baseRegOffset (DoubleReg ILIT2(1))	= OFFSET_Dbl1
baseRegOffset (DoubleReg ILIT2(2))	= OFFSET_Dbl2
baseRegOffset TagReg			= OFFSET_Tag
baseRegOffset RetReg			= OFFSET_Ret
baseRegOffset SpA			= OFFSET_SpA
baseRegOffset SuA			= OFFSET_SuA
baseRegOffset SpB			= OFFSET_SpB
baseRegOffset SuB			= OFFSET_SuB
baseRegOffset Hp			= OFFSET_Hp
baseRegOffset HpLim			= OFFSET_HpLim
baseRegOffset LivenessReg		= OFFSET_Liveness
--baseRegOffset ActivityReg		= OFFSET_Activity
#ifdef DEBUG
baseRegOffset BaseReg			= panic "baseRegOffset:BaseReg"
baseRegOffset StdUpdRetVecReg		= panic "baseRegOffset:StgUpdRetVecReg"
baseRegOffset StkStubReg		= panic "baseRegOffset:StkStubReg"
baseRegOffset CurCostCentre		= panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg			= panic "baseRegOffset:VoidReg"
#endif

callerSaves :: MagicId -> Bool
#ifdef CALLER_SAVES_Base
callerSaves BaseReg    	    	= True
#endif
#ifdef CALLER_SAVES_StkO
callerSaves StkOReg         	= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg _ ILIT2(1))	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg _ ILIT2(2))    = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg _ ILIT2(3))    = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg _ ILIT2(4))    = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg _ ILIT2(5))    = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg _ ILIT2(6))    = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg _ ILIT2(7))	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg _ ILIT2(8))    = True
#endif
#ifdef CALLER_SAVES_FltReg1
callerSaves (FloatReg ILIT2(1))   	= True
#endif
#ifdef CALLER_SAVES_FltReg2
callerSaves (FloatReg ILIT2(2))   	= True
#endif
#ifdef CALLER_SAVES_FltReg3
callerSaves (FloatReg ILIT2(3))   	= True
#endif
#ifdef CALLER_SAVES_FltReg4
callerSaves (FloatReg ILIT2(4))    	= True
#endif
#ifdef CALLER_SAVES_DblReg1
callerSaves (DoubleReg ILIT2(1))    	= True
#endif
#ifdef CALLER_SAVES_DblReg2
callerSaves (DoubleReg ILIT2(2))    	= True
#endif
#ifdef CALLER_SAVES_Tag
callerSaves TagReg      	= True
#endif
#ifdef CALLER_SAVES_Ret
callerSaves RetReg      	= True
#endif
#ifdef CALLER_SAVES_SpA
callerSaves SpA	    	    	= True
#endif
#ifdef CALLER_SAVES_SuA
callerSaves SuA	    	    	= True
#endif
#ifdef CALLER_SAVES_SpB
callerSaves SpB	    	    	= True
#endif
#ifdef CALLER_SAVES_SuB
callerSaves SuB	    	    	= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp	    	    	= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim   	    	= True
#endif
#ifdef CALLER_SAVES_Liveness
callerSaves LivenessReg	        = True
#endif
#ifdef CALLER_SAVES_Activity
--callerSaves ActivityReg	        = True
#endif
#ifdef CALLER_SAVES_StdUpdRetVec
callerSaves StdUpdRetVecReg    	= True
#endif
#ifdef CALLER_SAVES_StkStub
callerSaves StkStubReg 	    	= True
#endif
callerSaves _	    	    	= False

stgRegMap :: MagicId -> Maybe Reg
#ifdef REG_Base
stgRegMap BaseReg	   = Just (FixedReg ILIT(REG_Base))
#endif
#ifdef REG_StkO
stgRegMap StkOReg   	   = Just (FixedReg ILIT(REG_StkOReg))
#endif
#ifdef REG_R1
stgRegMap (VanillaReg _ ILIT2(1)) = Just (FixedReg ILIT(REG_R1))
#endif
#ifdef REG_R2
stgRegMap (VanillaReg _ ILIT2(2)) = Just (FixedReg ILIT(REG_R2))
#endif
#ifdef REG_R3
stgRegMap (VanillaReg _ ILIT2(3)) = Just (FixedReg ILIT(REG_R3))
#endif
#ifdef REG_R4
stgRegMap (VanillaReg _ ILIT2(4)) = Just (FixedReg ILIT(REG_R4))
#endif
#ifdef REG_R5
stgRegMap (VanillaReg _ ILIT2(5)) = Just (FixedReg ILIT(REG_R5))
#endif
#ifdef REG_R6
stgRegMap (VanillaReg _ ILIT2(6)) = Just (FixedReg ILIT(REG_R6))
#endif
#ifdef REG_R7
stgRegMap (VanillaReg _ ILIT2(7)) = Just (FixedReg ILIT(REG_R7))
#endif
#ifdef REG_R8
stgRegMap (VanillaReg _ ILIT2(8)) = Just (FixedReg ILIT(REG_R8))
#endif
#ifdef REG_Flt1
stgRegMap (FloatReg ILIT2(1)) 	   = Just (FixedReg ILIT(REG_Flt1))
#endif
#ifdef REG_Flt2
stgRegMap (FloatReg ILIT2(2)) 	   = Just (FixedReg ILIT(REG_Flt2))
#endif
#ifdef REG_Flt3
stgRegMap (FloatReg ILIT2(3)) 	   = Just (FixedReg ILIT(REG_Flt3))
#endif
#ifdef REG_Flt4
stgRegMap (FloatReg ILIT2(4)) 	   = Just (FixedReg ILIT(REG_Flt4))
#endif
#ifdef REG_Dbl1
stgRegMap (DoubleReg ILIT2(1))	   = Just (FixedReg ILIT(REG_Dbl1))
#endif
#ifdef REG_Dbl2
stgRegMap (DoubleReg ILIT2(2))	   = Just (FixedReg ILIT(REG_Dbl2))
#endif
#ifdef REG_Tag
stgRegMap TagReg    	   = Just (FixedReg ILIT(REG_TagReg))
#endif
#ifdef REG_Ret
stgRegMap RetReg    	   = Just (FixedReg ILIT(REG_Ret))
#endif
#ifdef REG_SpA
stgRegMap SpA	    	   = Just (FixedReg ILIT(REG_SpA))
#endif
#ifdef REG_SuA
stgRegMap SuA	    	   = Just (FixedReg ILIT(REG_SuA))
#endif
#ifdef REG_SpB
stgRegMap SpB	    	   = Just (FixedReg ILIT(REG_SpB))
#endif
#ifdef REG_SuB
stgRegMap SuB	    	   = Just (FixedReg ILIT(REG_SuB))
#endif
#ifdef REG_Hp
stgRegMap Hp	    	   = Just (FixedReg ILIT(REG_Hp))
#endif
#ifdef REG_HpLim
stgRegMap HpLim	    	   = Just (FixedReg ILIT(REG_HpLim))
#endif
#ifdef REG_Liveness
stgRegMap LivenessReg	   = Just (FixedReg ILIT(REG_Liveness))
#endif
#ifdef REG_Activity
--stgRegMap ActivityReg	   = Just (FixedReg ILIT(REG_Activity))
#endif
#ifdef REG_StdUpdRetVec
stgRegMap StdUpdRetVecReg  = Just (FixedReg ILIT(REG_StdUpdRetVec))
#endif
#ifdef REG_StkStub
stgRegMap StkStubReg	   = Just (FixedReg ILIT(REG_StkStub))
#endif
stgRegMap _		   = Nothing

\end{code}

Here is the list of registers we can use in register allocation.

\begin{code}

freeReg :: FAST_INT -> FAST_BOOL

freeReg ILIT(g0) = _FALSE_  --	%g0 is always 0.
freeReg ILIT(g5) = _FALSE_  --	%g5 is reserved (ABI).
freeReg ILIT(g6) = _FALSE_  --	%g6 is reserved (ABI).
freeReg ILIT(g7) = _FALSE_  --	%g7 is reserved (ABI).
freeReg ILIT(i6) = _FALSE_  --	%i6 is our frame pointer.
freeReg ILIT(o6) = _FALSE_  --	%o6 is our stack pointer.

#ifdef REG_Base
freeReg ILIT(REG_Base) = _FALSE_
#endif
#ifdef REG_StkO
freeReg ILIT(REG_StkO) = _FALSE_
#endif
#ifdef REG_R1
freeReg ILIT(REG_R1) = _FALSE_
#endif
#ifdef REG_R2
freeReg ILIT(REG_R2) = _FALSE_
#endif
#ifdef REG_R3
freeReg ILIT(REG_R3) = _FALSE_
#endif
#ifdef REG_R4
freeReg ILIT(REG_R4) = _FALSE_
#endif
#ifdef REG_R5
freeReg ILIT(REG_R5) = _FALSE_
#endif
#ifdef REG_R6
freeReg ILIT(REG_R6) = _FALSE_
#endif
#ifdef REG_R7
freeReg ILIT(REG_R7) = _FALSE_
#endif
#ifdef REG_R8
freeReg ILIT(REG_R8) = _FALSE_
#endif
#ifdef REG_Flt1
freeReg ILIT(REG_Flt1) = _FALSE_
#endif
#ifdef REG_Flt2
freeReg ILIT(REG_Flt2) = _FALSE_
#endif
#ifdef REG_Flt3
freeReg ILIT(REG_Flt3) = _FALSE_
#endif
#ifdef REG_Flt4
freeReg ILIT(REG_Flt4) = _FALSE_
#endif
#ifdef REG_Dbl1
freeReg ILIT(REG_Dbl1) = _FALSE_
#endif
#ifdef REG_Dbl2
freeReg ILIT(REG_Dbl2) = _FALSE_
#endif
#ifdef REG_Tag
freeReg ILIT(REG_Tag) = _FALSE_
#endif
#ifdef REG_Ret
freeReg ILIT(REG_Ret) = _FALSE_
#endif
#ifdef REG_SpA
freeReg ILIT(REG_SpA) = _FALSE_
#endif
#ifdef REG_SuA
freeReg ILIT(REG_SuA) = _FALSE_
#endif
#ifdef REG_SpB
freeReg ILIT(REG_SpB) = _FALSE_
#endif
#ifdef REG_SuB
freeReg ILIT(REG_SuB) = _FALSE_
#endif
#ifdef REG_Hp
freeReg ILIT(REG_Hp) = _FALSE_
#endif
#ifdef REG_HpLim
freeReg ILIT(REG_HpLim) = _FALSE_
#endif
#ifdef REG_Liveness
freeReg ILIT(REG_Liveness) = _FALSE_
#endif
#ifdef REG_Activity
--freeReg ILIT(REG_Activity) = _FALSE_
#endif
#ifdef REG_StdUpdRetVec
freeReg ILIT(REG_StdUpdRetVec) = _FALSE_
#endif
#ifdef REG_StkStub
freeReg ILIT(REG_StkStub) = _FALSE_
#endif
freeReg n
#ifdef REG_Dbl1
  | n _EQ_ (ILIT(REG_Dbl1) _ADD_ ILIT(1)) = _FALSE_
#endif
#ifdef REG_Dbl2
  | n _EQ_ (ILIT(REG_Dbl2) _ADD_ ILIT(1)) = _FALSE_
#endif
  | otherwise = _TRUE_

reservedRegs :: [Int]
reservedRegs = [NCG_Reserved_I1, NCG_Reserved_I2,
    	    	NCG_Reserved_F1, NCG_Reserved_F2,
    	    	NCG_Reserved_D1, NCG_Reserved_D2]

\end{code}

