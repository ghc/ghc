%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[AlphaCode]{The Native (Alpha) Machine Code}

\begin{code}
#include "HsVersions.h"

module AlphaCode (
	Addr(..),Cond(..),Imm(..),RI(..),Size(..),
	AlphaCode(..),AlphaInstr(..),AlphaRegs,
	strImmLab,

	printLabeledCodes,

	baseRegOffset, stgRegMap, callerSaves,

	kindToSize,

	v0, f0, sp, ra, pv, gp, zero, argRegs,

	freeRegs, reservedRegs,

	-- and, for self-sufficiency ...
	CLabel, CodeSegment, OrdList, PrimKind, Reg, UniqSet(..),
	UniqFM, FiniteMap, Unique, MagicId, CSeq, BitSet
    ) where

IMPORT_Trace

import AbsCSyn	    ( MagicId(..) )
import AsmRegAlloc  ( MachineCode(..), MachineRegisters(..), FutureLive(..),
		      Reg(..), RegUsage(..), RegLiveness(..)
		    )
import BitSet
import CLabelInfo   ( CLabel, pprCLabel, externallyVisibleCLabel, charToC )
import CgCompInfo   ( mAX_Double_REG, mAX_Float_REG, mAX_Vanilla_REG )
import FiniteMap
import Maybes	    ( Maybe(..), maybeToBool )
import OrdList	    ( OrdList, mkUnitList, flattenOrdList )
import Outputable
import PrimKind	    ( PrimKind(..) )
import UniqSet
import Stix
import Unpretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection[AlphaReg]{The Native (Alpha) Machine Register Table}
%*									*
%************************************************************************

The alpha has 64 registers of interest; 32 integer registers and 32 floating
point registers.  The mapping of STG registers to alpha machine registers
is defined in StgRegs.h.  We are, of course, prepared for any eventuality.

\begin{code}

fReg :: Int -> Int
fReg x = (32 + x)

v0, f0, ra, pv, gp, sp, zero :: Reg
v0   = realReg 0
f0   = realReg (fReg 0)
ra   = FixedReg ILIT(26)
pv   = t12
gp   = FixedReg ILIT(29)
sp   = FixedReg ILIT(30)
zero = FixedReg ILIT(31)

t9, t10, t11, t12 :: Reg
t9  = realReg 23
t10 = realReg 24
t11 = realReg 25
t12 = realReg 27

argRegs :: [(Reg, Reg)]
argRegs = [(realReg i, realReg (fReg i)) | i <- [16..21]]

realReg :: Int -> Reg
realReg n@IBOX(i) = if _IS_TRUE_(freeReg i) then MappedReg i else FixedReg i

\end{code}

%************************************************************************
%*									*
\subsection[TheAlphaCode]{The datatype for alpha assembly language}
%*									*
%************************************************************************

Here is a definition of the Alpha assembly language.

\begin{code}

data Imm = ImmInt Int
	 | ImmInteger Integer	      -- Sigh.
	 | ImmCLbl CLabel	      -- AbstractC Label (with baggage)
	 | ImmLab  Unpretty	      -- Simple string label
	 deriving ()

strImmLab s = ImmLab (uppStr s)

data Addr = AddrImm Imm
	  | AddrReg Reg
	  | AddrRegImm Reg Imm
	  deriving ()

data Cond = EQ			      -- For CMP and BI
	  | LT			      -- For CMP and BI
	  | LE			      -- For CMP and BI
	  | ULT			      -- For CMP only
	  | ULE			      -- For CMP only
	  | NE			      -- For BI only
	  | GT			      -- For BI only
	  | GE			      -- For BI only
	  | ALWAYS		      -- For BI (same as BR)
	  | NEVER		      -- For BI (null instruction)
	  deriving ()

data RI = RIReg Reg
	| RIImm Imm
	deriving ()

data Size = B
	  | BU
	  | W
	  | WU
	  | L
	  | Q
	  | FF
	  | DF
	  | GF
	  | SF
	  | TF
	  deriving ()

data AlphaInstr =

-- Loads and stores.

		LD	      Size Reg Addr -- size, dst, src
	      | LDA	      Reg Addr	    -- dst, src
	      | LDAH	      Reg Addr	    -- dst, src
	      | LDGP	      Reg Addr	    -- dst, src
	      | LDI	      Size Reg Imm  -- size, dst, src
	      | ST	      Size Reg Addr -- size, src, dst

-- Int Arithmetic.

	      | CLR	      Reg		    -- dst
	      | ABS	      Size RI Reg	    -- size, src, dst
	      | NEG	      Size Bool RI Reg	    -- size, overflow, src, dst
	      | ADD	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SADD	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | SUB	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | SSUB	      Size Size Reg RI Reg  -- size, scale, src, src, dst
	      | MUL	      Size Bool Reg RI Reg  -- size, overflow, src, src, dst
	      | DIV	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst
	      | REM	      Size Bool Reg RI Reg  -- size, unsigned, src, src, dst

-- Simple bit-twiddling.

	      | NOT	      RI Reg
	      | AND	      Reg RI Reg
	      | ANDNOT	      Reg RI Reg
	      | OR	      Reg RI Reg
	      | ORNOT	      Reg RI Reg
	      | XOR	      Reg RI Reg
	      | XORNOT	      Reg RI Reg
	      | SLL	      Reg RI Reg
	      | SRL	      Reg RI Reg
	      | SRA	      Reg RI Reg

	      | ZAP	      Reg RI Reg
	      | ZAPNOT	      Reg RI Reg

	      | NOP

-- Comparison

	      | CMP	      Cond Reg RI Reg

-- Float Arithmetic.

	      | FCLR	      Reg
	      | FABS	      Reg Reg
	      | FNEG	      Size Reg Reg
	      | FADD	      Size Reg Reg Reg
	      | FDIV	      Size Reg Reg Reg
	      | FMUL	      Size Reg Reg Reg
	      | FSUB	      Size Reg Reg Reg
	      | CVTxy	      Size Size Reg Reg
	      | FCMP	      Size Cond Reg Reg Reg
	      | FMOV	      Reg Reg

-- Jumping around.

	      | BI	      Cond Reg Imm
	      | BF	      Cond Reg Imm
	      | BR	      Imm
	      | JMP	      Reg Addr Int
	      | BSR	      Imm Int
	      | JSR	      Reg Addr Int

-- Pseudo-ops.

	      | LABEL CLabel
	      | FUNBEGIN CLabel
	      | FUNEND CLabel
	      | COMMENT FAST_STRING
	      | SEGMENT CodeSegment
	      | ASCII Bool String   -- needs backslash conversion?
	      | DATA Size [Imm]

type AlphaCode	= OrdList AlphaInstr

\end{code}

%************************************************************************
%*									*
\subsection[TheAlphaPretty]{Pretty-printing the Alpha Assembly Language}
%*									*
%************************************************************************

\begin{code}

printLabeledCodes :: PprStyle -> [AlphaInstr] -> Unpretty
printLabeledCodes sty codes = uppAboves (map (pprAlphaInstr sty) codes)

\end{code}

Printing the pieces...

\begin{code}

pprReg :: Reg -> Unpretty

pprReg (FixedReg i) = pprAlphaReg i
pprReg (MappedReg i) = pprAlphaReg i
pprReg other = uppStr (show other)   -- should only happen when debugging

pprAlphaReg :: FAST_INT -> Unpretty
pprAlphaReg i = uppPStr
    (case i of {
	ILIT( 0) -> SLIT("$0");   ILIT( 1) -> SLIT("$1");
	ILIT( 2) -> SLIT("$2");   ILIT( 3) -> SLIT("$3");
	ILIT( 4) -> SLIT("$4");   ILIT( 5) -> SLIT("$5");
	ILIT( 6) -> SLIT("$6");   ILIT( 7) -> SLIT("$7");
	ILIT( 8) -> SLIT("$8");   ILIT( 9) -> SLIT("$9");
	ILIT(10) -> SLIT("$10");  ILIT(11) -> SLIT("$11");
	ILIT(12) -> SLIT("$12");  ILIT(13) -> SLIT("$13");
	ILIT(14) -> SLIT("$14");  ILIT(15) -> SLIT("$15");
	ILIT(16) -> SLIT("$16");  ILIT(17) -> SLIT("$17");
	ILIT(18) -> SLIT("$18");  ILIT(19) -> SLIT("$19");
	ILIT(20) -> SLIT("$20");  ILIT(21) -> SLIT("$21");
	ILIT(22) -> SLIT("$22");  ILIT(23) -> SLIT("$23");
	ILIT(24) -> SLIT("$24");  ILIT(25) -> SLIT("$25");
	ILIT(26) -> SLIT("$26");  ILIT(27) -> SLIT("$27");
	ILIT(28) -> SLIT("$28");  ILIT(29) -> SLIT("$29");
	ILIT(30) -> SLIT("$30");  ILIT(31) -> SLIT("$31");
	ILIT(32) -> SLIT("$f0");  ILIT(33) -> SLIT("$f1");
	ILIT(34) -> SLIT("$f2");  ILIT(35) -> SLIT("$f3");
	ILIT(36) -> SLIT("$f4");  ILIT(37) -> SLIT("$f5");
	ILIT(38) -> SLIT("$f6");  ILIT(39) -> SLIT("$f7");
	ILIT(40) -> SLIT("$f8");  ILIT(41) -> SLIT("$f9");
	ILIT(42) -> SLIT("$f10"); ILIT(43) -> SLIT("$f11");
	ILIT(44) -> SLIT("$f12"); ILIT(45) -> SLIT("$f13");
	ILIT(46) -> SLIT("$f14"); ILIT(47) -> SLIT("$f15");
	ILIT(48) -> SLIT("$f16"); ILIT(49) -> SLIT("$f17");
	ILIT(50) -> SLIT("$f18"); ILIT(51) -> SLIT("$f19");
	ILIT(52) -> SLIT("$f20"); ILIT(53) -> SLIT("$f21");
	ILIT(54) -> SLIT("$f22"); ILIT(55) -> SLIT("$f23");
	ILIT(56) -> SLIT("$f24"); ILIT(57) -> SLIT("$f25");
	ILIT(58) -> SLIT("$f26"); ILIT(59) -> SLIT("$f27");
	ILIT(60) -> SLIT("$f28"); ILIT(61) -> SLIT("$f29");
	ILIT(62) -> SLIT("$f30"); ILIT(63) -> SLIT("$f31");
	_ -> SLIT("very naughty alpha register")
    })

pprCond :: Cond -> Unpretty
pprCond EQ  = uppPStr SLIT("eq")
pprCond LT  = uppPStr SLIT("lt")
pprCond LE  = uppPStr SLIT("le")
pprCond ULT = uppPStr SLIT("ult")
pprCond ULE = uppPStr SLIT("ule")
pprCond NE  = uppPStr SLIT("ne")
pprCond GT  = uppPStr SLIT("gt")
pprCond GE  = uppPStr SLIT("ge")

pprImm :: PprStyle -> Imm -> Unpretty

pprImm sty (ImmInt i) = uppInt i
pprImm sty (ImmInteger i) = uppInteger i

pprImm sty (ImmCLbl l) = pprCLabel sty l

pprImm sty (ImmLab s) = s

pprAddr :: PprStyle -> Addr -> Unpretty
pprAddr sty (AddrReg reg) = uppBesides [uppLparen, pprReg reg, uppRparen]

pprAddr sty (AddrImm imm) = pprImm sty imm

pprAddr sty (AddrRegImm r1 imm) =
    uppBesides [
	pprImm sty imm,
	uppLparen,
	pprReg r1,
	uppRparen
    ]

pprRI :: PprStyle -> RI -> Unpretty
pprRI sty (RIReg r) = pprReg r
pprRI sty (RIImm r) = pprImm sty r

pprRegRIReg :: PprStyle -> FAST_STRING -> Reg -> RI -> Reg -> Unpretty
pprRegRIReg sty name reg1 ri reg2 =
    uppBesides [
 	uppChar '\t',
	uppPStr name,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprSizeRegRegReg :: FAST_STRING -> Size -> Reg -> Reg -> Reg -> Unpretty
pprSizeRegRegReg name size reg1 reg2 reg3 =
    uppBesides [
	uppChar '\t',
	uppPStr name,
	pprSize size,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprReg reg2,
	uppComma,
	pprReg reg3
    ]

pprSize :: Size -> Unpretty
pprSize x = uppPStr
    (case x of
	 B  -> SLIT("b")
	 BU -> SLIT("bu")
	 W  -> SLIT("w")
	 WU -> SLIT("wu")
	 L  -> SLIT("l")
	 Q  -> SLIT("q")
	 FF -> SLIT("f")
	 DF -> SLIT("d")
	 GF -> SLIT("g")
	 SF -> SLIT("s")
	 TF -> SLIT("t")
    )

pprAlphaInstr :: PprStyle -> AlphaInstr -> Unpretty

pprAlphaInstr sty (LD size reg addr) =
    uppBesides [
	uppPStr SLIT("\tld"),
	pprSize size,
	uppChar '\t',
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (LDA reg addr) =
    uppBesides [
	uppPStr SLIT("\tlda\t"),
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (LDAH reg addr) =
    uppBesides [
	uppPStr SLIT("\tldah\t"),
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (LDGP reg addr) =
    uppBesides [
	uppPStr SLIT("\tldgp\t"),
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (LDI size reg imm) =
    uppBesides [
	uppPStr SLIT("\tldi"),
	pprSize size,
	uppChar '\t',
	pprReg reg,
	uppComma,
	pprImm sty imm
    ]

pprAlphaInstr sty (ST size reg addr) =
    uppBesides [
	uppPStr SLIT("\tst"),
	pprSize size,
	uppChar '\t',
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (CLR reg) =
    uppBesides [
	uppPStr SLIT("\tclr\t"),
	pprReg reg
    ]

pprAlphaInstr sty (ABS size ri reg) =
    uppBesides [
	uppPStr SLIT("\tabs"),
	pprSize size,
	uppChar '\t',
	pprRI sty ri,
	uppComma,
	pprReg reg
    ]

pprAlphaInstr sty (NEG size ov ri reg) =
    uppBesides [
	uppPStr SLIT("\tneg"),
	pprSize size,
	if ov then uppPStr SLIT("v\t") else uppChar '\t',
	pprRI sty ri,
	uppComma,
	pprReg reg
    ]

pprAlphaInstr sty (ADD size ov reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\tadd"),
	pprSize size,
	if ov then uppPStr SLIT("v\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (SADD size scale reg1 ri reg2) =
    uppBesides [
	uppPStr (case scale of {L -> SLIT("\ts4"); Q -> SLIT("\ts8")}),
	uppPStr SLIT("add"),
	pprSize size,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (SUB size ov reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\tsub"),
	pprSize size,
	if ov then uppPStr SLIT("v\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (SSUB size scale reg1 ri reg2) =
    uppBesides [
	uppPStr (case scale of {L -> SLIT("\ts4"); Q -> SLIT("\ts8")}),
	uppPStr SLIT("sub"),
	pprSize size,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (MUL size ov reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\tmul"),
	pprSize size,
	if ov then uppPStr SLIT("v\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (DIV size uns reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\tdiv"),
	pprSize size,
	if uns then uppPStr SLIT("u\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (REM size uns reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\trem"),
	pprSize size,
	if uns then uppPStr SLIT("u\t") else uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (NOT ri reg) =
    uppBesides [
	uppPStr SLIT("\tnot"),
	uppChar '\t',
	pprRI sty ri,
	uppComma,
	pprReg reg
    ]

pprAlphaInstr sty (AND reg1 ri reg2) = pprRegRIReg sty SLIT("and") reg1 ri reg2
pprAlphaInstr sty (ANDNOT reg1 ri reg2) = pprRegRIReg sty SLIT("andnot") reg1 ri reg2
pprAlphaInstr sty (OR reg1 ri reg2) = pprRegRIReg sty SLIT("or") reg1 ri reg2
pprAlphaInstr sty (ORNOT reg1 ri reg2) = pprRegRIReg sty SLIT("ornot") reg1 ri reg2
pprAlphaInstr sty (XOR reg1 ri reg2) = pprRegRIReg sty SLIT("xor") reg1 ri reg2
pprAlphaInstr sty (XORNOT reg1 ri reg2) = pprRegRIReg sty SLIT("xornot") reg1 ri reg2

pprAlphaInstr sty (SLL reg1 ri reg2) = pprRegRIReg sty SLIT("sll") reg1 ri reg2
pprAlphaInstr sty (SRL reg1 ri reg2) = pprRegRIReg sty SLIT("srl") reg1 ri reg2
pprAlphaInstr sty (SRA reg1 ri reg2) = pprRegRIReg sty SLIT("sra") reg1 ri reg2

pprAlphaInstr sty (ZAP reg1 ri reg2) = pprRegRIReg sty SLIT("zap") reg1 ri reg2
pprAlphaInstr sty (ZAPNOT reg1 ri reg2) = pprRegRIReg sty SLIT("zapnot") reg1 ri reg2

pprAlphaInstr sty (NOP) = uppPStr SLIT("\tnop")

pprAlphaInstr sty (CMP cond reg1 ri reg2) =
    uppBesides [
	uppPStr SLIT("\tcmp"),
	pprCond cond,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprRI sty ri,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (FCLR reg) =
    uppBesides [
	uppPStr SLIT("\tfclr\t"),
	pprReg reg
    ]

pprAlphaInstr sty (FABS reg1 reg2) =
    uppBesides [
	uppPStr SLIT("\tfabs\t"),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (FNEG size reg1 reg2) =
    uppBesides [
	uppPStr SLIT("\tneg"),
	pprSize size,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (FADD size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("add") size reg1 reg2 reg3
pprAlphaInstr sty (FDIV size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("div") size reg1 reg2 reg3
pprAlphaInstr sty (FMUL size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("mul") size reg1 reg2 reg3
pprAlphaInstr sty (FSUB size reg1 reg2 reg3) = pprSizeRegRegReg SLIT("sub") size reg1 reg2 reg3

pprAlphaInstr sty (CVTxy size1 size2 reg1 reg2) =
    uppBesides [
	uppPStr SLIT("\tcvt"),
	pprSize size1,
	case size2 of {Q -> uppPStr SLIT("qc"); _ -> pprSize size2},
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (FCMP size cond reg1 reg2 reg3) =
    uppBesides [
	uppPStr SLIT("\tcmp"),
	pprSize size,
	pprCond cond,
	uppChar '\t',
	pprReg reg1,
	uppComma,
	pprReg reg2,
	uppComma,
	pprReg reg3
    ]

pprAlphaInstr sty (FMOV reg1 reg2) =
    uppBesides [
	uppPStr SLIT("\tfmov\t"),
	pprReg reg1,
	uppComma,
	pprReg reg2
    ]

pprAlphaInstr sty (BI ALWAYS reg lab) = pprAlphaInstr sty (BR lab)

pprAlphaInstr sty (BI NEVER reg lab) = uppNil

pprAlphaInstr sty (BI cond reg lab) =
    uppBesides [
	uppPStr SLIT("\tb"),
	pprCond cond,
	uppChar '\t',
	pprReg reg,
	uppComma,
	pprImm sty lab
    ]

pprAlphaInstr sty (BF cond reg lab) =
    uppBesides [
	uppPStr SLIT("\tfb"),
	pprCond cond,
	uppChar '\t',
	pprReg reg,
	uppComma,
	pprImm sty lab
    ]

pprAlphaInstr sty (BR lab) =
    uppBeside (uppPStr SLIT("\tbr\t")) (pprImm sty lab)

pprAlphaInstr sty (JMP reg addr hint) =
    uppBesides [
	uppPStr SLIT("\tjmp\t"),
	pprReg reg,
	uppComma,
	pprAddr sty addr,
	uppComma,
	uppInt hint
    ]

pprAlphaInstr sty (BSR imm n) =
    uppBeside (uppPStr SLIT("\tbsr\t")) (pprImm sty imm)

pprAlphaInstr sty (JSR reg addr n) =
    uppBesides [
	uppPStr SLIT("\tjsr\t"),
	pprReg reg,
	uppComma,
	pprAddr sty addr
    ]

pprAlphaInstr sty (LABEL clab) =
    uppBesides [
	if (externallyVisibleCLabel clab) then
	    uppBesides [uppPStr SLIT("\t.globl\t"), pprLab, uppChar '\n']
	else
	    uppNil,
	pprLab,
	uppChar ':'
    ]
    where pprLab = pprCLabel sty clab

pprAlphaInstr sty (FUNBEGIN clab) =
    uppBesides [
	if (externallyVisibleCLabel clab) then
	    uppBesides [uppPStr SLIT("\t.globl\t"), pprLab, uppChar '\n']
	else
	    uppNil,
	uppPStr SLIT("\t.ent "),
	pprLab,
	uppChar '\n',
	pprLab,
	pp_ldgp,
	pprLab,
	pp_frame
    ]
    where
	pprLab = pprCLabel sty clab
#ifdef USE_FAST_STRINGS
	pp_ldgp  = uppPStr (_packCString (A# ":\n\tldgp $29,0($27)\n"#))
	pp_frame = uppPStr (_packCString (A# "..ng:\n\t.frame $30,4240,$26,0\n\t.prologue 1"#))
#else
	pp_ldgp  = uppStr ":\n\tldgp $29,0($27)\n"
	pp_frame = uppStr "..ng:\n\t.frame $30,4240,$26,0\n\t.prologue 1"
#endif

pprAlphaInstr sty (FUNEND clab) =
    uppBeside (uppPStr SLIT("\t.align 4\n\t.end ")) (pprCLabel sty clab)

pprAlphaInstr sty (COMMENT s) = uppBeside (uppPStr SLIT("\t# ")) (uppPStr s)

pprAlphaInstr sty (SEGMENT TextSegment)
    = uppPStr SLIT("\t.text\n\t.align 3")

pprAlphaInstr sty (SEGMENT DataSegment)
    = uppPStr SLIT("\t.data\n\t.align 3")

pprAlphaInstr sty (ASCII False str) =
    uppBesides [
	uppStr "\t.asciz \"",
	uppStr str,
	uppChar '"'
    ]

pprAlphaInstr sty (ASCII True str) = uppBeside (uppStr "\t.ascii \"") (asciify str 60)
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

pprAlphaInstr sty (DATA s xs) = uppInterleave (uppChar '\n') (map pp_item xs)
    where pp_item x = case s of
	    B  -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    BU -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    W  -> uppBeside (uppPStr SLIT("\t.word\t")) (pprImm sty x)
	    WU -> uppBeside (uppPStr SLIT("\t.word\t")) (pprImm sty x)
	    L  -> uppBeside (uppPStr SLIT("\t.long\t")) (pprImm sty x)
	    Q  -> uppBeside (uppPStr SLIT("\t.quad\t")) (pprImm sty x)
	    FF -> uppBeside (uppPStr SLIT("\t.f_floating\t")) (pprImm sty x)
	    DF -> uppBeside (uppPStr SLIT("\t.d_floating\t")) (pprImm sty x)
	    GF -> uppBeside (uppPStr SLIT("\t.g_floating\t")) (pprImm sty x)
	    SF -> uppBeside (uppPStr SLIT("\t.s_floating\t")) (pprImm sty x)
	    TF -> uppBeside (uppPStr SLIT("\t.t_floating\t")) (pprImm sty x)

\end{code}

%************************************************************************
%*									*
\subsection[Schedule]{Register allocation information}
%*									*
%************************************************************************

\begin{code}

data AlphaRegs = SRegs BitSet BitSet

instance MachineRegisters AlphaRegs where
    mkMRegs xs = SRegs (mkBS ints) (mkBS floats')
      where
	(ints, floats) = partition (< 32) xs
	floats' = map (subtract 32) floats

    possibleMRegs FloatKind (SRegs _ floats) = [ x + 32 | x <- listBS floats]
    possibleMRegs DoubleKind (SRegs _ floats) = [ x + 32 | x <- listBS floats]
    possibleMRegs _ (SRegs ints _) = listBS ints

    useMReg (SRegs ints floats) n =
	if n _LT_ ILIT(32) then SRegs (ints `minusBS` singletonBS IBOX(n)) floats
	else SRegs ints (floats `minusBS` singletonBS (IBOX(n _SUB_ ILIT(32))))

    useMRegs (SRegs ints floats) xs =
	SRegs (ints `minusBS` ints')
	      (floats `minusBS` floats')
      where
	SRegs ints' floats' = mkMRegs xs

    freeMReg (SRegs ints floats) n =
	if n _LT_ ILIT(32) then SRegs (ints `unionBS` singletonBS IBOX(n)) floats
	else SRegs ints (floats `unionBS` singletonBS (IBOX(n _SUB_ ILIT(32))))

    freeMRegs (SRegs ints floats) xs =
	SRegs (ints `unionBS` ints')
	      (floats `unionBS` floats')
      where
	SRegs ints' floats' = mkMRegs xs

instance MachineCode AlphaInstr where
    -- Alas, we don't do anything clever with our OrdLists
--OLD:
--  flatten = flattenOrdList

    regUsage = alphaRegUsage
    regLiveness = alphaRegLiveness
    patchRegs = alphaPatchRegs

    -- We spill just below the frame pointer, leaving two words per spill location.
    spillReg dyn (MemoryReg i pk) = mkUnitList (ST (kindToSize pk) dyn (spRel i))
    loadReg (MemoryReg i pk) dyn = mkUnitList (LD (kindToSize pk) dyn (spRel i))

spRel :: Int -> Addr
spRel n = AddrRegImm sp (ImmInt (n * 8))

kindToSize :: PrimKind -> Size
kindToSize PtrKind	    = Q
kindToSize CodePtrKind	    = Q
kindToSize DataPtrKind	    = Q
kindToSize RetKind	    = Q
kindToSize InfoPtrKind	    = Q
kindToSize CostCentreKind   = Q
kindToSize CharKind	    = BU
kindToSize IntKind	    = Q
kindToSize WordKind	    = Q
kindToSize AddrKind	    = Q
kindToSize FloatKind	    = TF
kindToSize DoubleKind	    = TF
kindToSize ArrayKind	    = Q
kindToSize ByteArrayKind    = Q
kindToSize StablePtrKind    = Q
kindToSize MallocPtrKind    = Q

\end{code}

@alphaRegUsage@ returns the sets of src and destination registers used by
a particular instruction.  Machine registers that are pre-allocated
to stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.	 (We wouldn't want them to end up on
the free list!)

\begin{code}

alphaRegUsage :: AlphaInstr -> RegUsage
alphaRegUsage instr = case instr of
    LD B reg addr	-> usage (regAddr addr, [reg, t9])
    LD BU reg addr	-> usage (regAddr addr, [reg, t9])
    LD W reg addr	-> usage (regAddr addr, [reg, t9])
    LD WU reg addr	-> usage (regAddr addr, [reg, t9])
    LD sz reg addr	-> usage (regAddr addr, [reg])
    LDA reg addr	-> usage (regAddr addr, [reg])
    LDAH reg addr	-> usage (regAddr addr, [reg])
    LDGP reg addr	-> usage (regAddr addr, [reg])
    LDI sz reg imm	-> usage ([], [reg])
    ST B reg addr	-> usage (reg : regAddr addr, [t9, t10])
    ST W reg addr	-> usage (reg : regAddr addr, [t9, t10])
    ST sz reg addr	-> usage (reg : regAddr addr, [])
    CLR reg		-> usage ([], [reg])
    ABS sz ri reg	-> usage (regRI ri, [reg])
    NEG sz ov ri reg	-> usage (regRI ri, [reg])
    ADD sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SADD sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    SUB sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SSUB sz sc r1 ar r2 -> usage (r1 : regRI ar, [r2])
    MUL sz ov r1 ar r2	-> usage (r1 : regRI ar, [r2])
    DIV sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    REM sz un r1 ar r2	-> usage (r1 : regRI ar, [r2, t9, t10, t11, t12])
    NOT ri reg		-> usage (regRI ri, [reg])
    AND r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ANDNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    OR r1 ar r2		-> usage (r1 : regRI ar, [r2])
    ORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XOR r1 ar r2	-> usage (r1 : regRI ar, [r2])
    XORNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SLL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRL r1 ar r2	-> usage (r1 : regRI ar, [r2])
    SRA r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAP r1 ar r2	-> usage (r1 : regRI ar, [r2])
    ZAPNOT r1 ar r2	-> usage (r1 : regRI ar, [r2])
    CMP co r1 ar r2	-> usage (r1 : regRI ar, [r2])
    FCLR reg		-> usage ([], [reg])
    FABS r1 r2		-> usage ([r1], [r2])
    FNEG sz r1 r2	-> usage ([r1], [r2])
    FADD sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FDIV sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FMUL sz r1 r2 r3	-> usage ([r1, r2], [r3])
    FSUB sz r1 r2 r3	-> usage ([r1, r2], [r3])
    CVTxy sz1 sz2 r1 r2 -> usage ([r1], [r2])
    FCMP sz co r1 r2 r3 -> usage ([r1, r2], [r3])
    FMOV r1 r2		-> usage ([r1], [r2])


    -- We assume that all local jumps will be BI/BF/BR.	 JMP must be out-of-line.
    BI cond reg lbl	-> usage ([reg], [])
    BF cond reg lbl	-> usage ([reg], [])
    JMP reg addr hint	-> RU (mkUniqSet (filter interesting (regAddr addr))) freeSet

    BSR _ n		-> RU (argSet n) callClobberedSet
    JSR reg addr n	-> RU (argSet n) callClobberedSet

    _			-> noUsage

  where
    usage (src, dst) = RU (mkUniqSet (filter interesting src))
			  (mkUniqSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

    regAddr (AddrReg r1)      = [r1]
    regAddr (AddrRegImm r1 _) = [r1]
    regAddr (AddrImm _)	      = []

    regRI (RIReg r) = [r]
    regRI  _	= []

freeRegs :: [Reg]
freeRegs = freeMappedRegs [0..63]

freeMappedRegs :: [Int] -> [Reg]

freeMappedRegs nums
  = foldr free [] nums
  where
    free IBOX(i) acc
      = if _IS_TRUE_(freeReg i) then (MappedReg i) : acc else acc

freeSet :: UniqSet Reg
freeSet = mkUniqSet freeRegs

noUsage :: RegUsage
noUsage = RU emptyUniqSet emptyUniqSet

--OLD:
--endUsage :: RegUsage
--endUsage = RU emptyUniqSet freeSet

-- Color me CAF-like
argSet :: Int -> UniqSet Reg
argSet 0 = emptyUniqSet
argSet 1 = mkUniqSet (freeMappedRegs [16, fReg 16])
argSet 2 = mkUniqSet (freeMappedRegs [16, 17, fReg 16, fReg 17])
argSet 3 = mkUniqSet (freeMappedRegs [16, 17, 18, fReg 16, fReg 17, fReg 18])
argSet 4 = mkUniqSet (freeMappedRegs [16, 17, 18, 19, fReg 16, fReg 17, fReg 18, fReg 19])
argSet 5 = mkUniqSet (freeMappedRegs [16, 17, 18, 19, 20, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20])
argSet 6 = mkUniqSet (freeMappedRegs [16, 17, 18, 19, 20, 21, fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21])

callClobberedSet :: UniqSet Reg
callClobberedSet = mkUniqSet callClobberedRegs
  where
    callClobberedRegs
      = freeMappedRegs
	  [0, 1, 2, 3, 4, 5, 6, 7, 8,
	   16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
	   fReg 0, fReg 1, fReg 10, fReg 11, fReg 12, fReg 13, fReg 14, fReg 15,
	   fReg 16, fReg 17, fReg 18, fReg 19, fReg 20, fReg 21, fReg 22, fReg 23,
	   fReg 24, fReg 25, fReg 26, fReg 27, fReg 28, fReg 29, fReg 30]

\end{code}

@alphaRegLiveness@ takes future liveness information and modifies it according to
the semantics of branches and labels.  (An out-of-line branch clobbers the liveness
passed back by the following instruction; a forward local branch passes back the
liveness from the target label; a conditional branch merges the liveness from the
target and the liveness from its successor; a label stashes away the current liveness
in the future liveness environment).

\begin{code}
alphaRegLiveness :: AlphaInstr -> RegLiveness -> RegLiveness
alphaRegLiveness instr info@(RL live future@(FL all env)) = case instr of

    -- We assume that all local jumps will be BI/BF.  JMP must be out-of-line.

    BR (ImmCLbl lbl)	 -> RL (lookup lbl) future
    BI _ _ (ImmCLbl lbl) -> RL (lookup lbl `unionUniqSets` live) future
    BF _ _ (ImmCLbl lbl) -> RL (lookup lbl `unionUniqSets` live) future
    JMP _ _ _		 -> RL emptyUniqSet future
    BSR _ _		 -> RL live future
    JSR _ _ _		 -> RL live future
    LABEL lbl		 -> RL live (FL (all `unionUniqSets` live) (addToFM env lbl live))
    _			 -> info  

  where
    lookup lbl = case lookupFM env lbl of
	Just regs -> regs
	Nothing -> trace ("Missing " ++ (uppShow 80 (pprCLabel (PprForAsm (\_->False) False id) lbl)) ++
			  " in future?") emptyUniqSet

\end{code}

@alphaPatchRegs@ takes an instruction (possibly with
MemoryReg/UnmappedReg registers) and changes all register references
according to the supplied environment.

\begin{code}

alphaPatchRegs :: AlphaInstr -> (Reg -> Reg) -> AlphaInstr
alphaPatchRegs instr env = case instr of
    LD sz reg addr -> LD sz (env reg) (fixAddr addr)
    LDA reg addr -> LDA (env reg) (fixAddr addr)
    LDAH reg addr -> LDAH (env reg) (fixAddr addr)
    LDGP reg addr -> LDGP (env reg) (fixAddr addr)
    LDI sz reg imm -> LDI sz (env reg) imm
    ST sz reg addr -> ST sz (env reg) (fixAddr addr)
    CLR reg -> CLR (env reg)
    ABS sz ar reg -> ABS sz (fixRI ar) (env reg)
    NEG sz ov ar reg -> NEG sz ov (fixRI ar) (env reg)
    ADD sz ov r1 ar r2 -> ADD sz ov (env r1) (fixRI ar) (env r2)
    SADD sz sc r1 ar r2 -> SADD sz sc (env r1) (fixRI ar) (env r2)
    SUB sz ov r1 ar r2 -> SUB sz ov (env r1) (fixRI ar) (env r2)
    SSUB sz sc r1 ar r2 -> SSUB sz sc (env r1) (fixRI ar) (env r2)
    MUL sz ov r1 ar r2 -> MUL sz ov (env r1) (fixRI ar) (env r2)
    DIV sz un r1 ar r2 -> DIV sz un (env r1) (fixRI ar) (env r2)
    REM sz un r1 ar r2 -> REM sz un (env r1) (fixRI ar) (env r2)
    NOT ar reg -> NOT (fixRI ar) (env reg)
    AND r1 ar r2 -> AND (env r1) (fixRI ar) (env r2)
    ANDNOT r1 ar r2 -> ANDNOT (env r1) (fixRI ar) (env r2)
    OR r1 ar r2 -> OR (env r1) (fixRI ar) (env r2)
    ORNOT r1 ar r2 -> ORNOT (env r1) (fixRI ar) (env r2)
    XOR r1 ar r2 -> XOR (env r1) (fixRI ar) (env r2)
    XORNOT r1 ar r2 -> XORNOT (env r1) (fixRI ar) (env r2)
    SLL r1 ar r2 -> SLL (env r1) (fixRI ar) (env r2)
    SRL r1 ar r2 -> SRL (env r1) (fixRI ar) (env r2)
    SRA r1 ar r2 -> SRA (env r1) (fixRI ar) (env r2)
    ZAP r1 ar r2 -> ZAP (env r1) (fixRI ar) (env r2)
    ZAPNOT r1 ar r2 -> ZAPNOT (env r1) (fixRI ar) (env r2)
    CMP co r1 ar r2 -> CMP co (env r1) (fixRI ar) (env r2)
    FCLR reg -> FCLR (env reg)
    FABS r1 r2 -> FABS (env r1) (env r2)
    FNEG s r1 r2 -> FNEG s (env r1) (env r2)
    FADD s r1 r2 r3 -> FADD s (env r1) (env r2) (env r3)
    FDIV s r1 r2 r3 -> FDIV s (env r1) (env r2) (env r3)
    FMUL s r1 r2 r3 -> FMUL s (env r1) (env r2) (env r3)
    FSUB s r1 r2 r3 -> FSUB s (env r1) (env r2) (env r3)
    CVTxy s1 s2 r1 r2 -> CVTxy s1 s2 (env r1) (env r2)
    FCMP s co r1 r2 r3 -> FCMP s co (env r1) (env r2) (env r3)
    FMOV r1 r2 -> FMOV (env r1) (env r2)
    BI cond reg lbl -> BI cond (env reg) lbl
    BF cond reg lbl -> BF cond (env reg) lbl
    JMP reg addr hint -> JMP (env reg) (fixAddr addr) hint
    JSR reg addr i -> JSR (env reg) (fixAddr addr) i
    _ -> instr

  where
    fixAddr (AddrReg r1)       = AddrReg (env r1)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i
    fixAddr other	       = other

    fixRI (RIReg r) = RIReg (env r)
    fixRI other	= other

\end{code}

If you value your sanity, do not venture below this line.

\begin{code}

-- platform.h is generate and tells us what the target architecture is
#include "../../includes/platform.h"
#include "../../includes/MachRegs.h"
#include "../../includes/alpha-dec-osf1.h"

-- Redefine the literals used for Alpha floating point register names
-- in the header files.	 Gag me with a spoon, eh?

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
baseRegOffset StkOReg		     = OFFSET_StkO
baseRegOffset (VanillaReg _ ILIT(1)) = OFFSET_R1
baseRegOffset (VanillaReg _ ILIT(2)) = OFFSET_R2
baseRegOffset (VanillaReg _ ILIT(3)) = OFFSET_R3
baseRegOffset (VanillaReg _ ILIT(4)) = OFFSET_R4
baseRegOffset (VanillaReg _ ILIT(5)) = OFFSET_R5
baseRegOffset (VanillaReg _ ILIT(6)) = OFFSET_R6
baseRegOffset (VanillaReg _ ILIT(7)) = OFFSET_R7
baseRegOffset (VanillaReg _ ILIT(8)) = OFFSET_R8
baseRegOffset (FloatReg ILIT(1))     = OFFSET_Flt1
baseRegOffset (FloatReg ILIT(2))     = OFFSET_Flt2
baseRegOffset (FloatReg ILIT(3))     = OFFSET_Flt3
baseRegOffset (FloatReg ILIT(4))     = OFFSET_Flt4
baseRegOffset (DoubleReg ILIT(1))     = OFFSET_Dbl1
baseRegOffset (DoubleReg ILIT(2))     = OFFSET_Dbl2
baseRegOffset TagReg		     = OFFSET_Tag
baseRegOffset RetReg		     = OFFSET_Ret
baseRegOffset SpA		     = OFFSET_SpA
baseRegOffset SuA		     = OFFSET_SuA
baseRegOffset SpB		     = OFFSET_SpB
baseRegOffset SuB		     = OFFSET_SuB
baseRegOffset Hp		     = OFFSET_Hp
baseRegOffset HpLim		     = OFFSET_HpLim
baseRegOffset LivenessReg	     = OFFSET_Liveness
--baseRegOffset ActivityReg	     = OFFSET_Activity
#ifdef DEBUG
baseRegOffset BaseReg		     = panic "baseRegOffset:BaseReg"
baseRegOffset StdUpdRetVecReg	     = panic "baseRegOffset:StgUpdRetVecReg"
baseRegOffset StkStubReg	     = panic "baseRegOffset:StkStubReg"
baseRegOffset CurCostCentre	     = panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg		     = panic "baseRegOffset:VoidReg"
#endif

callerSaves :: MagicId -> Bool
#ifdef CALLER_SAVES_Base
callerSaves BaseReg		= True
#endif
#ifdef CALLER_SAVES_StkO
callerSaves StkOReg		= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg _ ILIT(1))	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg _ ILIT(2))    = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg _ ILIT(3))    = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg _ ILIT(4))    = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg _ ILIT(5))    = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg _ ILIT(6))    = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg _ ILIT(7))	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg _ ILIT(8))    = True
#endif
#ifdef CALLER_SAVES_FltReg1
callerSaves (FloatReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_FltReg2
callerSaves (FloatReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_FltReg3
callerSaves (FloatReg ILIT(3))		= True
#endif
#ifdef CALLER_SAVES_FltReg4
callerSaves (FloatReg ILIT(4))		= True
#endif
#ifdef CALLER_SAVES_DblReg1
callerSaves (DoubleReg ILIT(1))		= True
#endif
#ifdef CALLER_SAVES_DblReg2
callerSaves (DoubleReg ILIT(2))		= True
#endif
#ifdef CALLER_SAVES_Tag
callerSaves TagReg		= True
#endif
#ifdef CALLER_SAVES_Ret
callerSaves RetReg		= True
#endif
#ifdef CALLER_SAVES_SpA
callerSaves SpA			= True
#endif
#ifdef CALLER_SAVES_SuA
callerSaves SuA			= True
#endif
#ifdef CALLER_SAVES_SpB
callerSaves SpB			= True
#endif
#ifdef CALLER_SAVES_SuB
callerSaves SuB			= True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp			= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim		= True
#endif
#ifdef CALLER_SAVES_Liveness
callerSaves LivenessReg		= True
#endif
#ifdef CALLER_SAVES_Activity
--callerSaves ActivityReg		= True
#endif
#ifdef CALLER_SAVES_StdUpdRetVec
callerSaves StdUpdRetVecReg	= True
#endif
#ifdef CALLER_SAVES_StkStub
callerSaves StkStubReg		= True
#endif
callerSaves _			= False

stgRegMap :: MagicId -> Maybe Reg
#ifdef REG_Base
stgRegMap BaseReg	   = Just (FixedReg ILIT(REG_Base))
#endif
#ifdef REG_StkO
stgRegMap StkOReg	   = Just (FixedReg ILIT(REG_StkOReg))
#endif
#ifdef REG_R1
stgRegMap (VanillaReg _ ILIT(1)) = Just (FixedReg ILIT(REG_R1))
#endif
#ifdef REG_R2
stgRegMap (VanillaReg _ ILIT(2)) = Just (FixedReg ILIT(REG_R2))
#endif
#ifdef REG_R3
stgRegMap (VanillaReg _ ILIT(3)) = Just (FixedReg ILIT(REG_R3))
#endif
#ifdef REG_R4
stgRegMap (VanillaReg _ ILIT(4)) = Just (FixedReg ILIT(REG_R4))
#endif
#ifdef REG_R5
stgRegMap (VanillaReg _ ILIT(5)) = Just (FixedReg ILIT(REG_R5))
#endif
#ifdef REG_R6
stgRegMap (VanillaReg _ ILIT(6)) = Just (FixedReg ILIT(REG_R6))
#endif
#ifdef REG_R7
stgRegMap (VanillaReg _ ILIT(7)) = Just (FixedReg ILIT(REG_R7))
#endif
#ifdef REG_R8
stgRegMap (VanillaReg _ ILIT(8)) = Just (FixedReg ILIT(REG_R8))
#endif
#ifdef REG_Flt1
stgRegMap (FloatReg ILIT(1))	   = Just (FixedReg ILIT(REG_Flt1))
#endif
#ifdef REG_Flt2
stgRegMap (FloatReg ILIT(2))	   = Just (FixedReg ILIT(REG_Flt2))
#endif
#ifdef REG_Flt3
stgRegMap (FloatReg ILIT(3))	   = Just (FixedReg ILIT(REG_Flt3))
#endif
#ifdef REG_Flt4
stgRegMap (FloatReg ILIT(4))	   = Just (FixedReg ILIT(REG_Flt4))
#endif
#ifdef REG_Dbl1
stgRegMap (DoubleReg ILIT(1))	   = Just (FixedReg ILIT(REG_Dbl1))
#endif
#ifdef REG_Dbl2
stgRegMap (DoubleReg ILIT(2))	   = Just (FixedReg ILIT(REG_Dbl2))
#endif
#ifdef REG_Tag
stgRegMap TagReg	   = Just (FixedReg ILIT(REG_TagReg))
#endif
#ifdef REG_Ret
stgRegMap RetReg	   = Just (FixedReg ILIT(REG_Ret))
#endif
#ifdef REG_SpA
stgRegMap SpA		   = Just (FixedReg ILIT(REG_SpA))
#endif
#ifdef REG_SuA
stgRegMap SuA		   = Just (FixedReg ILIT(REG_SuA))
#endif
#ifdef REG_SpB
stgRegMap SpB		   = Just (FixedReg ILIT(REG_SpB))
#endif
#ifdef REG_SuB
stgRegMap SuB		   = Just (FixedReg ILIT(REG_SuB))
#endif
#ifdef REG_Hp
stgRegMap Hp		   = Just (FixedReg ILIT(REG_Hp))
#endif
#ifdef REG_HpLim
stgRegMap HpLim		   = Just (FixedReg ILIT(REG_HpLim))
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

With a per-instruction clobber list, we might be able to get some of
these back, but it's probably not worth the hassle.

\begin{code}

freeReg :: FAST_INT -> FAST_BOOL

freeReg ILIT(26) = _FALSE_  -- return address (ra)
freeReg ILIT(28) = _FALSE_  -- reserved for the assembler (at)
freeReg ILIT(29) = _FALSE_  -- global pointer (gp)
freeReg ILIT(30) = _FALSE_  -- stack pointer (sp)
freeReg ILIT(31) = _FALSE_  -- always zero (zero)
freeReg ILIT(63) = _FALSE_  -- always zero (f31)

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
freeReg _ = _TRUE_

reservedRegs :: [Int]
reservedRegs = [NCG_Reserved_I1, NCG_Reserved_I2, NCG_Reserved_F1, NCG_Reserved_F2]

\end{code}
