%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[I386Code]{The Native (I386) Machine Code}

\begin{code}
#define ILIT2(x) ILIT(x)
#include "HsVersions.h"

module I386Code (
	Addr(..), 
        Cond(..), Imm(..), Operand(..), Size(..),
        Base(..), Index(..), Displacement(..),
	I386Code(..),I386Instr(..),I386Regs,
	strImmLit, --UNUSED: strImmLab,
        spRel,

    	printLabeledCodes,

	baseRegOffset, stgRegMap, callerSaves,

	is13Bits, offset,

    	kindToSize,

    	st0, st1, eax, ebx, ecx, edx, esi, edi, ebp, esp,

    	freeRegs, reservedRegs,

	-- and, for self-sufficiency ...
	CLabel, CodeSegment, OrdList, PrimKind, Reg, UniqSet(..),
	UniqFM, FiniteMap, Unique, MagicId, CSeq, BitSet
    ) where

IMPORT_Trace

import AbsCSyn	    	( MagicId(..) )
import AsmRegAlloc  	( MachineCode(..), MachineRegisters(..), FutureLive(..),
    	    	    	  Reg(..), RegUsage(..), RegLiveness(..)
    	    	    	)
import BitSet	 
import CgCompInfo   	( mAX_Double_REG, mAX_Float_REG, mAX_Vanilla_REG )
import CLabelInfo   	( CLabel, pprCLabel, externallyVisibleCLabel, charToC )
import FiniteMap    
import Maybes	    	( Maybe(..), maybeToBool )
import OrdList	    	( OrdList, mkUnitList, flattenOrdList )
import Outputable    
import PrimKind	    	( PrimKind(..) )
import UniqSet
import Stix
import Unpretty
import Util
\end{code}

%************************************************************************
%*									*
\subsection[I386Reg]{The Native (I386) Machine Register Table}
%*									*
%************************************************************************

- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers 8-15 hold extended floating point values.

ToDo: Deal with stg registers that live as offsets from BaseReg!(JSM)

\begin{code}

gReg,fReg :: Int -> Int
gReg x = x
fReg x = (8 + x)

st0, st1, st2, st3, st4, st5, st6, st7, eax, ebx, ecx, edx, esp :: Reg
eax = case (gReg 0) of { IBOX(g0) -> FixedReg g0 }
ebx = case (gReg 1) of { IBOX(g1) -> FixedReg g1 }
ecx = case (gReg 2) of { IBOX(g2) -> FixedReg g2 }
edx = case (gReg 3) of { IBOX(g3) -> FixedReg g3 }
esi = case (gReg 4) of { IBOX(g4) -> FixedReg g4 }
edi = case (gReg 5) of { IBOX(g5) -> FixedReg g5 }
ebp = case (gReg 6) of { IBOX(g6) -> FixedReg g6 }
esp = case (gReg 7) of { IBOX(g7) -> FixedReg g7 }
st0 = realReg  (fReg 0)
st1 = realReg  (fReg 1)
st2 = realReg  (fReg 2)
st3 = realReg  (fReg 3)
st4 = realReg  (fReg 4)
st5 = realReg  (fReg 5)
st6 = realReg  (fReg 6)
st7 = realReg  (fReg 7)

realReg n@IBOX(i) = if _IS_TRUE_(freeReg i) then MappedReg i else FixedReg i

\end{code}

%************************************************************************
%*									*
\subsection[TheI386Code]{The datatype for i386 assembly language}
%*									*
%************************************************************************

Here is a definition of the I386 assembly language.

\begin{code}

data Imm = ImmInt Int
    	 | ImmInteger Integer	      -- Sigh.
	 | ImmCLbl CLabel	      -- AbstractC Label (with baggage)
	 | ImmLab  Unpretty	      -- Simple string label (underscored)
	 | ImmLit Unpretty	      -- Simple string
	 deriving ()

--UNUSED:strImmLab s = ImmLab (uppStr s)
strImmLit s = ImmLit (uppStr s)

data Cond = ALWAYS
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
	  deriving ()


data Size = B
	  | HB
	  | S -- unused ?
	  | L
	  | F
	  | D
	  deriving ()

data Operand = OpReg  Reg	-- register
             | OpImm  Imm	-- immediate value
             | OpAddr Addr	-- memory reference
	     deriving ()

data Addr = Addr Base Index Displacement
          | ImmAddr Imm Int
          -- deriving Eq

type Base         = Maybe Reg
type Index        = Maybe (Reg, Int)	-- Int is 2, 4 or 8
type Displacement = Imm

data I386Instr =

-- Moves.

		MOV	      Size Operand Operand 
	      | MOVZX	      Size Operand Operand -- size is the size of operand 2
	      | MOVSX	      Size Operand Operand -- size is the size of operand 2

-- Load effective address (also a very useful three-operand add instruction :-)

              | LEA           Size Operand Operand

-- Int Arithmetic.

	      | ADD	      Size Operand Operand 
	      | SUB	      Size Operand Operand 

-- Multiplication (signed and unsigned), Division (signed and unsigned),
-- result in %eax, %edx.

	      | IMUL	      Size Operand Operand
	      | IDIV	      Size Operand

-- Simple bit-twiddling.

	      | AND	      Size Operand Operand 
	      | OR	      Size Operand Operand 
	      | XOR	      Size Operand Operand 
	      | NOT	      Size Operand 
	      | NEGI	      Size Operand -- NEG instruction (name clash with Cond)
	      | SHL	      Size Operand Operand -- 1st operand must be an Imm
	      | SAR	      Size Operand Operand -- 1st operand must be an Imm
	      | SHR	      Size Operand Operand -- 1st operand must be an Imm
	      | NOP	      

-- Float Arithmetic. -- ToDo for 386

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

	      | SAHF	      -- stores ah into flags
    	      | FABS          
	      | FADD	      Size Operand -- src
	      | FADDP	      
	      | FIADD	      Size Addr -- src
    	      | FCHS          
    	      | FCOM	      Size Operand -- src
    	      | FCOS          
	      | FDIV	      Size Operand -- src
	      | FDIVP	      
	      | FIDIV	      Size Addr -- src
	      | FDIVR	      Size Operand -- src
	      | FDIVRP	      
	      | FIDIVR	      Size Addr -- src
    	      | FICOM	      Size Addr -- src
    	      | FILD	      Size Addr Reg -- src, dst
    	      | FIST	      Size Addr -- dst
    	      | FLD	      Size Operand -- src
    	      | FLD1          
    	      | FLDZ          
    	      | FMUL	      Size Operand -- src
    	      | FMULP	      
    	      | FIMUL	      Size Addr -- src
    	      | FRNDINT       
    	      | FSIN          
    	      | FSQRT         
    	      | FST	      Size Operand -- dst
    	      | FSTP	      Size Operand -- dst
	      | FSUB	      Size Operand -- src
	      | FSUBP	      
	      | FISUB	      Size Addr -- src
	      | FSUBR	      Size Operand -- src
	      | FSUBRP	      
	      | FISUBR	      Size Addr -- src
	      | FTST          
    	      | FCOMP	      Size Operand -- src
    	      | FUCOMPP	      
	      | FXCH
	      | FNSTSW
	      | FNOP

-- Comparison
        
              | TEST          Size Operand Operand
              | CMP           Size Operand Operand
              | SETCC         Cond Operand

-- Stack Operations.

              | PUSH          Size Operand
              | POP           Size Operand

-- Jumping around.

	      | JMP	      Operand -- target
	      | JXX	      Cond CLabel -- target
	      | CALL	      Imm 

-- Other things.

              | CLTD -- sign extend %eax into %edx:%eax

-- Pseudo-ops.

	      | LABEL CLabel
	      | COMMENT FAST_STRING
	      | SEGMENT CodeSegment
	      | ASCII Bool String   -- needs backslash conversion?
	      | DATA Size [Imm]

type I386Code	= OrdList I386Instr

\end{code}

%************************************************************************
%*									*
\subsection[TheI386Pretty]{Pretty-printing the I386 Assembly Language}
%*									*
%************************************************************************

\begin{code}

printLabeledCodes :: PprStyle -> [I386Instr] -> Unpretty
printLabeledCodes sty codes = uppAboves (map (pprI386Instr sty) codes)

\end{code}

Printing the pieces...

\begin{code}

pprReg :: Size -> Reg -> Unpretty

pprReg s (FixedReg i)  = pprI386Reg s i
pprReg s (MappedReg i) = pprI386Reg s i
pprReg s other         = uppStr (show other) -- should only happen when debugging

pprI386Reg :: Size -> FAST_INT -> Unpretty
pprI386Reg B i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%al");  ILIT( 1) -> SLIT("%bl");
	ILIT( 2) -> SLIT("%cl");  ILIT( 3) -> SLIT("%dl");
	_ -> SLIT("very naughty I386 byte register")
    })

pprI386Reg HB i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%ah");  ILIT( 1) -> SLIT("%bh");
	ILIT( 2) -> SLIT("%ch");  ILIT( 3) -> SLIT("%dh");
	_ -> SLIT("very naughty I386 high byte register")
    })

pprI386Reg S i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%ax");  ILIT( 1) -> SLIT("%bx");
	ILIT( 2) -> SLIT("%cx");  ILIT( 3) -> SLIT("%dx");
        ILIT( 4) -> SLIT("%si");  ILIT( 5) -> SLIT("%di");
	ILIT( 6) -> SLIT("%bp");  ILIT( 7) -> SLIT("%sp");
	_ -> SLIT("very naughty I386 word register")
    })

pprI386Reg L i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%eax");  ILIT( 1) -> SLIT("%ebx");
	ILIT( 2) -> SLIT("%ecx");  ILIT( 3) -> SLIT("%edx");
        ILIT( 4) -> SLIT("%esi");  ILIT( 5) -> SLIT("%edi");
	ILIT( 6) -> SLIT("%ebp");  ILIT( 7) -> SLIT("%esp");
	_ -> SLIT("very naughty I386 double word register")
    })

pprI386Reg F i = uppPStr
    (case i of {
--ToDo: rm these
        ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
        ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
    })

pprI386Reg D i = uppPStr
    (case i of {
--ToDo: rm these
        ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
        ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
    })

pprCond :: Cond -> Unpretty -- ToDo
pprCond x = uppPStr
    (case x of {
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQ	-> SLIT("e");	GT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
	ALWAYS	-> SLIT("mp");	-- hack
        _       -> error "Spix: iI386Code: unknown conditional!"
    })

pprDollImm :: PprStyle -> Imm -> Unpretty

pprDollImm sty i     = uppBesides [ uppPStr SLIT("$"), pprImm sty i]

pprImm :: PprStyle -> Imm -> Unpretty

pprImm sty (ImmInt i)     = uppInt i
pprImm sty (ImmInteger i) = uppInteger i
pprImm sty (ImmCLbl l)    = pprCLabel sty l
pprImm sty (ImmLab l)     = l

--pprImm (PprForAsm _ False _) (ImmLab s) = s
--pprImm _                     (ImmLab s) = uppBeside (uppChar '_') s

pprImm sty (ImmLit s) = s

pprAddr :: PprStyle -> Addr -> Unpretty
pprAddr sty (ImmAddr imm off)
  =  uppBesides [pprImm sty imm,
                 if off > 0 then uppChar '+' else uppPStr SLIT(""),
                 if off == 0 then uppPStr SLIT("") else uppInt off
                ]
pprAddr sty (Addr Nothing Nothing displacement)
  =  uppBesides [pprDisp sty displacement]
pprAddr sty (Addr base index displacement)
  =  uppBesides [pprDisp sty displacement,
                 uppChar '(',
                 pprBase base,
                 pprIndex index,
                 uppChar ')'
                ]
  where
    pprBase (Just r) = uppBesides [pprReg L r,
                                   case index of 
                                     Nothing -> uppPStr SLIT("")
                                     _       -> uppChar ','
                                  ]
    pprBase _        = uppPStr SLIT("")
    pprIndex (Just (r,i)) = uppBesides [pprReg L r, uppChar ',', uppInt i]
    pprIndex _       = uppPStr SLIT("")

pprDisp sty (ImmInt 0) = uppPStr SLIT("")
--pprDisp sty (ImmInteger 0) = uppPStr SLIT("")
pprDisp sty d = pprImm sty d

pprOperand :: PprStyle -> Size -> Operand -> Unpretty
pprOperand sty s (OpReg r) = pprReg s r
pprOperand sty s (OpImm i) = pprDollImm sty i
pprOperand sty s (OpAddr ea) = pprAddr sty ea

pprSize :: Size -> Unpretty
pprSize x = uppPStr
    (case x of
	B  -> SLIT("b")
	HB -> SLIT("b")
        S  -> SLIT("w")
	L  -> SLIT("l")
	F  -> SLIT("s")
	D  -> SLIT("l")
    )

pprSizeOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Unpretty
pprSizeOp sty name size op1 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1
    ]

pprSizeOpOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Operand -> Unpretty
pprSizeOpOp sty name size op1 op2 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprOperand sty size op2
    ]

pprSizeOpReg :: PprStyle -> FAST_STRING -> Size -> Operand -> Reg -> Unpretty
pprSizeOpReg sty name size op1 reg =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprReg size reg
    ]

pprSizeAddr :: PprStyle -> FAST_STRING -> Size -> Addr -> Unpretty
pprSizeAddr sty name size op =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprAddr sty op
    ]

pprSizeAddrReg :: PprStyle -> FAST_STRING -> Size -> Addr -> Reg -> Unpretty
pprSizeAddrReg sty name size op dst =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprAddr sty op,
	uppComma,
        pprReg size dst
    ]

pprOpOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Operand -> Unpretty
pprOpOp sty name size op1 op2 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprOperand sty size op2
    ]

pprSizeOpOpCoerce :: PprStyle -> FAST_STRING -> Size -> Size -> Operand -> Operand -> Unpretty
pprSizeOpOpCoerce sty name size1 size2 op1 op2 =
    uppBesides [ uppChar '\t', uppPStr name, uppChar ' ',
	pprOperand sty size1 op1,
	uppComma,
	pprOperand sty size2 op2
    ]

pprCondInstr :: PprStyle -> FAST_STRING -> Cond -> Unpretty -> Unpretty
pprCondInstr sty name cond arg =
    uppBesides [ uppChar '\t', uppPStr name, pprCond cond, uppChar ' ', arg]

pprI386Instr :: PprStyle -> I386Instr -> Unpretty
pprI386Instr sty (MOV size (OpReg src) (OpReg dst)) -- hack
  | src == dst
  = uppPStr SLIT("")
pprI386Instr sty (MOV size src dst) 
  = pprSizeOpOp sty SLIT("mov") size src dst
pprI386Instr sty (MOVZX size src dst) = pprSizeOpOpCoerce sty SLIT("movzx") L size src dst
pprI386Instr sty (MOVSX size src dst) = pprSizeOpOpCoerce sty SLIT("movxs") L size src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3)) 
  | reg1 == reg3
  = pprSizeOpOp sty SLIT("add") size (OpReg reg2) dst
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3)) 
  | reg2 == reg3
  = pprSizeOpOp sty SLIT("add") size (OpReg reg1) dst
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) Nothing displ)) dst@(OpReg reg3)) 
  | reg1 == reg3
  = pprI386Instr sty (ADD size (OpImm displ) dst)
pprI386Instr sty (LEA size src dst) = pprSizeOpOp sty SLIT("lea") size src dst

pprI386Instr sty (ADD size (OpImm (ImmInt (-1))) dst) 
  = pprSizeOp sty SLIT("dec") size dst
pprI386Instr sty (ADD size (OpImm (ImmInt 1)) dst) 
  = pprSizeOp sty SLIT("inc") size dst
pprI386Instr sty (ADD size src dst) 
  = pprSizeOpOp sty SLIT("add") size src dst
pprI386Instr sty (SUB size src dst) = pprSizeOpOp sty SLIT("sub") size src dst
pprI386Instr sty (IMUL size op1 op2) = pprSizeOpOp sty SLIT("imul") size op1 op2
pprI386Instr sty (IDIV size op) = pprSizeOp sty SLIT("idiv") size op

pprI386Instr sty (AND size src dst) = pprSizeOpOp sty SLIT("and") size src dst
pprI386Instr sty (OR  size src dst) = pprSizeOpOp sty SLIT("or")  size src dst
pprI386Instr sty (XOR size src dst) = pprSizeOpOp sty SLIT("xor")  size src dst
pprI386Instr sty (NOT size op) = pprSizeOp sty SLIT("not") size op
pprI386Instr sty (NEGI size op) = pprSizeOp sty SLIT("neg") size op
pprI386Instr sty (SHL size imm dst) = pprSizeOpOp sty SLIT("shl")  size imm dst
pprI386Instr sty (SAR size imm dst) = pprSizeOpOp sty SLIT("sar")  size imm dst
pprI386Instr sty (SHR size imm dst) = pprSizeOpOp sty SLIT("shr")  size imm dst

pprI386Instr sty (CMP size src dst) = pprSizeOpOp sty SLIT("cmp")  size src dst
pprI386Instr sty (TEST size src dst) = pprSizeOpOp sty SLIT("test")  size src dst
pprI386Instr sty (PUSH size op) = pprSizeOp sty SLIT("push") size op
pprI386Instr sty (POP size op) = pprSizeOp sty SLIT("pop") size op

pprI386Instr sty (NOP) = uppPStr SLIT("\tnop")
pprI386Instr sty (CLTD) = uppPStr SLIT("\tcltd")

pprI386Instr sty (SETCC cond op) = pprCondInstr sty SLIT("set") cond (pprOperand sty B op)

pprI386Instr sty (JXX cond lab) = pprCondInstr sty SLIT("j") cond (pprCLabel sty lab)

pprI386Instr sty (JMP (OpImm imm)) = uppBeside (uppPStr SLIT("\tjmp ")) (pprImm sty imm)
pprI386Instr sty (JMP op) = uppBeside (uppPStr SLIT("\tjmp *")) (pprOperand sty L op)

pprI386Instr sty (CALL imm) =
    uppBesides [ uppPStr SLIT("\tcall "), pprImm sty imm ]

pprI386Instr sty SAHF = uppPStr SLIT("\tsahf")
pprI386Instr sty FABS = uppPStr SLIT("\tfabs")

pprI386Instr sty (FADD sz src@(OpAddr _)) 
  = uppBesides [uppPStr SLIT("\tfadd"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty (FADD sz src) 
  = uppPStr SLIT("\tfadd")
pprI386Instr sty FADDP 
  = uppPStr SLIT("\tfaddp")
pprI386Instr sty (FMUL sz src) 
  = uppBesides [uppPStr SLIT("\tfmul"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FMULP 
  = uppPStr SLIT("\tfmulp")
pprI386Instr sty (FIADD size op) = pprSizeAddr sty SLIT("fiadd") size op
pprI386Instr sty FCHS = uppPStr SLIT("\tfchs")
pprI386Instr sty (FCOM size op) = pprSizeOp sty SLIT("fcom") size op
pprI386Instr sty FCOS = uppPStr SLIT("\tfcos")
pprI386Instr sty (FIDIV size op) = pprSizeAddr sty SLIT("fidiv") size op
pprI386Instr sty (FDIV sz src) 
  = uppBesides [uppPStr SLIT("\tfdiv"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FDIVP
  = uppPStr SLIT("\tfdivp")
pprI386Instr sty (FDIVR sz src)
  = uppBesides [uppPStr SLIT("\tfdivr"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FDIVRP
  = uppPStr SLIT("\tfdivpr")
pprI386Instr sty (FIDIVR size op) = pprSizeAddr sty SLIT("fidivr") size op
pprI386Instr sty (FICOM size op) = pprSizeAddr sty SLIT("ficom") size op
pprI386Instr sty (FILD sz op reg) = pprSizeAddrReg sty SLIT("fild") sz op reg
pprI386Instr sty (FIST size op) = pprSizeAddr sty SLIT("fist") size op
pprI386Instr sty (FLD sz (OpImm (ImmCLbl src))) 
  = uppBesides [uppPStr SLIT("\tfld"),pprSize sz,uppChar ' ',pprCLabel sty src]
pprI386Instr sty (FLD sz src) 
  = uppBesides [uppPStr SLIT("\tfld"),pprSize sz,uppChar ' ',pprOperand sty sz src]
pprI386Instr sty FLD1 = uppPStr SLIT("\tfld1")
pprI386Instr sty FLDZ = uppPStr SLIT("\tfldz")
pprI386Instr sty (FIMUL size op) = pprSizeAddr sty SLIT("fimul") size op
pprI386Instr sty FRNDINT = uppPStr SLIT("\tfrndint")
pprI386Instr sty FSIN = uppPStr SLIT("\tfsin")
pprI386Instr sty FSQRT = uppPStr SLIT("\tfsqrt")
pprI386Instr sty (FST sz dst) 
  = uppBesides [uppPStr SLIT("\tfst"), pprSize sz, uppChar ' ', pprOperand sty sz dst]
pprI386Instr sty (FSTP sz dst) 
  = uppBesides [uppPStr SLIT("\tfstp"), pprSize sz, uppChar ' ', pprOperand sty sz dst]
pprI386Instr sty (FISUB size op) = pprSizeAddr sty SLIT("fisub") size op
pprI386Instr sty (FSUB sz src) 
  = uppBesides [uppPStr SLIT("\tfsub"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FSUBP
  = uppPStr SLIT("\tfsubp")
pprI386Instr sty (FSUBR size src)
  = pprSizeOp sty SLIT("fsubr") size src
pprI386Instr sty FSUBRP
  = uppPStr SLIT("\tfsubpr")
pprI386Instr sty (FISUBR size op) 
  = pprSizeAddr sty SLIT("fisubr") size op
pprI386Instr sty FTST = uppPStr SLIT("\tftst")
pprI386Instr sty (FCOMP sz op) 
  = uppBesides [uppPStr SLIT("\tfcomp"), pprSize sz, uppChar ' ', pprOperand sty sz op]
pprI386Instr sty FUCOMPP = uppPStr SLIT("\tfucompp")
pprI386Instr sty FXCH = uppPStr SLIT("\tfxch")
pprI386Instr sty FNSTSW = uppPStr SLIT("\tfnstsw %ax")
pprI386Instr sty FNOP = uppPStr SLIT("")

pprI386Instr sty (LABEL clab) =
    uppBesides [
	if (externallyVisibleCLabel clab) then
	    uppBesides [uppPStr SLIT(".globl "), pprLab, uppChar '\n']
	else
	    uppNil,
    	pprLab,
	uppChar ':'
    ]
    where pprLab = pprCLabel sty clab

pprI386Instr sty (COMMENT s) = uppBeside (uppPStr SLIT("# ")) (uppPStr s)

pprI386Instr sty (SEGMENT TextSegment)
    = uppPStr SLIT(".text\n\t.align 4")

pprI386Instr sty (SEGMENT DataSegment)
    = uppPStr SLIT(".data\n\t.align 2")

pprI386Instr sty (ASCII False str) =
    uppBesides [
    	uppStr "\t.asciz \"",
    	uppStr str,
    	uppChar '"'
    ]

pprI386Instr sty (ASCII True str) = uppBeside (uppStr "\t.ascii \"") (asciify str 60)
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

pprI386Instr sty (DATA s xs) = uppInterleave (uppChar '\n') (map pp_item xs)
    where pp_item x = case s of
	    B -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    HB-> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    S -> uppBeside (uppPStr SLIT("\t.word\t")) (pprImm sty x)
	    L -> uppBeside (uppPStr SLIT("\t.long\t")) (pprImm sty x)
	    F -> uppBeside (uppPStr SLIT("\t.long\t")) (pprImm sty x)
    	    D -> uppBeside (uppPStr SLIT("\t.double\t")) (pprImm sty x)

\end{code}

%************************************************************************
%*									*
\subsection[Schedule]{Register allocation information}
%*									*
%************************************************************************

\begin{code}

data I386Regs = SRegs BitSet BitSet

instance MachineRegisters I386Regs where
    mkMRegs xs = SRegs (mkBS ints) (mkBS floats')
      where
    	(ints, floats) = partition (< 8) xs
    	floats' = map (subtract 8) floats

    possibleMRegs FloatKind (SRegs _ floats) = [ x + 8 | x <- listBS floats]
    possibleMRegs DoubleKind (SRegs _ floats) = [ x + 8 | x <- listBS floats]
    possibleMRegs _ (SRegs ints _) = listBS ints

    useMReg (SRegs ints floats) n =
    	if n _LT_ ILIT(8) then SRegs (ints `minusBS` singletonBS IBOX(n)) floats
    	else SRegs ints (floats `minusBS` singletonBS (IBOX(n _SUB_ ILIT(8))))

    useMRegs (SRegs ints floats) xs =
    	SRegs (ints `minusBS` ints')
    	      (floats `minusBS` floats')
      where
        SRegs ints' floats' = mkMRegs xs

    freeMReg (SRegs ints floats) n =
    	if n _LT_ ILIT(8) then SRegs (ints `unionBS` singletonBS IBOX(n)) floats
    	else SRegs ints (floats `unionBS` singletonBS (IBOX(n _SUB_ ILIT(8))))

    freeMRegs (SRegs ints floats) xs =
        SRegs (ints `unionBS` ints')
    	      (floats `unionBS` floats')
      where
        SRegs ints' floats' = mkMRegs xs

instance MachineCode I386Instr where
    -- Alas, we don't do anything clever with our OrdLists
--OLD:
--  flatten = flattenOrdList

    regUsage = i386RegUsage
    regLiveness = i386RegLiveness
    patchRegs = i386PatchRegs

    -- We spill just below the stack pointer, leaving two words per spill location.
    spillReg dyn (MemoryReg i pk) 
      = trace "spillsave"
        (mkUnitList (MOV (kindToSize pk) (OpReg dyn) (OpAddr (spRel (-2 * i)))))
    loadReg (MemoryReg i pk) dyn 
      = trace "spillload"
        (mkUnitList (MOV (kindToSize pk) (OpAddr (spRel (-2 * i))) (OpReg dyn)))

--spRel gives us a stack relative addressing mode for volatile temporaries
--and for excess call arguments.

spRel  
    :: Int      -- desired stack offset in words, positive or negative
    -> Addr
spRel n = Addr (Just esp) Nothing (ImmInt (n * 4))

kindToSize :: PrimKind -> Size
kindToSize PtrKind	    = L
kindToSize CodePtrKind	    = L
kindToSize DataPtrKind	    = L
kindToSize RetKind	    = L
kindToSize InfoPtrKind	    = L
kindToSize CostCentreKind   = L
kindToSize CharKind	    = L
kindToSize IntKind	    = L
kindToSize WordKind	    = L
kindToSize AddrKind	    = L
kindToSize FloatKind	    = F
kindToSize DoubleKind	    = D
kindToSize ArrayKind	    = L
kindToSize ByteArrayKind    = L
kindToSize StablePtrKind    = L
kindToSize MallocPtrKind    = L

\end{code}

@i386RegUsage@ returns the sets of src and destination registers used by
a particular instruction.  Machine registers that are pre-allocated
to stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.  (We wouldn't want them to end up on
the free list!)

\begin{code}

i386RegUsage :: I386Instr -> RegUsage
i386RegUsage instr = case instr of
    MOV  sz src dst	-> usage2 src dst
    MOVZX sz src dst	-> usage2 src dst
    MOVSX sz src dst	-> usage2 src dst
    LEA  sz src dst	-> usage2 src dst
    ADD  sz src dst	-> usage2 src dst
    SUB  sz src dst	-> usage2 src dst
    IMUL sz src dst	-> usage2 src dst
    IDIV sz src		-> usage (eax:edx:opToReg src) [eax,edx]
    AND  sz src dst	-> usage2 src dst
    OR   sz src dst	-> usage2 src dst
    XOR  sz src dst	-> usage2 src dst
    NOT  sz op		-> usage1 op
    NEGI sz op		-> usage1 op
    SHL  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SAR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SHR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    PUSH sz op		-> usage (opToReg op) []
    POP  sz op		-> usage [] (opToReg op)
    TEST sz src dst	-> usage (opToReg src ++ opToReg dst) []
    CMP  sz src dst	-> usage (opToReg src ++ opToReg dst) []
    SETCC cond op	-> usage [] (opToReg op)
    JXX cond label	-> usage [] []
    JMP op		-> usage (opToReg op) freeRegs
    CALL imm		-> usage [] callClobberedRegs
    CLTD		-> usage [eax] [edx]
    NOP			-> usage [] []
    SAHF 		-> usage [eax] []
    FABS 		-> usage [st0] [st0]
    FADD sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FADDP 		-> usage [st0,st1] [st0] -- allFPRegs
    FIADD sz asrc	-> usage (addrToRegs asrc) [st0]
    FCHS 		-> usage [st0] [st0]
    FCOM sz src		-> usage (st0:opToReg src) []
    FCOS 		-> usage [st0] [st0]
    FDIV sz src 	-> usage (st0:opToReg src) [st0]
    FDIVP  		-> usage [st0,st1] [st0]
    FDIVRP 		-> usage [st0,st1] [st0]
    FIDIV sz asrc	-> usage (addrToRegs asrc) [st0]
    FDIVR sz src 	-> usage (st0:opToReg src) [st0]
    FIDIVR sz asrc	-> usage (addrToRegs asrc) [st0]
    FICOM sz asrc	-> usage (addrToRegs asrc) []
    FILD sz asrc dst	-> usage (addrToRegs asrc) [dst] -- allFPRegs
    FIST sz adst	-> usage (st0:addrToRegs adst) []
    FLD	 sz src 	-> usage (opToReg src) [st0] -- allFPRegs
    FLD1 		-> usage [] [st0] -- allFPRegs
    FLDZ 		-> usage [] [st0] -- allFPRegs
    FMUL sz src 	-> usage (st0:opToReg src) [st0]
    FMULP 	 	-> usage [st0,st1] [st0]
    FIMUL sz asrc	-> usage (addrToRegs asrc) [st0]
    FRNDINT 		-> usage [st0] [st0]
    FSIN 		-> usage [st0] [st0]
    FSQRT 		-> usage [st0] [st0]
    FST sz (OpReg r)	-> usage [st0] [r]
    FST sz dst		-> usage (st0:opToReg dst) []
    FSTP sz (OpReg r)	-> usage [st0] [r] -- allFPRegs
    FSTP sz dst		-> usage (st0:opToReg dst) [] -- allFPRegs
    FSUB sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FSUBR sz src	-> usage (st0:opToReg src) [st0] -- allFPRegs
    FISUB sz asrc	-> usage (addrToRegs asrc) [st0]
    FSUBP 		-> usage [st0,st1] [st0] -- allFPRegs
    FSUBRP 		-> usage [st0,st1] [st0] -- allFPRegs
    FISUBR sz asrc	-> usage (addrToRegs asrc) [st0]
    FTST 		-> usage [st0] []
    FCOMP sz op		-> usage (st0:opToReg op) [st0] -- allFPRegs
    FUCOMPP 		-> usage [st0, st1] [] --  allFPRegs
    FXCH		-> usage [st0, st1] [st0, st1]
    FNSTSW		-> usage [] [eax]
    _			-> noUsage

 where

    usage2 :: Operand -> Operand -> RegUsage
    usage2 op (OpReg reg) = usage (opToReg op) [reg]
    usage2 op (OpAddr ea) = usage (opToReg op ++ addrToRegs ea) []
    usage2 op (OpImm imm) = usage (opToReg op) []
    usage1 :: Operand -> RegUsage
    usage1 (OpReg reg)    = usage [reg] [reg]
    usage1 (OpAddr ea)    = usage (addrToRegs ea) []
    allFPRegs = [st0,st1,st2,st3,st4,st5,st6,st7]
    --callClobberedRegs = [ eax, ecx, edx ] -- according to gcc, anyway.
    callClobberedRegs = [eax] 

-- General purpose register collecting functions.

    opToReg (OpReg reg)   = [reg]
    opToReg (OpImm imm)   = []
    opToReg (OpAddr  ea)  = addrToRegs ea

    addrToRegs (Addr base index _) = baseToReg base ++ indexToReg index
      where  baseToReg Nothing       = []
             baseToReg (Just r)      = [r]
             indexToReg Nothing      = []
             indexToReg (Just (r,_)) = [r]
    addrToRegs (ImmAddr _ _) = []

    usage src dst = RU (mkUniqSet (filter interesting src))
    	    	       (mkUniqSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

freeRegs :: [Reg]
freeRegs = freeMappedRegs (\ x -> x) [0..15]

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

\end{code}

@i386RegLiveness@ takes future liveness information and modifies it according to
the semantics of branches and labels.  (An out-of-line branch clobbers the liveness
passed back by the following instruction; a forward local branch passes back the
liveness from the target label; a conditional branch merges the liveness from the
target and the liveness from its successor; a label stashes away the current liveness
in the future liveness environment).

\begin{code}
i386RegLiveness :: I386Instr -> RegLiveness -> RegLiveness
i386RegLiveness instr info@(RL live future@(FL all env)) = case instr of

    JXX _ lbl	-> RL (lookup lbl `unionUniqSets` live) future
    JMP _	-> RL emptyUniqSet future
    CALL _      -> RL live future
    LABEL lbl   -> RL live (FL (all `unionUniqSets` live) (addToFM env lbl live))
    _		    -> info

  where
    lookup lbl = case lookupFM env lbl of
	Just regs -> regs
	Nothing -> trace ("Missing " ++ (uppShow 80 (pprCLabel (PprForAsm (\_->False) False id) lbl)) ++
                          " in future?") emptyUniqSet

\end{code}

@i386PatchRegs@ takes an instruction (possibly with MemoryReg/UnmappedReg registers) and
changes all register references according to the supplied environment.

\begin{code}

i386PatchRegs :: I386Instr -> (Reg -> Reg) -> I386Instr
i386PatchRegs instr env = case instr of
    MOV  sz src dst	-> patch2 (MOV  sz) src dst
    MOVZX sz src dst	-> patch2 (MOVZX sz) src dst
    MOVSX sz src dst	-> patch2 (MOVSX sz) src dst
    LEA  sz src dst	-> patch2 (LEA  sz) src dst
    ADD  sz src dst	-> patch2 (ADD  sz) src dst
    SUB  sz src dst	-> patch2 (SUB  sz) src dst
    IMUL sz src dst 	-> patch2 (IMUL sz) src dst
    IDIV sz src  	-> patch1 (IDIV sz) src 
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch1 (SHL  sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR  sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR  sz imm) dst
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op
    FADD sz src		-> FADD sz (patchOp src)
    FIADD sz asrc	-> FIADD sz (lookupAddr asrc)
    FCOM sz src		-> patch1 (FCOM sz) src
    FDIV sz src 	-> FDIV sz (patchOp src)
    --FDIVP sz src 	-> FDIVP sz (patchOp src)
    FIDIV sz asrc	-> FIDIV sz (lookupAddr asrc)
    FDIVR sz src 	-> FDIVR sz (patchOp src)
    --FDIVRP sz src 	-> FDIVRP sz (patchOp src)
    FIDIVR sz asrc	-> FIDIVR sz (lookupAddr asrc)
    FICOM sz asrc	-> FICOM sz (lookupAddr asrc)
    FILD sz asrc dst	-> FILD sz (lookupAddr asrc) (env dst)
    FIST sz adst	-> FIST sz (lookupAddr adst)
    FLD	sz src 		-> patch1 (FLD sz) (patchOp src)
    FMUL sz src 	-> FMUL sz (patchOp src)
    --FMULP sz src 	-> FMULP sz (patchOp src)
    FIMUL sz asrc	-> FIMUL sz (lookupAddr asrc)
    FST sz dst		-> FST sz (patchOp dst)
    FSTP sz dst		-> FSTP sz (patchOp dst)
    FSUB sz src		-> FSUB sz (patchOp src)
    --FSUBP sz src	-> FSUBP sz (patchOp src)
    FISUB sz asrc	-> FISUB sz (lookupAddr asrc)
    FSUBR sz src 	-> FSUBR sz (patchOp src)
    --FSUBRP sz src 	-> FSUBRP sz (patchOp src)
    FISUBR sz asrc	-> FISUBR sz (lookupAddr asrc)
    FCOMP sz src	-> FCOMP sz (patchOp src)
    _			-> instr
	
  where
		patch1 insn op = insn (patchOp op)
		patch2 insn src dst = insn (patchOp src) (patchOp dst)

		patchOp (OpReg  reg) = OpReg (env reg)
		patchOp (OpImm  imm) = OpImm imm
		patchOp (OpAddr ea)  = OpAddr (lookupAddr ea)

		lookupAddr (Addr base index disp) 
			= Addr (lookupBase base) (lookupIndex index) disp
			where lookupBase Nothing        = Nothing
 	      		      lookupBase (Just r)       = Just (env r)
	      		      lookupIndex Nothing       = Nothing
	      		      lookupIndex (Just (r,i))  = Just (env r, i)
		lookupAddr (ImmAddr imm off) 
			= ImmAddr imm off

\end{code}

Sometimes, we want to be able to modify addresses at compile time.
(Okay, just for chrCode of a fetch.)

\begin{code}

#ifdef __GLASGOW_HASKELL__

{-# SPECIALIZE
    is13Bits :: Int -> Bool
  #-}
{-# SPECIALIZE
    is13Bits :: Integer -> Bool
  #-}

#endif

is13Bits :: Integral a => a -> Bool
is13Bits x = x >= -4096 && x < 4096

offset :: Addr -> Int -> Maybe Addr
offset (Addr reg index (ImmInt n)) off
  = Just (Addr reg index (ImmInt n2))
  where n2 = n + off

offset (Addr reg index (ImmInteger n)) off
  = Just (Addr reg index (ImmInt (fromInteger n2)))
  where n2 = n + toInteger off

offset (ImmAddr imm off1) off2
  = Just (ImmAddr imm off3)
  where off3 = off1 + off2

offset _ _ = Nothing

\end{code}

If you value your sanity, do not venture below this line.

\begin{code}

-- platform.h is generate and tells us what the target architecture is
#include "../../includes/platform.h"
#define STOLEN_X86_REGS 5
#include "../../includes/MachRegs.h"
#include "../../includes/i386-unknown-linuxaout.h"

-- Redefine the literals used for I386 register names in the header
-- files.  Gag me with a spoon, eh?

#define eax 0
#define ebx 1
#define ecx 2
#define edx 3
#define esi 4
#define edi 5
#define ebp 6
#define esp 7
#define st0 8
#define st1 9
#define st2 10
#define st3 11
#define st4 12
#define st5 13
#define st6 14
#define st7 15
#define CALLER_SAVES_Hp 
-- ToDo: rm when we give esp back
#define REG_Hp esp
#define REG_R2 ecx

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

--freeReg ILIT(esp) = _FALSE_  --	%esp is our stack pointer.

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
reservedRegs = []
--reservedRegs = [NCG_Reserved_I1, NCG_Reserved_I2,
--    	    	NCG_Reserved_F1, NCG_Reserved_F2,
--    	    	NCG_Reserved_D1, NCG_Reserved_D2]

\end{code}

