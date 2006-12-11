-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "nativeGen/NCG.h"

module MachInstrs (
	-- * Cmm instantiations
	NatCmm, NatCmmTop, NatBasicBlock,	

	-- * Machine instructions
	Instr(..),
	Cond(..), condUnsigned, condToSigned, condToUnsigned,

#if !powerpc_TARGET_ARCH && !i386_TARGET_ARCH && !x86_64_TARGET_ARCH
	Size(..), machRepSize,
#endif
	RI(..),

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
	Operand(..),
#endif
#if i386_TARGET_ARCH
	i386_insert_ffrees,
#endif
#if sparc_TARGET_ARCH
	riZero, fpRelEA, moveSp, fPair,
#endif
    ) where

#include "HsVersions.h"

import MachRegs
import Cmm
import MachOp		( MachRep(..) )
import CLabel           ( CLabel, pprCLabel )
import Panic		( panic )
import Outputable
import FastString
import Constants       ( wORD_SIZE )

import GHC.Exts


-- -----------------------------------------------------------------------------
-- Our flavours of the Cmm types

-- Type synonyms for Cmm populated with native code
type NatCmm        = GenCmm CmmStatic Instr
type NatCmmTop     = GenCmmTop CmmStatic Instr
type NatBasicBlock = GenBasicBlock Instr

-- -----------------------------------------------------------------------------
-- Conditions on this architecture

data Cond
#if alpha_TARGET_ARCH
  = ALWAYS	-- For BI (same as BR)
  | EQQ		-- For CMP and BI (NB: "EQ" is a 1.3 Prelude name)
  | GE		-- For BI only
  | GTT		-- For BI only (NB: "GT" is a 1.3 Prelude name)
  | LE		-- For CMP and BI
  | LTT		-- For CMP and BI (NB: "LT" is a 1.3 Prelude name)
  | NE		-- For BI only
  | NEVER	-- For BI (null instruction)
  | ULE		-- For CMP only
  | ULT		-- For CMP only
#endif
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
  | EQQ
  | GE
  | GEU
  | GTT
  | GU
  | LE
  | LEU
  | LTT
  | LU
  | NE
  | NEG
  | POS
  | CARRY
  | OFLO
  | PARITY
  | NOTPARITY
#endif
#if sparc_TARGET_ARCH
  = ALWAYS	-- What's really used? ToDo
  | EQQ
  | GE
  | GEU
  | GTT
  | GU
  | LE
  | LEU
  | LTT
  | LU
  | NE
  | NEG
  | NEVER
  | POS
  | VC
  | VS
#endif
#if powerpc_TARGET_ARCH
  = ALWAYS
  | EQQ
  | GE
  | GEU
  | GTT
  | GU
  | LE
  | LEU
  | LTT
  | LU
  | NE
#endif
    deriving Eq  -- to make an assertion work

condUnsigned GU  = True
condUnsigned LU  = True
condUnsigned GEU = True
condUnsigned LEU = True
condUnsigned _   = False

condToSigned GU  = GTT
condToSigned LU  = LTT
condToSigned GEU = GE
condToSigned LEU = LE
condToSigned x   = x

condToUnsigned GTT = GU
condToUnsigned LTT = LU
condToUnsigned GE  = GEU
condToUnsigned LE  = LEU
condToUnsigned x   = x

-- -----------------------------------------------------------------------------
-- Sizes on this architecture

-- ToDo: it's not clear to me that we need separate signed-vs-unsigned sizes
-- here.  I've removed them from the x86 version, we'll see what happens --SDM

#if !powerpc_TARGET_ARCH && !i386_TARGET_ARCH && !x86_64_TARGET_ARCH
data Size
#if alpha_TARGET_ARCH
    = B	    -- byte
    | Bu
--  | W	    -- word (2 bytes): UNUSED
--  | Wu    -- : UNUSED
    | L	    -- longword (4 bytes)
    | Q	    -- quadword (8 bytes)
--  | FF    -- VAX F-style floating pt: UNUSED
--  | GF    -- VAX G-style floating pt: UNUSED
--  | DF    -- VAX D-style floating pt: UNUSED
--  | SF    -- IEEE single-precision floating pt: UNUSED
    | TF    -- IEEE double-precision floating pt
#endif
#if sparc_TARGET_ARCH || powerpc_TARGET_ARCH
    = B     -- byte (signed)
    | Bu    -- byte (unsigned)
    | H     -- halfword (signed, 2 bytes)
    | Hu    -- halfword (unsigned, 2 bytes)
    | W	    -- word (4 bytes)
    | F	    -- IEEE single-precision floating pt
    | DF    -- IEEE single-precision floating pt
#endif
  deriving Eq

machRepSize :: MachRep -> Size
machRepSize I8    = IF_ARCH_alpha(Bu, IF_ARCH_sparc(Bu, ))
machRepSize I16   = IF_ARCH_alpha(err,IF_ARCH_sparc(Hu, ))
machRepSize I32   = IF_ARCH_alpha(L,  IF_ARCH_sparc(W,  ))
machRepSize I64	  = panic "machRepSize: I64"
machRepSize I128  = panic "machRepSize: I128"
machRepSize F32   = IF_ARCH_alpha(TF, IF_ARCH_sparc(F, ))
machRepSize F64   = IF_ARCH_alpha(TF, IF_ARCH_sparc(DF,))
#endif

-- -----------------------------------------------------------------------------
-- Register or immediate (a handy type on some platforms)

data RI = RIReg Reg
	| RIImm Imm


-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

data Instr
  = COMMENT FastString		-- comment pseudo-op

  | LDATA   Section [CmmStatic]	-- some static data spat out during code
				-- generation.  Will be extracted before
				-- pretty-printing.

  | NEWBLOCK BlockId		-- start a new basic block.  Useful during
				-- codegen, removed later.  Preceding 
				-- instruction should be a jump, as per the
				-- invariants for a BasicBlock (see Cmm).

  | DELTA   Int                 -- specify current stack offset for
                                -- benefit of subsequent passes

-- -----------------------------------------------------------------------------
-- Alpha instructions

#if alpha_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.
	      |	LD	      Size Reg AddrMode -- size, dst, src
	      | LDA	      Reg AddrMode      -- dst, src
	      | LDAH	      Reg AddrMode      -- dst, src
	      | LDGP	      Reg AddrMode      -- dst, src
	      | LDI	      Size Reg Imm     -- size, dst, src
	      | ST	      Size Reg AddrMode -- size, src, dst

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
	      | JMP	      Reg AddrMode Int
	      | BSR	      Imm Int
	      | JSR	      Reg AddrMode Int

-- Alpha-specific pseudo-ops.
	      | FUNBEGIN CLabel
	      | FUNEND CLabel

data RI
  = RIReg Reg
  | RIImm Imm

#endif /* alpha_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- Intel x86 instructions

{-
Intel, in their infinite wisdom, selected a stack model for floating
point registers on x86.  That might have made sense back in 1979 --
nowadays we can see it for the nonsense it really is.  A stack model
fits poorly with the existing nativeGen infrastructure, which assumes
flat integer and FP register sets.  Prior to this commit, nativeGen
could not generate correct x86 FP code -- to do so would have meant
somehow working the register-stack paradigm into the register
allocator and spiller, which sounds very difficult.
  
We have decided to cheat, and go for a simple fix which requires no
infrastructure modifications, at the expense of generating ropey but
correct FP code.  All notions of the x86 FP stack and its insns have
been removed.  Instead, we pretend (to the instruction selector and
register allocator) that x86 has six floating point registers, %fake0
.. %fake5, which can be used in the usual flat manner.  We further
claim that x86 has floating point instructions very similar to SPARC
and Alpha, that is, a simple 3-operand register-register arrangement.
Code generation and register allocation proceed on this basis.
  
When we come to print out the final assembly, our convenient fiction
is converted to dismal reality.  Each fake instruction is
independently converted to a series of real x86 instructions.
%fake0 .. %fake5 are mapped to %st(0) .. %st(5).  To do reg-reg
arithmetic operations, the two operands are pushed onto the top of the
FP stack, the operation done, and the result copied back into the
relevant register.  There are only six %fake registers because 2 are
needed for the translation, and x86 has 8 in total.

The translation is inefficient but is simple and it works.  A cleverer
translation would handle a sequence of insns, simulating the FP stack
contents, would not impose a fixed mapping from %fake to %st regs, and
hopefully could avoid most of the redundant reg-reg moves of the
current translation.

We might as well make use of whatever unique FP facilities Intel have
chosen to bless us with (let's not be churlish, after all).
Hence GLDZ and GLD1.  Bwahahahahahahaha!
-}

{-
MORE FLOATING POINT MUSINGS...

Intel's internal floating point registers are by default 80 bit
extended precision.  This means that all operations done on values in
registers are done at 80 bits, and unless the intermediate values are
truncated to the appropriate size (32 or 64 bits) by storing in
memory, calculations in registers will give different results from
calculations which pass intermediate values in memory (eg. via
function calls).

One solution is to set the FPU into 64 bit precision mode.  Some OSs
do this (eg. FreeBSD) and some don't (eg. Linux).  The problem here is
that this will only affect 64-bit precision arithmetic; 32-bit
calculations will still be done at 64-bit precision in registers.  So
it doesn't solve the whole problem.  

There's also the issue of what the C library is expecting in terms of
precision.  It seems to be the case that glibc on Linux expects the
FPU to be set to 80 bit precision, so setting it to 64 bit could have
unexpected effects.  Changing the default could have undesirable
effects on other 3rd-party library code too, so the right thing would
be to save/restore the FPU control word across Haskell code if we were
to do this.

gcc's -ffloat-store gives consistent results by always storing the
results of floating-point calculations in memory, which works for both
32 and 64-bit precision.  However, it only affects the values of
user-declared floating point variables in C, not intermediate results.
GHC in -fvia-C mode uses -ffloat-store (see the -fexcess-precision
flag).

Another problem is how to spill floating point registers in the
register allocator.  Should we spill the whole 80 bits, or just 64?
On an OS which is set to 64 bit precision, spilling 64 is fine.  On
Linux, spilling 64 bits will round the results of some operations.
This is what gcc does.  Spilling at 80 bits requires taking up a full
128 bit slot (so we get alignment).  We spill at 80-bits and ignore
the alignment problems.

In the future, we'll use the SSE registers for floating point.  This
requires a CPU that supports SSE2 (ordinary SSE only supports 32 bit
precision float ops), which means P4 or Xeon and above.  Using SSE
will solve all these problems, because the SSE registers use fixed 32
bit or 64 bit precision.

--SDM 1/2003
-}

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH

-- data Instr continues...

-- Moves.
	| MOV	      MachRep Operand Operand
	| MOVZxL      MachRep Operand Operand -- size is the size of operand 1
	| MOVSxL      MachRep Operand Operand -- size is the size of operand 1
	-- x86_64 note: plain mov into a 32-bit register always zero-extends
	-- into the 64-bit reg, in contrast to the 8 and 16-bit movs which
	-- don't affect the high bits of the register.

-- Load effective address (also a very useful three-operand add instruction :-)
	| LEA         MachRep Operand Operand

-- Int Arithmetic.
	| ADD	      MachRep Operand Operand
	| ADC	      MachRep Operand Operand
	| SUB	      MachRep Operand Operand

	| MUL	      MachRep Operand Operand
	| IMUL	      MachRep Operand Operand	-- signed int mul
        | IMUL2       MachRep Operand -- %edx:%eax = operand * %eax

	| DIV	      MachRep Operand	-- eax := eax:edx/op, edx := eax:edx%op
	| IDIV	      MachRep Operand	-- ditto, but signed

-- Simple bit-twiddling.
	| AND	      MachRep Operand Operand
	| OR	      MachRep Operand Operand
	| XOR	      MachRep Operand Operand
	| NOT	      MachRep Operand
	| NEGI	      MachRep Operand -- NEG instruction (name clash with Cond)

-- Shifts (amount may be immediate or %cl only)
	| SHL	      MachRep Operand{-amount-} Operand
	| SAR	      MachRep Operand{-amount-} Operand
	| SHR	      MachRep Operand{-amount-} Operand

        | BT          MachRep Imm Operand
	| NOP

#if i386_TARGET_ARCH
-- Float Arithmetic.

-- Note that we cheat by treating G{ABS,MOV,NEG} of doubles 
-- as single instructions right up until we spit them out.
        -- all the 3-operand fake fp insns are src1 src2 dst
        -- and furthermore are constrained to be fp regs only.
        -- IMPORTANT: keep is_G_insn up to date with any changes here
    	| GMOV	      Reg Reg -- src(fpreg), dst(fpreg)
        | GLD         MachRep AddrMode Reg -- src, dst(fpreg)
        | GST         MachRep Reg AddrMode -- src(fpreg), dst
		      
        | GLDZ        Reg -- dst(fpreg)
        | GLD1        Reg -- dst(fpreg)
		      
        | GFTOI       Reg Reg -- src(fpreg), dst(intreg)
        | GDTOI       Reg Reg -- src(fpreg), dst(intreg)
		      
        | GITOF       Reg Reg -- src(intreg), dst(fpreg)
        | GITOD       Reg Reg -- src(intreg), dst(fpreg)
	
	| GADD	      MachRep Reg Reg Reg -- src1, src2, dst
	| GDIV	      MachRep Reg Reg Reg -- src1, src2, dst
	| GSUB	      MachRep Reg Reg Reg -- src1, src2, dst
	| GMUL	      MachRep Reg Reg Reg -- src1, src2, dst
	
		-- FP compare.  Cond must be `elem` [EQQ, NE, LE, LTT, GE, GTT]
		-- Compare src1 with src2; set the Zero flag iff the numbers are
		-- comparable and the comparison is True.  Subsequent code must
		-- test the %eflags zero flag regardless of the supplied Cond.
    	| GCMP	      Cond Reg Reg -- src1, src2
	
     	| GABS	      MachRep Reg Reg -- src, dst
    	| GNEG	      MachRep Reg Reg -- src, dst
    	| GSQRT	      MachRep Reg Reg -- src, dst
    	| GSIN	      MachRep Reg Reg -- src, dst
    	| GCOS	      MachRep Reg Reg -- src, dst
    	| GTAN	      MachRep Reg Reg -- src, dst
	
        | GFREE         -- do ffree on all x86 regs; an ugly hack
#endif

#if x86_64_TARGET_ARCH
-- SSE2 floating point: we use a restricted set of the available SSE2
-- instructions for floating-point.

	-- use MOV for moving (either movss or movsd (movlpd better?))

	| CVTSS2SD	Reg Reg		-- F32 to F64
	| CVTSD2SS	Reg Reg		-- F64 to F32
	| CVTSS2SI	Operand Reg	-- F32 to I32/I64 (with rounding)
	| CVTSD2SI	Operand	Reg	-- F64 to I32/I64 (with rounding)
	| CVTSI2SS	Operand Reg	-- I32/I64 to F32
	| CVTSI2SD	Operand Reg	-- I32/I64 to F64

	-- use ADD & SUB for arithmetic.  In both cases, operands
	-- are  Operand Reg.

 	-- SSE2 floating-point division:
	| FDIV		MachRep Operand Operand   -- divisor, dividend(dst)

	-- use CMP for comparisons.  ucomiss and ucomisd instructions
	-- compare single/double prec floating point respectively.

	| SQRT		MachRep Operand Reg	-- src, dst
#endif

-- Comparison
	| TEST          MachRep Operand Operand
	| CMP           MachRep Operand Operand
	| SETCC         Cond Operand

-- Stack Operations.
	| PUSH          MachRep Operand
	| POP           MachRep Operand
	-- both unused (SDM):
	--  | PUSHA
	--  | POPA

-- Jumping around.
	| JMP	      Operand
	| JXX	      Cond BlockId  -- includes unconditional branches
	| JMP_TBL     Operand [BlockId]  -- table jump
	| CALL	      (Either Imm Reg) [Reg]

-- Other things.
	| CLTD MachRep	 -- sign extend %eax into %edx:%eax

	| FETCHGOT    Reg  -- pseudo-insn for ELF position-independent code
                           -- pretty-prints as
                           --       call 1f
                           -- 1:    popl %reg
                           --       addl __GLOBAL_OFFSET_TABLE__+.-1b, %reg
	| FETCHPC     Reg  -- pseudo-insn for Darwin position-independent code
                           -- pretty-prints as
                           --       call 1f
                           -- 1:    popl %reg


data Operand
  = OpReg  Reg	        -- register
  | OpImm  Imm	        -- immediate value
  | OpAddr AddrMode	-- memory reference

#endif /* i386 or x86_64 */

#if i386_TARGET_ARCH
i386_insert_ffrees :: [Instr] -> [Instr]
i386_insert_ffrees insns
   | any is_G_instr insns
   = concatMap ffree_before_nonlocal_transfers insns
   | otherwise
   = insns

ffree_before_nonlocal_transfers insn
   = case insn of
        CALL _ _ -> [GFREE, insn]
        JMP _    -> [GFREE, insn]
        other    -> [insn]


-- if you ever add a new FP insn to the fake x86 FP insn set,
-- you must update this too
is_G_instr :: Instr -> Bool
is_G_instr instr
   = case instr of
        GMOV _ _ -> True; GLD _ _ _ -> True; GST _ _ _ -> True
        GLDZ _ -> True; GLD1 _ -> True
        GFTOI _ _ -> True; GDTOI _ _ -> True
        GITOF _ _ -> True; GITOD _ _ -> True
	GADD _ _ _ _ -> True; GDIV _ _ _ _ -> True
	GSUB _ _ _ _ -> True; GMUL _ _ _ _ -> True
    	GCMP _ _ _ -> True; GABS _ _ _ -> True
    	GNEG _ _ _ -> True; GSQRT _ _ _ -> True
        GSIN _ _ _ -> True; GCOS _ _ _ -> True; GTAN _ _ _ -> True
        GFREE -> panic "is_G_instr: GFREE (!)"
        other -> False
#endif /* i386_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- Sparc instructions

#if sparc_TARGET_ARCH

-- data Instr continues...

-- Loads and stores.
	      | LD	      MachRep AddrMode Reg -- size, src, dst
	      | ST	      MachRep Reg AddrMode -- size, src, dst

-- Int Arithmetic.
	      | ADD	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst
	      | SUB	      Bool Bool Reg RI Reg -- x?, cc?, src1, src2, dst
	      | UMUL	           Bool Reg RI Reg --     cc?, src1, src2, dst
	      | SMUL	           Bool Reg RI Reg --     cc?, src1, src2, dst
              | RDY           Reg	-- move contents of Y register to reg

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

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single
-- instructions right up until we spit them out.
    	      | FABS	      MachRep Reg Reg	   -- src dst
	      | FADD	      MachRep Reg Reg Reg  -- src1, src2, dst
    	      | FCMP	      Bool MachRep Reg Reg -- exception?, src1, src2, dst
	      | FDIV	      MachRep Reg Reg Reg -- src1, src2, dst
    	      | FMOV	      MachRep Reg Reg     -- src, dst
	      | FMUL	      MachRep Reg Reg Reg -- src1, src2, dst
    	      | FNEG	      MachRep Reg Reg     -- src, dst
    	      | FSQRT	      MachRep Reg Reg     -- src, dst
	      | FSUB	      MachRep Reg Reg Reg -- src1, src2, dst
    	      | FxTOy	      MachRep MachRep Reg Reg -- src, dst

-- Jumping around.
	      | BI	      Cond Bool Imm -- cond, annul?, target
    	      | BF  	      Cond Bool Imm -- cond, annul?, target

	      | JMP	      AddrMode     -- target
	      | CALL	      (Either Imm Reg) Int Bool -- target, args, terminal

riZero :: RI -> Bool

riZero (RIImm (ImmInt 0))	    = True
riZero (RIImm (ImmInteger 0))	    = True
riZero (RIReg (RealReg 0))          = True
riZero _			    = False

-- Calculate the effective address which would be used by the
-- corresponding fpRel sequence.  fpRel is in MachRegs.lhs,
-- alas -- can't have fpRelEA here because of module dependencies.
fpRelEA :: Int -> Reg -> Instr
fpRelEA n dst
   = ADD False False fp (RIImm (ImmInt (n * wORD_SIZE))) dst

-- Code to shift the stack pointer by n words.
moveSp :: Int -> Instr
moveSp n
   = ADD False False sp (RIImm (ImmInt (n * wORD_SIZE))) sp

-- Produce the second-half-of-a-double register given the first half.
fPair :: Reg -> Reg
fPair (RealReg n) | n >= 32 && n `mod` 2 == 0  = RealReg (n+1)
fPair other = pprPanic "fPair(sparc NCG)" (ppr other)
#endif /* sparc_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- PowerPC instructions

#ifdef powerpc_TARGET_ARCH
-- data Instr continues...

-- Loads and stores.
	      | LD	MachRep Reg AddrMode -- Load size, dst, src
	      | LA      MachRep Reg AddrMode -- Load arithmetic size, dst, src
	      | ST	MachRep Reg AddrMode -- Store size, src, dst 
	      | STU	MachRep Reg AddrMode -- Store with Update size, src, dst 
	      | LIS	Reg Imm -- Load Immediate Shifted dst, src
	      | LI	Reg Imm -- Load Immediate dst, src
	      | MR	Reg Reg -- Move Register dst, src -- also for fmr
	      
	      | CMP     MachRep Reg RI --- size, src1, src2
	      | CMPL    MachRep Reg RI --- size, src1, src2
	      
	      | BCC     Cond BlockId
              | JMP     CLabel          -- same as branch,
                                        -- but with CLabel instead of block ID
	      | MTCTR	Reg
	      | BCTR    [BlockId]       -- with list of local destinations
	      | BL	CLabel [Reg]	-- with list of argument regs
	      | BCTRL	[Reg]
	      
	      | ADD     Reg Reg RI -- dst, src1, src2
	      | ADDC    Reg Reg Reg -- (carrying) dst, src1, src2
	      | ADDE    Reg Reg Reg -- (extend) dst, src1, src2
	      | ADDIS   Reg Reg Imm -- Add Immediate Shifted dst, src1, src2
	      | SUBF    Reg Reg Reg -- dst, src1, src2 ; dst = src2 - src1  
	      | MULLW	Reg Reg RI
	      | DIVW	Reg Reg Reg
	      | DIVWU	Reg Reg Reg

	      | MULLW_MayOflo Reg Reg Reg
                        -- dst = 1 if src1 * src2 overflows
                        -- pseudo-instruction; pretty-printed as:
                        -- mullwo. dst, src1, src2
                        -- mfxer dst
                        -- rlwinm dst, dst, 2, 31,31
	      
	      | AND	Reg Reg RI -- dst, src1, src2
	      | OR	Reg Reg RI -- dst, src1, src2
	      | XOR	Reg Reg RI -- dst, src1, src2
	      | XORIS	Reg Reg Imm -- XOR Immediate Shifted dst, src1, src2
	      
              | EXTS    MachRep Reg Reg
		  
	      | NEG	Reg Reg
	      | NOT	Reg Reg
	      
	      | SLW	Reg Reg RI	-- shift left word
	      | SRW	Reg Reg RI	-- shift right word
	      | SRAW	Reg Reg RI	-- shift right arithmetic word
	      
        	        -- Rotate Left Word Immediate then AND with Mask
	      | RLWINM  Reg Reg Int Int Int
	      
	      | FADD	MachRep Reg Reg Reg
	      | FSUB	MachRep Reg Reg Reg
	      | FMUL	MachRep Reg Reg Reg
	      | FDIV	MachRep Reg Reg Reg
	      | FNEG	Reg Reg	 -- negate is the same for single and double prec.
	      
	      | FCMP	Reg Reg
	      
	      | FCTIWZ	Reg Reg		-- convert to integer word
              | FRSP    Reg Reg		-- reduce to single precision
					-- (but destination is a FP register)
	      
	      | CRNOR   Int Int Int    -- condition register nor
	      | MFCR    Reg            -- move from condition register
	      
	      | MFLR    Reg            -- move from link register
	      | FETCHPC Reg            -- pseudo-instruction:
	                               -- bcl to next insn, mflr reg
	      
	      | LWSYNC -- memory barrier
#endif /* powerpc_TARGET_ARCH */
