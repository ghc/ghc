-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "HsVersions.h"
#include "nativeGen/NCG.h"

module X86.Instr
where

import BlockId
import MachRegs
import Cmm
import FastString

data Cond
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


data Instr
	-- comment pseudo-op
	= COMMENT FastString		

	-- some static data spat out during code
	-- generation.  Will be extracted before
	-- pretty-printing.
	| LDATA   Section [CmmStatic]	

	-- start a new basic block.  Useful during
	-- codegen, removed later.  Preceding 
	-- instruction should be a jump, as per the
	-- invariants for a BasicBlock (see Cmm).
	| NEWBLOCK BlockId		

	-- specify current stack offset for
        -- benefit of subsequent passes
	| DELTA   Int

	-- | spill this reg to a stack slot
	| SPILL   Reg Int

	-- | reload this reg from a stack slot
	| RELOAD  Int Reg


	-- Moves.
	| MOV	      Size Operand Operand
	| MOVZxL      Size Operand Operand -- size is the size of operand 1
	| MOVSxL      Size Operand Operand -- size is the size of operand 1
	-- x86_64 note: plain mov into a 32-bit register always zero-extends
	-- into the 64-bit reg, in contrast to the 8 and 16-bit movs which
	-- don't affect the high bits of the register.

	-- Load effective address (also a very useful three-operand add instruction :-)
	| LEA         Size Operand Operand

	-- Int Arithmetic.
	| ADD	      Size Operand Operand
	| ADC	      Size Operand Operand
	| SUB	      Size Operand Operand

	| MUL	      Size Operand Operand
	| IMUL	      Size Operand Operand	-- signed int mul
        | IMUL2       Size Operand 		-- %edx:%eax = operand * %eax

	| DIV	      Size Operand		-- eax := eax:edx/op, edx := eax:edx%op
	| IDIV	      Size Operand		-- ditto, but signed

	-- Simple bit-twiddling.
	| AND	      Size Operand Operand
	| OR	      Size Operand Operand
	| XOR	      Size Operand Operand
	| NOT	      Size Operand
	| NEGI	      Size Operand 		-- NEG instruction (name clash with Cond)

	-- Shifts (amount may be immediate or %cl only)
	| SHL	      Size Operand{-amount-} Operand
	| SAR	      Size Operand{-amount-} Operand
	| SHR	      Size Operand{-amount-} Operand

        | BT          Size Imm Operand
	| NOP

#if i386_TARGET_ARCH
	-- Float Arithmetic.

	-- Note that we cheat by treating G{ABS,MOV,NEG} of doubles 
	-- as single instructions right up until we spit them out.
        -- all the 3-operand fake fp insns are src1 src2 dst
        -- and furthermore are constrained to be fp regs only.
        -- IMPORTANT: keep is_G_insn up to date with any changes here
    	| GMOV	      Reg Reg -- src(fpreg), dst(fpreg)
        | GLD         Size AddrMode Reg -- src, dst(fpreg)
        | GST         Size Reg AddrMode -- src(fpreg), dst
		      
        | GLDZ        Reg -- dst(fpreg)
        | GLD1        Reg -- dst(fpreg)
		      
        | GFTOI       Reg Reg -- src(fpreg), dst(intreg)
        | GDTOI       Reg Reg -- src(fpreg), dst(intreg)
		      
        | GITOF       Reg Reg -- src(intreg), dst(fpreg)
        | GITOD       Reg Reg -- src(intreg), dst(fpreg)
	
	| GADD	      Size Reg Reg Reg -- src1, src2, dst
	| GDIV	      Size Reg Reg Reg -- src1, src2, dst
	| GSUB	      Size Reg Reg Reg -- src1, src2, dst
	| GMUL	      Size Reg Reg Reg -- src1, src2, dst
	
		-- FP compare.  Cond must be `elem` [EQQ, NE, LE, LTT, GE, GTT]
		-- Compare src1 with src2; set the Zero flag iff the numbers are
		-- comparable and the comparison is True.  Subsequent code must
		-- test the %eflags zero flag regardless of the supplied Cond.
    	| GCMP	      Cond Reg Reg -- src1, src2
	
     	| GABS	      Size Reg Reg -- src, dst
    	| GNEG	      Size Reg Reg -- src, dst
    	| GSQRT	      Size Reg Reg -- src, dst
    	| GSIN	      Size CLabel CLabel Reg Reg -- src, dst
    	| GCOS	      Size CLabel CLabel Reg Reg -- src, dst
    	| GTAN	      Size CLabel CLabel Reg Reg -- src, dst
	
        | GFREE         -- do ffree on all x86 regs; an ugly hack
#endif

#if x86_64_TARGET_ARCH
-- SSE2 floating point: we use a restricted set of the available SSE2
-- instructions for floating-point.

	-- use MOV for moving (either movss or movsd (movlpd better?))

	| CVTSS2SD	Reg Reg		-- F32 to F64
	| CVTSD2SS	Reg Reg		-- F64 to F32
	| CVTTSS2SIQ	Operand Reg	-- F32 to I32/I64 (with truncation)
	| CVTTSD2SIQ	Operand	Reg	-- F64 to I32/I64 (with truncation)
	| CVTSI2SS	Operand Reg	-- I32/I64 to F32
	| CVTSI2SD	Operand Reg	-- I32/I64 to F64

	-- use ADD & SUB for arithmetic.  In both cases, operands
	-- are  Operand Reg.

 	-- SSE2 floating-point division:
	| FDIV		Size Operand Operand   -- divisor, dividend(dst)

	-- use CMP for comparisons.  ucomiss and ucomisd instructions
	-- compare single/double prec floating point respectively.

	| SQRT		Size Operand Reg	-- src, dst
#endif

	-- Comparison
	| TEST          Size Operand Operand
	| CMP           Size Operand Operand
	| SETCC         Cond Operand

	-- Stack Operations.
	| PUSH          Size Operand
	| POP           Size Operand
	-- both unused (SDM):
	--  | PUSHA
	--  | POPA

	-- Jumping around.
	| JMP	      Operand
	| JXX	      Cond BlockId  -- includes unconditional branches
	| JXX_GBL     Cond Imm      -- non-local version of JXX
	| JMP_TBL     Operand [BlockId]  -- table jump
	| CALL	      (Either Imm Reg) [Reg]

	-- Other things.
	| CLTD Size		 -- sign extend %eax into %edx:%eax

	| FETCHGOT    Reg 	 -- pseudo-insn for ELF position-independent code
                          	 -- pretty-prints as
                          	 --       call 1f
                          	 -- 1:    popl %reg
                          	 --       addl __GLOBAL_OFFSET_TABLE__+.-1b, %reg
	| FETCHPC     Reg 	 -- pseudo-insn for Darwin position-independent code
                          	 -- pretty-prints as
                          	 --       call 1f
                          	 -- 1:    popl %reg
	

data Operand
	= OpReg  Reg	        -- register
	| OpImm  Imm	        -- immediate value
	| OpAddr AddrMode	-- memory reference


#if i386_TARGET_ARCH
i386_insert_ffrees :: [GenBasicBlock Instr] -> [GenBasicBlock Instr]
i386_insert_ffrees blocks
   | or (map (any is_G_instr) [ instrs | BasicBlock id instrs <- blocks ])
   = map ffree_before_nonlocal_transfers blocks
   | otherwise
   = blocks
  where
   ffree_before_nonlocal_transfers (BasicBlock id insns) 
     = BasicBlock id (foldr p [] insns)
     where p insn r = case insn of
                        CALL _ _ -> GFREE : insn : r
                        JMP _    -> GFREE : insn : r
                        other    -> insn : r

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
        GSIN _ _ _ _ _ -> True; GCOS _ _ _ _ _ -> True; GTAN _ _ _ _ _ -> True
        GFREE -> panic "is_G_instr: GFREE (!)"
        other -> False
#endif /* i386_TARGET_ARCH */
