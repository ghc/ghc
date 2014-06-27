{-# LANGUAGE CPP, TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

module X86.Instr (Instr(..), Operand(..), PrefetchVariant(..), JumpDest,
                  getJumpDestBlockId, canShortcut, shortcutStatics,
                  shortcutJump, i386_insert_ffrees, allocMoreStack,
                  maxSpillSlots, archWordSize)
where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import X86.Cond
import X86.Regs
import Instruction
import Size
import RegClass
import Reg
import TargetReg

import BlockId
import CodeGen.Platform
import Cmm
import FastString
import FastBool
import Outputable
import Platform

import BasicTypes       (Alignment)
import CLabel
import DynFlags
import UniqSet
import Unique
import UniqSupply

import Control.Monad
import Data.Maybe       (fromMaybe)

-- Size of an x86/x86_64 memory address, in bytes.
--
archWordSize :: Bool -> Size
archWordSize is32Bit
 | is32Bit   = II32
 | otherwise = II64

-- | Instruction instance for x86 instruction set.
instance Instruction Instr where
        regUsageOfInstr         = x86_regUsageOfInstr
        patchRegsOfInstr        = x86_patchRegsOfInstr
        isJumpishInstr          = x86_isJumpishInstr
        jumpDestsOfInstr        = x86_jumpDestsOfInstr
        patchJumpInstr          = x86_patchJumpInstr
        mkSpillInstr            = x86_mkSpillInstr
        mkLoadInstr             = x86_mkLoadInstr
        takeDeltaInstr          = x86_takeDeltaInstr
        isMetaInstr             = x86_isMetaInstr
        mkRegRegMoveInstr       = x86_mkRegRegMoveInstr
        takeRegRegMoveInstr     = x86_takeRegRegMoveInstr
        mkJumpInstr             = x86_mkJumpInstr
        mkStackAllocInstr       = x86_mkStackAllocInstr
        mkStackDeallocInstr     = x86_mkStackDeallocInstr


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
Note [x86 Floating point precision]

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

In the future [edit: now available in GHC 7.0.1, with the -msse2
flag], we'll use the SSE registers for floating point.  This requires
a CPU that supports SSE2 (ordinary SSE only supports 32 bit precision
float ops), which means P4 or Xeon and above.  Using SSE will solve
all these problems, because the SSE registers use fixed 32 bit or 64
bit precision.

--SDM 1/2003
-}

data Instr
        -- comment pseudo-op
        = COMMENT FastString

        -- some static data spat out during code
        -- generation.  Will be extracted before
        -- pretty-printing.
        | LDATA   Section (Alignment, CmmStatics)

        -- start a new basic block.  Useful during
        -- codegen, removed later.  Preceding
        -- instruction should be a jump, as per the
        -- invariants for a BasicBlock (see Cmm).
        | NEWBLOCK BlockId

        -- specify current stack offset for
        -- benefit of subsequent passes
        | DELTA   Int

        -- Moves.
        | MOV         Size Operand Operand
        | MOVZxL      Size Operand Operand -- size is the size of operand 1
        | MOVSxL      Size Operand Operand -- size is the size of operand 1
        -- x86_64 note: plain mov into a 32-bit register always zero-extends
        -- into the 64-bit reg, in contrast to the 8 and 16-bit movs which
        -- don't affect the high bits of the register.

        -- Load effective address (also a very useful three-operand add instruction :-)
        | LEA         Size Operand Operand

        -- Int Arithmetic.
        | ADD         Size Operand Operand
        | ADC         Size Operand Operand
        | SUB         Size Operand Operand

        | MUL         Size Operand Operand
        | MUL2        Size Operand              -- %edx:%eax = operand * %rax
        | IMUL        Size Operand Operand      -- signed int mul
        | IMUL2       Size Operand              -- %edx:%eax = operand * %eax

        | DIV         Size Operand              -- eax := eax:edx/op, edx := eax:edx%op
        | IDIV        Size Operand              -- ditto, but signed

        -- Simple bit-twiddling.
        | AND         Size Operand Operand
        | OR          Size Operand Operand
        | XOR         Size Operand Operand
        | NOT         Size Operand
        | NEGI        Size Operand              -- NEG instruction (name clash with Cond)
        | BSWAP       Size Reg

        -- Shifts (amount may be immediate or %cl only)
        | SHL         Size Operand{-amount-} Operand
        | SAR         Size Operand{-amount-} Operand
        | SHR         Size Operand{-amount-} Operand

        | BT          Size Imm Operand
        | NOP

        -- x86 Float Arithmetic.
        -- Note that we cheat by treating G{ABS,MOV,NEG} of doubles
        -- as single instructions right up until we spit them out.
        -- all the 3-operand fake fp insns are src1 src2 dst
        -- and furthermore are constrained to be fp regs only.
        -- IMPORTANT: keep is_G_insn up to date with any changes here
        | GMOV        Reg Reg -- src(fpreg), dst(fpreg)
        | GLD         Size AddrMode Reg -- src, dst(fpreg)
        | GST         Size Reg AddrMode -- src(fpreg), dst

        | GLDZ        Reg -- dst(fpreg)
        | GLD1        Reg -- dst(fpreg)

        | GFTOI       Reg Reg -- src(fpreg), dst(intreg)
        | GDTOI       Reg Reg -- src(fpreg), dst(intreg)

        | GITOF       Reg Reg -- src(intreg), dst(fpreg)
        | GITOD       Reg Reg -- src(intreg), dst(fpreg)

        | GDTOF       Reg Reg -- src(fpreg), dst(fpreg)

        | GADD        Size Reg Reg Reg -- src1, src2, dst
        | GDIV        Size Reg Reg Reg -- src1, src2, dst
        | GSUB        Size Reg Reg Reg -- src1, src2, dst
        | GMUL        Size Reg Reg Reg -- src1, src2, dst

                -- FP compare.  Cond must be `elem` [EQQ, NE, LE, LTT, GE, GTT]
                -- Compare src1 with src2; set the Zero flag iff the numbers are
                -- comparable and the comparison is True.  Subsequent code must
                -- test the %eflags zero flag regardless of the supplied Cond.
        | GCMP        Cond Reg Reg -- src1, src2

        | GABS        Size Reg Reg -- src, dst
        | GNEG        Size Reg Reg -- src, dst
        | GSQRT       Size Reg Reg -- src, dst
        | GSIN        Size CLabel CLabel Reg Reg -- src, dst
        | GCOS        Size CLabel CLabel Reg Reg -- src, dst
        | GTAN        Size CLabel CLabel Reg Reg -- src, dst

        | GFREE         -- do ffree on all x86 regs; an ugly hack


        -- SSE2 floating point: we use a restricted set of the available SSE2
        -- instructions for floating-point.
        -- use MOV for moving (either movss or movsd (movlpd better?))
        | CVTSS2SD      Reg Reg         -- F32 to F64
        | CVTSD2SS      Reg Reg         -- F64 to F32
        | CVTTSS2SIQ    Size Operand Reg -- F32 to I32/I64 (with truncation)
        | CVTTSD2SIQ    Size Operand Reg -- F64 to I32/I64 (with truncation)
        | CVTSI2SS      Size Operand Reg -- I32/I64 to F32
        | CVTSI2SD      Size Operand Reg -- I32/I64 to F64

        -- use ADD & SUB for arithmetic.  In both cases, operands
        -- are  Operand Reg.

        -- SSE2 floating-point division:
        | FDIV          Size Operand Operand   -- divisor, dividend(dst)

        -- use CMP for comparisons.  ucomiss and ucomisd instructions
        -- compare single/double prec floating point respectively.

        | SQRT          Size Operand Reg        -- src, dst


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
        | JMP         Operand [Reg] -- including live Regs at the call
        | JXX         Cond BlockId  -- includes unconditional branches
        | JXX_GBL     Cond Imm      -- non-local version of JXX
        -- Table jump
        | JMP_TBL     Operand   -- Address to jump to
                      [Maybe BlockId] -- Blocks in the jump table
                      Section   -- Data section jump table should be put in
                      CLabel    -- Label of jump table
        | CALL        (Either Imm Reg) [Reg]

        -- Other things.
        | CLTD Size              -- sign extend %eax into %edx:%eax

        | FETCHGOT    Reg        -- pseudo-insn for ELF position-independent code
                                 -- pretty-prints as
                                 --       call 1f
                                 -- 1:    popl %reg
                                 --       addl __GLOBAL_OFFSET_TABLE__+.-1b, %reg
        | FETCHPC     Reg        -- pseudo-insn for Darwin position-independent code
                                 -- pretty-prints as
                                 --       call 1f
                                 -- 1:    popl %reg

    -- SSE4.2
        | POPCNT      Size Operand Reg -- src, dst

    -- prefetch
        | PREFETCH  PrefetchVariant Size Operand -- prefetch Variant, addr size, address to prefetch
                                        -- variant can be NTA, Lvl0, Lvl1, or Lvl2

        | LOCK  -- lock prefix
        | XADD        Size Operand Operand  -- src (r), dst (r/m)
        | CMPXCHG     Size Operand Operand  -- src (r), dst (r/m), eax implicit

data PrefetchVariant = NTA | Lvl0 | Lvl1 | Lvl2


data Operand
        = OpReg  Reg            -- register
        | OpImm  Imm            -- immediate value
        | OpAddr AddrMode       -- memory reference



-- | Returns which registers are read and written as a (read, written)
-- pair.
x86_regUsageOfInstr :: Platform -> Instr -> RegUsage
x86_regUsageOfInstr platform instr
 = case instr of
    MOV    _ src dst    -> usageRW src dst
    MOVZxL _ src dst    -> usageRW src dst
    MOVSxL _ src dst    -> usageRW src dst
    LEA    _ src dst    -> usageRW src dst
    ADD    _ src dst    -> usageRM src dst
    ADC    _ src dst    -> usageRM src dst
    SUB    _ src dst    -> usageRM src dst
    IMUL   _ src dst    -> usageRM src dst
    IMUL2  _ src       -> mkRU (eax:use_R src []) [eax,edx]
    MUL    _ src dst    -> usageRM src dst
    MUL2   _ src        -> mkRU (eax:use_R src []) [eax,edx]
    DIV    _ op -> mkRU (eax:edx:use_R op []) [eax,edx]
    IDIV   _ op -> mkRU (eax:edx:use_R op []) [eax,edx]
    AND    _ src dst    -> usageRM src dst
    OR     _ src dst    -> usageRM src dst

    XOR    _ (OpReg src) (OpReg dst)
        | src == dst    -> mkRU [] [dst]

    XOR    _ src dst    -> usageRM src dst
    NOT    _ op         -> usageM op
    BSWAP  _ reg        -> mkRU [reg] [reg]
    NEGI   _ op         -> usageM op
    SHL    _ imm dst    -> usageRM imm dst
    SAR    _ imm dst    -> usageRM imm dst
    SHR    _ imm dst    -> usageRM imm dst
    BT     _ _   src    -> mkRUR (use_R src [])

    PUSH   _ op         -> mkRUR (use_R op [])
    POP    _ op         -> mkRU [] (def_W op)
    TEST   _ src dst    -> mkRUR (use_R src $! use_R dst [])
    CMP    _ src dst    -> mkRUR (use_R src $! use_R dst [])
    SETCC  _ op         -> mkRU [] (def_W op)
    JXX    _ _          -> mkRU [] []
    JXX_GBL _ _         -> mkRU [] []
    JMP     op regs     -> mkRUR (use_R op regs)
    JMP_TBL op _ _ _    -> mkRUR (use_R op [])
    CALL (Left _)  params   -> mkRU params (callClobberedRegs platform)
    CALL (Right reg) params -> mkRU (reg:params) (callClobberedRegs platform)
    CLTD   _            -> mkRU [eax] [edx]
    NOP                 -> mkRU [] []

    GMOV   src dst      -> mkRU [src] [dst]
    GLD    _ src dst    -> mkRU (use_EA src []) [dst]
    GST    _ src dst    -> mkRUR (src : use_EA dst [])

    GLDZ   dst          -> mkRU [] [dst]
    GLD1   dst          -> mkRU [] [dst]

    GFTOI  src dst      -> mkRU [src] [dst]
    GDTOI  src dst      -> mkRU [src] [dst]

    GITOF  src dst      -> mkRU [src] [dst]
    GITOD  src dst      -> mkRU [src] [dst]

    GDTOF  src dst      -> mkRU [src] [dst]

    GADD   _ s1 s2 dst  -> mkRU [s1,s2] [dst]
    GSUB   _ s1 s2 dst  -> mkRU [s1,s2] [dst]
    GMUL   _ s1 s2 dst  -> mkRU [s1,s2] [dst]
    GDIV   _ s1 s2 dst  -> mkRU [s1,s2] [dst]

    GCMP   _ src1 src2   -> mkRUR [src1,src2]
    GABS   _ src dst     -> mkRU [src] [dst]
    GNEG   _ src dst     -> mkRU [src] [dst]
    GSQRT  _ src dst     -> mkRU [src] [dst]
    GSIN   _ _ _ src dst -> mkRU [src] [dst]
    GCOS   _ _ _ src dst -> mkRU [src] [dst]
    GTAN   _ _ _ src dst -> mkRU [src] [dst]

    CVTSS2SD   src dst  -> mkRU [src] [dst]
    CVTSD2SS   src dst  -> mkRU [src] [dst]
    CVTTSS2SIQ _ src dst -> mkRU (use_R src []) [dst]
    CVTTSD2SIQ _ src dst -> mkRU (use_R src []) [dst]
    CVTSI2SS   _ src dst -> mkRU (use_R src []) [dst]
    CVTSI2SD   _ src dst -> mkRU (use_R src []) [dst]
    FDIV _     src dst  -> usageRM src dst

    FETCHGOT reg        -> mkRU [] [reg]
    FETCHPC  reg        -> mkRU [] [reg]

    COMMENT _           -> noUsage
    DELTA   _           -> noUsage

    POPCNT _ src dst -> mkRU (use_R src []) [dst]

    -- note: might be a better way to do this
    PREFETCH _  _ src -> mkRU (use_R src []) []
    LOCK                -> noUsage
    XADD _ src dst      -> usageMM src dst
    CMPXCHG _ src dst   -> usageRMM src dst (OpReg eax)

    _other              -> panic "regUsage: unrecognised instr"
 where
    -- # Definitions
    --
    -- Written: If the operand is a register, it's written. If it's an
    -- address, registers mentioned in the address are read.
    --
    -- Modified: If the operand is a register, it's both read and
    -- written. If it's an address, registers mentioned in the address
    -- are read.

    -- 2 operand form; first operand Read; second Written
    usageRW :: Operand -> Operand -> RegUsage
    usageRW op (OpReg reg)      = mkRU (use_R op []) [reg]
    usageRW op (OpAddr ea)      = mkRUR (use_R op $! use_EA ea [])
    usageRW _ _                 = panic "X86.RegInfo.usageRW: no match"

    -- 2 operand form; first operand Read; second Modified
    usageRM :: Operand -> Operand -> RegUsage
    usageRM op (OpReg reg)      = mkRU (use_R op [reg]) [reg]
    usageRM op (OpAddr ea)      = mkRUR (use_R op $! use_EA ea [])
    usageRM _ _                 = panic "X86.RegInfo.usageRM: no match"

    -- 2 operand form; first operand Modified; second Modified
    usageMM :: Operand -> Operand -> RegUsage
    usageMM (OpReg src) (OpReg dst) = mkRU [src, dst] [src, dst]
    usageMM (OpReg src) (OpAddr ea) = mkRU (use_EA ea [src]) [src]
    usageMM _ _                     = panic "X86.RegInfo.usageMM: no match"

    -- 3 operand form; first operand Read; second Modified; third Modified
    usageRMM :: Operand -> Operand -> Operand -> RegUsage
    usageRMM (OpReg src) (OpReg dst) (OpReg reg) = mkRU [src, dst, reg] [dst, reg]
    usageRMM (OpReg src) (OpAddr ea) (OpReg reg) = mkRU (use_EA ea [src, reg]) [reg]
    usageRMM _ _ _                               = panic "X86.RegInfo.usageRMM: no match"

    -- 1 operand form; operand Modified
    usageM :: Operand -> RegUsage
    usageM (OpReg reg)          = mkRU [reg] [reg]
    usageM (OpAddr ea)          = mkRUR (use_EA ea [])
    usageM _                    = panic "X86.RegInfo.usageM: no match"

    -- Registers defd when an operand is written.
    def_W (OpReg reg)           = [reg]
    def_W (OpAddr _ )           = []
    def_W _                     = panic "X86.RegInfo.def_W: no match"

    -- Registers used when an operand is read.
    use_R (OpReg reg)  tl = reg : tl
    use_R (OpImm _)    tl = tl
    use_R (OpAddr ea)  tl = use_EA ea tl

    -- Registers used to compute an effective address.
    use_EA (ImmAddr _ _) tl = tl
    use_EA (AddrBaseIndex base index _) tl =
        use_base base $! use_index index tl
        where use_base (EABaseReg r)  tl = r : tl
              use_base _              tl = tl
              use_index EAIndexNone   tl = tl
              use_index (EAIndex i _) tl = i : tl

    mkRUR src = src' `seq` RU src' []
        where src' = filter (interesting platform) src

    mkRU src dst = src' `seq` dst' `seq` RU src' dst'
        where src' = filter (interesting platform) src
              dst' = filter (interesting platform) dst

-- | Is this register interesting for the register allocator?
interesting :: Platform -> Reg -> Bool
interesting _        (RegVirtual _)              = True
interesting platform (RegReal (RealRegSingle i)) = isFastTrue (freeReg platform i)
interesting _        (RegReal (RealRegPair{}))   = panic "X86.interesting: no reg pairs on this arch"



-- | Applies the supplied function to all registers in instructions.
-- Typically used to change virtual registers to real registers.
x86_patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
x86_patchRegsOfInstr instr env
 = case instr of
    MOV  sz src dst     -> patch2 (MOV  sz) src dst
    MOVZxL sz src dst   -> patch2 (MOVZxL sz) src dst
    MOVSxL sz src dst   -> patch2 (MOVSxL sz) src dst
    LEA  sz src dst     -> patch2 (LEA  sz) src dst
    ADD  sz src dst     -> patch2 (ADD  sz) src dst
    ADC  sz src dst     -> patch2 (ADC  sz) src dst
    SUB  sz src dst     -> patch2 (SUB  sz) src dst
    IMUL sz src dst     -> patch2 (IMUL sz) src dst
    IMUL2 sz src        -> patch1 (IMUL2 sz) src
    MUL sz src dst      -> patch2 (MUL sz) src dst
    MUL2 sz src         -> patch1 (MUL2 sz) src
    IDIV sz op          -> patch1 (IDIV sz) op
    DIV sz op           -> patch1 (DIV sz) op
    AND  sz src dst     -> patch2 (AND  sz) src dst
    OR   sz src dst     -> patch2 (OR   sz) src dst
    XOR  sz src dst     -> patch2 (XOR  sz) src dst
    NOT  sz op          -> patch1 (NOT  sz) op
    BSWAP sz reg        -> BSWAP sz (env reg)
    NEGI sz op          -> patch1 (NEGI sz) op
    SHL  sz imm dst     -> patch1 (SHL sz imm) dst
    SAR  sz imm dst     -> patch1 (SAR sz imm) dst
    SHR  sz imm dst     -> patch1 (SHR sz imm) dst
    BT   sz imm src     -> patch1 (BT  sz imm) src
    TEST sz src dst     -> patch2 (TEST sz) src dst
    CMP  sz src dst     -> patch2 (CMP  sz) src dst
    PUSH sz op          -> patch1 (PUSH sz) op
    POP  sz op          -> patch1 (POP  sz) op
    SETCC cond op       -> patch1 (SETCC cond) op
    JMP op regs         -> JMP (patchOp op) regs
    JMP_TBL op ids s lbl-> JMP_TBL (patchOp op) ids s lbl

    GMOV src dst        -> GMOV (env src) (env dst)
    GLD  sz src dst     -> GLD sz (lookupAddr src) (env dst)
    GST  sz src dst     -> GST sz (env src) (lookupAddr dst)

    GLDZ dst            -> GLDZ (env dst)
    GLD1 dst            -> GLD1 (env dst)

    GFTOI src dst       -> GFTOI (env src) (env dst)
    GDTOI src dst       -> GDTOI (env src) (env dst)

    GITOF src dst       -> GITOF (env src) (env dst)
    GITOD src dst       -> GITOD (env src) (env dst)

    GDTOF src dst       -> GDTOF (env src) (env dst)

    GADD sz s1 s2 dst   -> GADD sz (env s1) (env s2) (env dst)
    GSUB sz s1 s2 dst   -> GSUB sz (env s1) (env s2) (env dst)
    GMUL sz s1 s2 dst   -> GMUL sz (env s1) (env s2) (env dst)
    GDIV sz s1 s2 dst   -> GDIV sz (env s1) (env s2) (env dst)

    GCMP sz src1 src2   -> GCMP sz (env src1) (env src2)
    GABS sz src dst     -> GABS sz (env src) (env dst)
    GNEG sz src dst     -> GNEG sz (env src) (env dst)
    GSQRT sz src dst    -> GSQRT sz (env src) (env dst)
    GSIN sz l1 l2 src dst       -> GSIN sz l1 l2 (env src) (env dst)
    GCOS sz l1 l2 src dst       -> GCOS sz l1 l2 (env src) (env dst)
    GTAN sz l1 l2 src dst       -> GTAN sz l1 l2 (env src) (env dst)

    CVTSS2SD src dst    -> CVTSS2SD (env src) (env dst)
    CVTSD2SS src dst    -> CVTSD2SS (env src) (env dst)
    CVTTSS2SIQ sz src dst -> CVTTSS2SIQ sz (patchOp src) (env dst)
    CVTTSD2SIQ sz src dst -> CVTTSD2SIQ sz (patchOp src) (env dst)
    CVTSI2SS sz src dst -> CVTSI2SS sz (patchOp src) (env dst)
    CVTSI2SD sz src dst -> CVTSI2SD sz (patchOp src) (env dst)
    FDIV sz src dst     -> FDIV sz (patchOp src) (patchOp dst)

    CALL (Left _)  _    -> instr
    CALL (Right reg) p  -> CALL (Right (env reg)) p

    FETCHGOT reg        -> FETCHGOT (env reg)
    FETCHPC  reg        -> FETCHPC  (env reg)

    NOP                 -> instr
    COMMENT _           -> instr
    DELTA _             -> instr

    JXX _ _             -> instr
    JXX_GBL _ _         -> instr
    CLTD _              -> instr

    POPCNT sz src dst -> POPCNT sz (patchOp src) (env dst)

    PREFETCH lvl size src -> PREFETCH lvl size (patchOp src)

    LOCK                -> instr
    XADD sz src dst     -> patch2 (XADD sz) src dst
    CMPXCHG sz src dst  -> patch2 (CMPXCHG sz) src dst

    _other              -> panic "patchRegs: unrecognised instr"

  where
    patch1 :: (Operand -> a) -> Operand -> a
    patch1 insn op      = insn $! patchOp op
    patch2 :: (Operand -> Operand -> a) -> Operand -> Operand -> a
    patch2 insn src dst = (insn $! patchOp src) $! patchOp dst

    patchOp (OpReg  reg) = OpReg $! env reg
    patchOp (OpImm  imm) = OpImm imm
    patchOp (OpAddr ea)  = OpAddr $! lookupAddr ea

    lookupAddr (ImmAddr imm off) = ImmAddr imm off
    lookupAddr (AddrBaseIndex base index disp)
      = ((AddrBaseIndex $! lookupBase base) $! lookupIndex index) disp
      where
        lookupBase EABaseNone       = EABaseNone
        lookupBase EABaseRip        = EABaseRip
        lookupBase (EABaseReg r)    = EABaseReg $! env r

        lookupIndex EAIndexNone     = EAIndexNone
        lookupIndex (EAIndex r i)   = (EAIndex $! env r) i


--------------------------------------------------------------------------------
x86_isJumpishInstr
        :: Instr -> Bool

x86_isJumpishInstr instr
 = case instr of
        JMP{}           -> True
        JXX{}           -> True
        JXX_GBL{}       -> True
        JMP_TBL{}       -> True
        CALL{}          -> True
        _               -> False


x86_jumpDestsOfInstr
        :: Instr
        -> [BlockId]

x86_jumpDestsOfInstr insn
  = case insn of
        JXX _ id        -> [id]
        JMP_TBL _ ids _ _ -> [id | Just id <- ids]
        _               -> []


x86_patchJumpInstr
        :: Instr -> (BlockId -> BlockId) -> Instr

x86_patchJumpInstr insn patchF
  = case insn of
        JXX cc id       -> JXX cc (patchF id)
        JMP_TBL op ids section lbl
          -> JMP_TBL op (map (fmap patchF) ids) section lbl
        _               -> insn




-- -----------------------------------------------------------------------------
-- | Make a spill instruction.
x86_mkSpillInstr
    :: DynFlags
    -> Reg      -- register to spill
    -> Int      -- current stack delta
    -> Int      -- spill slot to use
    -> Instr

x86_mkSpillInstr dflags reg delta slot
  = let off     = spillSlotToOffset platform slot - delta
    in
    case targetClassOfReg platform reg of
           RcInteger   -> MOV (archWordSize is32Bit)
                              (OpReg reg) (OpAddr (spRel dflags off))
           RcDouble    -> GST FF80 reg (spRel dflags off) {- RcFloat/RcDouble -}
           RcDoubleSSE -> MOV FF64 (OpReg reg) (OpAddr (spRel dflags off))
           _         -> panic "X86.mkSpillInstr: no match"
    where platform = targetPlatform dflags
          is32Bit = target32Bit platform

-- | Make a spill reload instruction.
x86_mkLoadInstr
    :: DynFlags
    -> Reg      -- register to load
    -> Int      -- current stack delta
    -> Int      -- spill slot to use
    -> Instr

x86_mkLoadInstr dflags reg delta slot
  = let off     = spillSlotToOffset platform slot - delta
    in
        case targetClassOfReg platform reg of
              RcInteger -> MOV (archWordSize is32Bit)
                               (OpAddr (spRel dflags off)) (OpReg reg)
              RcDouble  -> GLD FF80 (spRel dflags off) reg {- RcFloat/RcDouble -}
              RcDoubleSSE -> MOV FF64 (OpAddr (spRel dflags off)) (OpReg reg)
              _           -> panic "X86.x86_mkLoadInstr"
    where platform = targetPlatform dflags
          is32Bit = target32Bit platform

spillSlotSize :: Platform -> Int
spillSlotSize dflags = if is32Bit then 12 else 8
    where is32Bit = target32Bit dflags

maxSpillSlots :: DynFlags -> Int
maxSpillSlots dflags
    = ((rESERVED_C_STACK_BYTES dflags - 64) `div` spillSlotSize (targetPlatform dflags)) - 1
--     = 0 -- useful for testing allocMoreStack

-- number of bytes that the stack pointer should be aligned to
stackAlign :: Int
stackAlign = 16

-- convert a spill slot number to a *byte* offset, with no sign:
-- decide on a per arch basis whether you are spilling above or below
-- the C stack pointer.
spillSlotToOffset :: Platform -> Int -> Int
spillSlotToOffset platform slot
   = 64 + spillSlotSize platform * slot

--------------------------------------------------------------------------------

-- | See if this instruction is telling us the current C stack delta
x86_takeDeltaInstr
        :: Instr
        -> Maybe Int

x86_takeDeltaInstr instr
 = case instr of
        DELTA i         -> Just i
        _               -> Nothing


x86_isMetaInstr
        :: Instr
        -> Bool

x86_isMetaInstr instr
 = case instr of
        COMMENT{}       -> True
        LDATA{}         -> True
        NEWBLOCK{}      -> True
        DELTA{}         -> True
        _               -> False



-- | Make a reg-reg move instruction.
--      On SPARC v8 there are no instructions to move directly between
--      floating point and integer regs. If we need to do that then we
--      have to go via memory.
--
x86_mkRegRegMoveInstr
    :: Platform
    -> Reg
    -> Reg
    -> Instr

x86_mkRegRegMoveInstr platform src dst
 = case targetClassOfReg platform src of
        RcInteger -> case platformArch platform of
                     ArchX86    -> MOV II32 (OpReg src) (OpReg dst)
                     ArchX86_64 -> MOV II64 (OpReg src) (OpReg dst)
                     _          -> panic "x86_mkRegRegMoveInstr: Bad arch"
        RcDouble    -> GMOV src dst
        RcDoubleSSE -> MOV FF64 (OpReg src) (OpReg dst)
        _     -> panic "X86.RegInfo.mkRegRegMoveInstr: no match"

-- | Check whether an instruction represents a reg-reg move.
--      The register allocator attempts to eliminate reg->reg moves whenever it can,
--      by assigning the src and dest temporaries to the same real register.
--
x86_takeRegRegMoveInstr
        :: Instr
        -> Maybe (Reg,Reg)

x86_takeRegRegMoveInstr (MOV _ (OpReg r1) (OpReg r2))
        = Just (r1,r2)

x86_takeRegRegMoveInstr _  = Nothing


-- | Make an unconditional branch instruction.
x86_mkJumpInstr
        :: BlockId
        -> [Instr]

x86_mkJumpInstr id
        = [JXX ALWAYS id]


x86_mkStackAllocInstr
        :: Platform
        -> Int
        -> Instr
x86_mkStackAllocInstr platform amount
  = case platformArch platform of
      ArchX86    -> SUB II32 (OpImm (ImmInt amount)) (OpReg esp)
      ArchX86_64 -> SUB II64 (OpImm (ImmInt amount)) (OpReg rsp)
      _ -> panic "x86_mkStackAllocInstr"

x86_mkStackDeallocInstr
        :: Platform
        -> Int
        -> Instr
x86_mkStackDeallocInstr platform amount
  = case platformArch platform of
      ArchX86    -> ADD II32 (OpImm (ImmInt amount)) (OpReg esp)
      ArchX86_64 -> ADD II64 (OpImm (ImmInt amount)) (OpReg rsp)
      _ -> panic "x86_mkStackDeallocInstr"

i386_insert_ffrees
        :: [GenBasicBlock Instr]
        -> [GenBasicBlock Instr]

i386_insert_ffrees blocks
   | any (any is_G_instr) [ instrs | BasicBlock _ instrs <- blocks ]
   = map insertGFREEs blocks
   | otherwise
   = blocks
 where
   insertGFREEs (BasicBlock id insns)
     = BasicBlock id (insertBeforeNonlocalTransfers GFREE insns)

insertBeforeNonlocalTransfers :: Instr -> [Instr] -> [Instr]
insertBeforeNonlocalTransfers insert insns
     = foldr p [] insns
     where p insn r = case insn of
                        CALL _ _    -> insert : insn : r
                        JMP _ _     -> insert : insn : r
                        JXX_GBL _ _ -> panic "insertBeforeNonlocalTransfers: cannot handle JXX_GBL"
                        _           -> insn : r


-- if you ever add a new FP insn to the fake x86 FP insn set,
-- you must update this too
is_G_instr :: Instr -> Bool
is_G_instr instr
   = case instr of
        GMOV{}          -> True
        GLD{}           -> True
        GST{}           -> True
        GLDZ{}          -> True
        GLD1{}          -> True
        GFTOI{}         -> True
        GDTOI{}         -> True
        GITOF{}         -> True
        GITOD{}         -> True
        GDTOF{}         -> True
        GADD{}          -> True
        GDIV{}          -> True
        GSUB{}          -> True
        GMUL{}          -> True
        GCMP{}          -> True
        GABS{}          -> True
        GNEG{}          -> True
        GSQRT{}         -> True
        GSIN{}          -> True
        GCOS{}          -> True
        GTAN{}          -> True
        GFREE           -> panic "is_G_instr: GFREE (!)"
        _               -> False


--
-- Note [extra spill slots]
--
-- If the register allocator used more spill slots than we have
-- pre-allocated (rESERVED_C_STACK_BYTES), then we must allocate more
-- C stack space on entry and exit from this proc.  Therefore we
-- insert a "sub $N, %rsp" at every entry point, and an "add $N, %rsp"
-- before every non-local jump.
--
-- This became necessary when the new codegen started bundling entire
-- functions together into one proc, because the register allocator
-- assigns a different stack slot to each virtual reg within a proc.
-- To avoid using so many slots we could also:
--
--   - split up the proc into connected components before code generator
--
--   - rename the virtual regs, so that we re-use vreg names and hence
--     stack slots for non-overlapping vregs.
--
-- Note that when a block is both a non-local entry point (with an
-- info table) and a local branch target, we have to split it into
-- two, like so:
--
--    <info table>
--    L:
--       <code>
--
-- becomes
--
--    <info table>
--    L:
--       subl $rsp, N
--       jmp Lnew
--    Lnew:
--       <code>
--
-- and all branches pointing to L are retargetted to point to Lnew.
-- Otherwise, we would repeat the $rsp adjustment for each branch to
-- L.
--
allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics X86.Instr.Instr
  -> UniqSM (NatCmmDecl statics X86.Instr.Instr)

allocMoreStack _ _ top@(CmmData _ _) = return top
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
    let entries = entryBlocks proc

    uniqs <- replicateM (length entries) getUniqueUs

    let
      delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where x = slots * spillSlotSize platform -- sp delta

      alloc   = mkStackAllocInstr   platform delta
      dealloc = mkStackDeallocInstr platform delta
  
      new_blockmap :: BlockEnv BlockId
      new_blockmap = mapFromList (zip entries (map mkBlockId uniqs))
  
      insert_stack_insns (BasicBlock id insns)
         | Just new_blockid <- mapLookup id new_blockmap
         = [ BasicBlock id [alloc, JXX ALWAYS new_blockid]
           , BasicBlock new_blockid block' ]
         | otherwise
         = [ BasicBlock id block' ]
         where
           block' = foldr insert_dealloc [] insns

      insert_dealloc insn r = case insn of
         JMP _ _     -> dealloc : insn : r
         JXX_GBL _ _ -> panic "insert_dealloc: cannot handle JXX_GBL"
         _other      -> x86_patchJumpInstr insn retarget : r
           where retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insns code
    -- in
    return (CmmProc info lbl live (ListGraph new_code))


data JumpDest = DestBlockId BlockId | DestImm Imm

getJumpDestBlockId :: JumpDest -> Maybe BlockId
getJumpDestBlockId (DestBlockId bid) = Just bid
getJumpDestBlockId _                 = Nothing

canShortcut :: Instr -> Maybe JumpDest
canShortcut (JXX ALWAYS id)      = Just (DestBlockId id)
canShortcut (JMP (OpImm imm) _)  = Just (DestImm imm)
canShortcut _                    = Nothing


-- This helper shortcuts a sequence of branches.
-- The blockset helps avoid following cycles.
shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump fn insn = shortcutJump' fn (setEmpty :: BlockSet) insn
  where shortcutJump' fn seen insn@(JXX cc id) =
          if setMember id seen then insn
          else case fn id of
                 Nothing                -> insn
                 Just (DestBlockId id') -> shortcutJump' fn seen' (JXX cc id')
                 Just (DestImm imm)     -> shortcutJump' fn seen' (JXX_GBL cc imm)
               where seen' = setInsert id seen
        shortcutJump' _ _ other = other

-- Here because it knows about JumpDest
shortcutStatics :: (BlockId -> Maybe JumpDest) -> (Alignment, CmmStatics) -> (Alignment, CmmStatics)
shortcutStatics fn (align, Statics lbl statics)
  = (align, Statics lbl $ map (shortcutStatic fn) statics)
  -- we need to get the jump tables, so apply the mapping to the entries
  -- of a CmmData too.

shortcutLabel :: (BlockId -> Maybe JumpDest) -> CLabel -> CLabel
shortcutLabel fn lab
  | Just uq <- maybeAsmTemp lab = shortBlockId fn emptyUniqSet (mkBlockId uq)
  | otherwise                   = lab

shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic
shortcutStatic fn (CmmStaticLit (CmmLabel lab))
  = CmmStaticLit (CmmLabel (shortcutLabel fn lab))
shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off))
  = CmmStaticLit (CmmLabelDiffOff (shortcutLabel fn lbl1) lbl2 off)
        -- slightly dodgy, we're ignoring the second label, but this
        -- works with the way we use CmmLabelDiffOff for jump tables now.
shortcutStatic _ other_static
        = other_static

shortBlockId
        :: (BlockId -> Maybe JumpDest)
        -> UniqSet Unique
        -> BlockId
        -> CLabel

shortBlockId fn seen blockid =
  case (elementOfUniqSet uq seen, fn blockid) of
    (True, _)    -> mkAsmTempLabel uq
    (_, Nothing) -> mkAsmTempLabel uq
    (_, Just (DestBlockId blockid'))  -> shortBlockId fn (addOneToUniqSet seen uq) blockid'
    (_, Just (DestImm (ImmCLbl lbl))) -> lbl
    (_, _other) -> panic "shortBlockId"
  where uq = getUnique blockid
