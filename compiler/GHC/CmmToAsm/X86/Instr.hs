{-# LANGUAGE CPP, TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.X86.Instr
   ( Instr(..), Operand(..), PrefetchVariant(..), JumpDest(..)
   , getJumpDestBlockId, canShortcut, shortcutStatics
   , shortcutJump, allocMoreStack
   , maxSpillSlots, archWordFormat
   )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg.Class
import GHC.Platform.Reg
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Platform.Regs
import GHC.Cmm
import FastString
import Outputable
import GHC.Platform

import GHC.Types.Basic (Alignment)
import GHC.Cmm.CLabel
import GHC.Types.Unique.Set
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Cmm.DebugBlock (UnwindTable)

import Control.Monad
import Data.Maybe       (fromMaybe)

-- Format of an x86/x86_64 memory address, in bytes.
--
archWordFormat :: Bool -> Format
archWordFormat is32Bit
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

        -- location pseudo-op (file, line, col, name)
        | LOCATION Int Int Int String

        -- some static data spat out during code
        -- generation.  Will be extracted before
        -- pretty-printing.
        | LDATA   Section (Alignment, RawCmmStatics)

        -- start a new basic block.  Useful during
        -- codegen, removed later.  Preceding
        -- instruction should be a jump, as per the
        -- invariants for a BasicBlock (see Cmm).
        | NEWBLOCK BlockId

        -- unwinding information
        -- See Note [Unwinding information in the NCG].
        | UNWIND CLabel UnwindTable

        -- specify current stack offset for benefit of subsequent passes.
        -- This carries a BlockId so it can be used in unwinding information.
        | DELTA  Int

        -- Moves.
        | MOV         Format Operand Operand
        | CMOV   Cond Format Operand Reg
        | MOVZxL      Format Operand Operand -- format is the size of operand 1
        | MOVSxL      Format Operand Operand -- format is the size of operand 1
        -- x86_64 note: plain mov into a 32-bit register always zero-extends
        -- into the 64-bit reg, in contrast to the 8 and 16-bit movs which
        -- don't affect the high bits of the register.

        -- Load effective address (also a very useful three-operand add instruction :-)
        | LEA         Format Operand Operand

        -- Int Arithmetic.
        | ADD         Format Operand Operand
        | ADC         Format Operand Operand
        | SUB         Format Operand Operand
        | SBB         Format Operand Operand

        | MUL         Format Operand Operand
        | MUL2        Format Operand         -- %edx:%eax = operand * %rax
        | IMUL        Format Operand Operand -- signed int mul
        | IMUL2       Format Operand         -- %edx:%eax = operand * %eax

        | DIV         Format Operand         -- eax := eax:edx/op, edx := eax:edx%op
        | IDIV        Format Operand         -- ditto, but signed

        -- Int Arithmetic, where the effects on the condition register
        -- are important. Used in specialized sequences such as MO_Add2.
        -- Do not rewrite these instructions to "equivalent" ones that
        -- have different effect on the condition register! (See #9013.)
        | ADD_CC      Format Operand Operand
        | SUB_CC      Format Operand Operand

        -- Simple bit-twiddling.
        | AND         Format Operand Operand
        | OR          Format Operand Operand
        | XOR         Format Operand Operand
        | NOT         Format Operand
        | NEGI        Format Operand         -- NEG instruction (name clash with Cond)
        | BSWAP       Format Reg

        -- Shifts (amount may be immediate or %cl only)
        | SHL         Format Operand{-amount-} Operand
        | SAR         Format Operand{-amount-} Operand
        | SHR         Format Operand{-amount-} Operand

        | BT          Format Imm Operand
        | NOP


        -- We need to support the FSTP (x87 store and pop) instruction
        -- so that we can correctly read off the return value of an
        -- x86 CDECL C function call when its floating point.
        -- so we dont include a register argument, and just use st(0)
        -- this instruction is used ONLY for return values of C ffi calls
        -- in x86_32 abi
        | X87Store         Format  AddrMode -- st(0), dst


        -- SSE2 floating point: we use a restricted set of the available SSE2
        -- instructions for floating-point.
        -- use MOV for moving (either movss or movsd (movlpd better?))
        | CVTSS2SD      Reg Reg            -- F32 to F64
        | CVTSD2SS      Reg Reg            -- F64 to F32
        | CVTTSS2SIQ    Format Operand Reg -- F32 to I32/I64 (with truncation)
        | CVTTSD2SIQ    Format Operand Reg -- F64 to I32/I64 (with truncation)
        | CVTSI2SS      Format Operand Reg -- I32/I64 to F32
        | CVTSI2SD      Format Operand Reg -- I32/I64 to F64

        -- use ADD, SUB, and SQRT for arithmetic.  In both cases, operands
        -- are  Operand Reg.

        -- SSE2 floating-point division:
        | FDIV          Format Operand Operand   -- divisor, dividend(dst)

        -- use CMP for comparisons.  ucomiss and ucomisd instructions
        -- compare single/double prec floating point respectively.

        | SQRT          Format Operand Reg      -- src, dst


        -- Comparison
        | TEST          Format Operand Operand
        | CMP           Format Operand Operand
        | SETCC         Cond Operand

        -- Stack Operations.
        | PUSH          Format Operand
        | POP           Format Operand
        -- both unused (SDM):
        --  | PUSHA
        --  | POPA

        -- Jumping around.
        | JMP         Operand [Reg] -- including live Regs at the call
        | JXX         Cond BlockId  -- includes unconditional branches
        | JXX_GBL     Cond Imm      -- non-local version of JXX
        -- Table jump
        | JMP_TBL     Operand   -- Address to jump to
                      [Maybe JumpDest] -- Targets of the jump table
                      Section   -- Data section jump table should be put in
                      CLabel    -- Label of jump table
        -- | X86 call instruction
        | CALL        (Either Imm Reg) -- ^ Jump target
                      [Reg]            -- ^ Arguments (required for register allocation)

        -- Other things.
        | CLTD Format            -- sign extend %eax into %edx:%eax

        | FETCHGOT    Reg        -- pseudo-insn for ELF position-independent code
                                 -- pretty-prints as
                                 --       call 1f
                                 -- 1:    popl %reg
                                 --       addl __GLOBAL_OFFSET_TABLE__+.-1b, %reg
        | FETCHPC     Reg        -- pseudo-insn for Darwin position-independent code
                                 -- pretty-prints as
                                 --       call 1f
                                 -- 1:    popl %reg

    -- bit counting instructions
        | POPCNT      Format Operand Reg -- [SSE4.2] count number of bits set to 1
        | LZCNT       Format Operand Reg -- [BMI2] count number of leading zeros
        | TZCNT       Format Operand Reg -- [BMI2] count number of trailing zeros
        | BSF         Format Operand Reg -- bit scan forward
        | BSR         Format Operand Reg -- bit scan reverse

    -- bit manipulation instructions
        | PDEP        Format Operand Operand Reg -- [BMI2] deposit bits to   the specified mask
        | PEXT        Format Operand Operand Reg -- [BMI2] extract bits from the specified mask

    -- prefetch
        | PREFETCH  PrefetchVariant Format Operand -- prefetch Variant, addr size, address to prefetch
                                        -- variant can be NTA, Lvl0, Lvl1, or Lvl2

        | LOCK        Instr -- lock prefix
        | XADD        Format Operand Operand -- src (r), dst (r/m)
        | CMPXCHG     Format Operand Operand -- src (r), dst (r/m), eax implicit
        | MFENCE

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
    CMOV _ _ src dst    -> mkRU (use_R src [dst]) [dst]
    MOVZxL _ src dst    -> usageRW src dst
    MOVSxL _ src dst    -> usageRW src dst
    LEA    _ src dst    -> usageRW src dst
    ADD    _ src dst    -> usageRM src dst
    ADC    _ src dst    -> usageRM src dst
    SUB    _ src dst    -> usageRM src dst
    SBB    _ src dst    -> usageRM src dst
    IMUL   _ src dst    -> usageRM src dst

    -- Result of IMULB will be in just in %ax
    IMUL2  II8 src       -> mkRU (eax:use_R src []) [eax]
    -- Result of IMUL for wider values, will be split between %dx/%edx/%rdx and
    -- %ax/%eax/%rax.
    IMUL2  _ src        -> mkRU (eax:use_R src []) [eax,edx]

    MUL    _ src dst    -> usageRM src dst
    MUL2   _ src        -> mkRU (eax:use_R src []) [eax,edx]
    DIV    _ op -> mkRU (eax:edx:use_R op []) [eax,edx]
    IDIV   _ op -> mkRU (eax:edx:use_R op []) [eax,edx]
    ADD_CC _ src dst    -> usageRM src dst
    SUB_CC _ src dst    -> usageRM src dst
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

    X87Store    _  dst    -> mkRUR ( use_EA dst [])

    CVTSS2SD   src dst  -> mkRU [src] [dst]
    CVTSD2SS   src dst  -> mkRU [src] [dst]
    CVTTSS2SIQ _ src dst -> mkRU (use_R src []) [dst]
    CVTTSD2SIQ _ src dst -> mkRU (use_R src []) [dst]
    CVTSI2SS   _ src dst -> mkRU (use_R src []) [dst]
    CVTSI2SD   _ src dst -> mkRU (use_R src []) [dst]
    FDIV _     src dst  -> usageRM src dst
    SQRT _ src dst      -> mkRU (use_R src []) [dst]

    FETCHGOT reg        -> mkRU [] [reg]
    FETCHPC  reg        -> mkRU [] [reg]

    COMMENT _           -> noUsage
    LOCATION{}          -> noUsage
    UNWIND{}            -> noUsage
    DELTA   _           -> noUsage

    POPCNT _ src dst -> mkRU (use_R src []) [dst]
    LZCNT  _ src dst -> mkRU (use_R src []) [dst]
    TZCNT  _ src dst -> mkRU (use_R src []) [dst]
    BSF    _ src dst -> mkRU (use_R src []) [dst]
    BSR    _ src dst -> mkRU (use_R src []) [dst]

    PDEP   _ src mask dst -> mkRU (use_R src $ use_R mask []) [dst]
    PEXT   _ src mask dst -> mkRU (use_R src $ use_R mask []) [dst]

    -- note: might be a better way to do this
    PREFETCH _  _ src -> mkRU (use_R src []) []
    LOCK i              -> x86_regUsageOfInstr platform i
    XADD _ src dst      -> usageMM src dst
    CMPXCHG _ src dst   -> usageRMM src dst (OpReg eax)
    MFENCE -> noUsage

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
interesting platform (RegReal (RealRegSingle i)) = freeReg platform i
interesting _        (RegReal (RealRegPair{}))   = panic "X86.interesting: no reg pairs on this arch"



-- | Applies the supplied function to all registers in instructions.
-- Typically used to change virtual registers to real registers.
x86_patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
x86_patchRegsOfInstr instr env
 = case instr of
    MOV  fmt src dst     -> patch2 (MOV  fmt) src dst
    CMOV cc fmt src dst  -> CMOV cc fmt (patchOp src) (env dst)
    MOVZxL fmt src dst   -> patch2 (MOVZxL fmt) src dst
    MOVSxL fmt src dst   -> patch2 (MOVSxL fmt) src dst
    LEA  fmt src dst     -> patch2 (LEA  fmt) src dst
    ADD  fmt src dst     -> patch2 (ADD  fmt) src dst
    ADC  fmt src dst     -> patch2 (ADC  fmt) src dst
    SUB  fmt src dst     -> patch2 (SUB  fmt) src dst
    SBB  fmt src dst     -> patch2 (SBB  fmt) src dst
    IMUL fmt src dst     -> patch2 (IMUL fmt) src dst
    IMUL2 fmt src        -> patch1 (IMUL2 fmt) src
    MUL fmt src dst      -> patch2 (MUL fmt) src dst
    MUL2 fmt src         -> patch1 (MUL2 fmt) src
    IDIV fmt op          -> patch1 (IDIV fmt) op
    DIV fmt op           -> patch1 (DIV fmt) op
    ADD_CC fmt src dst   -> patch2 (ADD_CC fmt) src dst
    SUB_CC fmt src dst   -> patch2 (SUB_CC fmt) src dst
    AND  fmt src dst     -> patch2 (AND  fmt) src dst
    OR   fmt src dst     -> patch2 (OR   fmt) src dst
    XOR  fmt src dst     -> patch2 (XOR  fmt) src dst
    NOT  fmt op          -> patch1 (NOT  fmt) op
    BSWAP fmt reg        -> BSWAP fmt (env reg)
    NEGI fmt op          -> patch1 (NEGI fmt) op
    SHL  fmt imm dst     -> patch1 (SHL fmt imm) dst
    SAR  fmt imm dst     -> patch1 (SAR fmt imm) dst
    SHR  fmt imm dst     -> patch1 (SHR fmt imm) dst
    BT   fmt imm src     -> patch1 (BT  fmt imm) src
    TEST fmt src dst     -> patch2 (TEST fmt) src dst
    CMP  fmt src dst     -> patch2 (CMP  fmt) src dst
    PUSH fmt op          -> patch1 (PUSH fmt) op
    POP  fmt op          -> patch1 (POP  fmt) op
    SETCC cond op        -> patch1 (SETCC cond) op
    JMP op regs          -> JMP (patchOp op) regs
    JMP_TBL op ids s lbl -> JMP_TBL (patchOp op) ids s lbl

    -- literally only support storing the top x87 stack value st(0)
    X87Store  fmt  dst     -> X87Store fmt  (lookupAddr dst)

    CVTSS2SD src dst    -> CVTSS2SD (env src) (env dst)
    CVTSD2SS src dst    -> CVTSD2SS (env src) (env dst)
    CVTTSS2SIQ fmt src dst -> CVTTSS2SIQ fmt (patchOp src) (env dst)
    CVTTSD2SIQ fmt src dst -> CVTTSD2SIQ fmt (patchOp src) (env dst)
    CVTSI2SS fmt src dst -> CVTSI2SS fmt (patchOp src) (env dst)
    CVTSI2SD fmt src dst -> CVTSI2SD fmt (patchOp src) (env dst)
    FDIV fmt src dst     -> FDIV fmt (patchOp src) (patchOp dst)
    SQRT fmt src dst    -> SQRT fmt (patchOp src) (env dst)

    CALL (Left _)  _    -> instr
    CALL (Right reg) p  -> CALL (Right (env reg)) p

    FETCHGOT reg        -> FETCHGOT (env reg)
    FETCHPC  reg        -> FETCHPC  (env reg)

    NOP                 -> instr
    COMMENT _           -> instr
    LOCATION {}         -> instr
    UNWIND {}           -> instr
    DELTA _             -> instr

    JXX _ _             -> instr
    JXX_GBL _ _         -> instr
    CLTD _              -> instr

    POPCNT fmt src dst -> POPCNT fmt (patchOp src) (env dst)
    LZCNT  fmt src dst -> LZCNT  fmt (patchOp src) (env dst)
    TZCNT  fmt src dst -> TZCNT  fmt (patchOp src) (env dst)
    PDEP   fmt src mask dst -> PDEP   fmt (patchOp src) (patchOp mask) (env dst)
    PEXT   fmt src mask dst -> PEXT   fmt (patchOp src) (patchOp mask) (env dst)
    BSF    fmt src dst -> BSF    fmt (patchOp src) (env dst)
    BSR    fmt src dst -> BSR    fmt (patchOp src) (env dst)

    PREFETCH lvl format src -> PREFETCH lvl format (patchOp src)

    LOCK i               -> LOCK (x86_patchRegsOfInstr i env)
    XADD fmt src dst     -> patch2 (XADD fmt) src dst
    CMPXCHG fmt src dst  -> patch2 (CMPXCHG fmt) src dst
    MFENCE               -> instr

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
        JMP_TBL _ ids _ _ -> [id | Just (DestBlockId id) <- ids]
        _               -> []


x86_patchJumpInstr
        :: Instr -> (BlockId -> BlockId) -> Instr

x86_patchJumpInstr insn patchF
  = case insn of
        JXX cc id       -> JXX cc (patchF id)
        JMP_TBL op ids section lbl
          -> JMP_TBL op (map (fmap (patchJumpDest patchF)) ids) section lbl
        _               -> insn
    where
        patchJumpDest f (DestBlockId id) = DestBlockId (f id)
        patchJumpDest _ dest             = dest





-- -----------------------------------------------------------------------------
-- | Make a spill instruction.
x86_mkSpillInstr
    :: NCGConfig
    -> Reg      -- register to spill
    -> Int      -- current stack delta
    -> Int      -- spill slot to use
    -> Instr

x86_mkSpillInstr config reg delta slot
  = let off     = spillSlotToOffset platform slot - delta
    in
    case targetClassOfReg platform reg of
           RcInteger   -> MOV (archWordFormat is32Bit)
                              (OpReg reg) (OpAddr (spRel platform off))
           RcDouble    -> MOV FF64 (OpReg reg) (OpAddr (spRel platform off))
           _         -> panic "X86.mkSpillInstr: no match"
    where platform = ncgPlatform config
          is32Bit = target32Bit platform

-- | Make a spill reload instruction.
x86_mkLoadInstr
    :: NCGConfig
    -> Reg      -- register to load
    -> Int      -- current stack delta
    -> Int      -- spill slot to use
    -> Instr

x86_mkLoadInstr config reg delta slot
  = let off     = spillSlotToOffset platform slot - delta
    in
        case targetClassOfReg platform reg of
              RcInteger -> MOV (archWordFormat is32Bit)
                               (OpAddr (spRel platform off)) (OpReg reg)
              RcDouble  -> MOV FF64 (OpAddr (spRel platform off)) (OpReg reg)
              _           -> panic "X86.x86_mkLoadInstr"
    where platform = ncgPlatform config
          is32Bit = target32Bit platform

spillSlotSize :: Platform -> Int
spillSlotSize platform
   | target32Bit platform = 12
   | otherwise            = 8

maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
    = ((ncgSpillPreallocSize config - 64) `div` spillSlotSize (ncgPlatform config)) - 1
--  = 0 -- useful for testing allocMoreStack

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
        LOCATION{}      -> True
        LDATA{}         -> True
        NEWBLOCK{}      -> True
        UNWIND{}        -> True
        DELTA{}         -> True
        _               -> False



---  TODO: why is there
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
        RcDouble    ->  MOV FF64 (OpReg src) (OpReg dst)
        -- this code is the lie we tell ourselves because both float and double
        -- use the same register class.on x86_64 and x86 32bit with SSE2,
        -- more plainly, both use the XMM registers
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

-- Note [Windows stack layout]
-- | On most OSes the kernel will place a guard page after the current stack
--   page.  If you allocate larger than a page worth you may jump over this
--   guard page.  Not only is this a security issue, but on certain OSes such
--   as Windows a new page won't be allocated if you don't hit the guard.  This
--   will cause a segfault or access fault.
--
--   This function defines if the current allocation amount requires a probe.
--   On Windows (for now) we emit a call to _chkstk for this.  For other OSes
--   this is not yet implemented.
--   See https://docs.microsoft.com/en-us/windows/desktop/DevNotes/-win32-chkstk
--   The Windows stack looks like this:
--
--                         +-------------------+
--                         |        SP         |
--                         +-------------------+
--                         |                   |
--                         |    GUARD PAGE     |
--                         |                   |
--                         +-------------------+
--                         |                   |
--                         |                   |
--                         |     UNMAPPED      |
--                         |                   |
--                         |                   |
--                         +-------------------+
--
--   In essence each allocation larger than a page size needs to be chunked and
--   a probe emitted after each page allocation.  You have to hit the guard
--   page so the kernel can map in the next page, otherwise you'll segfault.
--
needs_probe_call :: Platform -> Int -> Bool
needs_probe_call platform amount
  = case platformOS platform of
     OSMinGW32 -> case platformArch platform of
                    ArchX86    -> amount > (4 * 1024)
                    ArchX86_64 -> amount > (8 * 1024)
                    _          -> False
     _         -> False

x86_mkStackAllocInstr
        :: Platform
        -> Int
        -> [Instr]
x86_mkStackAllocInstr platform amount
  = case platformOS platform of
      OSMinGW32 ->
        -- These will clobber AX but this should be ok because
        --
        -- 1. It is the first thing we do when entering the closure and AX is
        --    a caller saved registers on Windows both on x86_64 and x86.
        --
        -- 2. The closures are only entered via a call or longjmp in which case
        --    there are no expectations for volatile registers.
        --
        -- 3. When the target is a local branch point it is re-targeted
        --    after the dealloc, preserving #2.  See note [extra spill slots].
        --
        -- We emit a call because the stack probes are quite involved and
        -- would bloat code size a lot.  GHC doesn't really have an -Os.
        -- __chkstk is guaranteed to leave all nonvolatile registers and AX
        -- untouched.  It's part of the standard prologue code for any Windows
        -- function dropping the stack more than a page.
        -- See Note [Windows stack layout]
        case platformArch platform of
            ArchX86    | needs_probe_call platform amount ->
                           [ MOV II32 (OpImm (ImmInt amount)) (OpReg eax)
                           , CALL (Left $ strImmLit "___chkstk_ms") [eax]
                           , SUB II32 (OpReg eax) (OpReg esp)
                           ]
                       | otherwise ->
                           [ SUB II32 (OpImm (ImmInt amount)) (OpReg esp)
                           , TEST II32 (OpReg esp) (OpReg esp)
                           ]
            ArchX86_64 | needs_probe_call platform amount ->
                           [ MOV II64 (OpImm (ImmInt amount)) (OpReg rax)
                           , CALL (Left $ strImmLit "___chkstk_ms") [rax]
                           , SUB II64 (OpReg rax) (OpReg rsp)
                           ]
                       | otherwise ->
                           [ SUB II64 (OpImm (ImmInt amount)) (OpReg rsp)
                           , TEST II64 (OpReg rsp) (OpReg rsp)
                           ]
            _ -> panic "x86_mkStackAllocInstr"
      _       ->
        case platformArch platform of
          ArchX86    -> [ SUB II32 (OpImm (ImmInt amount)) (OpReg esp) ]
          ArchX86_64 -> [ SUB II64 (OpImm (ImmInt amount)) (OpReg rsp) ]
          _ -> panic "x86_mkStackAllocInstr"

x86_mkStackDeallocInstr
        :: Platform
        -> Int
        -> [Instr]
x86_mkStackDeallocInstr platform amount
  = case platformArch platform of
      ArchX86    -> [ADD II32 (OpImm (ImmInt amount)) (OpReg esp)]
      ArchX86_64 -> [ADD II64 (OpImm (ImmInt amount)) (OpReg rsp)]
      _ -> panic "x86_mkStackDeallocInstr"


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
-- Returns a list of (L,Lnew) pairs.
--
allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics GHC.CmmToAsm.X86.Instr.Instr
  -> UniqSM (NatCmmDecl statics GHC.CmmToAsm.X86.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top,[])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
    let entries = entryBlocks proc

    uniqs <- replicateM (length entries) getUniqueM

    let
      delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where x = slots * spillSlotSize platform -- sp delta

      alloc   = mkStackAllocInstr   platform delta
      dealloc = mkStackDeallocInstr platform delta

      retargetList = (zip entries (map mkBlockId uniqs))

      new_blockmap :: LabelMap BlockId
      new_blockmap = mapFromList retargetList

      insert_stack_insns (BasicBlock id insns)
         | Just new_blockid <- mapLookup id new_blockmap
         = [ BasicBlock id $ alloc ++ [JXX ALWAYS new_blockid]
           , BasicBlock new_blockid block' ]
         | otherwise
         = [ BasicBlock id block' ]
         where
           block' = foldr insert_dealloc [] insns

      insert_dealloc insn r = case insn of
         JMP _ _     -> dealloc ++ (insn : r)
         JXX_GBL _ _ -> panic "insert_dealloc: cannot handle JXX_GBL"
         _other      -> x86_patchJumpInstr insn retarget : r
           where retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insns code
    -- in
    return (CmmProc info lbl live (ListGraph new_code), retargetList)

data JumpDest = DestBlockId BlockId | DestImm Imm

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid
  ppr (DestImm _imm)    = text "jd<imm>:noShow"


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
shortcutJump fn insn = shortcutJump' fn (setEmpty :: LabelSet) insn
  where
    shortcutJump' :: (BlockId -> Maybe JumpDest) -> LabelSet -> Instr -> Instr
    shortcutJump' fn seen insn@(JXX cc id) =
        if setMember id seen then insn
        else case fn id of
            Nothing                -> insn
            Just (DestBlockId id') -> shortcutJump' fn seen' (JXX cc id')
            Just (DestImm imm)     -> shortcutJump' fn seen' (JXX_GBL cc imm)
        where seen' = setInsert id seen
    shortcutJump' fn _ (JMP_TBL addr blocks section tblId) =
        let updateBlock (Just (DestBlockId bid))  =
                case fn bid of
                    Nothing   -> Just (DestBlockId bid )
                    Just dest -> Just dest
            updateBlock dest = dest
            blocks' = map updateBlock blocks
        in  JMP_TBL addr blocks' section tblId
    shortcutJump' _ _ other = other

-- Here because it knows about JumpDest
shortcutStatics :: (BlockId -> Maybe JumpDest) -> (Alignment, RawCmmStatics) -> (Alignment, RawCmmStatics)
shortcutStatics fn (align, CmmStaticsRaw lbl statics)
  = (align, CmmStaticsRaw lbl $ map (shortcutStatic fn) statics)
  -- we need to get the jump tables, so apply the mapping to the entries
  -- of a CmmData too.

shortcutLabel :: (BlockId -> Maybe JumpDest) -> CLabel -> CLabel
shortcutLabel fn lab
  | Just blkId <- maybeLocalBlockLabel lab = shortBlockId fn emptyUniqSet blkId
  | otherwise                              = lab

shortcutStatic :: (BlockId -> Maybe JumpDest) -> CmmStatic -> CmmStatic
shortcutStatic fn (CmmStaticLit (CmmLabel lab))
  = CmmStaticLit (CmmLabel (shortcutLabel fn lab))
shortcutStatic fn (CmmStaticLit (CmmLabelDiffOff lbl1 lbl2 off w))
  = CmmStaticLit (CmmLabelDiffOff (shortcutLabel fn lbl1) lbl2 off w)
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
    (True, _)    -> blockLbl blockid
    (_, Nothing) -> blockLbl blockid
    (_, Just (DestBlockId blockid'))  -> shortBlockId fn (addOneToUniqSet seen uq) blockid'
    (_, Just (DestImm (ImmCLbl lbl))) -> lbl
    (_, _other) -> panic "shortBlockId"
  where uq = getUnique blockid
