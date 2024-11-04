{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.X86.Instr
   ( Instr(..)
   , Operand(..)
   , PrefetchVariant(..)
   , FMAPermutation(..)
   , JumpDest(..)
   , getJumpDestBlockId
   , canShortcut
   , shortcutStatics
   , shortcutJump
   , allocMoreStack
   , maxSpillSlots
   , archWordFormat
   , takeRegRegMoveInstr
   , regUsageOfInstr
   , takeDeltaInstr
   , mkLoadInstr
   , mkJumpInstr
   , mkStackAllocInstr
   , mkStackDeallocInstr
   , mkSpillInstr
   , mkRegRegMoveInstr
   , movInstr
   , jumpDestsOfInstr
   , canFallthroughTo
   , patchRegsOfInstr
   , patchJumpInstr
   , isMetaInstr
   , isJumpishInstr
   , movdOutFormat
   , MinOrMax(..), MinMaxType(..)
   )
where

import GHC.Prelude
import GHC.Data.FastString

import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target (targetClassOfReg)
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Instr (RegUsage(..), noUsage)
import GHC.Platform.Reg
import GHC.Platform.Reg.Class.Unified

import GHC.CmmToAsm.Config

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Platform.Regs
import GHC.Cmm
import GHC.Utils.Constants ( debugIsOn )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

import GHC.Cmm.CLabel
import GHC.Types.Unique.Set
import GHC.Types.Unique
import GHC.Types.Unique.DSM
import GHC.Types.Basic (Alignment)
import GHC.Cmm.DebugBlock (UnwindTable)
import GHC.Utils.Misc ( HasDebugCallStack )

import GHC.Data.Maybe

-- Format of an x86/x86_64 memory address, in bytes.
--
archWordFormat :: Bool -> Format
archWordFormat is32Bit
 | is32Bit   = II32
 | otherwise = II64

-- -----------------------------------------------------------------------------
-- Intel x86 instructions

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

        -- | X86 scalar move instruction.
        --
        -- When used at a vector format, only moves the lower 64 bits of data;
        -- the rest of the data in the destination may either be zeroed or
        -- preserved, depending on the specific format and operands.
        | MOV Format Operand Operand
             -- N.B. Due to AT&T assembler quirks, when used with 'II64'
             -- 'Format' immediate source and memory target operand, the source
             -- operand is interpreted to be a 32-bit sign-extended value.
             -- True 64-bit operands need to be either first moved to a register or moved
             -- with @MOVABS@; we currently do not use this instruction in GHC.
             -- See https://stackoverflow.com/questions/52434073/whats-the-difference-between-the-x86-64-att-instructions-movq-and-movabsq.

        | MOVD   Format Operand Operand -- ^ MOVD/MOVQ SSE2 instructions
                                        -- (bitcast between a general purpose
                                        -- register and a float register).
                                        -- Format is input format, output format is
                                        -- calculated in the 'movdOutFormat' function.
        | CMOV   Cond Format Operand Reg
        | MOVZxL      Format Operand Operand
              -- ^ The format argument is the size of operand 1 (the number of bits we keep)
              -- We always zero *all* high bits, even though this isn't how the actual instruction
              -- works. The code generator also seems to rely on this behaviour and it's faster
              -- to execute on many cpus as well so for now I'm just documenting the fact.
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
        -- | AVX bitwise logical XOR operation
        | VXOR        Format Operand Reg Reg
        | NOT         Format Operand
        | NEGI        Format Operand         -- NEG instruction (name clash with Cond)
        | BSWAP       Format Reg

        -- Shifts (amount may be immediate or %cl only)
        | SHL         Format Operand{-amount-} Operand
        | SAR         Format Operand{-amount-} Operand
        | SHR         Format Operand{-amount-} Operand
        | SHRD        Format Operand{-amount-} Operand Operand
        | SHLD        Format Operand{-amount-} Operand Operand

        | BT          Format Imm Operand
        | NOP


        -- We need to support the FSTP (x87 store and pop) instruction
        -- so that we can correctly read off the return value of an
        -- x86 CDECL C function call when its floating point.
        -- so we don't include a register argument, and just use st(0)
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

        -- | FMA3 fused multiply-add operations.
        | FMA3         Format FMASign FMAPermutation Operand Reg Reg
          -- For the FMA213 permutation (the only one we use currently),
          -- this is: src3 (r/m), src2 (r), dst/src1 (r)
          -- (NB: this isexactly reversed from how Intel lists the arguments.)

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
        | JMP         Operand [RegWithFormat] -- including live Regs at the call
        | JXX         Cond BlockId  -- includes unconditional branches
        | JXX_GBL     Cond Imm      -- non-local version of JXX
        -- Table jump
        | JMP_TBL     Operand   -- Address to jump to
                      [Maybe JumpDest] -- Targets of the jump table
                      Section   -- Data section jump table should be put in
                      CLabel    -- Label of jump table
        -- | X86 call instruction
        | CALL        (Either Imm Reg) -- ^ Jump target
                      [RegWithFormat]  -- ^ Arguments (required for register allocation)

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
        | XCHG        Format Operand Reg     -- src (r/m), dst (r/m)
        | MFENCE

        -- Vector Instructions --
        -- NOTE: Instructions follow the AT&T syntax
        -- Constructors and deconstructors
        | VBROADCAST  Format Operand Reg
        | VEXTRACT    Format Imm Reg Operand
        | INSERTPS    Format Imm Operand Reg

        -- move operations

        -- | SSE2 unaligned move of floating-point vectors
        | MOVU        Format Operand Operand
        -- | AVX unaligned move of floating-point vectors
        | VMOVU       Format Operand Operand
        -- | SSE2 move between memory and low-part of an xmm register
        | MOVL        Format Operand Operand
        -- | SSE move between memory and high-part of an xmm register
        | MOVH        Format Operand Operand
        -- | SSE2 unaligned move of integer vectors
        | MOVDQU      Format Operand Operand
        -- | AVX unaligned move of integer vectors
        | VMOVDQU     Format Operand Operand

        -- logic operations
        | PXOR        Format Operand Reg
        | VPXOR       Format Reg Reg Reg

        -- Arithmetic
        | VADD       Format Operand Reg Reg
        | VSUB       Format Operand Reg Reg
        | VMUL       Format Operand Reg Reg
        | VDIV       Format Operand Reg Reg

        -- Shuffle
        | SHUF       Format Imm Operand Reg
        | VSHUF      Format Imm Operand Reg Reg
        | PSHUFD     Format Imm Operand Reg
        | VPSHUFD    Format Imm Operand Reg

        -- | Move two 32-bit floats from the high part of an xmm register
        -- to the low part of another xmm register.
        | MOVHLPS    Format Reg Reg
        | UNPCKL     Format Operand Reg
        | PUNPCKLQDQ Format Operand Reg

        -- Shift
        | PSLLDQ     Format Operand Reg
        | PSRLDQ     Format Operand Reg

        -- min/max
        | MINMAX  MinOrMax MinMaxType Format Operand Operand
        | VMINMAX MinOrMax MinMaxType Format Operand Reg Reg

data PrefetchVariant = NTA | Lvl0 | Lvl1 | Lvl2

-- | 'MIN' or 'MAX'
data MinOrMax = Min | Max
  deriving ( Eq, Show )
-- | What kind of min/max operation: signed or unsigned vector integer min/max,
-- or (scalar or vector) floating point min/max?
data MinMaxType =
  IntVecMinMax { minMaxSigned :: Bool } | FloatMinMax
  deriving ( Eq, Show )

data Operand
        = OpReg  Reg            -- register
        | OpImm  Imm            -- immediate value
        | OpAddr AddrMode       -- memory reference

-- NB: As of 2023 we only use the FMA213 permutation.
data FMAPermutation = FMA132 | FMA213 | FMA231

-- | Returns which registers are read and written as a (read, written)
-- pair.
regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr
 = case instr of
    MOV fmt src dst
      -- MOVSS/MOVSD preserve the upper half of vector registers,
      -- but only for reg-2-reg moves
      | VecFormat _ sFmt <- fmt
      , isFloatScalarFormat sFmt
      , OpReg {} <- src
      , OpReg {} <- dst
      -> usageRM fmt src dst
      -- other MOV instructions zero any remaining upper part of the destination
      -- (largely to avoid partial register stalls)
      | otherwise
      -> usageRW fmt src dst
    MOVD   fmt src dst    ->
      -- NB: MOVD/MOVQ always zero any remaining upper part of destination
      mkRU (use_R fmt src []) (use_R (movdOutFormat fmt) dst [])
    CMOV _ fmt src dst    -> mkRU (use_R fmt src [mk fmt dst]) [mk fmt dst]
    MOVZxL fmt src dst    -> usageRW fmt src dst
    MOVSxL fmt src dst    -> usageRW fmt src dst
    LEA    fmt src dst    -> usageRW fmt src dst
    ADD    fmt src dst    -> usageRM fmt src dst
    ADC    fmt src dst    -> usageRM fmt src dst
    SUB    fmt src dst    -> usageRM fmt src dst
    SBB    fmt src dst    -> usageRM fmt src dst
    IMUL   fmt src dst    -> usageRM fmt src dst

    -- Result of IMULB will be in just in %ax
    IMUL2  II8 src       -> mkRU (mk II8 eax:use_R II8 src []) [mk II8 eax]
    -- Result of IMUL for wider values, will be split between %dx/%edx/%rdx and
    -- %ax/%eax/%rax.
    IMUL2  fmt src        -> mkRU (mk fmt eax:use_R fmt src []) [mk fmt eax,mk fmt edx]

    MUL    fmt src dst    -> usageRM fmt src dst
    MUL2   fmt src        -> mkRU (mk fmt eax:use_R fmt src []) [mk fmt eax,mk fmt edx]
    DIV    fmt op -> mkRU (mk fmt eax:mk fmt edx:use_R fmt op []) [mk fmt eax, mk fmt edx]
    IDIV   fmt op -> mkRU (mk fmt eax:mk fmt edx:use_R fmt op []) [mk fmt eax, mk fmt edx]
    ADD_CC fmt src dst    -> usageRM fmt src dst
    SUB_CC fmt src dst    -> usageRM fmt src dst
    AND    fmt src dst    -> usageRM fmt src dst
    OR     fmt src dst    -> usageRM fmt src dst

    XOR    fmt (OpReg src) (OpReg dst)
      | src == dst
      -> mkRU [] [mk fmt dst]
    XOR    fmt src dst
      -> usageRM fmt src dst
    VXOR fmt (OpReg src1) src2 dst
      | src1 == src2, src1 == dst
      -> mkRU [] [mk fmt dst]
    VXOR fmt src1 src2 dst
      -> mkRU (use_R fmt src1 [mk fmt src2]) [mk fmt dst]

    NOT    fmt op         -> usageM fmt op
    BSWAP  fmt reg        -> mkRU [mk fmt reg] [mk fmt reg]
    NEGI   fmt op         -> usageM fmt op
    SHL    fmt imm dst    -> usageRM fmt imm dst
    SAR    fmt imm dst    -> usageRM fmt imm dst
    SHR    fmt imm dst    -> usageRM fmt imm dst
    SHLD   fmt imm dst1 dst2 -> usageRMM fmt imm dst1 dst2
    SHRD   fmt imm dst1 dst2 -> usageRMM fmt imm dst1 dst2
    BT     fmt _   src    -> mkRUR (use_R fmt src [])

    PUSH   fmt op         -> mkRUR (use_R fmt op [])
    POP    fmt op         -> mkRU [] (def_W fmt op)
    TEST   fmt src dst    -> mkRUR (use_R fmt src $! use_R fmt dst [])
    CMP    fmt src dst    -> mkRUR (use_R fmt src $! use_R fmt dst [])
    SETCC  _ op         -> mkRU [] (def_W II8 op)
    JXX    _ _          -> mkRU [] []
    JXX_GBL _ _         -> mkRU [] []
    JMP     op regs     -> mkRU (use_R addrFmt op regs) []
    JMP_TBL op _ _ _    -> mkRU (use_R addrFmt op []) []
    CALL (Left _)  params   -> mkRU params (map mkFmt $ callClobberedRegs platform)
    CALL (Right reg) params -> mkRU (mk addrFmt reg:params) (map mkFmt $ callClobberedRegs platform)
    CLTD   fmt          -> mkRU [mk fmt eax] [mk fmt edx]
    NOP                 -> mkRU [] []

    X87Store _fmt  dst -> mkRUR (use_EA dst [])

    CVTSS2SD   src dst  -> mkRU [mk FF32 src] [mk FF64 dst]
    CVTSD2SS   src dst  -> mkRU [mk FF64 src] [mk FF32 dst]
    CVTTSS2SIQ fmt src dst -> mkRU (use_R FF32 src []) [mk fmt dst]
    CVTTSD2SIQ fmt src dst -> mkRU (use_R FF64 src []) [mk fmt dst]
    CVTSI2SS   fmt src dst -> mkRU (use_R fmt src []) [mk FF32 dst]
    CVTSI2SD   fmt src dst -> mkRU (use_R fmt src []) [mk FF64 dst]
    FDIV fmt     src dst  -> usageRM fmt src dst
    SQRT fmt src dst      -> mkRU (use_R fmt src []) [mk fmt dst]

    FETCHGOT reg        -> mkRU [] [mk addrFmt reg]
    FETCHPC  reg        -> mkRU [] [mk addrFmt reg]

    COMMENT _           -> noUsage
    LOCATION{}          -> noUsage
    UNWIND{}            -> noUsage
    DELTA   _           -> noUsage

    POPCNT fmt src dst -> mkRU (use_R fmt src []) [mk fmt dst]
    LZCNT  fmt src dst -> mkRU (use_R fmt src []) [mk fmt dst]
    TZCNT  fmt src dst -> mkRU (use_R fmt src []) [mk fmt dst]
    BSF    fmt src dst -> mkRU (use_R fmt src []) [mk fmt dst]
    BSR    fmt src dst -> mkRU (use_R fmt src []) [mk fmt dst]

    PDEP   fmt src mask dst -> mkRU (use_R fmt src $ use_R fmt mask []) [mk fmt dst]
    PEXT   fmt src mask dst -> mkRU (use_R fmt src $ use_R fmt mask []) [mk fmt dst]

    FMA3 fmt _ _ src3 src2 dst -> usageFMA fmt src3 src2 dst

    -- note: might be a better way to do this
    PREFETCH _  fmt src -> mkRU (use_R fmt src []) []
    LOCK i              -> regUsageOfInstr platform i
    XADD fmt src dst      -> usageMM fmt src dst
    CMPXCHG fmt src dst   -> usageRMM fmt src dst (OpReg eax)
    XCHG fmt src dst      -> usageMM fmt src (OpReg dst)
    MFENCE -> noUsage

    -- vector instructions
    VBROADCAST fmt src dst   -> mkRU (use_R fmt src []) [mk fmt dst]
    VEXTRACT     fmt _off src dst -> mkRU [mk fmt src] (use_R fmt dst [])
    INSERTPS     fmt (ImmInt off) src dst
      -> mkRU ((use_R fmt src []) ++ [mk fmt dst | not doesNotReadDst]) [mk fmt dst]
        where
          -- Compute whether the instruction reads the destination register or not.
          -- Immediate bits: ss_dd_zzzz s = src pos, d = dst pos, z = zeroed components.
          doesNotReadDst = and [ testBit off i | i <- [0, 1, 2, 3], i /= pos ]
            -- Check whether the positions in which we are not inserting
            -- are being zeroed.
            where pos = ( off `shiftR` 4 ) .&. 0b11
    INSERTPS fmt _off src dst
      -> mkRU ((use_R fmt src []) ++ [mk fmt dst]) [mk fmt dst]

    VMOVU        fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])
    MOVU         fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])
    MOVL         fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])
    MOVH         fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])
    MOVDQU       fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])
    VMOVDQU      fmt src dst   -> mkRU (use_R fmt src []) (use_R fmt dst [])

    PXOR fmt (OpReg src) dst
      | src == dst
      -> mkRU [] [mk fmt dst]
      | otherwise
      -> mkRU [mk fmt src, mk fmt dst] [mk fmt dst]

    VPXOR        fmt s1 s2 dst
      | s1 == s2, s1 == dst
      -> mkRU [] [mk fmt dst]
      | otherwise
      -> mkRU [mk fmt s1, mk fmt s2] [mk fmt dst]

    VADD         fmt s1 s2 dst -> mkRU ((use_R fmt s1 []) ++ [mk fmt s2]) [mk fmt dst]
    VSUB         fmt s1 s2 dst -> mkRU ((use_R fmt s1 []) ++ [mk fmt s2]) [mk fmt dst]
    VMUL         fmt s1 s2 dst -> mkRU ((use_R fmt s1 []) ++ [mk fmt s2]) [mk fmt dst]
    VDIV         fmt s1 s2 dst -> mkRU ((use_R fmt s1 []) ++ [mk fmt s2]) [mk fmt dst]

    SHUF fmt _mask src dst
      -> mkRU (use_R fmt src [mk fmt dst]) [mk fmt dst]
    VSHUF fmt _mask src1 src2 dst
      -> mkRU (use_R fmt src1 [mk fmt src2]) [mk fmt dst]
    PSHUFD fmt _mask src dst
      -> mkRU (use_R fmt src []) [mk fmt dst]
    VPSHUFD fmt _mask src dst
      -> mkRU (use_R fmt src []) [mk fmt dst]

    PSLLDQ fmt off dst -> mkRU (use_R fmt off []) [mk fmt dst]

    MOVHLPS    fmt src dst
      -> mkRU [mk fmt src] [mk fmt dst]
    UNPCKL fmt src dst
      -> mkRU (use_R fmt src [mk fmt dst]) [mk fmt dst]
    PUNPCKLQDQ fmt src dst
      -> mkRU (use_R fmt src [mk fmt dst]) [mk fmt dst]

    MINMAX _ _ fmt src dst
      -> mkRU (use_R fmt src $ use_R fmt dst []) (use_R fmt dst [])
    VMINMAX _ _ fmt src1 src2 dst
      -> mkRU (use_R fmt src1 [mk fmt src2]) [mk fmt dst]
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
    usageRW :: HasDebugCallStack => Format -> Operand -> Operand -> RegUsage
    usageRW fmt op (OpReg reg)      = mkRU (use_R fmt op []) [mk fmt reg]
    usageRW fmt op (OpAddr ea)      = mkRUR (use_R fmt op $! use_EA ea [])
    usageRW _ _ _                   = panic "X86.RegInfo.usageRW: no match"

    -- 2 operand form; first operand Read; second Modified
    usageRM :: HasDebugCallStack => Format -> Operand -> Operand -> RegUsage
    usageRM fmt op (OpReg reg)      = mkRU (use_R fmt op [mk fmt reg]) [mk fmt reg]
    usageRM fmt op (OpAddr ea)      = mkRUR (use_R fmt op $! use_EA ea [])
    usageRM _ _ _                   = panic "X86.RegInfo.usageRM: no match"

    -- 2 operand form; first operand Modified; second Modified
    usageMM :: HasDebugCallStack => Format -> Operand -> Operand -> RegUsage
    usageMM fmt (OpReg src) (OpReg dst) = mkRU [mk fmt src, mk fmt dst] [mk fmt src, mk fmt dst]
    usageMM fmt (OpReg src) (OpAddr ea) = mkRU (use_EA ea [mk fmt src]) [mk fmt src]
    usageMM fmt (OpAddr ea) (OpReg dst) = mkRU (use_EA ea [mk fmt dst]) [mk fmt dst]
    usageMM _ _ _                       = panic "X86.RegInfo.usageMM: no match"

    -- 3 operand form; first operand Read; second Modified; third Modified
    usageRMM :: HasDebugCallStack => Format -> Operand -> Operand -> Operand -> RegUsage
    usageRMM fmt (OpReg src) (OpReg dst) (OpReg reg) = mkRU [mk fmt src, mk fmt dst, mk fmt reg] [mk fmt dst, mk fmt reg]
    usageRMM fmt (OpReg src) (OpAddr ea) (OpReg reg) = mkRU (use_EA ea [mk fmt src, mk fmt reg]) [mk fmt reg]
    usageRMM _ _ _ _                                 = panic "X86.RegInfo.usageRMM: no match"

    -- 3 operand form of FMA instructions.
    usageFMA :: HasDebugCallStack => Format -> Operand -> Reg -> Reg -> RegUsage
    usageFMA fmt (OpReg src1) src2 dst =
      mkRU [mk fmt src1, mk fmt src2, mk fmt dst] [mk fmt dst]
    usageFMA fmt (OpAddr ea1) src2 dst
      = mkRU (use_EA ea1 [mk fmt src2, mk fmt  dst]) [mk fmt dst]
    usageFMA _ _ _ _
      = panic "X86.RegInfo.usageFMA: no match"

    -- 1 operand form; operand Modified
    usageM :: HasDebugCallStack => Format -> Operand -> RegUsage
    usageM fmt (OpReg reg) =
      let r' = mk fmt reg
      in mkRU [r'] [r']
    usageM _ (OpAddr ea) = mkRUR (use_EA ea [])
    usageM _ _ = panic "X86.RegInfo.usageM: no match"

    -- Registers defd when an operand is written.
    def_W fmt (OpReg reg)         = [mk fmt reg]
    def_W _   (OpAddr _ )         = []
    def_W _   _                   = panic "X86.RegInfo.def_W: no match"

    -- Registers used when an operand is read.
    use_R :: HasDebugCallStack => Format -> Operand -> [RegWithFormat] -> [RegWithFormat]
    use_R fmt (OpReg reg)  tl = mk fmt reg : tl
    use_R _   (OpImm _)    tl = tl
    use_R _   (OpAddr ea)  tl = use_EA ea tl

    -- Registers used to compute an effective address.
    use_EA (ImmAddr _ _) tl = tl
    use_EA (AddrBaseIndex base index _) tl =
        use_base base $! use_index index tl
        where use_base (EABaseReg r)  tl = mk addrFmt r : tl
              use_base _              tl = tl
              use_index EAIndexNone   tl = tl
              use_index (EAIndex i _) tl = mk addrFmt i : tl

    mkRUR :: [RegWithFormat] -> RegUsage
    mkRUR src = mkRU src []

    mkRU :: [RegWithFormat] -> [RegWithFormat] -> RegUsage
    mkRU src dst = src' `seq` dst' `seq` RU src' dst'
        where src' = filter (interesting platform . regWithFormat_reg) src
              dst' = filter (interesting platform . regWithFormat_reg) dst

    addrFmt = archWordFormat (target32Bit platform)
    mk :: Format -> Reg -> RegWithFormat
    mk fmt r = RegWithFormat r fmt

    mkFmt :: Reg -> RegWithFormat
    mkFmt r = RegWithFormat r $ case targetClassOfReg platform r of
      RcInteger -> addrFmt
      RcFloatOrVector -> FF64

-- | Is this register interesting for the register allocator?
interesting :: Platform -> Reg -> Bool
interesting _        (RegVirtual _)              = True
interesting platform (RegReal (RealRegSingle i)) = freeReg platform i

movdOutFormat :: Format -> Format
movdOutFormat format = case format of
  II32 -> FF32
  II64 -> FF64
  FF32 -> II32
  FF64 -> II64
  _    -> pprPanic "X86: improper format for movd/movq" (ppr format)


-- | Applies the supplied function to all registers in instructions.
-- Typically used to change virtual registers to real registers.
patchRegsOfInstr :: HasDebugCallStack => Platform -> Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr platform instr env
  = case instr of
    MOV fmt src dst      -> MOV fmt (patchOp src) (patchOp dst)
    MOVD fmt src dst     -> patch2 (MOVD fmt) src dst
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
    VXOR fmt src1 src2 dst -> VXOR fmt (patchOp src1) (env src2) (env dst)
    NOT  fmt op          -> patch1 (NOT  fmt) op
    BSWAP fmt reg        -> BSWAP fmt (env reg)
    NEGI fmt op          -> patch1 (NEGI fmt) op
    SHL  fmt imm dst     -> patch1 (SHL fmt imm) dst
    SAR  fmt imm dst     -> patch1 (SAR fmt imm) dst
    SHR  fmt imm dst     -> patch1 (SHR fmt imm) dst
    SHLD fmt imm dst1 dst2 -> patch2 (SHLD fmt imm) dst1 dst2
    SHRD fmt imm dst1 dst2 -> patch2 (SHRD fmt imm) dst1 dst2
    BT   fmt imm src     -> patch1 (BT  fmt imm) src
    TEST fmt src dst     -> patch2 (TEST fmt) src dst
    CMP  fmt src dst     -> patch2 (CMP  fmt) src dst
    PUSH fmt op          -> patch1 (PUSH fmt) op
    POP  fmt op          -> patch1 (POP  fmt) op
    SETCC cond op        -> patch1 (SETCC cond) op
    JMP op regs          -> JMP (patchOp op) regs
    JMP_TBL op ids s lbl -> JMP_TBL (patchOp op) ids s lbl

    FMA3 fmt perm var x1 x2 x3 -> patch3 (FMA3 fmt perm var) x1 x2 x3

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
    LDATA {}            -> instr
    NEWBLOCK {}         -> instr

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

    LOCK i               -> LOCK (patchRegsOfInstr platform i env)
    XADD fmt src dst     -> patch2 (XADD fmt) src dst
    CMPXCHG fmt src dst  -> patch2 (CMPXCHG fmt) src dst
    XCHG fmt src dst     -> XCHG fmt (patchOp src) (env dst)
    MFENCE               -> instr

    -- vector instructions
    VBROADCAST   fmt src dst   -> VBROADCAST fmt (patchOp src) (env dst)
    VEXTRACT     fmt off src dst
      -> VEXTRACT fmt off (env src) (patchOp dst)
    INSERTPS    fmt off src dst
      -> INSERTPS fmt off (patchOp src) (env dst)

    VMOVU      fmt src dst   -> VMOVU fmt (patchOp src) (patchOp dst)
    MOVU       fmt src dst   -> MOVU  fmt (patchOp src) (patchOp dst)
    MOVL       fmt src dst   -> MOVL  fmt (patchOp src) (patchOp dst)
    MOVH       fmt src dst   -> MOVH  fmt (patchOp src) (patchOp dst)
    MOVDQU     fmt src dst   -> MOVDQU  fmt (patchOp src) (patchOp dst)
    VMOVDQU    fmt src dst   -> VMOVDQU fmt (patchOp src) (patchOp dst)

    PXOR       fmt src dst   -> PXOR fmt (patchOp src) (env dst)
    VPXOR      fmt s1 s2 dst -> VPXOR fmt (env s1) (env s2) (env dst)

    VADD       fmt s1 s2 dst -> VADD fmt (patchOp s1) (env s2) (env dst)
    VSUB       fmt s1 s2 dst -> VSUB fmt (patchOp s1) (env s2) (env dst)
    VMUL       fmt s1 s2 dst -> VMUL fmt (patchOp s1) (env s2) (env dst)
    VDIV       fmt s1 s2 dst -> VDIV fmt (patchOp s1) (env s2) (env dst)

    SHUF      fmt off src dst
      -> SHUF fmt off (patchOp src) (env dst)
    VSHUF      fmt off src1 src2 dst
      -> VSHUF fmt off (patchOp src1) (env src2) (env dst)
    PSHUFD       fmt off src dst
      -> PSHUFD  fmt off (patchOp src) (env dst)
    VPSHUFD      fmt off src dst
      -> VPSHUFD fmt off (patchOp src) (env dst)

    PSLLDQ       fmt off dst
      -> PSLLDQ  fmt (patchOp off) (env dst)
    PSRLDQ       fmt off dst
      -> PSRLDQ  fmt (patchOp off) (env dst)

    MOVHLPS    fmt src dst
      -> MOVHLPS fmt (env src) (env dst)
    UNPCKL fmt src dst
      -> UNPCKL fmt (patchOp src) (env dst)
    PUNPCKLQDQ fmt src dst
      -> PUNPCKLQDQ fmt (patchOp src) (env dst)

    MINMAX minMax ty fmt src dst
      -> MINMAX minMax ty fmt (patchOp src) (patchOp dst)
    VMINMAX minMax ty fmt src1 src2 dst
      -> VMINMAX minMax ty fmt (patchOp src1) (env src2) (env dst)

  where
    patch1 :: (Operand -> a) -> Operand -> a
    patch1 insn op      = insn $! patchOp op
    patch2 :: (Operand -> Operand -> a) -> Operand -> Operand -> a
    patch2 insn src dst = (insn $! patchOp src) $! patchOp dst
    patch3 :: (Operand -> Reg -> Reg -> a) -> Operand -> Reg -> Reg -> a
    patch3 insn src1 src2 dst = ((insn $! patchOp src1) $! env src2) $! env dst

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
isJumpishInstr
        :: Instr -> Bool

isJumpishInstr instr
 = case instr of
        JMP{}           -> True
        JXX{}           -> True
        JXX_GBL{}       -> True
        JMP_TBL{}       -> True
        CALL{}          -> True
        _               -> False

canFallthroughTo :: Instr -> BlockId -> Bool
canFallthroughTo insn bid
  = case insn of
    JXX _ target          -> bid == target
    JMP_TBL _ targets _ _ -> all isTargetBid targets
    _                     -> False
  where
    isTargetBid target = case target of
      Nothing                      -> True
      Just (DestBlockId target) -> target == bid
      _                  -> False

jumpDestsOfInstr
        :: Instr
        -> [BlockId]

jumpDestsOfInstr insn
  = case insn of
        JXX _ id        -> [id]
        JMP_TBL _ ids _ _ -> [id | Just (DestBlockId id) <- ids]
        _               -> []


patchJumpInstr
        :: Instr -> (BlockId -> BlockId) -> Instr

patchJumpInstr insn patchF
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
mkSpillInstr
    :: HasDebugCallStack
    => NCGConfig
    -> RegWithFormat -- register to spill
    -> Int       -- current stack delta
    -> Int       -- spill slot to use
    -> [Instr]

mkSpillInstr config (RegWithFormat reg fmt) delta slot =
  [ movInstr config fmt' (OpReg reg) (OpAddr (spRel platform off)) ]
  where
    fmt'
      | isVecFormat fmt
      = fmt
      | otherwise
      = scalarMoveFormat platform fmt
      -- Spill the platform word size, at a minimum
    platform = ncgPlatform config
    off = spillSlotToOffset platform slot - delta

-- | Make a spill reload instruction.
mkLoadInstr
    :: HasDebugCallStack
    => NCGConfig
    -> RegWithFormat      -- register to load
    -> Int      -- current stack delta
    -> Int      -- spill slot to use
    -> [Instr]

mkLoadInstr config (RegWithFormat reg fmt) delta slot =
  [ movInstr config fmt' (OpAddr (spRel platform off)) (OpReg reg) ]
  where
    fmt'
      | isVecFormat fmt
      = fmt
      | otherwise
      = scalarMoveFormat platform fmt
        -- Load the platform word size, at a minimum
    platform = ncgPlatform config
    off = spillSlotToOffset platform slot - delta

-- | A move instruction for moving the entire contents of an operand
-- at the given 'Format'.
movInstr :: HasDebugCallStack => NCGConfig -> Format -> (Operand -> Operand -> Instr)
movInstr config fmt =
  case fmt of
    VecFormat _ sFmt ->
      case formatToWidth fmt of
        W512 ->
          if avx512f
          then avx_move sFmt
          else sorry "512-bit wide vectors require -mavx512f"
        W256 ->
          if avx2
          then avx_move sFmt
          else sorry "256-bit wide vectors require -mavx2"
        W128 ->
          if avx
            -- Prefer AVX instructions over SSE when available
            -- (usually results in better performance).
          then avx_move sFmt
          else sse_move sFmt
        w -> sorry $ "Unhandled SIMD vector width: " ++ show w ++ " bits"
    _ -> MOV fmt
  where

    assertCompatibleRegs :: ( Operand -> Operand -> Instr ) -> Operand -> Operand -> Instr
    assertCompatibleRegs f
      | debugIsOn
      = \ op1 op2 ->
          if | OpReg r1 <- op1
             , OpReg r2 <- op2
             , targetClassOfReg plat r1 /= targetClassOfReg plat r2
             -> assertPpr False
                  ( vcat [ text "movInstr: move between incompatible registers"
                         , text "fmt:" <+> ppr fmt
                         , text "r1:" <+> ppr r1
                         , text "r2:" <+> ppr r2 ]
                  ) f op1 op2
             | otherwise
             -> f op1 op2
      | otherwise
      = f

    plat    = ncgPlatform config
    avx     = ncgAvxEnabled config
    avx2    = ncgAvx2Enabled config
    avx512f = ncgAvx512fEnabled config
    avx_move sFmt =
      if isFloatScalarFormat sFmt
      then assertCompatibleRegs $
           VMOVU   fmt
      else VMOVDQU fmt
    sse_move sFmt =
      if isFloatScalarFormat sFmt
      then assertCompatibleRegs $
           MOVU   fmt
      else MOVDQU fmt
    -- NB: we are using {V}MOVU and not {V}MOVA, because we have no guarantees
    -- about the stack being sufficiently aligned (even for even numbered stack slots).
    --
    -- (Ben Gamari told me that using MOVA instead of MOVU does not make a
    -- difference in practice when moving between registers.)

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
takeDeltaInstr
        :: Instr
        -> Maybe Int

takeDeltaInstr instr
 = case instr of
        DELTA i         -> Just i
        _               -> Nothing


isMetaInstr
        :: Instr
        -> Bool

isMetaInstr instr
 = case instr of
        COMMENT{}       -> True
        LOCATION{}      -> True
        LDATA{}         -> True
        NEWBLOCK{}      -> True
        UNWIND{}        -> True
        DELTA{}         -> True
        _               -> False

-- | Make a reg-reg move instruction.
mkRegRegMoveInstr
    :: HasDebugCallStack
    => NCGConfig
    -> Format
    -> Reg
    -> Reg
    -> Instr
mkRegRegMoveInstr config fmt src dst =
  movInstr config fmt' (OpReg src) (OpReg dst)
    -- Move the platform word size, at a minimum.
    --
    -- This ensures the upper part of the register is properly cleared
    -- and avoids partial register stalls.
    --
    -- See also the 'ArithInt8' and 'ArithWord8' tests,
    -- which fail without this logic.
  where
    platform = ncgPlatform config
    fmt'
      | isVecFormat fmt
      = fmt
      | otherwise
      = scalarMoveFormat platform fmt

scalarMoveFormat :: Platform -> Format -> Format
scalarMoveFormat platform fmt
  | isFloatFormat fmt
  = FF64
  | II64 <- fmt
  = II64
  | otherwise
  = archWordFormat (target32Bit platform)

-- | Check whether an instruction represents a reg-reg move.
--      The register allocator attempts to eliminate reg->reg moves whenever it can,
--      by assigning the src and dest temporaries to the same real register.
--
takeRegRegMoveInstr
        :: Platform
        -> Instr
        -> Maybe (Reg,Reg)

takeRegRegMoveInstr platform = \case
  MOV fmt (OpReg r1) (OpReg r2)
    -- When used with vector registers, MOV only moves the lower part,
    -- so it is not a real move. For example, MOVSS/MOVSD between xmm registers
    -- preserves the upper half, and MOVQ between xmm registers zeroes the upper half.
    | not $ isVecFormat fmt
    -- Don't eliminate a move between e.g. RAX and XMM:
    -- even though we might be using XMM to store a scalar integer value,
    -- some instructions only support XMM registers.
    , targetClassOfReg platform r1 == targetClassOfReg platform r2
    -> Just (r1, r2)
  MOVD {}
    -- MOVD moves between xmm registers and general-purpose registers,
    -- and we don't want to eliminate those moves (as noted for MOV).
    -> Nothing

  -- SSE2/AVX move instructions always move the full register.
  MOVU _ (OpReg r1) (OpReg r2)
    -> Just (r1, r2)
  VMOVU _ (OpReg r1) (OpReg r2)
    -> Just (r1, r2)
  MOVDQU _ (OpReg r1) (OpReg r2)
    -> Just (r1, r2)
  VMOVDQU _ (OpReg r1) (OpReg r2)
    -> Just (r1, r2)

  -- TODO: perhaps we can eliminate MOVZxL in certain situations?
  MOVZxL {} -> Nothing
  MOVSxL {} -> Nothing

  -- MOVL, MOVH and MOVHLPS preserve some part of the destination register,
  -- so are not simple moves.
  MOVL {} -> Nothing
  MOVH {} -> Nothing
  MOVHLPS {} -> Nothing

  -- Other instructions are not moves.
  _ -> Nothing

-- | Make an unconditional branch instruction.
mkJumpInstr
        :: BlockId
        -> [Instr]

mkJumpInstr id
        = [JXX ALWAYS id]

-- Note [Windows stack layout]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
--   See Note [Windows stack allocations].
--
needs_probe_call :: Platform -> Int -> Bool
needs_probe_call platform amount
  = case platformOS platform of
     OSMinGW32 -> case platformArch platform of
                    ArchX86_64 -> amount > (4 * 1024)
                    _          -> False
     _         -> False

mkStackAllocInstr
        :: Platform
        -> Int
        -> [Instr]
mkStackAllocInstr platform amount
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
        --    after the dealloc, preserving #2.  See Note [extra spill slots].
        --
        -- We emit a call because the stack probes are quite involved and
        -- would bloat code size a lot.  GHC doesn't really have an -Os.
        -- ___chkstk is guaranteed to leave all nonvolatile registers and AX
        -- untouched.  It's part of the standard prologue code for any Windows
        -- function dropping the stack more than a page.
        -- See Note [Windows stack layout]
        case platformArch platform of
            ArchX86_64 | needs_probe_call platform amount ->
                           [ MOV II64 (OpImm (ImmInt amount)) (OpReg rax)
                           , CALL (Left $ strImmLit (fsLit "___chkstk_ms")) [RegWithFormat rax II64]
                           , SUB II64 (OpReg rax) (OpReg rsp)
                           ]
                       | otherwise ->
                           [ SUB II64 (OpImm (ImmInt amount)) (OpReg rsp)
                           , TEST II64 (OpReg rsp) (OpReg rsp)
                           ]
            _ -> panic "X86.mkStackAllocInstr"
      _       ->
        case platformArch platform of
          ArchX86    -> [ SUB II32 (OpImm (ImmInt amount)) (OpReg esp) ]
          ArchX86_64 -> [ SUB II64 (OpImm (ImmInt amount)) (OpReg rsp) ]
          _ -> panic "X86.mkStackAllocInstr"

mkStackDeallocInstr
        :: Platform
        -> Int
        -> [Instr]
mkStackDeallocInstr platform amount
  = case platformArch platform of
      ArchX86    -> [ADD II32 (OpImm (ImmInt amount)) (OpReg esp)]
      ArchX86_64 -> [ADD II64 (OpImm (ImmInt amount)) (OpReg rsp)]
      _ -> panic "X86.mkStackDeallocInstr"


-- Note [extra spill slots]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
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
  -> UniqDSM (NatCmmDecl statics GHC.CmmToAsm.X86.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top,[])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
    let entries = entryBlocks proc

    retargetList <- mapM (\e -> (e,) <$> newBlockId) entries

    let
      delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where x = slots * spillSlotSize platform -- sp delta

      alloc   = mkStackAllocInstr   platform delta
      dealloc = mkStackDeallocInstr platform delta

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
         _other      -> patchJumpInstr insn retarget : r
           where retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insns code
    -- in
    return (CmmProc info lbl live (ListGraph new_code), retargetList)

data JumpDest = DestBlockId BlockId | DestImm Imm

-- Debug Instance
instance Outputable JumpDest where
  ppr (DestBlockId bid) = text "jd<blk>:" <> ppr bid
  ppr (DestImm _imm)    = text "jd<imm>:noShow"

-- Implementations of the methods of 'NgcImpl'

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
  | Just blkId <- maybeLocalBlockLabel lab = shortBlockId fn emptyUniqueSet blkId
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
        -> UniqueSet
        -> BlockId
        -> CLabel

shortBlockId fn seen blockid =
  case (memberUniqueSet uq seen, fn blockid) of
    (True, _)    -> blockLbl blockid
    (_, Nothing) -> blockLbl blockid
    (_, Just (DestBlockId blockid'))  -> shortBlockId fn (insertUniqueSet uq seen) blockid'
    (_, Just (DestImm (ImmCLbl lbl))) -> lbl
    (_, _other) -> panic "shortBlockId"
  where uq = getUnique blockid
