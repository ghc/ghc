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
import GHC.CmmToAsm.RegUsage
import GHC.Platform.Reg
import GHC.Platform.Reg.Class.Unified

import GHC.CmmToAsm.Config

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Utils.Constants ( debugIsOn )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Platform.CpuFeatures

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
        -- The format is the format the destination is written to. For an XMM
        -- register, using a scalar format means that we don't care about the
        -- upper bits, while using a vector format means that we care about the
        -- upper bits, even though we are only writing to the lower bits.
        --
        -- See also Note [Allocated register formats] in GHC.CmmToAsm.Reg.Linear.
        | MOV Format Operand Operand
             -- N.B. Due to AT&T assembler quirks, when used with 'II64'
             -- 'Format' immediate source and memory target operand, the source
             -- operand is interpreted to be a 32-bit sign-extended value.
             -- True 64-bit operands need to be either first moved to a register or moved
             -- with @MOVABS@; we currently do not use this instruction in GHC.
             -- See https://stackoverflow.com/questions/52434073/whats-the-difference-between-the-x86-64-att-instructions-movq-and-movabsq.

        -- | MOVD/MOVQ SSE2 instructions
        -- (bitcast between a general purpose register and a float register).
        | MOVD
           Format -- ^ input format
           Format -- ^ output format
           Operand Operand
           -- NB: MOVD stores both the input and output formats. This is because
           -- neither format fully determines the other, as either might be
           -- a vector format, and we need to know the exact format in order to
           -- correctly spill/unspill. See #25659.
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
        | NOT         Format Operand
        | NEGI        Format Operand         -- NEG instruction (name clash with Cond)
        | BSWAP       Format Reg
        -- Vector bitwise logical operations
        | VAND        Format Operand Reg Reg
        | VOR         Format Operand Reg Reg
        | VXOR        Format Operand Reg Reg

        -- Shifts (amount may be immediate or %cl only)
        | SHL         Format Operand{-amount-} Operand
        | SAR         Format Operand{-amount-} Operand
        | SHR         Format Operand{-amount-} Operand
        | SHRD        Format Operand{-amount-} Operand Operand
        | SHLD        Format Operand{-amount-} Operand Operand

        | BT          Format Imm Operand
        -- | Bit test-and-reset
        | BTR         Format Operand{- ^ bit offset (imm/reg) -} Operand
        -- | Bit set
        | BTS         Format Operand{- ^ bit offset (imm/reg) -} Operand
        -- | Bit complement
        | BTC         Format Operand{- ^ bit offset (imm/reg) -} Operand
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
        | FDIV          Format Operand Reg   -- divisor, dividend(dst)

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
                      !(Maybe CLabel) -- Label used to compute relative offsets. Otherwise we store absolute addresses.
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
        | VPBROADCAST Format Format Operand Reg -- scalar format, vector format, source, destination
        | VEXTRACT    Format Imm Reg Operand
        | INSERTPS    Format Imm Operand Reg
        | VINSERTPS   Format Imm Operand Reg Reg
        | PINSR       Format Format Imm Operand Reg -- scalar format, vector format, offset, scalar src, vector
        | PEXTR       Format Format Imm Reg Operand -- scalar format, vector format, offset, vector src, scalar dst

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
        -- | Alias for VMOVSS/VMOVSD, used to merge two vectors
        | VMOV_MERGE  Format Reg Reg Reg

        -- logic operations
        | PXOR        Format Operand Reg
        | VPXOR       Format Operand Reg Reg
        | PAND        Format Operand Reg
        | PANDN       Format Operand Reg
        | VPAND       Format Operand Reg Reg
        | POR         Format Operand Reg
        | VPOR        Format Operand Reg Reg

        -- Arithmetic
        | VADD       Format Operand Reg Reg
        | VSUB       Format Operand Reg Reg
        | VMUL       Format Operand Reg Reg
        | VDIV       Format Operand Reg Reg
        | PADD       Format Operand Reg
        | PSUB       Format Operand Reg
        | PMULL      Format Operand Reg
        | VPMULL     Format Operand Reg Reg
        | PMULUDQ    Format Operand Reg

        -- Absolute value and square root
        | PABS       Format Operand Reg -- SSE2
        | VPABS      Format Operand Reg -- AVX512F Int64
        | VSQRT      Format Operand Reg -- AVX

        -- SIMD compare
        | PCMPGT     Format Operand Reg

        -- Shuffle
        | SHUF       Format Imm Operand Reg
        | VSHUF      Format Imm Operand Reg Reg
        | PSHUFB     Format Operand Reg
        | PSHUFLW    Format Imm Operand Reg
        | PSHUFHW    Format Imm Operand Reg
        | PSHUFD     Format Imm Operand Reg
        | VPSHUFD    Format Imm Operand Reg
        | BLEND      Format Imm Operand Reg
        | VBLEND     Format Imm Operand Reg Reg
        | PBLENDW    Format Imm Operand Reg

        -- | Move two 32-bit floats from the high part of an xmm register
        -- to the low part of another xmm register.
        --
        -- If the format is a vector format, the destination register is treated as the second source.
        -- If the format is FF32 or FF64, the destination register is not treated as a source.
        | MOVHLPS    Format Reg Reg
        | VMOVHLPS   Format Reg Reg Reg
        | MOVLHPS    Format Reg Reg
        | VMOVLHPS   Format Reg Reg Reg
        | UNPCKL     Format Operand Reg
        | VUNPCKL    Format Operand Reg Reg
        | UNPCKH     Format Operand Reg
        | VUNPCKH    Format Operand Reg Reg
        | PUNPCKLQDQ Format Operand Reg
        | PUNPCKLDQ  Format Operand Reg
        | PUNPCKLWD  Format Operand Reg
        | PUNPCKLBW  Format Operand Reg
        | PUNPCKHQDQ Format Operand Reg
        | PUNPCKHDQ  Format Operand Reg
        | PUNPCKHWD  Format Operand Reg
        | PUNPCKHBW  Format Operand Reg
        | PACKUSWB   Format Operand Reg

        -- Shift
        | PSLL       Format Operand Reg
        | PSLLDQ     Format Imm Reg
        | PSRA       Format Operand Reg
        | PSRL       Format Operand Reg
        | PSRLDQ     Format Imm Reg
        | PALIGNR    Format Imm Operand Reg

        -- min/max
        | MINMAX  MinOrMax MinMaxType Format Operand Reg
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
-- See Note [Register usage DSL] in GHC.CmmToAsm.RegUsage.
regUsageOfInstr :: Platform -> Instr -> RegUsage
-- The LOCK prefix does not change the usage of the instruction. It is
-- handled here so that the case below is not recursive.
-- See Note [Register usage DSL] in GHC.CmmToAsm.RegUsage.
regUsageOfInstr platform (LOCK i) = regUsageOfInstr platform i
regUsageOfInstr platform instr
 = runUsage platform $ case instr of

    -- Recall that MOV is always a scalar move instruction, but when the destination
    -- is an XMM register, we make the distinction between:
    --
    --  - a scalar format, meaning that from now on we no longer care about the top bits
    --    of the register, and
    --  - a vector format, meaning that we still care about what's in the high bits.
    --
    -- See Note [Allocated register formats] in GHC.CmmToAsm.Reg.Linear.
    MOV dst_fmt src dst
      -- MOVSS/MOVSD preserve the upper half of vector registers,
      -- but only for reg-2-reg moves
      | VecFormat _ sFmt <- dst_fmt
      , isFloatScalarFormat sFmt
      , OpReg {} <- src
      , OpReg {} <- dst
      -> readOp dst_fmt src <.> modifyOp dst_fmt dst
      -- other MOV instructions zero any remaining upper part of the destination
      -- (largely to avoid partial register stalls)
      | otherwise
      -> readOp dst_fmt src <.> writeOp dst_fmt dst
    MOVD fmt1 fmt2 src dst    ->
      -- NB: MOVD and MOVQ always zero any remaining upper part of destination,
      -- so the destination is "written" not "modified".
      readOp fmt1 src <.> writeOp fmt2 dst
    CMOV _ fmt src dst             -> readOp fmt src <.> modifyReg fmt dst
    MOVZxL fmt src dst             -> readOp fmt src <.> writeOp fmt dst
    MOVSxL fmt src dst             -> readOp fmt src <.> writeOp fmt dst
    LEA    fmt src dst             -> readOp fmt src <.> writeOp fmt dst
    ADD    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    ADC    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    SUB    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    SBB    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    IMUL   fmt src dst             -> readOp fmt src <.> modifyOp fmt dst

    -- Result of IMULB will be in just in %ax
    IMUL2  II8 src                 -> readReg II8 eax <.> readOp II8 src <.> writeReg II16 eax
    -- Result of IMUL for wider values, will be split between %dx/%edx/%rdx and
    -- %ax/%eax/%rax.
    IMUL2  fmt src                 -> readReg fmt eax <.> readOp fmt src <.> writeReg fmt eax <.> writeReg fmt edx

    MUL    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    MUL2   fmt src                 -> readReg fmt eax <.> readOp fmt src <.> writeReg fmt eax <.> writeReg fmt edx
    DIV    fmt op                  -> readReg fmt eax <.> readReg fmt edx <.> readOp fmt op <.> writeReg fmt eax <.> writeReg fmt edx
    IDIV   fmt op                  -> readReg fmt eax <.> readReg fmt edx <.> readOp fmt op <.> writeReg fmt eax <.> writeReg fmt edx
    ADD_CC fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    SUB_CC fmt src dst             -> readOp fmt src <.> modifyOp fmt dst

    AND    fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    VAND   fmt src1 src2 dst
      -> readOp fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst

    OR     fmt src dst             -> readOp fmt src <.> modifyOp fmt dst
    VOR    fmt src1 src2 dst
      -> readOp fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst

    XOR    fmt (OpReg src) (OpReg dst)
      | src == dst
      -> writeReg fmt dst
    XOR    fmt src dst
      -> readOp fmt src <.> modifyOp fmt dst
    VXOR fmt (OpReg src1) src2 dst
      | src1 == src2, src1 == dst
      -> writeReg fmt dst
    VXOR fmt src1 src2 dst
      -> readOp fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst

    NOT    fmt op                  -> modifyOp fmt op
    BSWAP  fmt reg                 -> modifyReg fmt reg
    NEGI   fmt op                  -> modifyOp fmt op
    SHL    fmt imm dst             -> readOp fmt imm <.> modifyOp fmt dst
    SAR    fmt imm dst             -> readOp fmt imm <.> modifyOp fmt dst
    SHR    fmt imm dst             -> readOp fmt imm <.> modifyOp fmt dst
    SHLD   fmt imm dst1 dst2       -> readModifyModify fmt imm dst1 dst2
    SHRD   fmt imm dst1 dst2       -> readModifyModify fmt imm dst1 dst2
    BT     fmt _   src             -> readOp fmt src
    BTR    fmt off dst             -> readOp fmt off <.> modifyOp fmt dst
    BTS    fmt off dst             -> readOp fmt off <.> modifyOp fmt dst
    BTC    fmt off dst             -> readOp fmt off <.> modifyOp fmt dst

    PUSH   fmt op                  -> readOp fmt op
    POP    fmt op                  -> defOp fmt op
    TEST   fmt src dst             -> readOp fmt src <.> readOp fmt dst
    CMP    fmt src dst             -> readOp fmt src <.> readOp fmt dst
    SETCC  _ op                    -> defOp II8 op
    JXX    _ _                     -> mempty
    JXX_GBL _ _                    -> mempty
    JMP     op regs                -> atAddrFmt (\fmt -> readOp fmt op) <.> readRegs regs
    JMP_TBL op _ _ _ _             -> atAddrFmt (\fmt -> readOp fmt op)
    CALL (Left _)  params          -> readRegs params <.> clobbered
    CALL (Right reg) params        -> atAddrFmt (\fmt -> readReg fmt reg) <.> readRegs params <.> clobbered
    CLTD   fmt                     -> readReg fmt eax <.> writeReg fmt edx
    NOP                            -> mempty

    X87Store _fmt  dst             -> readAddr dst

    CVTSS2SD   src dst             -> readReg FF32 src <.> writeReg FF64 dst
    CVTSD2SS   src dst             -> readReg FF64 src <.> writeReg FF32 dst
    CVTTSS2SIQ fmt src dst         -> readOp FF32 src <.> writeReg fmt dst
    CVTTSD2SIQ fmt src dst         -> readOp FF64 src <.> writeReg fmt dst
    CVTSI2SS   fmt src dst         -> readOp fmt src <.> writeReg FF32 dst
    CVTSI2SD   fmt src dst         -> readOp fmt src <.> writeReg FF64 dst
    FDIV fmt     src dst           -> readOp fmt src <.> modifyReg fmt dst
    SQRT fmt src dst               -> readOp fmt src <.> writeReg fmt dst

    FETCHGOT reg                   -> atAddrFmt (\fmt -> writeReg fmt reg)
    FETCHPC  reg                   -> atAddrFmt (\fmt -> writeReg fmt reg)

    COMMENT _                      -> mempty
    LOCATION{}                     -> mempty
    UNWIND{}                       -> mempty
    DELTA   _                      -> mempty
    LDATA{}                        -> mempty
    NEWBLOCK{}                     -> mempty

    POPCNT fmt src dst             -> readOp fmt src <.> writeReg fmt dst
    LZCNT  fmt src dst             -> readOp fmt src <.> writeReg fmt dst
    TZCNT  fmt src dst             -> readOp fmt src <.> writeReg fmt dst
    BSF    fmt src dst             -> readOp fmt src <.> writeReg fmt dst
    BSR    fmt src dst             -> readOp fmt src <.> writeReg fmt dst

    PDEP   fmt src mask dst        -> readOp fmt src <.> readOp fmt mask <.> writeReg fmt dst
    PEXT   fmt src mask dst        -> readOp fmt src <.> readOp fmt mask <.> writeReg fmt dst

    FMA3 fmt _ _ src3 src2 dst     -> readOp fmt src3 <.> readReg fmt src2 <.> modifyReg fmt dst

    -- note: might be a better way to do this
    PREFETCH _  fmt src            -> readOp fmt src
    XADD fmt (OpReg src) (OpAddr ea) -> readAddr ea <.> modifyReg fmt src
    XADD fmt src dst               -> modifyOp fmt src <.> modifyOp fmt dst
    CMPXCHG fmt src dst            -> readModifyModify fmt src dst (OpReg eax)
    XCHG fmt src dst               -> modifyOp fmt src <.> modifyReg fmt dst
    MFENCE                         -> mempty

    -- vector instructions
    VBROADCAST fmt src dst         -> readOp fmt src <.> writeReg fmt dst
    VPBROADCAST sFmt vFmt src dst  -> readOp sFmt src <.> writeReg vFmt dst
    VEXTRACT     fmt _off src dst  -> readReg fmt src <.> writeOp fmt dst
    INSERTPS     fmt (ImmInt off) src dst
      -> readOp fmt src <.> (if doesNotReadDst then writeReg fmt dst else modifyReg fmt dst)
        where
          -- Compute whether the instruction reads the destination register or not.
          -- Immediate bits: ss_dd_zzzz s = src pos, d = dst pos, z = zeroed components.
          doesNotReadDst = and [ testBit off i | i <- [0, 1, 2, 3], i /= pos ]
            -- Check whether the positions in which we are not inserting
            -- are being zeroed.
            where pos = ( off `shiftR` 4 ) .&. 0b11
    INSERTPS fmt _off src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VINSERTPS fmt _imm src2 src1 dst
      -> readOp fmt src2 <.> readReg fmt src1 <.> writeReg fmt dst
    PINSR sFmt vFmt _off src dst
      -> readOp sFmt src <.> modifyReg vFmt dst
    PEXTR sFmt vFmt _off src dst
      -> readReg vFmt src <.> writeOp sFmt dst

    VMOVU        fmt src dst       -> readOp fmt src <.> writeOp fmt dst
    MOVU         fmt src dst       -> readOp fmt src <.> writeOp fmt dst
    MOVL         fmt src dst       -> readOp fmt src <.> modifyOp fmt dst
    MOVH         fmt src dst       -> readOp fmt src <.> modifyOp fmt dst
    MOVDQU       fmt src dst       -> readOp fmt src <.> writeOp fmt dst
    VMOVDQU      fmt src dst       -> readOp fmt src <.> writeOp fmt dst
    VMOV_MERGE   fmt src2 src1 dst -> readReg fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst

    PXOR fmt src dst
      | OpReg src_reg <- src
      , src_reg == dst
      -> writeReg fmt dst
      | otherwise
      -> readOp fmt src <.> modifyReg fmt dst

    VPXOR        fmt s1 s2 dst
      | OpReg s1_reg <- s1
      , s1_reg == s2, s1_reg == dst
      -> writeReg fmt dst
      | otherwise
      -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst

    PAND         fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    PANDN        fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    VPAND        fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    POR          fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    VPOR         fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst

    VADD         fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    VSUB         fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    VMUL         fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    VDIV         fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    PADD         fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    PSUB         fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    PMULL        fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    VPMULL       fmt s1 s2 dst     -> readOp fmt s1 <.> readReg fmt s2 <.> writeReg fmt dst
    PMULUDQ      fmt src dst       -> readOp fmt src <.> modifyReg fmt dst
    PABS         fmt src dst       -> readOp fmt src <.> writeReg fmt dst
    VPABS        fmt src dst       -> readOp fmt src <.> writeReg fmt dst
    VSQRT        fmt src dst       -> readOp fmt src <.> writeReg fmt dst

    PCMPGT       fmt src dst       -> readOp fmt src <.> modifyReg fmt dst

    SHUF fmt _mask src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VSHUF fmt _mask src1 src2 dst
      -> readOp fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst
    PSHUFB fmt mask dst
      -> readOp fmt mask <.> modifyReg fmt dst
    PSHUFLW fmt _mask src dst
      -> readOp fmt src <.> writeReg fmt dst
    PSHUFHW fmt _mask src dst
      -> readOp fmt src <.> writeReg fmt dst
    PSHUFD fmt _mask src dst
      -> readOp fmt src <.> writeReg fmt dst
    VPSHUFD fmt _mask src dst
      -> readOp fmt src <.> writeReg fmt dst
    BLEND fmt _mask src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VBLEND fmt _mask src2 src1 dst
      -> readOp fmt src2 <.> readReg fmt src1 <.> writeReg fmt dst
    PBLENDW fmt _mask src dst
      -> readOp fmt src <.> modifyReg fmt dst

    PSLL   fmt off dst             -> readOp fmt off <.> modifyReg fmt dst
    PSLLDQ fmt _off dst            -> modifyReg fmt dst
    PSRL   fmt off dst             -> readOp fmt off <.> modifyReg fmt dst
    PSRA   fmt off dst             -> readOp fmt off <.> modifyReg fmt dst
    PSRLDQ fmt _off dst            -> modifyReg fmt dst
    PALIGNR fmt _off src dst       -> readOp fmt src <.> modifyReg fmt dst

    MOVHLPS    fmt src dst
      -> case fmt of
           VecFormat {} -> readReg fmt src <.> modifyReg fmt dst
           -- MOVHLPS moves the high 64 bits of src to the low 64 bits of dst,
           -- keeping the high 64 bits of dst intact.
           -- If we only care about the lower 64 bits of the result,
           -- dst is only written to, not read.
           FF64 -> readReg (VecFormat 2 FmtDouble) src <.> writeReg fmt dst
           FF32 -> readReg (VecFormat 4 FmtFloat) src <.> writeReg fmt dst
           _    -> pprPanic "regUsage: invalid format for MOVHLPS" (ppr fmt)
    VMOVHLPS   fmt src2 src1 dst
      -> readReg fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst
    MOVLHPS    fmt src dst
      -> readReg fmt src <.> modifyReg fmt dst
    VMOVLHPS   fmt src2 src1 dst
      -> readReg fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst
    UNPCKL fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VUNPCKL fmt src2 src1 dst
      -> readOp fmt src2 <.> readReg fmt src1 <.> writeReg fmt dst
    UNPCKH fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VUNPCKH fmt src2 src1 dst
      -> readOp fmt src2 <.> readReg fmt src1 <.> writeReg fmt dst
    PUNPCKLQDQ fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKLDQ fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKLWD fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKLBW fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKHQDQ fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKHDQ fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKHWD fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PUNPCKHBW fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    PACKUSWB fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst

    MINMAX _ _ fmt src dst
      -> readOp fmt src <.> modifyReg fmt dst
    VMINMAX _ _ fmt src1 src2 dst
      -> readOp fmt src1 <.> readReg fmt src2 <.> writeReg fmt dst

-- Helpers for regUsageOfInstr. See Note [Register usage DSL] in
-- GHC.CmmToAsm.RegUsage.
--
-- Written: if the operand is a register, it is written. If it is an
-- address, the registers of the address are read.
--
-- Modified: if the operand is a register, it is both read and written. If
-- it is an address, the registers of the address are read.

readOp :: Format -> Operand -> Usage
readOp fmt (OpReg r)   = readReg fmt r
readOp _   (OpImm _)   = mempty
readOp _   (OpAddr ea) = readAddr ea
{-# NOINLINE readOp #-}

writeOp :: Format -> Operand -> Usage
writeOp fmt (OpReg r)   = writeReg fmt r
writeOp _   (OpAddr ea) = readAddr ea
writeOp _   (OpImm _)   = panic "X86.regUsageOfInstr.writeOp: immediate"
{-# NOINLINE writeOp #-}

modifyOp :: Format -> Operand -> Usage
modifyOp fmt (OpReg r)   = modifyReg fmt r
modifyOp _   (OpAddr ea) = readAddr ea
modifyOp _   (OpImm _)   = panic "X86.regUsageOfInstr.modifyOp: immediate"
{-# NOINLINE modifyOp #-}

-- | Registers used to compute an effective address: base, then index.
readAddr :: AddrMode -> Usage
readAddr (ImmAddr _ _) = mempty
readAddr (AddrBaseIndex base index _) = atAddrFmt $ \fmt ->
  let read_base  (EABaseReg r) = readReg fmt r
      read_base  _             = mempty
      read_index (EAIndex r _) = readReg fmt r
      read_index EAIndexNone   = mempty
  in read_base base <.> read_index index
{-# NOINLINE readAddr #-}

-- | Format of an address.
addrFmt :: Platform -> Format
addrFmt platform = archWordFormat (target32Bit platform)

-- | A usage at the format of an address. The format is forced here: it is
-- cheap, and a lazy format becomes a thunk when several items use it.
-- See Note [Register usage DSL implementation] in GHC.CmmToAsm.RegUsage.
atAddrFmt :: (Format -> Usage) -> Usage
atAddrFmt f = withPlatform $ \platform -> let !fmt = addrFmt platform in f fmt
{-# INLINE atAddrFmt #-}

-- | Operand defined by POP and SETCC. A memory operand contributes nothing,
-- not even its address registers (unchanged behaviour; the code generator
-- only uses registers here).
defOp :: Format -> Operand -> Usage
defOp fmt (OpReg r)  = writeReg fmt r
defOp _   (OpAddr _) = mempty
defOp _   (OpImm _)  = panic "X86.regUsageOfInstr.defOp: immediate"

-- | src read, dst modified, reg modified (SHLD, SHRD, CMPXCHG). For a
-- memory dst its address registers come first, then src: this is the order
-- the allocator has always seen.
readModifyModify :: Format -> Operand -> Operand -> Operand -> Usage
readModifyModify fmt src (OpAddr ea) reg = readAddr ea <.> readOp fmt src <.> modifyOp fmt reg
readModifyModify fmt src dst         reg = readOp fmt src <.> modifyOp fmt dst <.> modifyOp fmt reg

-- | Registers clobbered by a call.
clobbered :: Usage
clobbered = withPlatform $ \platform ->
  writeRegs [ RegWithFormat r (fmtOfReg platform r) | r <- callClobberedRegs platform ]
{-# NOINLINE clobbered #-}

-- | Format of a real register whose use has no format of its own.
fmtOfReg :: Platform -> Reg -> Format
fmtOfReg platform r = case targetClassOfReg platform r of
  RcInteger       -> addrFmt platform
  RcFloatOrVector -> FF64

-- | Applies the supplied function to all registers in instructions.
-- Typically used to change virtual registers to real registers.
patchRegsOfInstr :: HasDebugCallStack => Platform -> Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr platform instr env
  = case instr of
    MOV fmt src dst      -> MOV fmt (patchOp src) (patchOp dst)
    MOVD fmt1 fmt2 src dst -> patch2 (MOVD fmt1 fmt2) src dst
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
    VAND fmt src1 src2 dst -> VAND fmt (patchOp src1) (env src2) (env dst)
    VOR fmt src1 src2 dst  -> VOR fmt (patchOp src1) (env src2) (env dst)
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
    BTR  fmt off dst     -> patch2 (BTR fmt) off dst
    BTS  fmt off dst     -> patch2 (BTS fmt) off dst
    BTC  fmt off dst     -> patch2 (BTC fmt) off dst
    TEST fmt src dst     -> patch2 (TEST fmt) src dst
    CMP  fmt src dst     -> patch2 (CMP  fmt) src dst
    PUSH fmt op          -> patch1 (PUSH fmt) op
    POP  fmt op          -> patch1 (POP  fmt) op
    SETCC cond op        -> patch1 (SETCC cond) op
    JMP op regs          -> JMP (patchOp op) regs
    JMP_TBL op ids s tl jl -> JMP_TBL (patchOp op) ids s tl jl

    FMA3 fmt perm var x1 x2 x3 -> patch3 (FMA3 fmt perm var) x1 x2 x3

    -- literally only support storing the top x87 stack value st(0)
    X87Store  fmt  dst     -> X87Store fmt  (lookupAddr dst)

    CVTSS2SD src dst    -> CVTSS2SD (env src) (env dst)
    CVTSD2SS src dst    -> CVTSD2SS (env src) (env dst)
    CVTTSS2SIQ fmt src dst -> CVTTSS2SIQ fmt (patchOp src) (env dst)
    CVTTSD2SIQ fmt src dst -> CVTTSD2SIQ fmt (patchOp src) (env dst)
    CVTSI2SS fmt src dst -> CVTSI2SS fmt (patchOp src) (env dst)
    CVTSI2SD fmt src dst -> CVTSI2SD fmt (patchOp src) (env dst)
    FDIV fmt src dst     -> FDIV fmt (patchOp src) (env dst)
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
    VPBROADCAST  fmt1 fmt2 src dst
      -> VPBROADCAST fmt1 fmt2 (patchOp src) (env dst)
    VEXTRACT     fmt off src dst
      -> VEXTRACT fmt off (env src) (patchOp dst)
    INSERTPS    fmt off src dst
      -> INSERTPS fmt off (patchOp src) (env dst)
    VINSERTPS   fmt off src2 src1 dst
      -> VINSERTPS fmt off (patchOp src2) (env src1) (env dst)
    PINSR       fmt1 fmt2 off src dst
      -> PINSR fmt1 fmt2 off (patchOp src) (env dst)
    PEXTR       fmt1 fmt2 off src dst
      -> PEXTR fmt1 fmt2 off (env src) (patchOp dst)

    VMOVU      fmt src dst   -> VMOVU fmt (patchOp src) (patchOp dst)
    MOVU       fmt src dst   -> MOVU  fmt (patchOp src) (patchOp dst)
    MOVL       fmt src dst   -> MOVL  fmt (patchOp src) (patchOp dst)
    MOVH       fmt src dst   -> MOVH  fmt (patchOp src) (patchOp dst)
    MOVDQU     fmt src dst   -> MOVDQU  fmt (patchOp src) (patchOp dst)
    VMOVDQU    fmt src dst   -> VMOVDQU fmt (patchOp src) (patchOp dst)
    VMOV_MERGE fmt src2 src1 dst -> VMOV_MERGE fmt (env src2) (env src1) (env dst)

    PXOR       fmt src dst   -> PXOR  fmt (patchOp src) (env dst)
    VPXOR      fmt s1 s2 dst -> VPXOR fmt (patchOp s1) (env s2) (env dst)
    PAND       fmt src dst   -> PAND  fmt (patchOp src) (env dst)
    VPAND      fmt s1 s2 dst -> VPAND fmt (patchOp s1) (env s2) (env dst)
    PANDN      fmt src dst   -> PANDN fmt (patchOp src) (env dst)
    POR        fmt src dst   -> POR   fmt (patchOp src) (env dst)
    VPOR       fmt s1 s2 dst -> VPOR  fmt (patchOp s1) (env s2) (env dst)

    VADD       fmt s1 s2 dst -> VADD fmt (patchOp s1) (env s2) (env dst)
    VSUB       fmt s1 s2 dst -> VSUB fmt (patchOp s1) (env s2) (env dst)
    VMUL       fmt s1 s2 dst -> VMUL fmt (patchOp s1) (env s2) (env dst)
    VDIV       fmt s1 s2 dst -> VDIV fmt (patchOp s1) (env s2) (env dst)
    PADD       fmt src dst   -> PADD fmt (patchOp src) (env dst)
    PSUB       fmt src dst   -> PSUB fmt (patchOp src) (env dst)
    PMULL      fmt src dst   -> PMULL fmt (patchOp src) (env dst)
    VPMULL     fmt s1 s2 dst -> VPMULL fmt (patchOp s1) (env s2) (env dst)
    PMULUDQ    fmt src dst   -> PMULUDQ fmt (patchOp src) (env dst)
    PABS       fmt src dst   -> PABS fmt (patchOp src) (env dst)
    VPABS      fmt src dst   -> VPABS fmt (patchOp src) (env dst)
    VSQRT      fmt src dst   -> VSQRT fmt (patchOp src) (env dst)

    PCMPGT     fmt src dst   -> PCMPGT fmt (patchOp src) (env dst)

    SHUF      fmt off src dst
      -> SHUF fmt off (patchOp src) (env dst)
    VSHUF      fmt off src1 src2 dst
      -> VSHUF fmt off (patchOp src1) (env src2) (env dst)
    PSHUFB       fmt mask dst
      -> PSHUFB fmt (patchOp mask) (env dst)
    PSHUFLW      fmt off src dst
      -> PSHUFLW fmt off (patchOp src) (env dst)
    PSHUFHW      fmt off src dst
      -> PSHUFHW fmt off (patchOp src) (env dst)
    PSHUFD       fmt off src dst
      -> PSHUFD  fmt off (patchOp src) (env dst)
    VPSHUFD      fmt off src dst
      -> VPSHUFD fmt off (patchOp src) (env dst)
    BLEND        fmt mask src dst
      -> BLEND   fmt mask (patchOp src) (env dst)
    VBLEND       fmt mask src2 src1 dst
      -> VBLEND  fmt mask (patchOp src2) (env src1) (env dst)
    PBLENDW      fmt mask src dst
      -> PBLENDW fmt mask (patchOp src) (env dst)

    PSLL         fmt off dst
      -> PSLL    fmt (patchOp off) (env dst)
    PSRA         fmt off dst
      -> PSRA    fmt (patchOp off) (env dst)
    PSLLDQ       fmt off dst
      -> PSLLDQ  fmt off (env dst)
    PSRL         fmt off dst
      -> PSRL    fmt (patchOp off) (env dst)
    PSRLDQ       fmt off dst
      -> PSRLDQ  fmt off (env dst)
    PALIGNR      fmt off src dst
      -> PALIGNR fmt off (patchOp src) (env dst)

    MOVHLPS    fmt src dst
      -> MOVHLPS fmt (env src) (env dst)
    VMOVHLPS   fmt src2 src1 dst
      -> VMOVHLPS fmt (env src2) (env src1) (env dst)
    MOVLHPS    fmt src dst
      -> MOVLHPS fmt (env src) (env dst)
    VMOVLHPS   fmt src2 src1 dst
      -> VMOVLHPS fmt (env src2) (env src1) (env dst)
    UNPCKL fmt src dst
      -> UNPCKL fmt (patchOp src) (env dst)
    VUNPCKL fmt src2 src1 dst
      -> VUNPCKL fmt (patchOp src2) (env src1) (env dst)
    UNPCKH fmt src dst
      -> UNPCKH fmt (patchOp src) (env dst)
    VUNPCKH fmt src2 src1 dst
      -> VUNPCKH fmt (patchOp src2) (env src1) (env dst)
    PUNPCKLQDQ fmt src dst
      -> PUNPCKLQDQ fmt (patchOp src) (env dst)
    PUNPCKLDQ fmt src dst
      -> PUNPCKLDQ fmt (patchOp src) (env dst)
    PUNPCKLWD fmt src dst
      -> PUNPCKLWD fmt (patchOp src) (env dst)
    PUNPCKLBW fmt src dst
      -> PUNPCKLBW fmt (patchOp src) (env dst)
    PUNPCKHQDQ fmt src dst
      -> PUNPCKHQDQ fmt (patchOp src) (env dst)
    PUNPCKHDQ fmt src dst
      -> PUNPCKHDQ fmt (patchOp src) (env dst)
    PUNPCKHWD fmt src dst
      -> PUNPCKHWD fmt (patchOp src) (env dst)
    PUNPCKHBW fmt src dst
      -> PUNPCKHBW fmt (patchOp src) (env dst)
    PACKUSWB fmt src dst
      -> PACKUSWB fmt (patchOp src) (env dst)

    MINMAX minMax ty fmt src dst
      -> MINMAX minMax ty fmt (patchOp src) (env dst)
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
    JXX _ target            -> bid == target
    JMP_TBL _ targets _ _ _ -> all isTargetBid targets
    _                       -> False
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
        JXX _ id            -> [id]
        JMP_TBL _ ids _ _ _ -> [id | Just (DestBlockId id) <- ids]
        _                   -> []


patchJumpInstr
        :: Instr -> (BlockId -> BlockId) -> Instr

patchJumpInstr insn patchF
  = case insn of
        JXX cc id       -> JXX cc (patchF id)
        JMP_TBL op ids section table_lbl rel_lbl
          -> JMP_TBL op (map (fmap (patchJumpDest patchF)) ids) section table_lbl rel_lbl
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
    feats   = ncgX86Features config
    avx     = x86AvxEnabled feats
    avx2    = x86Avx2Enabled feats
    avx512f = x86Avx512FEnabled feats
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
    shortcutJump' fn _ (JMP_TBL addr blocks section table_lbl rel_lbl) =
        let updateBlock (Just (DestBlockId bid))  =
                case fn bid of
                    Nothing   -> Just (DestBlockId bid )
                    Just dest -> Just dest
            updateBlock dest = dest
            blocks' = map updateBlock blocks
        in  JMP_TBL addr blocks' section table_lbl rel_lbl
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
