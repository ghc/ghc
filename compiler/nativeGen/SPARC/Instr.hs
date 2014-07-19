{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------
#include "HsVersions.h"
#include "nativeGen/NCG.h"

module SPARC.Instr (
        RI(..),
        riZero,

        fpRelEA,
        moveSp,

        isUnconditionalJump,

        Instr(..),
        maxSpillSlots
)

where

import SPARC.Stack
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Cond
import SPARC.Regs
import SPARC.Base
import TargetReg
import Instruction
import RegClass
import Reg
import Size

import CLabel
import CodeGen.Platform
import BlockId
import DynFlags
import Cmm
import FastString
import FastBool
import Outputable
import Platform


-- | Register or immediate
data RI
        = RIReg Reg
        | RIImm Imm

-- | Check if a RI represents a zero value.
--      - a literal zero
--      - register %g0, which is always zero.
--
riZero :: RI -> Bool
riZero (RIImm (ImmInt 0))                       = True
riZero (RIImm (ImmInteger 0))                   = True
riZero (RIReg (RegReal (RealRegSingle 0)))      = True
riZero _                                        = False


-- | Calculate the effective address which would be used by the
--      corresponding fpRel sequence.
fpRelEA :: Int -> Reg -> Instr
fpRelEA n dst
   = ADD False False fp (RIImm (ImmInt (n * wordLength))) dst


-- | Code to shift the stack pointer by n words.
moveSp :: Int -> Instr
moveSp n
   = ADD False False sp (RIImm (ImmInt (n * wordLength))) sp

-- | An instruction that will cause the one after it never to be exectuted
isUnconditionalJump :: Instr -> Bool
isUnconditionalJump ii
 = case ii of
        CALL{}          -> True
        JMP{}           -> True
        JMP_TBL{}       -> True
        BI ALWAYS _ _   -> True
        BF ALWAYS _ _   -> True
        _               -> False


-- | instance for sparc instruction set
instance Instruction Instr where
        regUsageOfInstr         = sparc_regUsageOfInstr
        patchRegsOfInstr        = sparc_patchRegsOfInstr
        isJumpishInstr          = sparc_isJumpishInstr
        jumpDestsOfInstr        = sparc_jumpDestsOfInstr
        patchJumpInstr          = sparc_patchJumpInstr
        mkSpillInstr            = sparc_mkSpillInstr
        mkLoadInstr             = sparc_mkLoadInstr
        takeDeltaInstr          = sparc_takeDeltaInstr
        isMetaInstr             = sparc_isMetaInstr
        mkRegRegMoveInstr       = sparc_mkRegRegMoveInstr
        takeRegRegMoveInstr     = sparc_takeRegRegMoveInstr
        mkJumpInstr             = sparc_mkJumpInstr
        mkStackAllocInstr       = panic "no sparc_mkStackAllocInstr"
        mkStackDeallocInstr     = panic "no sparc_mkStackDeallocInstr"


-- | SPARC instruction set.
--      Not complete. This is only the ones we need.
--
data Instr

        -- meta ops --------------------------------------------------
        -- comment pseudo-op
        = COMMENT FastString

        -- some static data spat out during code generation.
        -- Will be extracted before pretty-printing.
        | LDATA   Section CmmStatics

        -- Start a new basic block.  Useful during codegen, removed later.
        -- Preceding instruction should be a jump, as per the invariants
        -- for a BasicBlock (see Cmm).
        | NEWBLOCK BlockId

        -- specify current stack offset for benefit of subsequent passes.
        | DELTA   Int

        -- real instrs -----------------------------------------------
        -- Loads and stores.
        | LD            Size AddrMode Reg               -- size, src, dst
        | ST            Size Reg AddrMode               -- size, src, dst

        -- Int Arithmetic.
        --      x:   add/sub with carry bit.
        --              In SPARC V9 addx and friends were renamed addc.
        --
        --      cc:  modify condition codes
        --
        | ADD           Bool Bool Reg RI Reg            -- x?, cc?, src1, src2, dst
        | SUB           Bool Bool Reg RI Reg            -- x?, cc?, src1, src2, dst

        | UMUL          Bool Reg RI Reg                 --     cc?, src1, src2, dst
        | SMUL          Bool Reg RI Reg                 --     cc?, src1, src2, dst


        -- The SPARC divide instructions perform 64bit by 32bit division
        --   The Y register is xored into the first operand.

        --   On _some implementations_ the Y register is overwritten by
        --   the remainder, so we have to make sure it is 0 each time.

        --   dst <- ((Y `shiftL` 32) `or` src1) `div` src2
        | UDIV          Bool Reg RI Reg                 --     cc?, src1, src2, dst
        | SDIV          Bool Reg RI Reg                 --     cc?, src1, src2, dst

        | RDY           Reg                             -- move contents of Y register to reg
        | WRY           Reg  Reg                        -- Y <- src1 `xor` src2

        -- Logic operations.
        | AND           Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | ANDN          Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | OR            Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | ORN           Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | XOR           Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | XNOR          Bool Reg RI Reg                 -- cc?, src1, src2, dst
        | SLL           Reg RI Reg                      -- src1, src2, dst
        | SRL           Reg RI Reg                      -- src1, src2, dst
        | SRA           Reg RI Reg                      -- src1, src2, dst

        -- Load immediates.
        | SETHI         Imm Reg                         -- src, dst

        -- Do nothing.
        -- Implemented by the assembler as SETHI 0, %g0, but worth an alias
        | NOP

        -- Float Arithmetic.
        -- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single
        -- instructions right up until we spit them out.
        --
        | FABS          Size Reg Reg                    -- src dst
        | FADD          Size Reg Reg Reg                -- src1, src2, dst
        | FCMP          Bool Size Reg Reg               -- exception?, src1, src2, dst
        | FDIV          Size Reg Reg Reg                -- src1, src2, dst
        | FMOV          Size Reg Reg                    -- src, dst
        | FMUL          Size Reg Reg Reg                -- src1, src2, dst
        | FNEG          Size Reg Reg                    -- src, dst
        | FSQRT         Size Reg Reg                    -- src, dst
        | FSUB          Size Reg Reg Reg                -- src1, src2, dst
        | FxTOy         Size Size Reg Reg               -- src, dst

        -- Jumping around.
        | BI            Cond Bool BlockId               -- cond, annul?, target
        | BF            Cond Bool BlockId               -- cond, annul?, target

        | JMP           AddrMode                        -- target

        -- With a tabled jump we know all the possible destinations.
        -- We also need this info so we can work out what regs are live across the jump.
        --
        | JMP_TBL       AddrMode [Maybe BlockId] CLabel

        | CALL          (Either Imm Reg) Int Bool       -- target, args, terminal


-- | regUsage returns the sets of src and destination registers used
--      by a particular instruction.  Machine registers that are
--      pre-allocated to stgRegs are filtered out, because they are
--      uninteresting from a register allocation standpoint.  (We wouldn't
--      want them to end up on the free list!)  As far as we are concerned,
--      the fixed registers simply don't exist (for allocation purposes,
--      anyway).

--      regUsage doesn't need to do any trickery for jumps and such.  Just
--      state precisely the regs read and written by that insn.  The
--      consequences of control flow transfers, as far as register
--      allocation goes, are taken care of by the register allocator.
--
sparc_regUsageOfInstr :: Platform -> Instr -> RegUsage
sparc_regUsageOfInstr platform instr
 = case instr of
    LD    _ addr reg            -> usage (regAddr addr,         [reg])
    ST    _ reg addr            -> usage (reg : regAddr addr,   [])
    ADD   _ _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SUB   _ _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    UMUL    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SMUL    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    UDIV    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SDIV    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    RDY       rd                -> usage ([],                   [rd])
    WRY       r1 r2             -> usage ([r1, r2],             [])
    AND     _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    ANDN    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    OR      _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    ORN     _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    XOR     _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    XNOR    _ r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SLL       r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SRL       r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SRA       r1 ar r2          -> usage (r1 : regRI ar,        [r2])
    SETHI   _ reg               -> usage ([],                   [reg])
    FABS    _ r1 r2             -> usage ([r1],                 [r2])
    FADD    _ r1 r2 r3          -> usage ([r1, r2],             [r3])
    FCMP    _ _  r1 r2          -> usage ([r1, r2],             [])
    FDIV    _ r1 r2 r3          -> usage ([r1, r2],             [r3])
    FMOV    _ r1 r2             -> usage ([r1],                 [r2])
    FMUL    _ r1 r2 r3          -> usage ([r1, r2],             [r3])
    FNEG    _ r1 r2             -> usage ([r1],                 [r2])
    FSQRT   _ r1 r2             -> usage ([r1],                 [r2])
    FSUB    _ r1 r2 r3          -> usage ([r1, r2],             [r3])
    FxTOy   _ _  r1 r2          -> usage ([r1],                 [r2])

    JMP     addr                -> usage (regAddr addr, [])
    JMP_TBL addr _ _            -> usage (regAddr addr, [])

    CALL  (Left _  )  _ True    -> noUsage
    CALL  (Left _  )  n False   -> usage (argRegs n, callClobberedRegs)
    CALL  (Right reg) _ True    -> usage ([reg], [])
    CALL  (Right reg) n False   -> usage (reg : (argRegs n), callClobberedRegs)
    _                           -> noUsage

  where
    usage (src, dst)
     = RU (filter (interesting platform) src)
          (filter (interesting platform) dst)

    regAddr (AddrRegReg r1 r2)  = [r1, r2]
    regAddr (AddrRegImm r1 _)   = [r1]

    regRI (RIReg r)             = [r]
    regRI  _                    = []


-- | Interesting regs are virtuals, or ones that are allocatable
--      by the register allocator.
interesting :: Platform -> Reg -> Bool
interesting platform reg
 = case reg of
        RegVirtual _                    -> True
        RegReal (RealRegSingle r1)      -> isFastTrue (freeReg platform r1)
        RegReal (RealRegPair r1 _)      -> isFastTrue (freeReg platform r1)



-- | Apply a given mapping to tall the register references in this instruction.
sparc_patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
sparc_patchRegsOfInstr instr env = case instr of
    LD    sz addr reg           -> LD sz (fixAddr addr) (env reg)
    ST    sz reg addr           -> ST sz (env reg) (fixAddr addr)

    ADD   x cc r1 ar r2         -> ADD   x cc  (env r1) (fixRI ar) (env r2)
    SUB   x cc r1 ar r2         -> SUB   x cc  (env r1) (fixRI ar) (env r2)
    UMUL    cc r1 ar r2         -> UMUL    cc  (env r1) (fixRI ar) (env r2)
    SMUL    cc r1 ar r2         -> SMUL    cc  (env r1) (fixRI ar) (env r2)
    UDIV    cc r1 ar r2         -> UDIV    cc  (env r1) (fixRI ar) (env r2)
    SDIV    cc r1 ar r2         -> SDIV    cc  (env r1) (fixRI ar) (env r2)
    RDY   rd                    -> RDY         (env rd)
    WRY   r1 r2                 -> WRY         (env r1) (env r2)
    AND   b r1 ar r2            -> AND   b     (env r1) (fixRI ar) (env r2)
    ANDN  b r1 ar r2            -> ANDN  b     (env r1) (fixRI ar) (env r2)
    OR    b r1 ar r2            -> OR    b     (env r1) (fixRI ar) (env r2)
    ORN   b r1 ar r2            -> ORN   b     (env r1) (fixRI ar) (env r2)
    XOR   b r1 ar r2            -> XOR   b     (env r1) (fixRI ar) (env r2)
    XNOR  b r1 ar r2            -> XNOR  b     (env r1) (fixRI ar) (env r2)
    SLL   r1 ar r2              -> SLL         (env r1) (fixRI ar) (env r2)
    SRL   r1 ar r2              -> SRL         (env r1) (fixRI ar) (env r2)
    SRA   r1 ar r2              -> SRA         (env r1) (fixRI ar) (env r2)

    SETHI imm reg               -> SETHI imm (env reg)

    FABS  s r1 r2               -> FABS    s   (env r1) (env r2)
    FADD  s r1 r2 r3            -> FADD    s   (env r1) (env r2) (env r3)
    FCMP  e s r1 r2             -> FCMP e  s   (env r1) (env r2)
    FDIV  s r1 r2 r3            -> FDIV    s   (env r1) (env r2) (env r3)
    FMOV  s r1 r2               -> FMOV    s   (env r1) (env r2)
    FMUL  s r1 r2 r3            -> FMUL    s   (env r1) (env r2) (env r3)
    FNEG  s r1 r2               -> FNEG    s   (env r1) (env r2)
    FSQRT s r1 r2               -> FSQRT   s   (env r1) (env r2)
    FSUB  s r1 r2 r3            -> FSUB    s   (env r1) (env r2) (env r3)
    FxTOy s1 s2 r1 r2           -> FxTOy s1 s2 (env r1) (env r2)

    JMP     addr                -> JMP     (fixAddr addr)
    JMP_TBL addr ids l          -> JMP_TBL (fixAddr addr) ids l

    CALL  (Left i) n t          -> CALL (Left i) n t
    CALL  (Right r) n t         -> CALL (Right (env r)) n t
    _                           -> instr

  where
    fixAddr (AddrRegReg r1 r2)  = AddrRegReg   (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)   = AddrRegImm   (env r1) i

    fixRI (RIReg r)             = RIReg (env r)
    fixRI other                 = other


--------------------------------------------------------------------------------
sparc_isJumpishInstr :: Instr -> Bool
sparc_isJumpishInstr instr
 = case instr of
        BI{}            -> True
        BF{}            -> True
        JMP{}           -> True
        JMP_TBL{}       -> True
        CALL{}          -> True
        _               -> False

sparc_jumpDestsOfInstr :: Instr -> [BlockId]
sparc_jumpDestsOfInstr insn
  = case insn of
        BI   _ _ id     -> [id]
        BF   _ _ id     -> [id]
        JMP_TBL _ ids _ -> [id | Just id <- ids]
        _               -> []


sparc_patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
sparc_patchJumpInstr insn patchF
  = case insn of
        BI cc annul id  -> BI cc annul (patchF id)
        BF cc annul id  -> BF cc annul (patchF id)
        JMP_TBL n ids l -> JMP_TBL n (map (fmap patchF) ids) l
        _               -> insn


--------------------------------------------------------------------------------
-- | Make a spill instruction.
--      On SPARC we spill below frame pointer leaving 2 words/spill
sparc_mkSpillInstr
    :: DynFlags
    -> Reg      -- ^ register to spill
    -> Int      -- ^ current stack delta
    -> Int      -- ^ spill slot to use
    -> Instr

sparc_mkSpillInstr dflags reg _ slot
 = let  platform = targetPlatform dflags
        off      = spillSlotToOffset dflags slot
        off_w    = 1 + (off `div` 4)
        sz      = case targetClassOfReg platform reg of
                        RcInteger -> II32
                        RcFloat   -> FF32
                        RcDouble  -> FF64
                        _         -> panic "sparc_mkSpillInstr"

    in ST sz reg (fpRel (negate off_w))


-- | Make a spill reload instruction.
sparc_mkLoadInstr
    :: DynFlags
    -> Reg      -- ^ register to load into
    -> Int      -- ^ current stack delta
    -> Int      -- ^ spill slot to use
    -> Instr

sparc_mkLoadInstr dflags reg _ slot
  = let platform = targetPlatform dflags
        off      = spillSlotToOffset dflags slot
        off_w   = 1 + (off `div` 4)
        sz      = case targetClassOfReg platform reg of
                        RcInteger -> II32
                        RcFloat   -> FF32
                        RcDouble  -> FF64
                        _         -> panic "sparc_mkLoadInstr"

        in LD sz (fpRel (- off_w)) reg


--------------------------------------------------------------------------------
-- | See if this instruction is telling us the current C stack delta
sparc_takeDeltaInstr
        :: Instr
        -> Maybe Int

sparc_takeDeltaInstr instr
 = case instr of
        DELTA i         -> Just i
        _               -> Nothing


sparc_isMetaInstr
        :: Instr
        -> Bool

sparc_isMetaInstr instr
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
sparc_mkRegRegMoveInstr
    :: Platform
    -> Reg
    -> Reg
    -> Instr

sparc_mkRegRegMoveInstr platform src dst
        | srcClass      <- targetClassOfReg platform src
        , dstClass      <- targetClassOfReg platform dst
        , srcClass == dstClass
        = case srcClass of
                RcInteger -> ADD  False False src (RIReg g0) dst
                RcDouble  -> FMOV FF64 src dst
                RcFloat   -> FMOV FF32 src dst
                _         -> panic "sparc_mkRegRegMoveInstr"

        | otherwise
        = panic "SPARC.Instr.mkRegRegMoveInstr: classes of src and dest not the same"


-- | Check whether an instruction represents a reg-reg move.
--      The register allocator attempts to eliminate reg->reg moves whenever it can,
--      by assigning the src and dest temporaries to the same real register.
--
sparc_takeRegRegMoveInstr :: Instr -> Maybe (Reg,Reg)
sparc_takeRegRegMoveInstr instr
 = case instr of
        ADD False False src (RIReg src2) dst
         | g0 == src2           -> Just (src, dst)

        FMOV FF64 src dst       -> Just (src, dst)
        FMOV FF32  src dst      -> Just (src, dst)
        _                       -> Nothing


-- | Make an unconditional branch instruction.
sparc_mkJumpInstr
        :: BlockId
        -> [Instr]

sparc_mkJumpInstr id
 =       [BI ALWAYS False id
        , NOP]                  -- fill the branch delay slot.
