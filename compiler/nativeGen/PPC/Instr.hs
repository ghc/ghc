-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

#include "HsVersions.h"
#include "nativeGen/NCG.h"

module PPC.Instr (
    archWordSize,
    RI(..),
    Instr(..),
    maxSpillSlots,
    allocMoreStack,
    makeFarBranches
)

where

import PPC.Regs
import PPC.Cond
import Instruction
import Size
import TargetReg
import RegClass
import Reg

import CodeGen.Platform
import BlockId
import DynFlags
import Cmm
import CmmInfo
import FastString
import CLabel
import Outputable
import Platform
import FastBool
import UniqFM (listToUFM, lookupUFM)
import UniqSupply

--------------------------------------------------------------------------------
-- Size of a PPC memory address, in bytes.
--
archWordSize :: Size
archWordSize = II32


-- | Instruction instance for powerpc
instance Instruction Instr where
        regUsageOfInstr         = ppc_regUsageOfInstr
        patchRegsOfInstr        = ppc_patchRegsOfInstr
        isJumpishInstr          = ppc_isJumpishInstr
        jumpDestsOfInstr        = ppc_jumpDestsOfInstr
        patchJumpInstr          = ppc_patchJumpInstr
        mkSpillInstr            = ppc_mkSpillInstr
        mkLoadInstr             = ppc_mkLoadInstr
        takeDeltaInstr          = ppc_takeDeltaInstr
        isMetaInstr             = ppc_isMetaInstr
        mkRegRegMoveInstr _     = ppc_mkRegRegMoveInstr
        takeRegRegMoveInstr     = ppc_takeRegRegMoveInstr
        mkJumpInstr             = ppc_mkJumpInstr
        mkStackAllocInstr       = ppc_mkStackAllocInstr
        mkStackDeallocInstr     = ppc_mkStackDeallocInstr


ppc_mkStackAllocInstr :: Platform -> Int -> Instr
ppc_mkStackAllocInstr platform amount
  = case platformArch platform of
      ArchPPC -> -- SUB II32 (OpImm (ImmInt amount)) (OpReg esp)
                 ADD sp sp (RIImm (ImmInt (-amount)))
      arch -> panic $ "ppc_mkStackAllocInstr " ++ show arch

ppc_mkStackDeallocInstr :: Platform -> Int -> Instr
ppc_mkStackDeallocInstr platform amount
  = case platformArch platform of
      ArchPPC -> -- ADD II32 (OpImm (ImmInt amount)) (OpReg esp)
                 ADD sp sp (RIImm (ImmInt amount))
      arch -> panic $ "ppc_mkStackDeallocInstr " ++ show arch

allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics PPC.Instr.Instr
  -> UniqSM (NatCmmDecl statics PPC.Instr.Instr)

allocMoreStack _ _ top@(CmmData _ _) = return top
allocMoreStack platform amount (CmmProc info lbl live (ListGraph code)) =
        return (CmmProc info lbl live (ListGraph (map insert_stack_insns code)))
  where
    alloc   = mkStackAllocInstr platform amount
    dealloc = mkStackDeallocInstr platform amount

    is_entry_point id = id `mapMember` info

    insert_stack_insns (BasicBlock id insns)
       | is_entry_point id  = BasicBlock id (alloc : block')
       | otherwise          = BasicBlock id block'
       where
         block' = insertBeforeNonlocalTransfers dealloc insns

insertBeforeNonlocalTransfers :: Instr -> [Instr] -> [Instr]
insertBeforeNonlocalTransfers insert insns
     = foldr p [] insns
     where p insn r = case insn of
                        BCC    _ _  -> insert : insn : r
                        BCCFAR _ _  -> insert : insn : r
                        JMP    _    -> insert : insn : r
                        MTCTR  _    -> insert : insn : r
                        BCTR   _ _  -> insert : insn : r
                        BL     _ _  -> insert : insn : r
                        BCTRL  _    -> insert : insn : r
                        _           -> insn : r

-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

-- Register or immediate
data RI
    = RIReg Reg
    | RIImm Imm

data Instr
    -- comment pseudo-op
    = COMMENT FastString

    -- some static data spat out during code
    -- generation.  Will be extracted before
    -- pretty-printing.
    | LDATA   Section CmmStatics

    -- start a new basic block.  Useful during
    -- codegen, removed later.  Preceding
    -- instruction should be a jump, as per the
    -- invariants for a BasicBlock (see Cmm).
    | NEWBLOCK BlockId

    -- specify current stack offset for
    -- benefit of subsequent passes
    | DELTA   Int

    -- Loads and stores.
    | LD      Size Reg AddrMode     -- Load size, dst, src
    | LA      Size Reg AddrMode     -- Load arithmetic size, dst, src
    | ST      Size Reg AddrMode     -- Store size, src, dst
    | STU     Size Reg AddrMode     -- Store with Update size, src, dst
    | LIS     Reg Imm               -- Load Immediate Shifted dst, src
    | LI      Reg Imm               -- Load Immediate dst, src
    | MR      Reg Reg               -- Move Register dst, src -- also for fmr

    | CMP     Size Reg RI           -- size, src1, src2
    | CMPL    Size Reg RI           -- size, src1, src2

    | BCC     Cond BlockId
    | BCCFAR  Cond BlockId
    | JMP     CLabel                -- same as branch,
                                    -- but with CLabel instead of block ID
    | MTCTR   Reg
    | BCTR    [Maybe BlockId] (Maybe CLabel) -- with list of local destinations, and jump table location if necessary
    | BL      CLabel [Reg]          -- with list of argument regs
    | BCTRL   [Reg]

    | ADD     Reg Reg RI            -- dst, src1, src2
    | ADDC    Reg Reg Reg           -- (carrying) dst, src1, src2
    | ADDE    Reg Reg Reg           -- (extend) dst, src1, src2
    | ADDIS   Reg Reg Imm           -- Add Immediate Shifted dst, src1, src2
    | SUBF    Reg Reg Reg           -- dst, src1, src2 ; dst = src2 - src1
    | MULLW   Reg Reg RI
    | DIVW    Reg Reg Reg
    | DIVWU   Reg Reg Reg

    | MULLW_MayOflo Reg Reg Reg
                                    -- dst = 1 if src1 * src2 overflows
                                    -- pseudo-instruction; pretty-printed as:
                                    -- mullwo. dst, src1, src2
                                    -- mfxer dst
                                    -- rlwinm dst, dst, 2, 31,31

    | AND     Reg Reg RI            -- dst, src1, src2
    | OR      Reg Reg RI            -- dst, src1, src2
    | XOR     Reg Reg RI            -- dst, src1, src2
    | XORIS   Reg Reg Imm           -- XOR Immediate Shifted dst, src1, src2

    | EXTS    Size Reg Reg

    | NEG     Reg Reg
    | NOT     Reg Reg

    | SLW     Reg Reg RI            -- shift left word
    | SRW     Reg Reg RI            -- shift right word
    | SRAW    Reg Reg RI            -- shift right arithmetic word

    | RLWINM  Reg Reg Int Int Int   -- Rotate Left Word Immediate then AND with Mask

    | FADD    Size Reg Reg Reg
    | FSUB    Size Reg Reg Reg
    | FMUL    Size Reg Reg Reg
    | FDIV    Size Reg Reg Reg
    | FNEG    Reg Reg           -- negate is the same for single and double prec.

    | FCMP    Reg Reg

    | FCTIWZ  Reg Reg           -- convert to integer word
    | FRSP    Reg Reg           -- reduce to single precision
                                -- (but destination is a FP register)

    | CRNOR   Int Int Int       -- condition register nor
    | MFCR    Reg               -- move from condition register

    | MFLR    Reg               -- move from link register
    | FETCHPC Reg               -- pseudo-instruction:
                                -- bcl to next insn, mflr reg

    | LWSYNC                    -- memory barrier


-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
ppc_regUsageOfInstr :: Platform -> Instr -> RegUsage
ppc_regUsageOfInstr platform instr
 = case instr of
    LD      _ reg addr       -> usage (regAddr addr, [reg])
    LA      _ reg addr       -> usage (regAddr addr, [reg])
    ST      _ reg addr       -> usage (reg : regAddr addr, [])
    STU     _ reg addr       -> usage (reg : regAddr addr, [])
    LIS     reg _            -> usage ([], [reg])
    LI      reg _            -> usage ([], [reg])
    MR      reg1 reg2        -> usage ([reg2], [reg1])
    CMP     _ reg ri         -> usage (reg : regRI ri,[])
    CMPL    _ reg ri         -> usage (reg : regRI ri,[])
    BCC     _ _              -> noUsage
    BCCFAR  _ _              -> noUsage
    MTCTR   reg              -> usage ([reg],[])
    BCTR    _ _              -> noUsage
    BL      _ params         -> usage (params, callClobberedRegs platform)
    BCTRL   params           -> usage (params, callClobberedRegs platform)

    ADD     reg1 reg2 ri     -> usage (reg2 : regRI ri, [reg1])
    ADDC    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    ADDE    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    ADDIS   reg1 reg2 _      -> usage ([reg2], [reg1])
    SUBF    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    MULLW   reg1 reg2 ri     -> usage (reg2 : regRI ri, [reg1])
    DIVW    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    DIVWU   reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])

    MULLW_MayOflo reg1 reg2 reg3
                            -> usage ([reg2,reg3], [reg1])
    AND     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    OR      reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    XOR     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    XORIS   reg1 reg2 _     -> usage ([reg2], [reg1])
    EXTS    _  reg1 reg2    -> usage ([reg2], [reg1])
    NEG     reg1 reg2       -> usage ([reg2], [reg1])
    NOT     reg1 reg2       -> usage ([reg2], [reg1])
    SLW     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    SRW     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    SRAW    reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    RLWINM  reg1 reg2 _ _ _ -> usage ([reg2], [reg1])

    FADD    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FSUB    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FMUL    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FDIV    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FNEG    r1 r2           -> usage ([r2], [r1])
    FCMP    r1 r2           -> usage ([r1,r2], [])
    FCTIWZ  r1 r2           -> usage ([r2], [r1])
    FRSP    r1 r2           -> usage ([r2], [r1])
    MFCR    reg             -> usage ([], [reg])
    MFLR    reg             -> usage ([], [reg])
    FETCHPC reg             -> usage ([], [reg])
    _                       -> noUsage
  where
    usage (src, dst) = RU (filter (interesting platform) src)
                          (filter (interesting platform) dst)
    regAddr (AddrRegReg r1 r2) = [r1, r2]
    regAddr (AddrRegImm r1 _)  = [r1]

    regRI (RIReg r) = [r]
    regRI  _        = []

interesting :: Platform -> Reg -> Bool
interesting _        (RegVirtual _)              = True
interesting platform (RegReal (RealRegSingle i))
    = isFastTrue (freeReg platform i)

interesting _        (RegReal (RealRegPair{}))
    = panic "PPC.Instr.interesting: no reg pairs on this arch"



-- | Apply a given mapping to all the register references in this
-- instruction.
ppc_patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
ppc_patchRegsOfInstr instr env
 = case instr of
    LD      sz reg addr     -> LD sz (env reg) (fixAddr addr)
    LA      sz reg addr     -> LA sz (env reg) (fixAddr addr)
    ST      sz reg addr     -> ST sz (env reg) (fixAddr addr)
    STU     sz reg addr     -> STU sz (env reg) (fixAddr addr)
    LIS     reg imm         -> LIS (env reg) imm
    LI      reg imm         -> LI (env reg) imm
    MR      reg1 reg2       -> MR (env reg1) (env reg2)
    CMP     sz reg ri       -> CMP sz (env reg) (fixRI ri)
    CMPL    sz reg ri       -> CMPL sz (env reg) (fixRI ri)
    BCC     cond lbl        -> BCC cond lbl
    BCCFAR  cond lbl        -> BCCFAR cond lbl
    MTCTR   reg             -> MTCTR (env reg)
    BCTR    targets lbl     -> BCTR targets lbl
    BL      imm argRegs     -> BL imm argRegs    -- argument regs
    BCTRL   argRegs         -> BCTRL argRegs     -- cannot be remapped
    ADD     reg1 reg2 ri    -> ADD (env reg1) (env reg2) (fixRI ri)
    ADDC    reg1 reg2 reg3  -> ADDC (env reg1) (env reg2) (env reg3)
    ADDE    reg1 reg2 reg3  -> ADDE (env reg1) (env reg2) (env reg3)
    ADDIS   reg1 reg2 imm   -> ADDIS (env reg1) (env reg2) imm
    SUBF    reg1 reg2 reg3  -> SUBF (env reg1) (env reg2) (env reg3)
    MULLW   reg1 reg2 ri    -> MULLW (env reg1) (env reg2) (fixRI ri)
    DIVW    reg1 reg2 reg3  -> DIVW (env reg1) (env reg2) (env reg3)
    DIVWU   reg1 reg2 reg3  -> DIVWU (env reg1) (env reg2) (env reg3)
    MULLW_MayOflo reg1 reg2 reg3
                            -> MULLW_MayOflo (env reg1) (env reg2) (env reg3)
    AND     reg1 reg2 ri    -> AND (env reg1) (env reg2) (fixRI ri)
    OR      reg1 reg2 ri    -> OR  (env reg1) (env reg2) (fixRI ri)
    XOR     reg1 reg2 ri    -> XOR (env reg1) (env reg2) (fixRI ri)
    XORIS   reg1 reg2 imm   -> XORIS (env reg1) (env reg2) imm
    EXTS    sz reg1 reg2    -> EXTS sz (env reg1) (env reg2)
    NEG     reg1 reg2       -> NEG (env reg1) (env reg2)
    NOT     reg1 reg2       -> NOT (env reg1) (env reg2)
    SLW     reg1 reg2 ri    -> SLW (env reg1) (env reg2) (fixRI ri)
    SRW     reg1 reg2 ri    -> SRW (env reg1) (env reg2) (fixRI ri)
    SRAW    reg1 reg2 ri    -> SRAW (env reg1) (env reg2) (fixRI ri)
    RLWINM  reg1 reg2 sh mb me
                            -> RLWINM (env reg1) (env reg2) sh mb me
    FADD    sz r1 r2 r3     -> FADD sz (env r1) (env r2) (env r3)
    FSUB    sz r1 r2 r3     -> FSUB sz (env r1) (env r2) (env r3)
    FMUL    sz r1 r2 r3     -> FMUL sz (env r1) (env r2) (env r3)
    FDIV    sz r1 r2 r3     -> FDIV sz (env r1) (env r2) (env r3)
    FNEG    r1 r2           -> FNEG (env r1) (env r2)
    FCMP    r1 r2           -> FCMP (env r1) (env r2)
    FCTIWZ  r1 r2           -> FCTIWZ (env r1) (env r2)
    FRSP    r1 r2           -> FRSP (env r1) (env r2)
    MFCR    reg             -> MFCR (env reg)
    MFLR    reg             -> MFLR (env reg)
    FETCHPC reg             -> FETCHPC (env reg)
    _                       -> instr
  where
    fixAddr (AddrRegReg r1 r2) = AddrRegReg (env r1) (env r2)
    fixAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i

    fixRI (RIReg r) = RIReg (env r)
    fixRI other     = other


--------------------------------------------------------------------------------
-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
ppc_isJumpishInstr :: Instr -> Bool
ppc_isJumpishInstr instr
 = case instr of
    BCC{}       -> True
    BCCFAR{}    -> True
    BCTR{}      -> True
    BCTRL{}     -> True
    BL{}        -> True
    JMP{}       -> True
    _           -> False


-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
ppc_jumpDestsOfInstr :: Instr -> [BlockId]
ppc_jumpDestsOfInstr insn
  = case insn of
        BCC _ id        -> [id]
        BCCFAR _ id     -> [id]
        BCTR targets _  -> [id | Just id <- targets]
        _               -> []


-- | Change the destination of this jump instruction.
-- Used in the linear allocator when adding fixup blocks for join
-- points.
ppc_patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
ppc_patchJumpInstr insn patchF
  = case insn of
        BCC cc id       -> BCC cc (patchF id)
        BCCFAR cc id    -> BCCFAR cc (patchF id)
        BCTR ids lbl    -> BCTR (map (fmap patchF) ids) lbl
        _               -> insn


-- -----------------------------------------------------------------------------

-- | An instruction to spill a register into a spill slot.
ppc_mkSpillInstr
   :: DynFlags
   -> Reg       -- register to spill
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> Instr

ppc_mkSpillInstr dflags reg delta slot
  = let platform = targetPlatform dflags
        off      = spillSlotToOffset dflags slot
    in
    let sz = case targetClassOfReg platform reg of
                RcInteger -> II32
                RcDouble  -> FF64
                _      -> panic "PPC.Instr.mkSpillInstr: no match"
    in ST sz reg (AddrRegImm sp (ImmInt (off-delta)))


ppc_mkLoadInstr
   :: DynFlags
   -> Reg       -- register to load
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> Instr

ppc_mkLoadInstr dflags reg delta slot
  = let platform = targetPlatform dflags
        off     = spillSlotToOffset dflags slot
    in
    let sz = case targetClassOfReg platform reg of
                RcInteger -> II32
                RcDouble  -> FF64
                _         -> panic "PPC.Instr.mkLoadInstr: no match"
    in LD sz reg (AddrRegImm sp (ImmInt (off-delta)))


spillSlotSize :: DynFlags -> Int
spillSlotSize dflags = if is32Bit then 12 else 8
    where is32Bit = target32Bit (targetPlatform dflags)

maxSpillSlots :: DynFlags -> Int
maxSpillSlots dflags
    = ((rESERVED_C_STACK_BYTES dflags - 64) `div` spillSlotSize dflags) - 1

-- convert a spill slot number to a *byte* offset, with no sign:
-- decide on a per arch basis whether you are spilling above or below
-- the C stack pointer.
spillSlotToOffset :: DynFlags -> Int -> Int
spillSlotToOffset dflags slot
   = 64 + spillSlotSize dflags * slot

--------------------------------------------------------------------------------
-- | See if this instruction is telling us the current C stack delta
ppc_takeDeltaInstr
    :: Instr
    -> Maybe Int

ppc_takeDeltaInstr instr
 = case instr of
     DELTA i  -> Just i
     _        -> Nothing


ppc_isMetaInstr
    :: Instr
    -> Bool

ppc_isMetaInstr instr
 = case instr of
    COMMENT{}   -> True
    LDATA{}     -> True
    NEWBLOCK{}  -> True
    DELTA{}     -> True
    _           -> False


-- | Copy the value in a register to another one.
-- Must work for all register classes.
ppc_mkRegRegMoveInstr
    :: Reg
    -> Reg
    -> Instr

ppc_mkRegRegMoveInstr src dst
    = MR dst src


-- | Make an unconditional jump instruction.
-- For architectures with branch delay slots, its ok to put
-- a NOP after the jump. Don't fill the delay slot with an
-- instruction that references regs or you'll confuse the
-- linear allocator.
ppc_mkJumpInstr
    :: BlockId
    -> [Instr]

ppc_mkJumpInstr id
    = [BCC ALWAYS id]


-- | Take the source and destination from this reg -> reg move instruction
-- or Nothing if it's not one
ppc_takeRegRegMoveInstr :: Instr -> Maybe (Reg,Reg)
ppc_takeRegRegMoveInstr (MR dst src) = Just (src,dst)
ppc_takeRegRegMoveInstr _  = Nothing

-- -----------------------------------------------------------------------------
-- Making far branches

-- Conditional branches on PowerPC are limited to +-32KB; if our Procs get too
-- big, we have to work around this limitation.

makeFarBranches
        :: BlockEnv CmmStatics
        -> [NatBasicBlock Instr]
        -> [NatBasicBlock Instr]
makeFarBranches info_env blocks
    | last blockAddresses < nearLimit = blocks
    | otherwise = zipWith handleBlock blockAddresses blocks
    where
        blockAddresses = scanl (+) 0 $ map blockLen blocks
        blockLen (BasicBlock _ instrs) = length instrs

        handleBlock addr (BasicBlock id instrs)
                = BasicBlock id (zipWith makeFar [addr..] instrs)

        makeFar _ (BCC ALWAYS tgt) = BCC ALWAYS tgt
        makeFar addr (BCC cond tgt)
            | abs (addr - targetAddr) >= nearLimit
            = BCCFAR cond tgt
            | otherwise
            = BCC cond tgt
            where Just targetAddr = lookupUFM blockAddressMap tgt
        makeFar _ other            = other

        -- 8192 instructions are allowed; let's keep some distance, as
        -- we have a few pseudo-insns that are pretty-printed as
        -- multiple instructions, and it's just not worth the effort
        -- to calculate things exactly
        nearLimit = 7000 - mapSize info_env * maxRetInfoTableSizeW

        blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddresses
