{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- Machine-dependent assembly language
--
-- (c) The University of Glasgow 1993-2004
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.PPC.Instr
   ( Instr(..)
   , RI(..)
   , archWordFormat
   , stackFrameHeaderSize
   , maxSpillSlots
   , allocMoreStack
   , makeFarBranches
   , mkJumpInstr
   , mkLoadInstr
   , mkSpillInstr
   , patchJumpInstr
   , patchRegsOfInstr
   , jumpDestsOfInstr
   , canFallthroughTo
   , takeRegRegMoveInstr
   , takeDeltaInstr
   , mkRegRegMoveInstr
   , mkStackAllocInstr
   , mkStackDeallocInstr
   , regUsageOfInstr
   , isJumpishInstr
   , isMetaInstr
   )
where

import GHC.Prelude hiding (head, init, last, tail)

import GHC.CmmToAsm.PPC.Regs
import GHC.CmmToAsm.PPC.Cond
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Instr (RegUsage(..), noUsage)
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.Platform.Reg.Class
import GHC.Platform.Reg

import GHC.Platform.Regs
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.Info
import GHC.Cmm.CLabel
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.Unique.FM (listToUFM, lookupUFM)
import GHC.Types.Unique.Supply

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import GHC.Data.FastString (FastString)
import Data.Maybe (fromMaybe)


--------------------------------------------------------------------------------
-- Format of a PPC memory address.
--
archWordFormat :: Bool -> Format
archWordFormat is32Bit
 | is32Bit   = II32
 | otherwise = II64


mkStackAllocInstr :: Platform -> Int -> [Instr]
mkStackAllocInstr platform amount
  = mkStackAllocInstr' platform (-amount)

mkStackDeallocInstr :: Platform -> Int -> [Instr]
mkStackDeallocInstr platform amount
  = mkStackAllocInstr' platform amount

mkStackAllocInstr' :: Platform -> Int -> [Instr]
mkStackAllocInstr' platform amount
  | fits16Bits amount
  = [ LD fmt r0 (AddrRegImm sp zero)
    , STU fmt r0 (AddrRegImm sp immAmount)
    ]
  | otherwise
  = [ LD fmt r0 (AddrRegImm sp zero)
    , ADDIS tmp sp (HA immAmount)
    , ADD tmp tmp (RIImm (LO immAmount))
    , STU fmt r0 (AddrRegReg sp tmp)
    ]
  where
    fmt = intFormat $ widthFromBytes (platformWordSizeInBytes platform)
    zero = ImmInt 0
    tmp = tmpReg platform
    immAmount = ImmInt amount

--
-- See Note [extra spill slots] in X86/Instr.hs
--
allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics GHC.CmmToAsm.PPC.Instr.Instr
  -> UniqSM (NatCmmDecl statics GHC.CmmToAsm.PPC.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top,[])
allocMoreStack platform slots (CmmProc info lbl live (ListGraph code)) = do
    let
        infos   = mapKeys info
        entries = case code of
                    [] -> infos
                    BasicBlock entry _ : _ -- first block is the entry point
                        | entry `elem` infos -> infos
                        | otherwise          -> entry : infos

    uniqs <- getUniquesM

    let
        delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
            where x = slots * spillSlotSize -- sp delta

        alloc   = mkStackAllocInstr   platform delta
        dealloc = mkStackDeallocInstr platform delta

        retargetList = (zip entries (map mkBlockId uniqs))

        new_blockmap :: LabelMap BlockId
        new_blockmap = mapFromList retargetList

        insert_stack_insns (BasicBlock id insns)
            | Just new_blockid <- mapLookup id new_blockmap
                = [ BasicBlock id $ alloc ++ [BCC ALWAYS new_blockid Nothing]
                  , BasicBlock new_blockid block'
                  ]
            | otherwise
                = [ BasicBlock id block' ]
            where
              block' = foldr insert_dealloc [] insns

        insert_dealloc insn r
            -- BCTR might or might not be a non-local jump. For
            -- "labeled-goto" we use JMP, and for "computed-goto" we
            -- use MTCTR followed by BCTR. See 'PPC.CodeGen.genJump'.
            = case insn of
                JMP _ _           -> dealloc ++ (insn : r)
                BCTR [] Nothing _ -> dealloc ++ (insn : r)
                BCTR ids label rs -> BCTR (map (fmap retarget) ids) label rs : r
                BCCFAR cond b p   -> BCCFAR cond (retarget b) p : r
                BCC    cond b p   -> BCC    cond (retarget b) p : r
                _                 -> insn : r
            -- BL and BCTRL are call-like instructions rather than
            -- jumps, and are used only for C calls.

        retarget :: BlockId -> BlockId
        retarget b
            = fromMaybe b (mapLookup b new_blockmap)

        new_code
            = concatMap insert_stack_insns code

    -- in
    return (CmmProc info lbl live (ListGraph new_code),retargetList)


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

    -- location pseudo-op (file, line, col, name)
    | LOCATION Int Int Int String

    -- some static data spat out during code
    -- generation.  Will be extracted before
    -- pretty-printing.
    | LDATA   Section RawCmmStatics

    -- start a new basic block.  Useful during
    -- codegen, removed later.  Preceding
    -- instruction should be a jump, as per the
    -- invariants for a BasicBlock (see Cmm).
    | NEWBLOCK BlockId

    -- specify current stack offset for
    -- benefit of subsequent passes
    | DELTA   Int

    -- Loads and stores.
    | LD      Format Reg AddrMode   -- Load format, dst, src
    | LDFAR   Format Reg AddrMode   -- Load format, dst, src 32 bit offset
    | LDR     Format Reg AddrMode   -- Load and reserve format, dst, src
    | LA      Format Reg AddrMode   -- Load arithmetic format, dst, src
    | ST      Format Reg AddrMode   -- Store format, src, dst
    | STFAR   Format Reg AddrMode   -- Store format, src, dst 32 bit offset
    | STU     Format Reg AddrMode   -- Store with Update format, src, dst
    | STC     Format Reg AddrMode   -- Store conditional format, src, dst
    | LIS     Reg Imm               -- Load Immediate Shifted dst, src
    | LI      Reg Imm               -- Load Immediate dst, src
    | MR      Reg Reg               -- Move Register dst, src -- also for fmr

    | CMP     Format Reg RI         -- format, src1, src2
    | CMPL    Format Reg RI         -- format, src1, src2

    | BCC     Cond BlockId (Maybe Bool) -- cond, block, hint
    | BCCFAR  Cond BlockId (Maybe Bool) -- cond, block, hint
                                    --   hint:
                                    --    Just True:  branch likely taken
                                    --    Just False: branch likely not taken
                                    --    Nothing:    no hint
    | JMP     CLabel [Reg]          -- same as branch,
                                    -- but with CLabel instead of block ID
                                    -- and live global registers
    | MTCTR   Reg
    | BCTR    [Maybe BlockId] (Maybe CLabel) [Reg]
                                    -- with list of local destinations, and
                                    -- jump table location if necessary
    | BL      CLabel [Reg]          -- with list of argument regs
    | BCTRL   [Reg]

    | ADD     Reg Reg RI            -- dst, src1, src2
    | ADDO    Reg Reg Reg           -- add and set overflow
    | ADDC    Reg Reg Reg           -- (carrying) dst, src1, src2
    | ADDE    Reg Reg Reg           -- (extended) dst, src1, src2
    | ADDZE   Reg Reg               -- (to zero extended) dst, src
    | ADDIS   Reg Reg Imm           -- Add Immediate Shifted dst, src1, src2
    | SUBF    Reg Reg Reg           -- dst, src1, src2 ; dst = src2 - src1
    | SUBFO   Reg Reg Reg           -- subtract from and set overflow
    | SUBFC   Reg Reg RI            -- (carrying) dst, src1, src2 ;
                                    -- dst = src2 - src1
    | SUBFE   Reg Reg Reg           -- (extended) dst, src1, src2 ;
                                    -- dst = src2 - src1
    | MULL    Format Reg Reg RI
    | MULLO   Format Reg Reg Reg    -- multiply and set overflow
    | MFOV    Format Reg            -- move overflow bit (1|33) to register
                                    -- pseudo-instruction; pretty printed as
                                    -- mfxer dst
                                    -- extr[w|d]i dst, dst, 1, [1|33]
    | MULHU   Format Reg Reg Reg
    | DIV     Format Bool Reg Reg Reg
    | AND     Reg Reg RI            -- dst, src1, src2
    | ANDC    Reg Reg Reg           -- AND with complement, dst = src1 & ~ src2
    | NAND    Reg Reg Reg           -- dst, src1, src2
    | OR      Reg Reg RI            -- dst, src1, src2
    | ORIS    Reg Reg Imm           -- OR Immediate Shifted dst, src1, src2
    | XOR     Reg Reg RI            -- dst, src1, src2
    | XORIS   Reg Reg Imm           -- XOR Immediate Shifted dst, src1, src2

    | EXTS    Format Reg Reg
    | CNTLZ   Format Reg Reg

    | NEG     Reg Reg
    | NOT     Reg Reg

    | SL      Format Reg Reg RI            -- shift left
    | SR      Format Reg Reg RI            -- shift right
    | SRA     Format Reg Reg RI            -- shift right arithmetic

    | RLWINM  Reg Reg Int Int Int   -- Rotate Left Word Immediate then AND with Mask
    | CLRLI   Format Reg Reg Int    -- clear left immediate (extended mnemonic)
    | CLRRI   Format Reg Reg Int    -- clear right immediate (extended mnemonic)

    | FADD    Format Reg Reg Reg
    | FSUB    Format Reg Reg Reg
    | FMUL    Format Reg Reg Reg
    | FDIV    Format Reg Reg Reg
    | FABS    Reg Reg               -- abs is the same for single and double
    | FNEG    Reg Reg               -- negate is the same for single and double prec.

    | FCMP    Reg Reg

    | FCTIWZ  Reg Reg           -- convert to integer word
    | FCTIDZ  Reg Reg           -- convert to integer double word
    | FCFID   Reg Reg           -- convert from integer double word
    | FRSP    Reg Reg           -- reduce to single precision
                                -- (but destination is a FP register)

    | CRNOR   Int Int Int       -- condition register nor
    | MFCR    Reg               -- move from condition register

    | MFLR    Reg               -- move from link register
    | FETCHPC Reg               -- pseudo-instruction:
                                -- bcl to next insn, mflr reg
    | HWSYNC                    -- heavy weight sync
    | ISYNC                     -- instruction synchronize
    | LWSYNC                    -- memory barrier
    | NOP                       -- no operation, PowerPC 64 bit
                                -- needs this as place holder to
                                -- reload TOC pointer

-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr
 = case instr of
    LD      _ reg addr       -> usage (regAddr addr, [reg])
    LDFAR   _ reg addr       -> usage (regAddr addr, [reg])
    LDR     _ reg addr       -> usage (regAddr addr, [reg])
    LA      _ reg addr       -> usage (regAddr addr, [reg])
    ST      _ reg addr       -> usage (reg : regAddr addr, [])
    STFAR   _ reg addr       -> usage (reg : regAddr addr, [])
    STU     _ reg addr       -> usage (reg : regAddr addr, [])
    STC     _ reg addr       -> usage (reg : regAddr addr, [])
    LIS     reg _            -> usage ([], [reg])
    LI      reg _            -> usage ([], [reg])
    MR      reg1 reg2        -> usage ([reg2], [reg1])
    CMP     _ reg ri         -> usage (reg : regRI ri,[])
    CMPL    _ reg ri         -> usage (reg : regRI ri,[])
    BCC     _ _ _            -> noUsage
    BCCFAR  _ _ _            -> noUsage
    JMP     _ regs           -> usage (regs, [])
    MTCTR   reg              -> usage ([reg],[])
    BCTR    _ _ regs         -> usage (regs, [])
    BL      _ params         -> usage (params, callClobberedRegs platform)
    BCTRL   params           -> usage (params, callClobberedRegs platform)

    ADD     reg1 reg2 ri     -> usage (reg2 : regRI ri, [reg1])
    ADDO    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    ADDC    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    ADDE    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    ADDZE   reg1 reg2        -> usage ([reg2], [reg1])
    ADDIS   reg1 reg2 _      -> usage ([reg2], [reg1])
    SUBF    reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    SUBFO   reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    SUBFC   reg1 reg2 ri     -> usage (reg2 : regRI ri, [reg1])
    SUBFE   reg1 reg2 reg3   -> usage ([reg2,reg3], [reg1])
    MULL    _ reg1 reg2 ri   -> usage (reg2 : regRI ri, [reg1])
    MULLO   _ reg1 reg2 reg3 -> usage ([reg2,reg3], [reg1])
    MFOV    _ reg            -> usage ([], [reg])
    MULHU   _ reg1 reg2 reg3 -> usage ([reg2,reg3], [reg1])
    DIV     _ _ reg1 reg2 reg3
                             -> usage ([reg2,reg3], [reg1])

    AND     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    ANDC    reg1 reg2 reg3  -> usage ([reg2,reg3], [reg1])
    NAND    reg1 reg2 reg3  -> usage ([reg2,reg3], [reg1])
    OR      reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    ORIS    reg1 reg2 _     -> usage ([reg2], [reg1])
    XOR     reg1 reg2 ri    -> usage (reg2 : regRI ri, [reg1])
    XORIS   reg1 reg2 _     -> usage ([reg2], [reg1])
    EXTS    _  reg1 reg2    -> usage ([reg2], [reg1])
    CNTLZ   _  reg1 reg2    -> usage ([reg2], [reg1])
    NEG     reg1 reg2       -> usage ([reg2], [reg1])
    NOT     reg1 reg2       -> usage ([reg2], [reg1])
    SL      _ reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SR      _ reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    SRA     _ reg1 reg2 ri  -> usage (reg2 : regRI ri, [reg1])
    RLWINM  reg1 reg2 _ _ _ -> usage ([reg2], [reg1])
    CLRLI   _ reg1 reg2 _   -> usage ([reg2], [reg1])
    CLRRI   _ reg1 reg2 _   -> usage ([reg2], [reg1])

    FADD    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FSUB    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FMUL    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FDIV    _ r1 r2 r3      -> usage ([r2,r3], [r1])
    FABS    r1 r2           -> usage ([r2], [r1])
    FNEG    r1 r2           -> usage ([r2], [r1])
    FCMP    r1 r2           -> usage ([r1,r2], [])
    FCTIWZ  r1 r2           -> usage ([r2], [r1])
    FCTIDZ  r1 r2           -> usage ([r2], [r1])
    FCFID   r1 r2           -> usage ([r2], [r1])
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
interesting platform (RegReal (RealRegSingle i)) = freeReg platform i


-- | Apply a given mapping to all the register references in this
-- instruction.
patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr instr env
 = case instr of
    LD      fmt reg addr    -> LD fmt (env reg) (fixAddr addr)
    LDFAR   fmt reg addr    -> LDFAR fmt (env reg) (fixAddr addr)
    LDR     fmt reg addr    -> LDR fmt (env reg) (fixAddr addr)
    LA      fmt reg addr    -> LA fmt (env reg) (fixAddr addr)
    ST      fmt reg addr    -> ST fmt (env reg) (fixAddr addr)
    STFAR   fmt reg addr    -> STFAR fmt (env reg) (fixAddr addr)
    STU     fmt reg addr    -> STU fmt (env reg) (fixAddr addr)
    STC     fmt reg addr    -> STC fmt (env reg) (fixAddr addr)
    LIS     reg imm         -> LIS (env reg) imm
    LI      reg imm         -> LI (env reg) imm
    MR      reg1 reg2       -> MR (env reg1) (env reg2)
    CMP     fmt reg ri      -> CMP fmt (env reg) (fixRI ri)
    CMPL    fmt reg ri      -> CMPL fmt (env reg) (fixRI ri)
    BCC     cond lbl p      -> BCC cond lbl p
    BCCFAR  cond lbl p      -> BCCFAR cond lbl p
    JMP     l regs          -> JMP l regs -- global regs will not be remapped
    MTCTR   reg             -> MTCTR (env reg)
    BCTR    targets lbl rs  -> BCTR targets lbl rs
    BL      imm argRegs     -> BL imm argRegs    -- argument regs
    BCTRL   argRegs         -> BCTRL argRegs     -- cannot be remapped
    ADD     reg1 reg2 ri    -> ADD (env reg1) (env reg2) (fixRI ri)
    ADDO    reg1 reg2 reg3  -> ADDO (env reg1) (env reg2) (env reg3)
    ADDC    reg1 reg2 reg3  -> ADDC (env reg1) (env reg2) (env reg3)
    ADDE    reg1 reg2 reg3  -> ADDE (env reg1) (env reg2) (env reg3)
    ADDZE   reg1 reg2       -> ADDZE (env reg1) (env reg2)
    ADDIS   reg1 reg2 imm   -> ADDIS (env reg1) (env reg2) imm
    SUBF    reg1 reg2 reg3  -> SUBF (env reg1) (env reg2) (env reg3)
    SUBFO   reg1 reg2 reg3  -> SUBFO (env reg1) (env reg2) (env reg3)
    SUBFC   reg1 reg2 ri    -> SUBFC (env reg1) (env reg2) (fixRI ri)
    SUBFE   reg1 reg2 reg3  -> SUBFE (env reg1) (env reg2) (env reg3)
    MULL    fmt reg1 reg2 ri
                            -> MULL fmt (env reg1) (env reg2) (fixRI ri)
    MULLO   fmt reg1 reg2 reg3
                            -> MULLO fmt (env reg1) (env reg2) (env reg3)
    MFOV    fmt reg         -> MFOV fmt (env reg)
    MULHU   fmt reg1 reg2 reg3
                            -> MULHU fmt (env reg1) (env reg2) (env reg3)
    DIV     fmt sgn reg1 reg2 reg3
                            -> DIV fmt sgn (env reg1) (env reg2) (env reg3)

    AND     reg1 reg2 ri    -> AND (env reg1) (env reg2) (fixRI ri)
    ANDC    reg1 reg2 reg3  -> ANDC (env reg1) (env reg2) (env reg3)
    NAND    reg1 reg2 reg3  -> NAND (env reg1) (env reg2) (env reg3)
    OR      reg1 reg2 ri    -> OR  (env reg1) (env reg2) (fixRI ri)
    ORIS    reg1 reg2 imm   -> ORIS (env reg1) (env reg2) imm
    XOR     reg1 reg2 ri    -> XOR (env reg1) (env reg2) (fixRI ri)
    XORIS   reg1 reg2 imm   -> XORIS (env reg1) (env reg2) imm
    EXTS    fmt reg1 reg2   -> EXTS fmt (env reg1) (env reg2)
    CNTLZ   fmt reg1 reg2   -> CNTLZ fmt (env reg1) (env reg2)
    NEG     reg1 reg2       -> NEG (env reg1) (env reg2)
    NOT     reg1 reg2       -> NOT (env reg1) (env reg2)
    SL      fmt reg1 reg2 ri
                            -> SL fmt (env reg1) (env reg2) (fixRI ri)
    SR      fmt reg1 reg2 ri
                            -> SR fmt (env reg1) (env reg2) (fixRI ri)
    SRA     fmt reg1 reg2 ri
                            -> SRA fmt (env reg1) (env reg2) (fixRI ri)
    RLWINM  reg1 reg2 sh mb me
                            -> RLWINM (env reg1) (env reg2) sh mb me
    CLRLI   fmt reg1 reg2 n -> CLRLI fmt (env reg1) (env reg2) n
    CLRRI   fmt reg1 reg2 n -> CLRRI fmt (env reg1) (env reg2) n
    FADD    fmt r1 r2 r3    -> FADD fmt (env r1) (env r2) (env r3)
    FSUB    fmt r1 r2 r3    -> FSUB fmt (env r1) (env r2) (env r3)
    FMUL    fmt r1 r2 r3    -> FMUL fmt (env r1) (env r2) (env r3)
    FDIV    fmt r1 r2 r3    -> FDIV fmt (env r1) (env r2) (env r3)
    FABS    r1 r2           -> FABS (env r1) (env r2)
    FNEG    r1 r2           -> FNEG (env r1) (env r2)
    FCMP    r1 r2           -> FCMP (env r1) (env r2)
    FCTIWZ  r1 r2           -> FCTIWZ (env r1) (env r2)
    FCTIDZ  r1 r2           -> FCTIDZ (env r1) (env r2)
    FCFID   r1 r2           -> FCFID (env r1) (env r2)
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
isJumpishInstr :: Instr -> Bool
isJumpishInstr instr
 = case instr of
    BCC{}       -> True
    BCCFAR{}    -> True
    BCTR{}      -> True
    BCTRL{}     -> True
    BL{}        -> True
    JMP{}       -> True
    _           -> False

canFallthroughTo :: Instr -> BlockId -> Bool
canFallthroughTo instr bid
 = case instr of
        BCC _ target _      -> target == bid
        BCCFAR _ target _   -> target == bid
        _                   -> False


-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr insn
  = case insn of
        BCC _ id _       -> [id]
        BCCFAR _ id _    -> [id]
        BCTR targets _ _ -> [id | Just id <- targets]
        _                -> []


-- | Change the destination of this jump instruction.
-- Used in the linear allocator when adding fixup blocks for join
-- points.
patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
patchJumpInstr insn patchF
  = case insn of
        BCC cc id p     -> BCC cc (patchF id) p
        BCCFAR cc id p  -> BCCFAR cc (patchF id) p
        BCTR ids lbl rs -> BCTR (map (fmap patchF) ids) lbl rs
        _               -> insn


-- -----------------------------------------------------------------------------

-- | An instruction to spill a register into a spill slot.
mkSpillInstr
   :: NCGConfig
   -> Reg       -- register to spill
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> [Instr]

mkSpillInstr config reg delta slot
  = let platform = ncgPlatform config
        off      = spillSlotToOffset platform slot
        arch     = platformArch platform
    in
    let fmt = case targetClassOfReg platform reg of
                RcInteger -> case arch of
                                ArchPPC -> II32
                                _       -> II64
                RcDouble  -> FF64
                _         -> panic "PPC.Instr.mkSpillInstr: no match"
        instr = case makeImmediate W32 True (off-delta) of
                Just _  -> ST
                Nothing -> STFAR -- pseudo instruction: 32 bit offsets

    in [instr fmt reg (AddrRegImm sp (ImmInt (off-delta)))]


mkLoadInstr
   :: NCGConfig
   -> Reg       -- register to load
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> [Instr]

mkLoadInstr config reg delta slot
  = let platform = ncgPlatform config
        off      = spillSlotToOffset platform slot
        arch     = platformArch platform
    in
    let fmt = case targetClassOfReg platform reg of
                RcInteger ->  case arch of
                                 ArchPPC -> II32
                                 _       -> II64
                RcDouble  -> FF64
                _         -> panic "PPC.Instr.mkLoadInstr: no match"
        instr = case makeImmediate W32 True (off-delta) of
                Just _  -> LD
                Nothing -> LDFAR -- pseudo instruction: 32 bit offsets

    in [instr fmt reg (AddrRegImm sp (ImmInt (off-delta)))]


-- | The size of a minimal stackframe header including minimal
-- parameter save area.
stackFrameHeaderSize :: Platform -> Int
stackFrameHeaderSize platform
  = case platformOS platform of
      OSAIX    -> 24 + 8 * 4
      _ -> case platformArch platform of
                             -- header + parameter save area
             ArchPPC           -> 64 -- TODO: check ABI spec
             ArchPPC_64 ELF_V1 -> 48 + 8 * 8
             ArchPPC_64 ELF_V2 -> 32 + 8 * 8
             _ -> panic "PPC.stackFrameHeaderSize: not defined for this OS"

-- | The maximum number of bytes required to spill a register. PPC32
-- has 32-bit GPRs and 64-bit FPRs, while PPC64 has 64-bit GPRs and
-- 64-bit FPRs. So the maximum is 8 regardless of platforms unlike
-- x86. Note that AltiVec's vector registers are 128-bit wide so we
-- must not use this to spill them.
spillSlotSize :: Int
spillSlotSize = 8

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
--  = 0 -- useful for testing allocMoreStack
    = let platform = ncgPlatform config
      in ((ncgSpillPreallocSize config - stackFrameHeaderSize platform)
         `div` spillSlotSize) - 1

-- | The number of bytes that the stack pointer should be aligned
-- to. This is 16 both on PPC32 and PPC64 ELF (see ELF processor
-- specific supplements).
stackAlign :: Int
stackAlign = 16

-- | Convert a spill slot number to a *byte* offset, with no sign.
spillSlotToOffset :: Platform -> Int -> Int
spillSlotToOffset platform slot
   = stackFrameHeaderSize platform + spillSlotSize * slot


--------------------------------------------------------------------------------
-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr
    :: Instr
    -> Maybe Int

takeDeltaInstr instr
 = case instr of
     DELTA i  -> Just i
     _        -> Nothing


isMetaInstr
    :: Instr
    -> Bool

isMetaInstr instr
 = case instr of
    COMMENT{}   -> True
    LOCATION{}  -> True
    LDATA{}     -> True
    NEWBLOCK{}  -> True
    DELTA{}     -> True
    _           -> False


-- | Copy the value in a register to another one.
-- Must work for all register classes.
mkRegRegMoveInstr
    :: Reg
    -> Reg
    -> Instr

mkRegRegMoveInstr src dst
    = MR dst src


-- | Make an unconditional jump instruction.
mkJumpInstr
    :: BlockId
    -> [Instr]

mkJumpInstr id
    = [BCC ALWAYS id Nothing]


-- | Take the source and destination from this reg -> reg move instruction
-- or Nothing if it's not one
takeRegRegMoveInstr :: Instr -> Maybe (Reg,Reg)
takeRegRegMoveInstr (MR dst src) = Just (src,dst)
takeRegRegMoveInstr _  = Nothing

-- -----------------------------------------------------------------------------
-- Making far branches

-- Conditional branches on PowerPC are limited to +-32KB; if our Procs get too
-- big, we have to work around this limitation.

makeFarBranches
        :: Platform
        -> LabelMap RawCmmStatics
        -> [NatBasicBlock Instr]
        -> UniqSM [NatBasicBlock Instr]
makeFarBranches _platform info_env blocks
    | NE.last blockAddresses < nearLimit = return blocks
    | otherwise = return $ zipWith handleBlock blockAddressList blocks
    where
        blockAddresses = NE.scanl (+) 0 $ map blockLen blocks
        blockAddressList = toList blockAddresses
        blockLen (BasicBlock _ instrs) = length instrs

        handleBlock addr (BasicBlock id instrs)
                = BasicBlock id (zipWith makeFar [addr..] instrs)

        makeFar _ (BCC ALWAYS tgt _) = BCC ALWAYS tgt Nothing
        makeFar addr (BCC cond tgt p)
            | abs (addr - targetAddr) >= nearLimit
            = BCCFAR cond tgt p
            | otherwise
            = BCC cond tgt p
            where Just targetAddr = lookupUFM blockAddressMap tgt
        makeFar _ other            = other

        -- 8192 instructions are allowed; let's keep some distance, as
        -- we have a few pseudo-insns that are pretty-printed as
        -- multiple instructions, and it's just not worth the effort
        -- to calculate things exactly
        nearLimit = 7000 - mapSize info_env * maxRetInfoTableSizeW

        blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddressList
