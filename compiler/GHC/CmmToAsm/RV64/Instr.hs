{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.CmmToAsm.RV64.Instr

where

import GHC.Prelude

import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.RV64.Regs

import GHC.CmmToAsm.Instr (RegUsage(..))
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Config
import GHC.Platform.Reg

import GHC.Platform.Regs
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique.Supply

import GHC.Utils.Panic

import Data.Maybe

import GHC.Stack
import qualified Data.List.NonEmpty as NE
import Data.Foldable
import GHC.Cmm.Info (maxRetInfoTableSizeW)
import GHC.Types.Unique.FM (listToUFM, lookupUFM)

-- | Stack frame header size in bytes.
--
-- The stack frame header is made of the values that are always saved
-- (regardless of the context.) It consists of the saved return address and a
-- pointer to the previous frame. Thus, its size is two stack frame slots which
-- equals two addresses/words (2 * 8 byte).
stackFrameHeaderSize :: Int
stackFrameHeaderSize = 2 * spillSlotSize

-- | All registers are 8 byte wide.
spillSlotSize :: Int
spillSlotSize = 8

-- | The number of bytes that the stack pointer should be aligned
-- to.
stackAlign :: Int
stackAlign = 16

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
    = ((ncgSpillPreallocSize config - stackFrameHeaderSize)
         `div` spillSlotSize) - 1

-- | Convert a spill slot number to a *byte* offset, with no sign.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
   = stackFrameHeaderSize + spillSlotSize * slot

-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
-- RegUsage = RU [<read regs>] [<write regs>]

instance Outputable RegUsage where
    ppr (RU reads writes) = text "RegUsage(reads:" <+> ppr reads <> comma <+> text "writes:" <+> ppr writes <> char ')'

regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr = case instr of
  ANN _ i                  -> regUsageOfInstr platform i
  COMMENT{}                -> usage ([], [])
  MULTILINE_COMMENT{}      -> usage ([], [])
  PUSH_STACK_FRAME         -> usage ([], [])
  POP_STACK_FRAME          -> usage ([], [])
  DELTA{}                  -> usage ([], [])

  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MUL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  NEG dst src              -> usage (regOp src, regOp dst)
  SMULH dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIV dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  REM dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  REMU dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  SUB dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIVU dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)

  -- 2. Bit Manipulation Instructions ------------------------------------------
  SBFM dst src _ _         -> usage (regOp src, regOp dst)
  UBFM dst src _ _         -> usage (regOp src, regOp dst)
  UBFX dst src _ _         -> usage (regOp src, regOp dst)
  -- 3. Logical and Move Instructions ------------------------------------------
  AND dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  OR dst src1 src2         -> usage (regOp src1 ++ regOp src2, regOp dst)
  ASR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  BIC dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  BICS dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  XOR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  LSL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  LSR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MOV dst src              -> usage (regOp src, regOp dst)
  -- ORI's third operand is always an immediate
  ORI dst src1 _           -> usage (regOp src1, regOp dst)
  XORI dst src1 _          -> usage (regOp src1, regOp dst)
  -- 4. Branch Instructions ----------------------------------------------------
  J t                      -> usage (regTarget t, [])
  J_TBL _ _ t              -> usage ([t], [])
  B t                      -> usage (regTarget t, [])
  B_FAR _t                 -> usage ([], [])
  BCOND _ l r t            -> usage (regTarget t ++ regOp l ++ regOp r, [])
  BCOND_FAR _ l r _ t        -> usage (regTarget t ++ regOp l ++ regOp r, [])
  BL t ps _rs              -> usage (regTarget t ++ ps, callerSavedRegisters)

  -- 5. Atomic Instructions ----------------------------------------------------
  -- 6. Conditional Instructions -----------------------------------------------
  CSET dst l r _           -> usage (regOp l ++ regOp r, regOp dst)
  -- 7. Load and Store Instructions --------------------------------------------
  STR _ src dst            -> usage (regOp src ++ regOp dst, [])
  -- STLR _ src dst      L     -> usage (regOp src ++ regOp dst, [])
  LDR _ dst src            -> usage (regOp src, regOp dst)
  LDRU _ dst src           -> usage (regOp src, regOp dst)

  -- 8. Synchronization Instructions -------------------------------------------
  DMBSY _ _                  -> usage ([], [])

  -- 9. Floating Point Instructions --------------------------------------------
  FCVT dst src             -> usage (regOp src, regOp dst)
  SCVTF dst src            -> usage (regOp src, regOp dst)
  FCVTZS dst src           -> usage (regOp src, regOp dst)
  FABS dst src             -> usage (regOp src, regOp dst)

  _ -> panic $ "regUsageOfInstr: " ++ instrCon instr

  where
        -- filtering the usage is necessary, otherwise the register
        -- allocator will try to allocate pre-defined fixed stg
        -- registers as well, as they show up.
        usage (src, dst) = RU (filter (interesting platform) src)
                              (filter (interesting platform) dst)

        regAddr :: AddrMode -> [Reg]
        regAddr (AddrRegImm r1 _)  = [r1]
        regAddr (AddrReg r1)       = [r1]
        regOp :: Operand -> [Reg]
        regOp (OpReg _ r1) = [r1]
        regOp (OpAddr a) = regAddr a
        regOp (OpImm _) = []
        regTarget :: Target -> [Reg]
        regTarget (TBlock _) = []
        regTarget (TLabel _) = []
        regTarget (TReg r1)  = [r1]

        -- Is this register interesting for the register allocator?
        interesting :: Platform -> Reg -> Bool
        interesting _        (RegVirtual _)                 = True
        interesting _        (RegReal (RealRegSingle (-1))) = False
        interesting platform (RegReal (RealRegSingle i))    = freeReg platform i

-- Save caller save registers
-- This is x0-x18
--
-- For SIMD/FP Registers:
-- Registers v8-v15 must be preserved by a callee across subroutine calls;
-- the remaining registers (v0-v7, v16-v31) do not need to be preserved (or
-- should be preserved by the caller). Additionally, only the bottom 64 bits
-- of each value stored in v8-v15 need to be preserved [7]; it is the
-- responsibility of the caller to preserve larger values.
--
-- .---------------------------------------------------------------------------------------------------------------------------------------------------------------.
-- |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |
-- | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 42 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |
-- |== General Purpose registers ==================================================================================================================================|
-- | ZR | RA | SP | GP | TP | <- tmp r. -> | FP | <- | <---- argument passing -------------> | -- callee saved ------------------------------> | <--- tmp regs --> |
-- | -- | -- | -- | -- | -- | <- free r. > | -- | BR | <---- free registers ---------------> | SP | HP | R1 | R2 | R3 | R4 | R5 | R6 | R7 | SL | <-- free regs --> |
-- |== SIMD/FP Registers ==========================================================================================================================================|
-- | <--- temporary registers -----------> | <------ | <---- argument passing -------------> | -- callee saved ------------------------------> | <--- tmp regs --> |
-- | <---------- free registers ---------> | F1 | F2 | <---- free registers ---------------> | F3 | F4 | F5 | F6 | D1 | D2 | D3 | D4 | D5 | D6 | -- | -- | -- | -- |
-- '---------------------------------------------------------------------------------------------------------------------------------------------------------------'
-- ZR: Zero, RA: Return Address, SP: Stack Pointer, GP: Global Pointer, TP: Thread Pointer, FP: Frame Pointer
-- BR: Base, SL: SpLim
callerSavedRegisters :: [Reg]
callerSavedRegisters =
  map regSingle [t0RegNo .. t2RegNo]
    ++ map regSingle [a0RegNo .. a7RegNo]
    ++ map regSingle [t3RegNo .. t6RegNo]
    ++ map regSingle [ft0RegNo .. ft7RegNo]
    ++ map regSingle [fa0RegNo .. fa7RegNo]

-- | Apply a given mapping to all the register references in this
-- instruction.
patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr instr env = case instr of
    -- 0. Meta Instructions
    ANN d i             -> ANN d (patchRegsOfInstr i env)
    COMMENT{}           -> instr
    MULTILINE_COMMENT{} -> instr
    PUSH_STACK_FRAME    -> instr
    POP_STACK_FRAME     -> instr
    DELTA{}             -> instr
    -- 1. Arithmetic Instructions ----------------------------------------------
    ADD o1 o2 o3   -> ADD (patchOp o1) (patchOp o2) (patchOp o3)
    MUL o1 o2 o3   -> MUL (patchOp o1) (patchOp o2) (patchOp o3)
    NEG o1 o2      -> NEG (patchOp o1) (patchOp o2)
    SMULH o1 o2 o3 -> SMULH (patchOp o1) (patchOp o2)  (patchOp o3)
    DIV o1 o2 o3   -> DIV (patchOp o1) (patchOp o2) (patchOp o3)
    REM o1 o2 o3   -> REM (patchOp o1) (patchOp o2) (patchOp o3)
    REMU o1 o2 o3  -> REMU (patchOp o1) (patchOp o2) (patchOp o3)
    SUB o1 o2 o3   -> SUB  (patchOp o1) (patchOp o2) (patchOp o3)
    DIVU o1 o2 o3  -> DIVU (patchOp o1) (patchOp o2) (patchOp o3)

    -- 2. Bit Manipulation Instructions ----------------------------------------
    SBFM o1 o2 o3 o4 -> SBFM (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)
    UBFM o1 o2 o3 o4 -> UBFM (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)
    UBFX o1 o2 o3 o4 -> UBFX (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)

    -- 3. Logical and Move Instructions ----------------------------------------
    AND o1 o2 o3   -> AND  (patchOp o1) (patchOp o2) (patchOp o3)
    OR o1 o2 o3    -> OR   (patchOp o1) (patchOp o2) (patchOp o3)
    ASR o1 o2 o3   -> ASR  (patchOp o1) (patchOp o2) (patchOp o3)
    BIC o1 o2 o3   -> BIC  (patchOp o1) (patchOp o2) (patchOp o3)
    BICS o1 o2 o3  -> BICS (patchOp o1) (patchOp o2) (patchOp o3)
    XOR o1 o2 o3   -> XOR  (patchOp o1) (patchOp o2) (patchOp o3)
    LSL o1 o2 o3   -> LSL  (patchOp o1) (patchOp o2) (patchOp o3)
    LSR o1 o2 o3   -> LSR  (patchOp o1) (patchOp o2) (patchOp o3)
    MOV o1 o2      -> MOV  (patchOp o1) (patchOp o2)
    -- o3 cannot be a register for ORI (always an immediate)
    ORI o1 o2 o3   -> ORI  (patchOp o1) (patchOp o2) (patchOp o3)
    XORI o1 o2 o3  -> XORI  (patchOp o1) (patchOp o2) (patchOp o3)

    -- 4. Branch Instructions --------------------------------------------------
    J t            -> J (patchTarget t)
    J_TBL ids mbLbl t    -> J_TBL ids mbLbl (env t)
    B t            -> B (patchTarget t)
    B_FAR t            -> B_FAR t
    BL t rs ts     -> BL (patchTarget t) rs ts
    BCOND c o1 o2 t -> BCOND c (patchOp o1) (patchOp o2) (patchTarget t)
    BCOND_FAR c o1 o2 b t -> BCOND_FAR c (patchOp o1) (patchOp o2) (patchTarget b) (patchTarget t)

    -- 5. Atomic Instructions --------------------------------------------------
    -- 6. Conditional Instructions ---------------------------------------------
    CSET o l r c   -> CSET (patchOp o) (patchOp l) (patchOp r) c
    -- 7. Load and Store Instructions ------------------------------------------
    STR f o1 o2    -> STR f (patchOp o1) (patchOp o2)
    -- STLR f o1 o2   -> STLR f (patchOp o1) (patchOp o2)
    LDR f o1 o2    -> LDR f (patchOp o1) (patchOp o2)
    LDRU f o1 o2    -> LDRU f (patchOp o1) (patchOp o2)

    -- 8. Synchronization Instructions -----------------------------------------
    DMBSY o1 o2    -> DMBSY o1 o2

    -- 9. Floating Point Instructions ------------------------------------------
    FCVT o1 o2     -> FCVT (patchOp o1) (patchOp o2)
    SCVTF o1 o2    -> SCVTF (patchOp o1) (patchOp o2)
    FCVTZS o1 o2   -> FCVTZS (patchOp o1) (patchOp o2)
    FABS o1 o2     -> FABS (patchOp o1) (patchOp o2)
    _              -> panic $ "patchRegsOfInstr: " ++ instrCon instr
    where
        patchOp :: Operand -> Operand
        patchOp (OpReg w r) = OpReg w (env r)
        patchOp (OpAddr a) = OpAddr (patchAddr a)
        patchOp op = op
        patchTarget :: Target -> Target
        patchTarget (TReg r) = TReg (env r)
        patchTarget t = t
        patchAddr :: AddrMode -> AddrMode
        patchAddr (AddrRegImm r1 i)  = AddrRegImm (env r1) i
        patchAddr (AddrReg r) = AddrReg (env r)
--------------------------------------------------------------------------------

-- | Checks whether this instruction is a jump/branch instruction.
--
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
isJumpishInstr :: Instr -> Bool
isJumpishInstr instr = case instr of
  ANN _ i -> isJumpishInstr i
  J {} -> True
  J_TBL {} -> True
  B {} -> True
  B_FAR {} -> True
  BL {} -> True
  BCOND {} -> True
  BCOND_FAR {} -> True
  _ -> False

-- | Get the `BlockId`s of the jump destinations (if any)
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr (ANN _ i) = jumpDestsOfInstr i
jumpDestsOfInstr (J t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (J_TBL ids _mbLbl _r) = catMaybes ids
jumpDestsOfInstr (B t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (B_FAR t) = [t]
jumpDestsOfInstr (BL t _ _) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BCOND _ _ _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BCOND_FAR _ _ _ _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr _ = []

-- | Change the destination of this (potential) jump instruction.
--
-- Used in the linear allocator when adding fixup blocks for join
-- points.
patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
patchJumpInstr instr patchF =
  case instr of
    ANN d i -> ANN d (patchJumpInstr i patchF)
    J (TBlock bid) -> J (TBlock (patchF bid))
    J_TBL ids mbLbl r -> J_TBL (map (fmap patchF) ids) mbLbl r
    B (TBlock bid) -> B (TBlock (patchF bid))
    B_FAR bid -> B_FAR (patchF bid)
    BL (TBlock bid) ps rs -> BL (TBlock (patchF bid)) ps rs
    BCOND c o1 o2 (TBlock bid) -> BCOND c o1 o2 (TBlock (patchF bid))
    BCOND_FAR c o1 o2 b (TBlock bid) -> BCOND_FAR c o1 o2 b (TBlock (patchF bid))
    _ -> panic $ "patchJumpInstr: " ++ instrCon instr

-- -----------------------------------------------------------------------------
-- Note [Spills and Reloads]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- We reserve @RESERVED_C_STACK_BYTES@ on the C stack for spilling and reloading
-- registers.  AArch64s maximum displacement for SP relative spills and reloads
-- is essentially [-256,255], or [0, 0xFFF]*8 = [0, 32760] for 64bits.
--
-- The @RESERVED_C_STACK_BYTES@ is 16k, so we can't address any location in a
-- single instruction.  The idea is to use the Inter Procedure 0 (ip) register
-- to perform the computations for larger offsets.
--
-- Using sp to compute the offset will violate assumptions about the stack pointer
-- pointing to the top of the stack during signal handling.  As we can't force
-- every signal to use its own stack, we have to ensure that the stack pointer
-- always points to the top of the stack, and we can't use it for computation.
--
-- | An instruction to spill a register into a spill slot.
mkSpillInstr ::
  HasCallStack =>
  NCGConfig ->
  Reg -> -- ^ register to spill
  Int -> -- ^ current stack delta
  Int -> -- ^ spill slot to use
  [Instr]
mkSpillInstr _config reg delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkStrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkStrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < d0RegNo -> II64
      _ -> FF64
    mkStrSpImm imm = ANN (text "Spill@" <> int (off - delta)) $ STR fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToIp imm = ANN (text "Spill: IP <- " <> int imm) $ MOV ip (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Spill: IP <- SP + IP ") $ ADD ip ip sp
    mkStrIp = ANN (text "Spill@" <> int (off - delta)) $ STR fmt (OpReg W64 reg) (OpAddr (AddrReg ipReg))

    off = spillSlotToOffset slot

mkLoadInstr
   :: NCGConfig
   -> Reg       -- ^ register to load
   -> Int       -- ^ current stack delta
   -> Int       -- ^ spill slot to use
   -> [Instr]

mkLoadInstr _config reg delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkLdrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkLdrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < d0RegNo -> II64
      _ -> FF64
    mkLdrSpImm imm = ANN (text "Reload@" <> int (off - delta)) $ LDR fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToIp imm = ANN (text "Reload: IP <- " <> int imm) $ MOV ip (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Reload: IP <- SP + IP ") $ ADD ip ip sp
    mkLdrIp = ANN (text "Reload@" <> int (off - delta)) $ LDR fmt (OpReg W64 reg) (OpAddr (AddrReg ipReg))

    off = spillSlotToOffset slot

-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr :: Instr -> Maybe Int
takeDeltaInstr (ANN _ i) = takeDeltaInstr i
takeDeltaInstr (DELTA i) = Just i
takeDeltaInstr _         = Nothing

-- | Not real instructions.  Just meta data
isMetaInstr :: Instr -> Bool
isMetaInstr instr =
  case instr of
    ANN _ i -> isMetaInstr i
    COMMENT {} -> True
    MULTILINE_COMMENT {} -> True
    LOCATION {} -> True
    LDATA {} -> True
    NEWBLOCK {} -> True
    DELTA {} -> True
    PUSH_STACK_FRAME -> True
    POP_STACK_FRAME -> True
    _ -> False

-- | Copy the value in a register to another one.
--
-- Must work for all register classes.
mkRegRegMoveInstr :: Reg -> Reg -> Instr
mkRegRegMoveInstr src dst = ANN desc instr
  where
    desc = text "Reg->Reg Move: " <> ppr src <> text " -> " <> ppr dst
    instr = MOV (operandFromReg dst) (operandFromReg src)

-- | Take the source and destination from this (potential) reg -> reg move instruction
--
-- We have to be a bit careful here: A `MOV` can also mean an implicit
-- conversion. This case is filtered out.
takeRegRegMoveInstr :: Instr -> Maybe (Reg, Reg)
takeRegRegMoveInstr (MOV (OpReg width dst) (OpReg width' src))
  | width == width' && (isFloatReg dst == isFloatReg src) = pure (src, dst)
takeRegRegMoveInstr _ = Nothing

-- | Make an unconditional jump instruction.
mkJumpInstr :: BlockId -> [Instr]
mkJumpInstr = pure . B . TBlock

-- | Decrement @sp@ to allocate stack space.
--
-- The stack grows downwards, so we decrement the stack pointer by @n@ (bytes).
-- This is dual to `mkStackDeallocInstr`. @sp@ is the RISCV stack pointer, not
-- to be confused with the STG stack pointer.
mkStackAllocInstr :: Platform -> Int -> [Instr]
mkStackAllocInstr _platform = moveSp . negate

-- | Increment SP to deallocate stack space.
--
-- The stack grows downwards, so we increment the stack pointer by @n@ (bytes).
-- This is dual to `mkStackAllocInstr`. @sp@ is the RISCV stack pointer, not to
-- be confused with the STG stack pointer.
mkStackDeallocInstr :: Platform -> Int -> [Instr]
mkStackDeallocInstr _platform = moveSp

moveSp :: Int -> [Instr]
moveSp n
  | n == 0 = []
  | n /= 0 && fitsIn12bitImm n = pure . ANN desc $ ADD sp sp (OpImm (ImmInt n))
  | otherwise =
      -- This ends up in three effective instructions. We could get away with
      -- two for intMax12bit < n < 3 * intMax12bit by recursing once. However,
      -- this way is likely less surprising.
      [ ANN desc (MOV ip (OpImm (ImmInt n))),
        ADD sp sp ip
      ]
  where
    desc = text "Move SP:" <+> int n

--
-- See Note [extra spill slots] in X86/Instr.hs
--
allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr
  -> UniqSM (NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top,[])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
    let entries = entryBlocks proc

    uniqs <- getUniquesM

    let
      delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where x = slots * spillSlotSize -- sp delta

      alloc   = mkStackAllocInstr   platform delta
      dealloc = mkStackDeallocInstr platform delta

      retargetList = zip entries (map mkBlockId uniqs)

      new_blockmap :: LabelMap BlockId
      new_blockmap = mapFromList retargetList

      insert_stack_insn (BasicBlock id insns)
        | Just new_blockid <- mapLookup id new_blockmap
        = [ BasicBlock id $ alloc ++ [ B (TBlock new_blockid) ]
          , BasicBlock new_blockid block' ]
        | otherwise
        = [ BasicBlock id block' ]
        where
          block' = foldr insert_dealloc [] insns

      insert_dealloc insn r = case insn of
        J _ -> dealloc ++ (insn : r)
        J_TBL {} -> dealloc ++ (insn : r)
        ANN _ e -> insert_dealloc e r
        _other | jumpDestsOfInstr insn /= []
            -> patchJumpInstr insn retarget : r
        _other -> insn : r

        where retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insn code
    return (CmmProc info lbl live (ListGraph new_code), retargetList)

-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

-- RV64 reference card: https://cs61c.org/sp23/pdfs/resources/reference-card.pdf
-- RV64 pseudo instructions: https://github.com/riscv-non-isa/riscv-asm-manual/blob/master/riscv-asm.md#-a-listing-of-standard-risc-v-pseudoinstructions
-- We will target: RV64G(C). That is G = I+A+F+S+D
-- I: Integer Multiplication and Division
-- A: Atomic Instructions
-- F: Single Precision
-- D: Double Precision
-- C: Compressed (though we won't use that).

-- This most notably leaves out B. (Bit Manipulation) instructions.

data Instr
    -- comment pseudo-op
    = COMMENT SDoc
    | MULTILINE_COMMENT SDoc

    -- Annotated instruction. Should print <instr> # <doc>
    | ANN SDoc Instr

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

    -- 0. Pseudo Instructions --------------------------------------------------
    | PUSH_STACK_FRAME
    | POP_STACK_FRAME

    -- == Base Instructions (I) ================================================
    -- 1. Arithmetic Instructions ----------------------------------------------
    -- all of these instructions can also take an immediate, in which case they
    -- hafe a suffix I (except for U suffix, where it's IU then. E.g. SLTIU).
    | ADD Operand Operand Operand -- rd = rs1 + rs2
    | SUB Operand Operand Operand -- rd = rs1 - rs2

    | AND Operand Operand Operand -- rd = rs1 & rs2
    | OR  Operand Operand Operand -- rd = rs1 | rs2
    -- | XOR Operand Operand Operand -- rd = rs1 ^ rs2
    | LSL {- SLL -} Operand Operand Operand -- rd = rs1 << rs2 (zero ext)
    | LSR {- SRL -} Operand Operand Operand -- rd = rs1 >> rs2 (zero ext)
    | ASR {- SRA -} Operand Operand Operand -- rd = rs1 >> rs2 (sign ext)

    -- 2. Memory Load/Store Instructions ---------------------------------------
    -- Unlike arm, we don't have register shorthands for size.
    -- We do however have {L,S}{B,H,W,D}[U] instructions for Load/Store, Byte, Half, Word, Double, (Unsigned).
    -- Reusing the arm logic with the _format_ specifier will hopefully work.
    | STR Format Operand Operand -- str Xn, address-mode // Xn -> *addr
    | LDR Format Operand Operand -- ldr Xn, address-mode // Xn <- *addr (sign-extended)
    | LDRU Format Operand Operand -- ldr Xn, address-mode // Xn <- *addr (unsigned)

    -- 3. Control Flow ---------------------------------------------------------
    -- B{EQ,GE,GEU,LT,LTU}, these are effectively BCOND from AArch64;
    -- however, AArch64 desugars them into CMP + BCOND. So these are a bit more
    -- powerful.
    -- JAL / JARL are effectively the BL instruction from AArch64.

    | MUL Operand Operand Operand -- rd = rn × rm


    -- Pseudo/synthesized:
    | NEG Operand Operand -- rd = -op2

    | DIV Operand Operand Operand -- rd = rn ÷ rm
    | REM Operand Operand Operand -- rd = rn % rm (signed)
    | REMU Operand Operand Operand -- rd = rn % rm (unsigned)

    -- TODO: Rename: MULH
    | SMULH Operand Operand Operand
    | DIVU Operand Operand Operand -- rd = rn ÷ rm

    -- 2. Bit Manipulation Instructions ----------------------------------------
    | SBFM Operand Operand Operand Operand -- rd = rn[i,j]
    | UBFM Operand Operand Operand Operand -- rd = rn[i,j]
    -- Signed/Unsigned bitfield extract
    | UBFX Operand Operand Operand Operand -- rd = rn[i,j]

    -- 3. Logical and Move Instructions ----------------------------------------
    -- | AND Operand Operand Operand -- rd = rn & op2
    -- | ANDS Operand Operand Operand -- rd = rn & op2
    -- | ASR Operand Operand Operand -- rd = rn ≫ rm  or  rd = rn ≫ #i, i is 6 bits
    -- TODO: unused
    | BIC Operand Operand Operand -- rd = rn & ~op2
    -- TODO: unused
    | BICS Operand Operand Operand -- rd = rn & ~op2
    | XOR Operand Operand Operand -- rd = rn ⊕ op2
    -- | LSL Operand Operand Operand -- rd = rn ≪ rm  or rd = rn ≪ #i, i is 6 bits
    -- | LSR Operand Operand Operand -- rd = rn ≫ rm  or rd = rn ≫ #i, i is 6 bits
    | MOV Operand Operand -- rd = rn  or  rd = #i
    | ORI Operand Operand Operand -- rd = rn | op2
    | XORI Operand Operand Operand -- rd = rn `xor` imm
    -- Load and stores.

    -- Conditional instructions
    -- This is a synthetic operation.
    | CSET Operand Operand Operand Cond   -- if(o2 cond o3) op <- 1 else op <- 0

    -- Branching.
    -- TODO: Unused
    | J Target            -- like B, but only generated from genJump. Used to distinguish genJumps from others.
    -- | A `J` instruction with data for switch jump tables
    | J_TBL [Maybe BlockId] (Maybe CLabel) Reg
    | B Target            -- unconditional branching b/br. (To a blockid, label or register)
    -- | pseudo-op for far branch targets
    | B_FAR BlockId
    | BL Target [Reg] [Reg] -- branch and link (e.g. set x30 to next pc, and branch)
    | BCOND Cond Operand Operand Target   -- branch with condition. b.<cond>
    -- | pseudo-op for far branch targets
    | BCOND_FAR Cond Operand Operand Target Target

    -- 8. Synchronization Instructions -----------------------------------------
    | DMBSY DmbType DmbType
    -- 9. Floating Point Instructions
    -- Float ConVerT
    | FCVT Operand Operand
    -- Signed ConVerT Float
    | SCVTF Operand Operand
    -- Float ConVerT to Zero Signed
    | FCVTZS Operand Operand
    -- Float ABSolute value
    | FABS Operand Operand

data DmbType = DmbRead | DmbWrite | DmbReadWrite

instrCon :: Instr -> String
instrCon i =
    case i of
      COMMENT{} -> "COMMENT"
      MULTILINE_COMMENT{} -> "COMMENT"
      ANN{} -> "ANN"
      LOCATION{} -> "LOCATION"
      LDATA{} -> "LDATA"
      NEWBLOCK{} -> "NEWBLOCK"
      DELTA{} -> "DELTA"
      PUSH_STACK_FRAME{} -> "PUSH_STACK_FRAME"
      POP_STACK_FRAME{} -> "POP_STACK_FRAME"
      ADD{} -> "ADD"
      OR{} -> "OR"
      MUL{} -> "MUL"
      NEG{} -> "NEG"
      DIV{} -> "DIV"
      REM{} -> "REM"
      REMU{} -> "REMU"
      SMULH{} -> "SMULH"
      SUB{} -> "SUB"
      DIVU{} -> "DIVU"
      SBFM{} -> "SBFM"
      UBFM{} -> "UBFM"
      UBFX{} -> "UBFX"
      AND{} -> "AND"
      ASR{} -> "ASR"
      BIC{} -> "BIC"
      BICS{} -> "BICS"
      XOR{} -> "XOR"
      LSL{} -> "LSL"
      LSR{} -> "LSR"
      MOV{} -> "MOV"
      ORI{} -> "ORI"
      XORI{} -> "ORI"
      STR{} -> "STR"
      LDR{} -> "LDR"
      LDRU{} -> "LDRU"
      CSET{} -> "CSET"
      J{} -> "J"
      J_TBL{} -> "J_TBL"
      B{} -> "B"
      B_FAR{} -> "B_FAR"
      BL{} -> "BL"
      BCOND{} -> "BCOND"
      BCOND_FAR{} -> "BCOND_FAR"
      DMBSY{} -> "DMBSY"
      FCVT{} -> "FCVT"
      SCVTF{} -> "SCVTF"
      FCVTZS{} -> "FCVTZS"
      FABS{} -> "FABS"

-- TODO: We don't need TLabel.
data Target
    = TBlock BlockId
    | TLabel CLabel
    | TReg   Reg

data Operand
  = -- | register
    OpReg Width Reg
  | -- | immediate value
    OpImm Imm
  | -- | memory reference
    OpAddr AddrMode
  deriving (Eq, Show)

operandFromReg :: Reg -> Operand
operandFromReg = OpReg W64

operandFromRegNo :: RegNo -> Operand
operandFromRegNo = operandFromReg . regSingle

zero, ra, sp, gp, tp, fp, ip :: Operand
zero = operandFromReg zeroReg
ra  = operandFromReg raReg
sp  = operandFromReg spMachReg
gp  = operandFromRegNo 3
tp  = operandFromRegNo 4
fp  = operandFromRegNo 8
ip = operandFromReg ipReg

x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7  :: Operand
x8,  x9,  x10, x11, x12, x13, x14, x15 :: Operand
x16, x17, x18, x19, x20, x21, x22, x23 :: Operand
x24, x25, x26, x27, x28, x29, x30, x31 :: Operand
x0  = operandFromRegNo  x0RegNo
x1  = operandFromRegNo  1
x2  = operandFromRegNo  2
x3  = operandFromRegNo  3
x4  = operandFromRegNo  4
x5  = operandFromRegNo  x5RegNo
x6  = operandFromRegNo  6
x7  = operandFromRegNo  x7RegNo
x8  = operandFromRegNo  8
x9  = operandFromRegNo  9
x10 = operandFromRegNo x10RegNo
x11 = operandFromRegNo 11
x12 = operandFromRegNo 12
x13 = operandFromRegNo 13
x14 = operandFromRegNo 14
x15 = operandFromRegNo 15
x16 = operandFromRegNo 16
x17 = operandFromRegNo x17RegNo
x18 = operandFromRegNo 18
x19 = operandFromRegNo 19
x20 = operandFromRegNo 20
x21 = operandFromRegNo 21
x22 = operandFromRegNo 22
x23 = operandFromRegNo 23
x24 = operandFromRegNo 24
x25 = operandFromRegNo 25
x26 = operandFromRegNo 26
x27 = operandFromRegNo 27
x28 = operandFromRegNo x28RegNo
x29 = operandFromRegNo 29
x30 = operandFromRegNo 30
x31 = operandFromRegNo x31RegNo

d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7  :: Operand
d8,  d9,  d10, d11, d12, d13, d14, d15 :: Operand
d16, d17, d18, d19, d20, d21, d22, d23 :: Operand
d24, d25, d26, d27, d28, d29, d30, d31 :: Operand
d0  = operandFromRegNo d0RegNo
d1  = operandFromRegNo 33
d2  = operandFromRegNo 34
d3  = operandFromRegNo 35
d4  = operandFromRegNo 36
d5  = operandFromRegNo 37
d6  = operandFromRegNo 38
d7  = operandFromRegNo d7RegNo
d8  = operandFromRegNo 40
d9  = operandFromRegNo 41
d10 = operandFromRegNo d10RegNo
d11 = operandFromRegNo 43
d12 = operandFromRegNo 44
d13 = operandFromRegNo 45
d14 = operandFromRegNo 46
d15 = operandFromRegNo 47
d16 = operandFromRegNo 48
d17 = operandFromRegNo d17RegNo
d18 = operandFromRegNo 50
d19 = operandFromRegNo 51
d20 = operandFromRegNo 52
d21 = operandFromRegNo 53
d22 = operandFromRegNo 54
d23 = operandFromRegNo 55
d24 = operandFromRegNo 56
d25 = operandFromRegNo 57
d26 = operandFromRegNo 58
d27 = operandFromRegNo 59
d28 = operandFromRegNo 60
d29 = operandFromRegNo 61
d30 = operandFromRegNo 62
d31 = operandFromRegNo d31RegNo

fitsIn12bitImm :: (Num a, Ord a) => a -> Bool
fitsIn12bitImm off = off >= intMin12bit && off <= intMax12bit

intMin12bit :: Num a => a
intMin12bit = -2048

intMax12bit :: Num a => a
intMax12bit = 2047

fitsIn32bits  :: (Num a, Ord a, Bits a) => a -> Bool
fitsIn32bits i = (-1 `shiftL` 31) <= i && i <= (1 `shiftL` 31 -1)

isNbitEncodeable :: Int -> Integer -> Bool
isNbitEncodeable n i = let shift = n - 1 in (-1 `shiftL` shift) <= i && i < (1 `shiftL` shift)

isEncodeableInWidth :: Width -> Integer -> Bool
isEncodeableInWidth = isNbitEncodeable . widthInBits

isIntOp :: Operand -> Bool
isIntOp = not . isFloatOp

isFloatOp :: Operand -> Bool
isFloatOp (OpReg _ reg) | isFloatReg reg = True
isFloatOp _ = False

isFloatReg :: Reg -> Bool
isFloatReg (RegReal (RealRegSingle i)) | i > 31 = True
isFloatReg (RegVirtual (VirtualRegF _)) = True
isFloatReg (RegVirtual (VirtualRegD _)) = True
isFloatReg _ = False


-- | Making far branches

-- Conditional branch instructions can target labels in a range of +/- 4 KiB.
-- The assembler can transform this into a J instruction targeting +/- 1MiB.
-- There are rare cases where this is not enough (e.g. the Happy-generated
-- @Parser.hs@.) We need to manually transform these into register based jumps
-- using @ip@ (register reserved for calculations.) The trick is to invert the
-- condition, do a far jump in the fall-through case or a short jump when the
-- (inverted) condition is true.
makeFarBranches ::
  LabelMap RawCmmStatics ->
  [NatBasicBlock Instr] ->
  [NatBasicBlock Instr]
makeFarBranches info_env blocks
  | NE.last blockAddresses < nearLimit = blocks
  | otherwise = zipWith handleBlock blockAddressList blocks
  where
    blockAddresses = NE.scanl (+) 0 $ map blockLen blocks
    blockAddressList = toList blockAddresses
    blockLen (BasicBlock _ instrs) = length instrs

    handleBlock addr (BasicBlock id instrs) =
      BasicBlock id (zipWith (makeFar id) [addr ..] instrs)

    makeFar :: BlockId -> Int -> Instr -> Instr
    makeFar bid addr orig@(BCOND cond op1 op2 tgt@(TBlock tgtBid))
      | abs (addr - targetAddr) >= nearLimit =
          annotate addr targetAddr $
            BCOND_FAR cond op1 op2 (TBlock bid) tgt
      | otherwise =
          annotate addr targetAddr orig
      where
        targetAddr = fromJust $ lookupUFM blockAddressMap tgtBid
    makeFar _bid addr orig@(B (TBlock tgtBid))
      | abs (addr - targetAddr) >= nearLimit =
          annotate addr targetAddr $
            B_FAR tgtBid
      | otherwise =
          annotate addr targetAddr orig
      where
        targetAddr = fromJust $ lookupUFM blockAddressMap tgtBid
    makeFar bid addr (ANN desc other) = ANN desc $ makeFar bid addr other
    makeFar _bid _ other = other

    -- 262144 (2^20 / 4) instructions are allowed; let's keep some distance, as
    -- we have pseudo-insns that are pretty-printed as multiple instructions,
    -- and it's just not worth the effort to calculate things exactly as linker
    -- relaxations are applied later (optimizing away our flaws.) The educated
    -- guess here is that every instruction does not emit more than two in the
    -- mean.
    nearLimit = 131072 - mapSize info_env * maxRetInfoTableSizeW

    blockAddressMap = listToUFM $ zip (map blockId blocks) blockAddressList

    -- We may want to optimize the limit in future. So, annotate the most
    -- important values of the decision.
    annotate :: Int -> Int -> Instr -> Instr
    annotate addr targetAddr instr =
      ANN
        ( text (instrCon instr)
            <+> text "targetAddr" <> colon
            <+> int targetAddr <> comma
            <+> text "offset" <> colon
            <+> int (addr - targetAddr) <> comma
            <+> text "nearLimit" <> colon
            <+> int nearLimit
        )
        instr
