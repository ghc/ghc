-- All instructions will be rendered eventually. Thus, there's no benefit in
-- being lazy in data types.
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.CmmToAsm.RV64.Instr where

import Data.Maybe
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Instr (RegUsage (..))
import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.RV64.Regs
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.Data.FastString (LexicalFastString)
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs
import GHC.Platform.Reg.Class.Separate
import GHC.Prelude
import GHC.Stack
import GHC.Types.Unique.DSM
import GHC.Utils.Outputable
import GHC.Utils.Panic

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

-- | The number of bytes that the stack pointer should be aligned to.
stackAlign :: Int
stackAlign = 16

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config =
  ( (ncgSpillPreallocSize config - stackFrameHeaderSize)
      `div` spillSlotSize
  )
    - 1

-- | Convert a spill slot number to a *byte* offset.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot =
  stackFrameHeaderSize + spillSlotSize * slot

instance Outputable RegUsage where
  ppr (RU reads writes) = text "RegUsage(reads:" <+> ppr reads <> comma <+> text "writes:" <+> ppr writes <> char ')'

-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
-- RegUsage = RU [<read regs>] [<write regs>]
regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr = case instr of
  ANN _ i -> regUsageOfInstr platform i
  COMMENT {} -> usage ([], [])
  MULTILINE_COMMENT {} -> usage ([], [])
  PUSH_STACK_FRAME -> usage ([], [])
  POP_STACK_FRAME -> usage ([], [])
  LOCATION {} -> usage ([], [])
  DELTA {} -> usage ([], [])
  ADD dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  MUL dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  NEG dst src -> usage (regOp src, regOp dst)
  MULH dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIV dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  REM dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  REMU dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  SUB dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIVU dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  AND dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  OR dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  SRA dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  XOR dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  SLL dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  SRL dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  MOV dst src -> usage (regOp src, regOp dst)
  -- ORI's third operand is always an immediate
  ORI dst src1 _ -> usage (regOp src1, regOp dst)
  XORI dst src1 _ -> usage (regOp src1, regOp dst)
  J_TBL _ _ t -> usage ([t], [])
  J t -> usage (regTarget t, [])
  B t -> usage (regTarget t, [])
  BCOND _ l r t -> usage (regTarget t ++ regOp l ++ regOp r, [])
  BL t ps -> usage (t : ps, callerSavedRegisters)
  CSET dst l r _ -> usage (regOp l ++ regOp r, regOp dst)
  STR _ src dst -> usage (regOp src ++ regOp dst, [])
  LDR _ dst src -> usage (regOp src, regOp dst)
  LDRU _ dst src -> usage (regOp src, regOp dst)
  FENCE _ _ -> usage ([], [])
  FCVT _variant dst src -> usage (regOp src, regOp dst)
  FABS dst src -> usage (regOp src, regOp dst)
  FMIN dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  FMAX dst src1 src2 -> usage (regOp src1 ++ regOp src2, regOp dst)
  FMA _ dst src1 src2 src3 ->
    usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  _ -> panic $ "regUsageOfInstr: " ++ instrCon instr
  where
    -- filtering the usage is necessary, otherwise the register
    -- allocator will try to allocate pre-defined fixed stg
    -- registers as well, as they show up.
    usage :: ([Reg], [Reg]) -> RegUsage
    usage (srcRegs, dstRegs) =
      RU
        (map mkFmt $ filter (interesting platform) srcRegs)
        (map mkFmt $ filter (interesting platform) dstRegs)

      -- SIMD NCG TODO: the format here is used for register spilling/unspilling.
      -- As the RISCV64 NCG does not currently support SIMD registers,
      -- this simple logic is OK.
    mkFmt r = RegWithFormat r fmt
      where
        fmt = case cls of
                RcInteger -> II64
                RcFloat   -> FF64
                RcVector  -> sorry "The RISCV64 NCG does not (yet) support vectors; please use -fllvm."
        cls = case r of
                RegVirtual vr -> classOfVirtualReg (platformArch platform) vr
                RegReal rr -> classOfRealReg rr

    regAddr :: AddrMode -> [Reg]
    regAddr (AddrRegImm r1 _imm) = [r1]
    regAddr (AddrReg r1) = [r1]

    regOp :: Operand -> [Reg]
    regOp (OpReg _w r1) = [r1]
    regOp (OpAddr a) = regAddr a
    regOp (OpImm _imm) = []

    regTarget :: Target -> [Reg]
    regTarget (TBlock _bid) = []
    regTarget (TReg r1) = [r1]

    -- Is this register interesting for the register allocator?
    interesting :: Platform -> Reg -> Bool
    interesting _ (RegVirtual _) = True
    interesting platform (RegReal (RealRegSingle i)) = freeReg platform i

-- | Caller-saved registers (according to calling convention)
--
-- These registers may be clobbered after a jump.
callerSavedRegisters :: [Reg]
callerSavedRegisters =
  [regSingle raRegNo]
    ++ map regSingle [t0RegNo .. t2RegNo]
    ++ map regSingle [a0RegNo .. a7RegNo]
    ++ map regSingle [t3RegNo .. t6RegNo]
    ++ map regSingle [ft0RegNo .. ft7RegNo]
    ++ map regSingle [fa0RegNo .. fa7RegNo]

-- | Apply a given mapping to all the register references in this instruction.
patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr instr env = case instr of
  ANN d i -> ANN d (patchRegsOfInstr i env)
  COMMENT {} -> instr
  MULTILINE_COMMENT {} -> instr
  PUSH_STACK_FRAME -> instr
  POP_STACK_FRAME -> instr
  LOCATION {} -> instr
  DELTA {} -> instr
  ADD o1 o2 o3 -> ADD (patchOp o1) (patchOp o2) (patchOp o3)
  MUL o1 o2 o3 -> MUL (patchOp o1) (patchOp o2) (patchOp o3)
  NEG o1 o2 -> NEG (patchOp o1) (patchOp o2)
  MULH o1 o2 o3 -> MULH (patchOp o1) (patchOp o2) (patchOp o3)
  DIV o1 o2 o3 -> DIV (patchOp o1) (patchOp o2) (patchOp o3)
  REM o1 o2 o3 -> REM (patchOp o1) (patchOp o2) (patchOp o3)
  REMU o1 o2 o3 -> REMU (patchOp o1) (patchOp o2) (patchOp o3)
  SUB o1 o2 o3 -> SUB (patchOp o1) (patchOp o2) (patchOp o3)
  DIVU o1 o2 o3 -> DIVU (patchOp o1) (patchOp o2) (patchOp o3)
  AND o1 o2 o3 -> AND (patchOp o1) (patchOp o2) (patchOp o3)
  OR o1 o2 o3 -> OR (patchOp o1) (patchOp o2) (patchOp o3)
  SRA o1 o2 o3 -> SRA (patchOp o1) (patchOp o2) (patchOp o3)
  XOR o1 o2 o3 -> XOR (patchOp o1) (patchOp o2) (patchOp o3)
  SLL o1 o2 o3 -> SLL (patchOp o1) (patchOp o2) (patchOp o3)
  SRL o1 o2 o3 -> SRL (patchOp o1) (patchOp o2) (patchOp o3)
  MOV o1 o2 -> MOV (patchOp o1) (patchOp o2)
  -- o3 cannot be a register for ORI (always an immediate)
  ORI o1 o2 o3 -> ORI (patchOp o1) (patchOp o2) (patchOp o3)
  XORI o1 o2 o3 -> XORI (patchOp o1) (patchOp o2) (patchOp o3)
  J_TBL ids mbLbl t -> J_TBL ids mbLbl (env t)
  J t -> J (patchTarget t)
  B t -> B (patchTarget t)
  BL t ps -> BL (patchReg t) ps
  BCOND c o1 o2 t -> BCOND c (patchOp o1) (patchOp o2) (patchTarget t)
  CSET o l r c -> CSET (patchOp o) (patchOp l) (patchOp r) c
  STR f o1 o2 -> STR f (patchOp o1) (patchOp o2)
  LDR f o1 o2 -> LDR f (patchOp o1) (patchOp o2)
  LDRU f o1 o2 -> LDRU f (patchOp o1) (patchOp o2)
  FENCE o1 o2 -> FENCE o1 o2
  FCVT variant o1 o2 -> FCVT variant (patchOp o1) (patchOp o2)
  FABS o1 o2 -> FABS (patchOp o1) (patchOp o2)
  FMIN o1 o2 o3 -> FMIN (patchOp o1) (patchOp o2) (patchOp o3)
  FMAX o1 o2 o3 -> FMAX (patchOp o1) (patchOp o2) (patchOp o3)
  FMA s o1 o2 o3 o4 ->
    FMA s (patchOp o1) (patchOp o2) (patchOp o3) (patchOp o4)
  _ -> panic $ "patchRegsOfInstr: " ++ instrCon instr
  where
    patchOp :: Operand -> Operand
    patchOp (OpReg w r) = OpReg w (env r)
    patchOp (OpAddr a) = OpAddr (patchAddr a)
    patchOp opImm = opImm

    patchTarget :: Target -> Target
    patchTarget (TReg r) = TReg (env r)
    patchTarget tBlock = tBlock

    patchAddr :: AddrMode -> AddrMode
    patchAddr (AddrRegImm r1 imm) = AddrRegImm (env r1) imm
    patchAddr (AddrReg r) = AddrReg (env r)

    patchReg :: Reg -> Reg
    patchReg = env

-- | Checks whether this instruction is a jump/branch instruction.
--
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
isJumpishInstr :: Instr -> Bool
isJumpishInstr instr = case instr of
  ANN _ i -> isJumpishInstr i
  J_TBL {} -> True
  J {} -> True
  B {} -> True
  BL {} -> True
  BCOND {} -> True
  _ -> False

canFallthroughTo :: Instr -> BlockId -> Bool
canFallthroughTo insn bid =
  case insn of
    J (TBlock target) -> bid == target
    B (TBlock target) -> bid == target
    BCOND _ _ _ (TBlock target) -> bid == target
    J_TBL targets _ _ -> all isTargetBid targets
    _ -> False
  where
    isTargetBid target = case target of
      Nothing -> True
      Just target -> target == bid

-- | Get the `BlockId`s of the jump destinations (if any)
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr (ANN _ i) = jumpDestsOfInstr i
jumpDestsOfInstr (J_TBL ids _mbLbl _r) = catMaybes ids
jumpDestsOfInstr (J t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (B t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BCOND _ _ _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr _ = []

-- | Change the destination of this (potential) jump instruction.
--
-- Used in the linear allocator when adding fixup blocks for join
-- points.
patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
patchJumpInstr instr patchF =
  case instr of
    ANN d i -> ANN d (patchJumpInstr i patchF)
    J_TBL ids mbLbl r -> J_TBL (map (fmap patchF) ids) mbLbl r
    J (TBlock bid) -> J (TBlock (patchF bid))
    B (TBlock bid) -> B (TBlock (patchF bid))
    BCOND c o1 o2 (TBlock bid) -> BCOND c o1 o2 (TBlock (patchF bid))
    _ -> panic $ "patchJumpInstr: " ++ instrCon instr

-- -----------------------------------------------------------------------------
-- Note [RISCV64 Spills and Reloads]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We reserve @RESERVED_C_STACK_BYTES@ on the C stack for spilling and reloading
-- registers. The load and store instructions of RISCV64 address with a signed
-- 12-bit immediate + a register; machine stackpointer (sp/x2) in this case.
--
-- The @RESERVED_C_STACK_BYTES@ is 16k, so we can't always address into it in a
-- single load/store instruction. There are offsets to sp (not to be confused
-- with STG's SP!) which need a register to be calculated.
--
-- Using sp to compute the offset would violate assumptions about the stack pointer
-- pointing to the top of the stack during signal handling.  As we can't force
-- every signal to use its own stack, we have to ensure that the stack pointer
-- always points to the top of the stack, and we can't use it for computation.
--
-- So, we reserve one register (TMP) for this purpose (and other, unrelated
-- intermediate operations.) See Note [The made-up RISCV64 TMP (IP) register]

-- | Generate instructions to spill a register into a spill slot.
mkSpillInstr ::
  (HasCallStack) =>
  NCGConfig ->
  -- | register to spill
  RegWithFormat ->
  -- | current stack delta
  Int ->
  -- | spill slot to use
  Int ->
  [Instr]
mkSpillInstr _config (RegWithFormat reg _fmt) delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkStrSpImm imm]
    imm ->
      [ movImmToTmp imm,
        addSpToTmp,
        mkStrTmp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < d0RegNo -> II64
      _ -> FF64
    mkStrSpImm imm =
      ANN (text "Spill@" <> int (off - delta))
        $ STR fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToTmp imm =
      ANN (text "Spill: TMP <- " <> int imm)
        $ MOV tmp (OpImm (ImmInt imm))
    addSpToTmp =
      ANN (text "Spill: TMP <- SP + TMP ")
        $ ADD tmp tmp sp
    mkStrTmp =
      ANN (text "Spill@" <> int (off - delta))
        $ STR fmt (OpReg W64 reg) (OpAddr (AddrReg tmpReg))

    off = spillSlotToOffset slot

-- | Generate instructions to load a register from a spill slot.
mkLoadInstr ::
  NCGConfig ->
  -- | register to load
  RegWithFormat ->
  -- | current stack delta
  Int ->
  -- | spill slot to use
  Int ->
  [Instr]
mkLoadInstr _config (RegWithFormat reg _fmt) delta slot =
  case off - delta of
    imm | fitsIn12bitImm imm -> [mkLdrSpImm imm]
    imm ->
      [ movImmToTmp imm,
        addSpToTmp,
        mkLdrTmp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < d0RegNo -> II64
      _ -> FF64
    mkLdrSpImm imm =
      ANN (text "Reload@" <> int (off - delta))
        $ LDR fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToTmp imm =
      ANN (text "Reload: TMP <- " <> int imm)
        $ MOV tmp (OpImm (ImmInt imm))
    addSpToTmp =
      ANN (text "Reload: TMP <- SP + TMP ")
        $ ADD tmp tmp sp
    mkLdrTmp =
      ANN (text "Reload@" <> int (off - delta))
        $ LDR fmt (OpReg W64 reg) (OpAddr (AddrReg tmpReg))

    off = spillSlotToOffset slot

-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr :: Instr -> Maybe Int
takeDeltaInstr (ANN _ i) = takeDeltaInstr i
takeDeltaInstr (DELTA i) = Just i
takeDeltaInstr _ = Nothing

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
      [ ANN desc (MOV tmp (OpImm (ImmInt n))),
        ADD sp sp tmp
      ]
  where
    desc = text "Move SP:" <+> int n

--
-- See Note [extra spill slots] in X86/Instr.hs
--
allocMoreStack ::
  Platform ->
  Int ->
  NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr ->
  UniqDSM (NatCmmDecl statics GHC.CmmToAsm.RV64.Instr.Instr, [(BlockId, BlockId)])
allocMoreStack _ _ top@(CmmData _ _) = return (top, [])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
  let entries = entryBlocks proc

  retargetList <- mapM (\e -> (e,) <$> newBlockId) entries

  let delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
        where
          x = slots * spillSlotSize -- sp delta
      alloc = mkStackAllocInstr platform delta
      dealloc = mkStackDeallocInstr platform delta

      new_blockmap :: LabelMap BlockId
      new_blockmap = mapFromList retargetList

      insert_stack_insn (BasicBlock id insns)
        | Just new_blockid <- mapLookup id new_blockmap =
            [ BasicBlock id $ alloc ++ [B (TBlock new_blockid)],
              BasicBlock new_blockid block'
            ]
        | otherwise =
            [BasicBlock id block']
        where
          block' = foldr insert_dealloc [] insns

      insert_dealloc insn r = case insn of
        J {} -> dealloc ++ (insn : r)
        ANN _ e -> insert_dealloc e r
        _other
          | jumpDestsOfInstr insn /= [] ->
              patchJumpInstr insn retarget : r
        _other -> insn : r
        where
          retarget b = fromMaybe b (mapLookup b new_blockmap)

      new_code = concatMap insert_stack_insn code
  return (CmmProc info lbl live (ListGraph new_code), retargetList)

data Instr
  = -- | Comment pseudo-op
    COMMENT SDoc
  | -- | Multi-line comment pseudo-op
    MULTILINE_COMMENT SDoc
  | -- | Annotated instruction. Should print <instr> # <doc>
    ANN SDoc Instr
  | -- | Location pseudo-op @.loc@ (file, line, col, name)
    LOCATION Int Int Int LexicalFastString
  | -- | Static data spat out during code generation.
    LDATA Section RawCmmStatics
  | -- | Start a new basic block.
    --
    -- Useful during codegen, removed later. Preceding instruction should be a
    -- jump, as per the invariants for a BasicBlock (see Cmm).
    NEWBLOCK BlockId
  | -- | Specify current stack offset for benefit of subsequent passes
    DELTA Int
  | -- | Push a minimal stack frame consisting of the return address (RA) and the frame pointer (FP).
    PUSH_STACK_FRAME
  | -- | Pop the minimal stack frame of prior `PUSH_STACK_FRAME`.
    POP_STACK_FRAME
  | -- | Arithmetic addition (both integer and floating point)
    --
    -- @rd = rs1 + rs2@
    ADD Operand Operand Operand
  | -- | Arithmetic subtraction (both integer and floating point)
    --
    -- @rd = rs1 - rs2@
    SUB Operand Operand Operand
  | -- | Logical AND (integer only)
    --
    -- @rd = rs1 & rs2@
    AND Operand Operand Operand
  | -- | Logical OR (integer only)
    --
    -- @rd = rs1 | rs2@
    OR Operand Operand Operand
  | -- | Logical left shift (zero extened, integer only)
    --
    -- @rd = rs1 << rs2@
    SLL Operand Operand Operand
  | -- | Logical right shift (zero extened, integer only)
    --
    -- @rd = rs1 >> rs2@
    SRL Operand Operand Operand
  | -- | Arithmetic right shift (sign-extened, integer only)
    --
    -- @rd = rs1 >> rs2@
    SRA Operand Operand Operand
  | -- | Store to memory (both, integer and floating point)
    STR Format Operand Operand
  | -- | Load from memory (sign-extended, integer and floating point)
    LDR Format Operand Operand
  | -- | Load from memory (unsigned, integer and floating point)
    LDRU Format Operand Operand
  | -- | Arithmetic multiplication (both, integer and floating point)
    --
    -- @rd = rn × rm@
    MUL Operand Operand Operand
  | -- | Negation (both, integer and floating point)
    --
    -- @rd = -op2@
    NEG Operand Operand
  | -- | Division (both, integer and floating point)
    --
    -- @rd = rn ÷ rm@
    DIV Operand Operand Operand
  | -- | Remainder (integer only, signed)
    --
    -- @rd = rn % rm@
    REM Operand Operand Operand --
  | -- | Remainder (integer only, unsigned)
    --
    -- @rd = |rn % rm|@
    REMU Operand Operand Operand
  | -- | High part of a multiplication that doesn't fit into 64bits (integer only)
    --
    -- E.g. for a multiplication with 64bits width: @rd = (rs1 * rs2) >> 64@.
    MULH Operand Operand Operand
  | -- | Unsigned division (integer only)
    --
    -- @rd = |rn ÷ rm|@
    DIVU Operand Operand Operand
  | -- | XOR (integer only)
    --
    -- @rd = rn ⊕ op2@
    XOR Operand Operand Operand
  | -- | ORI with immediate (integer only)
    --
    -- @rd = rn | op2@
    ORI Operand Operand Operand
  | -- | OR with immediate (integer only)
    --
    -- @rd = rn ⊕ op2@
    XORI Operand Operand Operand
  | -- | Move to register (integer and floating point)
    --
    -- @rd = rn@  or  @rd = #imm@
    MOV Operand Operand
  | -- | Pseudo-op for conditional setting of a register.
    --
    -- @if(o2 cond o3) op <- 1 else op <- 0@
    CSET Operand Operand Operand Cond
    -- | Like B, but only used for non-local jumps. Used to distinguish genJumps from others.
  | J Target
  | -- | A jump instruction with data for switch/jump tables
    J_TBL [Maybe BlockId] (Maybe CLabel) Reg
  | -- | Unconditional jump (no linking)
    B Target
  | -- | Unconditional jump, links return address (sets @ra@/@x1@)
    BL Reg [Reg]
  | -- | branch with condition (integer only)
    BCOND Cond Operand Operand Target
  | -- | Fence instruction
    --
    -- Memory barrier.
    FENCE FenceType FenceType
  | -- | Floating point conversion
    FCVT FcvtVariant Operand Operand
  | -- | Floating point ABSolute value
    FABS Operand Operand

  | -- | Min
    -- dest = min(r1)
    FMIN Operand Operand Operand
  | -- | Max
    FMAX Operand Operand Operand

  | -- | Floating-point fused multiply-add instructions
    --
    -- - fmadd : d =   r1 * r2 + r3
    -- - fnmsub: d =   r1 * r2 - r3
    -- - fmsub : d = - r1 * r2 + r3
    -- - fnmadd: d = - r1 * r2 - r3
    FMA FMASign Operand Operand Operand Operand

-- | Operand of a FENCE instruction (@r@, @w@ or @rw@)
data FenceType = FenceRead | FenceWrite | FenceReadWrite

-- | Variant of a floating point conversion instruction
data FcvtVariant = FloatToFloat | IntToFloat | FloatToInt

instrCon :: Instr -> String
instrCon i =
  case i of
    COMMENT {} -> "COMMENT"
    MULTILINE_COMMENT {} -> "COMMENT"
    ANN {} -> "ANN"
    LOCATION {} -> "LOCATION"
    LDATA {} -> "LDATA"
    NEWBLOCK {} -> "NEWBLOCK"
    DELTA {} -> "DELTA"
    PUSH_STACK_FRAME {} -> "PUSH_STACK_FRAME"
    POP_STACK_FRAME {} -> "POP_STACK_FRAME"
    ADD {} -> "ADD"
    OR {} -> "OR"
    MUL {} -> "MUL"
    NEG {} -> "NEG"
    DIV {} -> "DIV"
    REM {} -> "REM"
    REMU {} -> "REMU"
    MULH {} -> "MULH"
    SUB {} -> "SUB"
    DIVU {} -> "DIVU"
    AND {} -> "AND"
    SRA {} -> "SRA"
    XOR {} -> "XOR"
    SLL {} -> "SLL"
    SRL {} -> "SRL"
    MOV {} -> "MOV"
    ORI {} -> "ORI"
    XORI {} -> "ORI"
    STR {} -> "STR"
    LDR {} -> "LDR"
    LDRU {} -> "LDRU"
    CSET {} -> "CSET"
    J_TBL {} -> "J_TBL"
    J {} -> "J"
    B {} -> "B"
    BL {} -> "BL"
    BCOND {} -> "BCOND"
    FENCE {} -> "FENCE"
    FCVT {} -> "FCVT"
    FABS {} -> "FABS"
    FMIN {} -> "FMIN"
    FMAX {} -> "FMAX"
    FMA variant _ _ _ _ ->
      case variant of
        FMAdd -> "FMADD"
        FMSub -> "FMSUB"
        FNMAdd -> "FNMADD"
        FNMSub -> "FNMSUB"

data Target
  = TBlock BlockId
  | TReg Reg

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

zero, ra, sp, gp, tp, fp, tmp :: Operand
zero = operandFromReg zeroReg
ra = operandFromReg raReg
sp = operandFromReg spMachReg
gp = operandFromRegNo 3
tp = operandFromRegNo 4
fp = operandFromRegNo 8
tmp = operandFromReg tmpReg

x0, x1, x2, x3, x4, x5, x6, x7 :: Operand
x8, x9, x10, x11, x12, x13, x14, x15 :: Operand
x16, x17, x18, x19, x20, x21, x22, x23 :: Operand
x24, x25, x26, x27, x28, x29, x30, x31 :: Operand
x0 = operandFromRegNo x0RegNo
x1 = operandFromRegNo 1
x2 = operandFromRegNo 2
x3 = operandFromRegNo 3
x4 = operandFromRegNo 4
x5 = operandFromRegNo x5RegNo
x6 = operandFromRegNo 6
x7 = operandFromRegNo x7RegNo

x8 = operandFromRegNo 8

x9 = operandFromRegNo 9

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

d0, d1, d2, d3, d4, d5, d6, d7 :: Operand
d8, d9, d10, d11, d12, d13, d14, d15 :: Operand
d16, d17, d18, d19, d20, d21, d22, d23 :: Operand
d24, d25, d26, d27, d28, d29, d30, d31 :: Operand
d0 = operandFromRegNo d0RegNo
d1 = operandFromRegNo 33
d2 = operandFromRegNo 34
d3 = operandFromRegNo 35
d4 = operandFromRegNo 36
d5 = operandFromRegNo 37
d6 = operandFromRegNo 38
d7 = operandFromRegNo d7RegNo

d8 = operandFromRegNo 40

d9 = operandFromRegNo 41

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

intMin12bit :: (Num a) => a
intMin12bit = -2048

intMax12bit :: (Num a) => a
intMax12bit = 2047

fitsIn32bits :: (Num a, Ord a, Bits a) => a -> Bool
fitsIn32bits i = (-1 `shiftL` 31) <= i && i <= (1 `shiftL` 31 - 1)

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
isFloatReg (RegVirtual (VirtualRegD _)) = True
isFloatReg _ = False
