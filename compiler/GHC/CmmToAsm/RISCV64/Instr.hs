{-# LANGUAGE EmptyCase #-}

module GHC.CmmToAsm.RISCV64.Instr where

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr hiding (isMetaInstr, jumpDestsOfInstr, patchRegsOfInstr, regUsageOfInstr, takeDeltaInstr)
import GHC.CmmToAsm.Types
import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Regs (freeReg)
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import Prelude

data Instr
  = -- comment pseudo-op
    COMMENT SDoc
  | MULTILINE_COMMENT SDoc
  | -- Annotated instruction. Should print <instr> # <doc>
    ANN SDoc Instr
  | -- specify current stack offset for
    -- benefit of subsequent passes
    DELTA Int
  | PUSH_STACK_FRAME
  | POP_STACK_FRAME
  | -- some static data spat out during code
    -- generation.  Will be extracted before
    -- pretty-printing.
    LDATA Section RawCmmStatics
  | -- start a new basic block.  Useful during
    -- codegen, removed later.  Preceding
    -- instruction should be a jump, as per the
    -- invariants for a BasicBlock (see Cmm).
    NEWBLOCK BlockId
  | -- load immediate pseudo-instruction
    LI Reg Integer
  | -- load address (label)
    LA Reg CLabel
  | -- jump pseudo-instruction
    J Target
  | -- call pseudo-instruction
    CALL CLabel
  | JALR Reg
  | -- copy register
    MV Reg Reg
  | FMV_S Reg Reg
  | FMV_D Reg Reg
  | FMV_D_X Reg Reg

data Target
  = TBlock BlockId
  | TReg Reg
  | TLabel CLabel
  deriving Show

allocMoreStack ::
  Int ->
  NatCmmDecl statics GHC.CmmToAsm.RISCV64.Instr.Instr ->
  UniqSM (NatCmmDecl statics GHC.CmmToAsm.RISCV64.Instr.Instr, [(BlockId, BlockId)])
allocMoreStack = error "TODO: allocMoreStack"

-- saved return address + previous fp
-- (https://pdos.csail.mit.edu/6.S081/2020/lec/l-riscv.txt)
stackFrameHeaderSize :: Int
stackFrameHeaderSize = 2 * spillSlotSize

-- | All registers are 8 byte wide.
spillSlotSize :: Int
spillSlotSize = 8

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config =
  --  = 0 -- set to zero, to see when allocMoreStack has to fire.
  ( (ncgSpillPreallocSize config - stackFrameHeaderSize)
      `div` spillSlotSize
  )
    - 1

makeFarBranches ::
  LabelMap RawCmmStatics ->
  [NatBasicBlock Instr] ->
  [NatBasicBlock Instr]
makeFarBranches _ _ = error "TODO: makeFarBranches"

-- | Get the registers that are being used by this instruction.
--      regUsage doesn't need to do any trickery for jumps and such.
--      Just state precisely the regs read and written by that insn.
--      The consequences of control flow transfers, as far as register
--      allocation goes, are taken care of by the register allocator.
regUsageOfInstr ::
  Platform ->
  Instr ->
  RegUsage
regUsageOfInstr platform instr = case instr of
  ANN _ i -> regUsageOfInstr platform i
  COMMENT {} -> none
  MULTILINE_COMMENT {} -> none
  LDATA {} -> none
  DELTA {} -> none
  NEWBLOCK {} -> none
  PUSH_STACK_FRAME -> none
  POP_STACK_FRAME -> none
  LI dst _ -> usage ([], [dst])
  LA dst _ -> usage ([], [dst])
  MV dst src -> usage ([src], [dst])
  FMV_S dst src -> usage ([src], [dst])
  FMV_D dst src -> usage ([src], [dst])
  FMV_D_X dst src -> usage ([src], [dst])
  -- Looks like J doesn't change registers (beside PC)
  -- This might be wrong.
  J {} -> none
  CALL {} -> usage([],[(RegReal . realRegSingle) 1]) -- call sets register x1 (ra)
  JALR reg -> usage([reg],[(RegReal . realRegSingle) 1]) -- call sets register x1 (ra)
  where
    none = usage ([], [])
    -- filtering the usage is necessary, otherwise the register
    -- allocator will try to allocate pre-defined fixed stg
    -- registers as well, as they show up.
    usage (src, dst) =
      RU
        (filter (interesting platform) src)
        (filter (interesting platform) dst)

    interesting :: Platform -> Reg -> Bool
    interesting _ (RegVirtual _) = True
    interesting platform (RegReal (RealRegSingle i)) = freeReg platform i

-- | Apply a given mapping to all the register references in this
--      instruction.
patchRegsOfInstr ::
  Instr ->
  (Reg -> Reg) ->
  Instr
patchRegsOfInstr instr env = case instr of
  ANN _ i -> patchRegsOfInstr i env
  COMMENT {} -> instr
  MULTILINE_COMMENT {} -> instr
  LDATA {} -> instr
  DELTA {} -> instr
  NEWBLOCK {} -> instr
  PUSH_STACK_FRAME {} -> instr
  POP_STACK_FRAME {} -> instr
  LI reg i -> LI (env reg) i
  LA reg i -> LA (env reg) i
  -- Looks like J doesn't change registers (beside PC)
  -- This might be wrong.
  J {} -> instr
  CALL {} -> instr
  JALR reg -> JALR (env reg)
  MV dst src -> MV (env dst) (env src)
  FMV_S dst src -> FMV_S (env dst) (env src)
  FMV_D dst src -> FMV_D (env dst) (env src)
  FMV_D_X dst src -> FMV_D_X (env dst) (env src)

-- | Checks whether this instruction is a jump/branch instruction.
--      One that can change the flow of control in a way that the
--      register allocator needs to worry about.
isJumpishInstr :: Instr -> Bool
isJumpishInstr COMMENT {} = False
isJumpishInstr MULTILINE_COMMENT {} = False
isJumpishInstr ANN {} = False
isJumpishInstr DELTA {} = False
isJumpishInstr LDATA {} = False
isJumpishInstr NEWBLOCK {} = False
isJumpishInstr MV {} = False
isJumpishInstr FMV_S {} = False
isJumpishInstr FMV_D {} = False
isJumpishInstr FMV_D_X {} = False
isJumpishInstr LA {} = False
isJumpishInstr LI {} = False
isJumpishInstr J {} = True
isJumpishInstr CALL {} = True
isJumpishInstr JALR {} = True

-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr (ANN _ i) = jumpDestsOfInstr i
jumpDestsOfInstr (J (TBlock t)) = [t]
jumpDestsOfInstr _ = []

-- | Change the destination of this jump instruction.
--      Used in the linear allocator when adding fixup blocks for join
--      points.
patchJumpInstr ::
  instr ->
  (BlockId -> BlockId) ->
  instr
patchJumpInstr _ _ = error "TODO: patchJumpInstr"

-- | An instruction to spill a register into a spill slot.
mkSpillInstr ::
  NCGConfig ->
  -- | the reg to spill
  Reg ->
  -- | the current stack delta
  Int ->
  -- | spill slot to use
  Int ->
  -- | instructions
  [instr]
mkSpillInstr _ _ _ _ = error "TODO: mkSpillInstr"

-- | An instruction to reload a register from a spill slot.
mkLoadInstr ::
  NCGConfig ->
  -- | the reg to reload.
  Reg ->
  -- | the current stack delta
  Int ->
  -- | the spill slot to use
  Int ->
  -- | instructions
  [instr]
mkLoadInstr _ _ _ _ = error "TODO: mkLoadInstr"

-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr :: Instr -> Maybe Int
takeDeltaInstr (ANN _ i) = takeDeltaInstr i
takeDeltaInstr (DELTA i) = Just i
takeDeltaInstr _ = Nothing

-- | Check whether this instruction is some meta thing inserted into
--      the instruction stream for other purposes.
--
--      Not something that has to be treated as a real machine instruction
--      and have its registers allocated.
--
--      eg, comments, delta, ldata, etc.
isMetaInstr :: Instr -> Bool
isMetaInstr instr =
  case instr of
    ANN _ i -> isMetaInstr i
    COMMENT {} -> True
    MULTILINE_COMMENT {} -> True
    LDATA {} -> True
    NEWBLOCK {} -> True
    DELTA {} -> True
    PUSH_STACK_FRAME -> True
    POP_STACK_FRAME -> True
    LI {} -> False
    LA {} -> False
    J {} -> False
    MV {} -> False
    FMV_S {} -> False
    FMV_D {} -> False
    FMV_D_X {} -> False
    CALL {} -> False
    JALR {} -> False

-- | Copy the value in a register to another one.
--      Must work for all register classes.
mkRegRegMoveInstr ::
  Platform ->
  -- | source register
  Reg ->
  -- | destination register
  Reg ->
  instr
mkRegRegMoveInstr _ _ _ = error "TODO: mkRegRegMoveInstr"

-- | Take the source and destination from this reg -> reg move instruction
--      or Nothing if it's not one
takeRegRegMoveInstr :: Instr -> Maybe (Reg, Reg)
takeRegRegMoveInstr COMMENT {} = Nothing
takeRegRegMoveInstr MULTILINE_COMMENT {} = Nothing
takeRegRegMoveInstr ANN {} = Nothing
takeRegRegMoveInstr DELTA {} = Nothing
takeRegRegMoveInstr LDATA {} = Nothing
takeRegRegMoveInstr NEWBLOCK {} = Nothing
takeRegRegMoveInstr PUSH_STACK_FRAME {} = Nothing
takeRegRegMoveInstr POP_STACK_FRAME {} = Nothing
takeRegRegMoveInstr LI {} = Nothing
takeRegRegMoveInstr LA {} = Nothing
takeRegRegMoveInstr J {} = Nothing
takeRegRegMoveInstr (MV dst src) = Just (src, dst)
takeRegRegMoveInstr (FMV_S dst src) = Just (src, dst)
takeRegRegMoveInstr (FMV_D dst src) = Just (src, dst)
takeRegRegMoveInstr (FMV_D_X _ _) = Nothing -- Just (src, dst)
takeRegRegMoveInstr CALL {} = Nothing
takeRegRegMoveInstr JALR {} = Nothing

-- | Make an unconditional jump instruction.
--      For architectures with branch delay slots, its ok to put
--      a NOP after the jump. Don't fill the delay slot with an
--      instruction that references regs or you'll confuse the
--      linear allocator.
mkJumpInstr ::
  BlockId ->
  [Instr]
mkJumpInstr id = [J (TBlock id)]

-- Subtract an amount from the C stack pointer
mkStackAllocInstr ::
  Platform ->
  Int ->
  [instr]
mkStackAllocInstr _ _ = error "TODO: mkStackAllocInstr"

-- Add an amount to the C stack pointer
mkStackDeallocInstr ::
  Platform ->
  Int ->
  [instr]
mkStackDeallocInstr _ _ = error "TODO: mkStackDeallocInstr"
