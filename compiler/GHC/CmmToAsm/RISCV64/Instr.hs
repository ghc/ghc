module GHC.CmmToAsm.RISCV64.Instr where
import GHC.Data.FastString
import GHC.CmmToAsm.Types
import GHC.Cmm.BlockId
import GHC.Types.Unique.Supply
import GHC.Cmm.Dataflow.Label
import Prelude
import GHC.Platform
import GHC.Utils.Outputable
import GHC.Platform.Reg
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr
import GHC.Cmm

data Instr
    -- comment pseudo-op
    = COMMENT FastString
    -- some static data spat out during code
    -- generation.  Will be extracted before
    -- pretty-printing.
    | LDATA   Section RawCmmStatics

    -- start a new basic block.  Useful during
    -- codegen, removed later.  Preceding
    -- instruction should be a jump, as per the
    -- invariants for a BasicBlock (see Cmm).
    | NEWBLOCK BlockId


allocMoreStack ::
   Int
  -> NatCmmDecl statics GHC.CmmToAsm.RISCV64.Instr.Instr
  -> UniqSM (NatCmmDecl statics GHC.CmmToAsm.RISCV64.Instr.Instr, [(BlockId,BlockId)])
allocMoreStack = error "TODO: allocMoreStack"

maxSpillSlots :: Int
maxSpillSlots = error "TODO: maxSpillSlots"

makeFarBranches
        :: LabelMap RawCmmStatics
        -> [NatBasicBlock Instr]
        -> [NatBasicBlock Instr]
makeFarBranches _ _ = error "TODO: makeFarBranches"

-- | Get the registers that are being used by this instruction.
--      regUsage doesn't need to do any trickery for jumps and such.
--      Just state precisely the regs read and written by that insn.
--      The consequences of control flow transfers, as far as register
--      allocation goes, are taken care of by the register allocator.
--
regUsageOfInstr
        :: Platform
        -> instr
        -> RegUsage
regUsageOfInstr _ _ = error "TODO: regUsageOfInstr"

-- | Apply a given mapping to all the register references in this
--      instruction.
patchRegsOfInstr
        :: instr
        -> (Reg -> Reg)
        -> instr
patchRegsOfInstr _ _ = error "TODO: patchRegsOfInstr"

-- | Checks whether this instruction is a jump/branch instruction.
--      One that can change the flow of control in a way that the
--      register allocator needs to worry about.
isJumpishInstr
        :: instr -> Bool
isJumpishInstr _ = error "TODO: isJumpishInstr"

-- | Give the possible destinations of this jump instruction.
--      Must be defined for all jumpish instructions.
jumpDestsOfInstr
        :: instr -> [BlockId]
jumpDestsOfInstr _ = error "TODO: jumpDestsOfInstr"

-- | Change the destination of this jump instruction.
--      Used in the linear allocator when adding fixup blocks for join
--      points.
patchJumpInstr
        :: instr
        -> (BlockId -> BlockId)
        -> instr
patchJumpInstr _ _ = error "TODO: patchJumpInstr"

-- | An instruction to spill a register into a spill slot.
mkSpillInstr
        :: NCGConfig
        -> Reg          -- ^ the reg to spill
        -> Int          -- ^ the current stack delta
        -> Int          -- ^ spill slot to use
        -> [instr]        -- ^ instructions
mkSpillInstr _ _ _ _ = error "TODO: mkSpillInstr"

-- | An instruction to reload a register from a spill slot.
mkLoadInstr
        :: NCGConfig
        -> Reg          -- ^ the reg to reload.
        -> Int          -- ^ the current stack delta
        -> Int          -- ^ the spill slot to use
        -> [instr]        -- ^ instructions
mkLoadInstr _ _ _ _ = error "TODO: mkLoadInstr"

-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr
        :: instr
        -> Maybe Int
takeDeltaInstr _ = error "TODO: takeDeltaInstr"

-- | Check whether this instruction is some meta thing inserted into
--      the instruction stream for other purposes.
--
--      Not something that has to be treated as a real machine instruction
--      and have its registers allocated.
--
--      eg, comments, delta, ldata, etc.
isMetaInstr
        :: instr
        -> Bool
isMetaInstr _ = error "TODO: isMetaInstr"


-- | Copy the value in a register to another one.
--      Must work for all register classes.
mkRegRegMoveInstr
        :: Platform
        -> Reg          -- ^ source register
        -> Reg          -- ^ destination register
        -> instr
mkRegRegMoveInstr _ _ _ = error "TODO: mkRegRegMoveInstr"

-- | Take the source and destination from this reg -> reg move instruction
--      or Nothing if it's not one
takeRegRegMoveInstr
        :: instr
        -> Maybe (Reg, Reg)
takeRegRegMoveInstr _ = error "TODO: takeRegRegMoveInstr"

-- | Make an unconditional jump instruction.
--      For architectures with branch delay slots, its ok to put
--      a NOP after the jump. Don't fill the delay slot with an
--      instruction that references regs or you'll confuse the
--      linear allocator.
mkJumpInstr
        :: BlockId
        -> [instr]
mkJumpInstr _ = error "TODO: mkJumpInstr"

-- Subtract an amount from the C stack pointer
mkStackAllocInstr
        :: Platform
        -> Int
        -> [instr]
mkStackAllocInstr _ _ = error "TODO: mkStackAllocInstr"

-- Add an amount to the C stack pointer
mkStackDeallocInstr
        :: Platform
        -> Int
        -> [instr]
mkStackDeallocInstr _ _ = error "TODO: mkStackDeallocInstr"

-- | Pretty-print an instruction
pprInstr :: Platform -> instr -> SDoc
pprInstr _ _ = error "TODO: pprInstr"

-- Create a comment instruction
mkComment :: FastString -> [instr]
mkComment _ = error "mkComment"
