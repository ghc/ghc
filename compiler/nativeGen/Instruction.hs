
module Instruction (
        RegUsage(..),
        noUsage,
        GenBasicBlock(..), blockId,
        ListGraph(..),
        NatCmm,
        NatCmmDecl,
        NatBasicBlock,
        topInfoTable,
        Instruction(..)
)

where

import Reg

import BlockId
import DynFlags
import Cmm hiding (topInfoTable)
import Platform

-- | Holds a list of source and destination registers used by a
--      particular instruction.
--
--   Machine registers that are pre-allocated to stgRegs are filtered
--      out, because they are uninteresting from a register allocation
--      standpoint.  (We wouldn't want them to end up on the free list!)
--
--   As far as we are concerned, the fixed registers simply don't exist
--      (for allocation purposes, anyway).
--
data RegUsage
        = RU [Reg] [Reg]

-- | No regs read or written to.
noUsage :: RegUsage
noUsage  = RU [] []

-- Our flavours of the Cmm types
-- Type synonyms for Cmm populated with native code
type NatCmm instr
        = GenCmmGroup
                CmmStatics
                (BlockEnv CmmStatics)
                (ListGraph instr)

type NatCmmDecl statics instr
        = GenCmmDecl
                statics
                (BlockEnv CmmStatics)
                (ListGraph instr)


type NatBasicBlock instr
        = GenBasicBlock instr


-- | Returns the info table associated with the CmmDecl's entry point,
-- if any.
topInfoTable :: GenCmmDecl a (BlockEnv i) (ListGraph b) -> Maybe i
topInfoTable (CmmProc infos _ _ (ListGraph (b:_)))
  = mapLookup (blockId b) infos
topInfoTable _
  = Nothing


-- | Common things that we can do with instructions, on all architectures.
--      These are used by the shared parts of the native code generator,
--      specifically the register allocators.
--
class   Instruction instr where

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


        -- | Apply a given mapping to all the register references in this
        --      instruction.
        patchRegsOfInstr
                :: instr
                -> (Reg -> Reg)
                -> instr


        -- | Checks whether this instruction is a jump/branch instruction.
        --      One that can change the flow of control in a way that the
        --      register allocator needs to worry about.
        isJumpishInstr
                :: instr -> Bool


        -- | Give the possible destinations of this jump instruction.
        --      Must be defined for all jumpish instructions.
        jumpDestsOfInstr
                :: instr -> [BlockId]


        -- | Change the destination of this jump instruction.
        --      Used in the linear allocator when adding fixup blocks for join
        --      points.
        patchJumpInstr
                :: instr
                -> (BlockId -> BlockId)
                -> instr


        -- | An instruction to spill a register into a spill slot.
        mkSpillInstr
                :: DynFlags
                -> Reg          -- ^ the reg to spill
                -> Int          -- ^ the current stack delta
                -> Int          -- ^ spill slot to use
                -> instr


        -- | An instruction to reload a register from a spill slot.
        mkLoadInstr
                :: DynFlags
                -> Reg          -- ^ the reg to reload.
                -> Int          -- ^ the current stack delta
                -> Int          -- ^ the spill slot to use
                -> instr

        -- | See if this instruction is telling us the current C stack delta
        takeDeltaInstr
                :: instr
                -> Maybe Int

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



        -- | Copy the value in a register to another one.
        --      Must work for all register classes.
        mkRegRegMoveInstr
                :: Platform
                -> Reg          -- ^ source register
                -> Reg          -- ^ destination register
                -> instr

        -- | Take the source and destination from this reg -> reg move instruction
        --      or Nothing if it's not one
        takeRegRegMoveInstr
                :: instr
                -> Maybe (Reg, Reg)

        -- | Make an unconditional jump instruction.
        --      For architectures with branch delay slots, its ok to put
        --      a NOP after the jump. Don't fill the delay slot with an
        --      instruction that references regs or you'll confuse the
        --      linear allocator.
        mkJumpInstr
                :: BlockId
                -> [instr]


        -- Subtract an amount from the C stack pointer
        mkStackAllocInstr
                :: Platform  -- TODO: remove (needed by x86/x86_64
                             -- because they share an Instr type)
                -> Int
                -> instr

        -- Add an amount to the C stack pointer
        mkStackDeallocInstr
                :: Platform  -- TODO: remove (needed by x86/x86_64
                             -- because they share an Instr type)
                -> Int
                -> instr
