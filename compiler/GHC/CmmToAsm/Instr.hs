
module GHC.CmmToAsm.Instr
   ( Instruction(..)
   , RegUsage(..)
   , noUsage
   )
where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg
import GHC.Utils.Outputable (SDoc)

import GHC.Cmm.BlockId

import GHC.CmmToAsm.Config
import GHC.Data.FastString

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
        = RU    {
                reads :: [Reg],
                writes :: [Reg]
                }
        deriving Show

-- | No regs read or written to.
noUsage :: RegUsage
noUsage  = RU [] []

-- | Common things that we can do with instructions, on all architectures.
--      These are used by the shared parts of the native code generator,
--      specifically the register allocators.
--
class Instruction instr where

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


        -- | Give the possible *local block* destinations of this jump instruction.
        --      Must be defined for all jumpish instructions.
        jumpDestsOfInstr
                :: instr -> [BlockId]

        -- | Check if the instr always transfers control flow
        -- to the given block. Used by code layout to eliminate
        -- jumps that can be replaced by fall through.
        canFallthroughTo
                :: instr -> BlockId -> Bool


        -- | Change the destination of this jump instruction.
        --      Used in the linear allocator when adding fixup blocks for join
        --      points.
        patchJumpInstr
                :: instr
                -> (BlockId -> BlockId)
                -> instr


        -- | An instruction to spill a register into a spill slot.
        mkSpillInstr
                :: NCGConfig
                -> Reg          -- ^ the reg to spill
                -> Int          -- ^ the current stack delta
                -> Int          -- ^ spill slot to use
                -> [instr]        -- ^ instructions


        -- | An instruction to reload a register from a spill slot.
        mkLoadInstr
                :: NCGConfig
                -> Reg          -- ^ the reg to reload.
                -> Int          -- ^ the current stack delta
                -> Int          -- ^ the spill slot to use
                -> [instr]        -- ^ instructions

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
                :: Platform
                -> Int
                -> [instr]

        -- Add an amount to the C stack pointer
        mkStackDeallocInstr
                :: Platform
                -> Int
                -> [instr]

        -- | Pretty-print an instruction
        pprInstr :: Platform -> instr -> SDoc

        -- Create a comment instruction
        mkComment :: FastString -> [instr]
