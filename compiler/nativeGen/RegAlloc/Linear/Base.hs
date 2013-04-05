
-- | Put common type definitions here to break recursive module dependencies.

module RegAlloc.Linear.Base (
        BlockAssignment,

        Loc(..),
        regsOfLoc,

        -- for stats
        SpillReason(..),
        RegAllocStats(..),

        -- the allocator monad
        RA_State(..),
)

where

import RegAlloc.Linear.StackMap
import RegAlloc.Liveness
import Reg

import DynFlags
import Outputable
import Unique
import UniqFM
import UniqSupply


-- | Used to store the register assignment on entry to a basic block.
--      We use this to handle join points, where multiple branch instructions
--      target a particular label. We have to insert fixup code to make
--      the register assignments from the different sources match up.
--
type BlockAssignment freeRegs
        = BlockMap (freeRegs, RegMap Loc)


-- | Where a vreg is currently stored
--      A temporary can be marked as living in both a register and memory
--      (InBoth), for example if it was recently loaded from a spill location.
--      This makes it cheap to spill (no save instruction required), but we
--      have to be careful to turn this into InReg if the value in the
--      register is changed.

--      This is also useful when a temporary is about to be clobbered.  We
--      save it in a spill location, but mark it as InBoth because the current
--      instruction might still want to read it.
--
data Loc
        -- | vreg is in a register
        = InReg   !RealReg

        -- | vreg is held in a stack slot
        | InMem   {-# UNPACK #-}  !StackSlot


        -- | vreg is held in both a register and a stack slot
        | InBoth   !RealReg
                   {-# UNPACK #-} !StackSlot
        deriving (Eq, Show, Ord)

instance Outputable Loc where
        ppr l = text (show l)


-- | Get the reg numbers stored in this Loc.
regsOfLoc :: Loc -> [RealReg]
regsOfLoc (InReg r)    = [r]
regsOfLoc (InBoth r _) = [r]
regsOfLoc (InMem _)    = []


-- | Reasons why instructions might be inserted by the spiller.
--      Used when generating stats for -ddrop-asm-stats.
--
data SpillReason
        -- | vreg was spilled to a slot so we could use its
        --      current hreg for another vreg
        = SpillAlloc    !Unique

        -- | vreg was moved because its hreg was clobbered
        | SpillClobber  !Unique

        -- | vreg was loaded from a spill slot
        | SpillLoad     !Unique

        -- | reg-reg move inserted during join to targets
        | SpillJoinRR   !Unique

        -- | reg-mem move inserted during join to targets
        | SpillJoinRM   !Unique


-- | Used to carry interesting stats out of the register allocator.
data RegAllocStats
        = RegAllocStats
        { ra_spillInstrs        :: UniqFM [Int] }


-- | The register allocator state
data RA_State freeRegs
        = RA_State

        {
        -- | the current mapping from basic blocks to
        --      the register assignments at the beginning of that block.
          ra_blockassig :: BlockAssignment freeRegs

        -- | free machine registers
        , ra_freeregs   :: !freeRegs

        -- | assignment of temps to locations
        , ra_assig      :: RegMap Loc

        -- | current stack delta
        , ra_delta      :: Int

        -- | free stack slots for spilling
        , ra_stack      :: StackMap

        -- | unique supply for generating names for join point fixup blocks.
        , ra_us         :: UniqSupply

        -- | Record why things were spilled, for -ddrop-asm-stats.
        --      Just keep a list here instead of a map of regs -> reasons.
        --      We don't want to slow down the allocator if we're not going to emit the stats.
        , ra_spills     :: [SpillReason]
        , ra_DynFlags   :: DynFlags }


