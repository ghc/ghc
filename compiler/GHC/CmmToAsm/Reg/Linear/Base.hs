{-# LANGUAGE RecordWildCards #-}

-- | Put common type definitions here to break recursive module dependencies.

module GHC.CmmToAsm.Reg.Linear.Base (
        BlockAssignment,
        lookupBlockAssignment,
        lookupFirstUsed,
        emptyBlockAssignment,
        updateBlockAssignment,

        Loc(..),
        regsOfLoc,

        -- for stats
        SpillReason(..),
        RegAllocStats(..),

        -- the allocator monad
        RA_State(..),
)

where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Linear.StackMap
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Config
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Reg.Utils

data ReadingOrWriting = Reading | Writing deriving (Eq,Ord)

-- | Used to store the register assignment on entry to a basic block.
--      We use this to handle join points, where multiple branch instructions
--      target a particular label. We have to insert fixup code to make
--      the register assignments from the different sources match up.
--
data BlockAssignment freeRegs
        = BlockAssignment { blockMap :: !(BlockMap (freeRegs, RegMap Loc))
                          , firstUsed :: !(UniqFM VirtualReg RealReg) }

-- | Find the register mapping for a specific BlockId.
lookupBlockAssignment :: BlockId -> BlockAssignment freeRegs -> Maybe (freeRegs, RegMap Loc)
lookupBlockAssignment bid ba = mapLookup bid (blockMap ba)

-- | Lookup which register a virtual register was first assigned to.
lookupFirstUsed :: VirtualReg -> BlockAssignment freeRegs -> Maybe RealReg
lookupFirstUsed vr ba = lookupUFM (firstUsed ba) vr

-- | An initial empty 'BlockAssignment'
emptyBlockAssignment :: BlockAssignment freeRegs
emptyBlockAssignment = BlockAssignment mapEmpty mempty

-- | Add new register mappings for a specific block.
updateBlockAssignment :: BlockId
  -> (freeRegs, RegMap Loc)
  -> BlockAssignment freeRegs
  -> BlockAssignment freeRegs
updateBlockAssignment dest (freeRegs, regMap) (BlockAssignment {..}) =
  BlockAssignment (mapInsert dest (freeRegs, regMap) blockMap)
                  (mergeUFM combWithExisting id (mapMaybeUFM fromLoc) (firstUsed) (toVRegMap regMap))
  where
    -- The blocks are processed in dependency order, so if there's already an
    -- entry in the map then keep that assignment rather than writing the new
    -- assignment.
    combWithExisting :: RealReg -> Loc -> Maybe RealReg
    combWithExisting old_reg _ = Just $ old_reg

    fromLoc :: Loc -> Maybe RealReg
    fromLoc (InReg rr) = Just rr
    fromLoc (InBoth rr _) = Just rr
    fromLoc _ = Nothing


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
        { ra_spillInstrs        :: UniqFM Unique [Int] -- Keys are the uniques of regs
                                                       -- and taken from SpillReason
                                                       -- See Note [UniqFM and the register allocator]
        , ra_fixupList     :: [(BlockId,BlockId,BlockId)]
        -- ^ (from,fixup,to) : We inserted fixup code between from and to
        }


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
        , ra_us         :: DUniqSupply

        -- | Record why things were spilled, for -ddrop-asm-stats.
        --      Just keep a list here instead of a map of regs -> reasons.
        --      We don't want to slow down the allocator if we're not going to emit the stats.
        , ra_spills     :: [SpillReason]

        -- | Native code generator configuration
        , ra_config     :: !NCGConfig

        -- | (from,fixup,to) : We inserted fixup code between from and to
        , ra_fixups     :: [(BlockId,BlockId,BlockId)]

        }


