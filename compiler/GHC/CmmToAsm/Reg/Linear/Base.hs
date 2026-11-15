{-# LANGUAGE RecordWildCards #-}

-- | Put common type definitions here to break recursive module dependencies.

module GHC.CmmToAsm.Reg.Linear.Base (
        BlockAssignment,
        lookupBlockAssignment,
        lookupFirstUsed,
        blocksHolding,
        markStaleFirst,
        isStaleFirst,
        emptyBlockAssignment,
        updateBlockAssignment,

        VLoc(..), Loc(..), IgnoreFormat(..),
        regsOfLoc,
        RealRegUsage(..),

        -- for stats
        SpillReason(..),
        RegAllocStats(..),

        -- the allocator monad
        RA_State(..),
        RA_Context(..),
        MoveDests(..),
        mkMoveDests,

        -- liveness
        liveInto,
        LastReads(..),
)

where

import GHC.Prelude

import Data.Word (Word64)

import GHC.CmmToAsm.Reg.Linear.StackMap
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Config
import GHC.Platform.Reg

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set (UniqSet, addOneToUniqSet, elemUniqSet_Directly)
import GHC.Types.Unique.DSM
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Reg.Utils
import GHC.CmmToAsm.Format

-- | Used to store the register assignment on entry to a basic block.
--      We use this to handle join points, where multiple branch instructions
--      target a particular label. We have to insert fixup code to make
--      the register assignments from the different sources match up.
--
data BlockAssignment
        = BlockAssignment { blockMap :: !(BlockMap (RegMap Loc))
                          , firstUsed :: !(UniqFM VirtualReg RealReg)
                            -- | the vregs whose first-used register a block
                            -- found to be another value's
                          , staleFirst :: !(UniqSet Unique) }

-- | Find the register mapping for a specific BlockId.
lookupBlockAssignment :: BlockId -> BlockAssignment -> Maybe (RegMap Loc)
lookupBlockAssignment bid ba = mapLookup bid (blockMap ba)

-- | The register a virtual register has in the first assignment that,
-- recorded for a block it is live into, holds it in a register:
-- see Note [The first jump into a block fixes its entry assignment] in
-- GHC.CmmToAsm.Reg.Linear.JoinToTargets.
lookupFirstUsed :: Unique -> BlockAssignment -> Maybe RealReg
lookupFirstUsed u ba = lookupUFM_Directly (firstUsed ba) u

-- | The blocks whose recorded assignment holds the virtual register with the
-- given unique in the given register, in the order of their labels' uniques, on
-- which no caller may depend: firstFreeInOrder only asks whether any of them
-- meets a region (Note [Unique Determinism and code generation]). Nothing
-- where more than 256 assignments are recorded, which bounds the listing, a
-- visit to every one: unbounded, in a procedure of N branches in sequence whose
-- arms pass a call a variable written afresh it visited N squared assignments
-- at -O0, and the compiler ran 3.0 times the instructions at the doubling of N
-- to 3200, on GHC 10.1.20261111. The 256 is arbitrary, and may be raised where
-- a case needs more: compiling nofib, no listing visited more than 132. (An
-- index of the holders, each recorded assignment adding a thunk to it, made
-- the compiler allocate 0.068% more compiling T9961 at -O0 and 0.048% more
-- compiling T783 at -O0.)
blocksHolding :: Unique -> RealReg -> BlockAssignment -> Maybe [BlockId]
blocksHolding !u r ba
  | null (drop 256 recorded)
  = Just [ b | (b, assig) <- recorded
             , Just (Loc l _) <- [lookupUFM_Directly assig u]
             , r `elem` regsOfLoc l ]
  | otherwise = Nothing
  where
    recorded = mapToList (blockMap ba)

-- | Record that a block found the first-used register of the virtual register
-- with the given unique to be another value's.
markStaleFirst :: Unique -> BlockAssignment -> BlockAssignment
markStaleFirst u ba = ba { staleFirst = addOneToUniqSet (staleFirst ba) u }

-- | Whether a block found the first-used register of the virtual register with
-- the given unique to be another value's.
isStaleFirst :: Unique -> BlockAssignment -> Bool
isStaleFirst u ba = elemUniqSet_Directly u (staleFirst ba)

-- | An initial empty 'BlockAssignment'
emptyBlockAssignment :: BlockAssignment
emptyBlockAssignment = BlockAssignment mapEmpty mempty mempty

-- | Add new register mappings for a specific block.
updateBlockAssignment :: BlockId
  -> RegMap Loc
  -> BlockAssignment
  -> BlockAssignment
updateBlockAssignment dest regMap (BlockAssignment {..}) =
  BlockAssignment
    (mapInsert dest regMap blockMap)
    (mergeUFM combWithExisting id
        (mapMaybeUFM (fromVLoc . locWithFormat_loc))
        firstUsed
        (toVRegMap regMap)
    )
    staleFirst
  where
    -- A vreg's first-used register is its register in the first assignment
    -- that, recorded for a block it is live into, holds it in a register
    -- (Note [The first jump into a block fixes its entry assignment] in
    -- GHC.CmmToAsm.Reg.Linear.JoinToTargets), so if there's already an
    -- entry in the map then keep that assignment rather than writing the new
    -- assignment.
    combWithExisting :: RealReg -> Loc -> Maybe RealReg
    combWithExisting old_reg _ = Just $ old_reg

    fromVLoc :: VLoc -> Maybe RealReg
    fromVLoc (InReg rr) = Just rr
    fromVLoc (InBoth rr _) = Just rr
    fromVLoc _ = Nothing

-- | Where a vreg is currently stored.
--
--
--      A temporary can be marked as living in both a register and memory
--      (InBoth), for example if it was recently loaded from a spill location.
--      This makes it cheap to spill (no save instruction required), but we
--      have to be careful to turn this into InReg if the value in the
--      register is changed.

--      This is also useful when a temporary is about to be clobbered.  We
--      save it in a spill location, but mark it as InBoth because the current
--      instruction might still want to read it.
--
data VLoc
        -- | vreg is in a register
        = InReg   {-# UNPACK #-} !RealReg

        -- | vreg is held in stack slots
        | InMem   {-# UNPACK #-} !StackSlot

        -- | vreg is held in both a register and stack slots
        | InBoth  {-# UNPACK #-} !RealReg
                  {-# UNPACK #-} !StackSlot
        deriving (Eq, Ord, Show)

-- | Where a virtual register is stored, together with the format it is stored at.
--
-- See 'VLoc'.
data Loc
  = Loc
  { locWithFormat_loc    :: {-# UNPACK #-} !VLoc
  , locWithFormat_format :: Format
  }

-- | A newtype used to hang off 'Eq' and 'Ord' instances for 'Loc' which
-- ignore the format, as used in 'GHC.CmmToAsm.Reg.Linear.JoinToTargets'.
newtype IgnoreFormat a = IgnoreFormat a
instance Eq (IgnoreFormat Loc) where
  IgnoreFormat (Loc l1 _) == IgnoreFormat (Loc l2 _) = l1 == l2
instance Ord (IgnoreFormat Loc) where
  compare (IgnoreFormat (Loc l1 _)) (IgnoreFormat (Loc l2 _)) = compare l1 l2

instance Outputable VLoc where
        ppr l = text (show l)

instance Outputable Loc where
  ppr (Loc loc fmt) = parens (ppr loc <+> dcolon <+> ppr fmt)

-- | A 'RealReg', together with the specific 'Format' it is used at.
data RealRegUsage
  = RealRegUsage
    { realReg :: !RealReg
    , realRegFormat :: !Format
    } deriving Show

instance Outputable RealRegUsage where
  ppr (RealRegUsage r fmt) = ppr r <> dcolon <+> ppr fmt

-- | Get the reg numbers stored in this Loc.
regsOfLoc :: VLoc -> [RealReg]
regsOfLoc (InReg r)    = [r]
regsOfLoc (InBoth r _) = [r]
regsOfLoc (InMem _)    = []

-- | Reasons why instructions might be inserted by the spiller.
--      Used when generating stats for -ddump-asm-stats.
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
          ra_blockassig :: BlockAssignment

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

        -- | Record why things were spilled, for -ddump-asm-stats.
        --      Just keep a list here instead of a map of regs -> reasons.
        --      We don't want to slow down the allocator if we're not going to emit the stats.
        , ra_spills     :: [SpillReason]

        -- | (from,fixup,to) : We inserted fixup code between from and to
        , ra_fixups     :: [(BlockId,BlockId,BlockId)]

        -- | see 'RA_Context'
        , ra_context    :: !(RA_Context freeRegs)

        }

-- | Of a block, the vregs live out of it that it reads, each with the position
--   in the block of the last instruction reading it (built only where it is
--   consulted), the registers live into each of its jump targets, and those
--   live into each of them with no recorded assignment when its allocation
--   began that another block jumps to as well, where neither is in a cycle
--   ('GHC.CmmToAsm.Reg.Linear.intoUnrecorded').
data LastReads
  = LastReads (UniqFM VirtualReg Int) [Regs] [Regs]

-- | What the allocator uses, but does not update, while it allocates the
--   instructions of a block, held in one field of 'RA_State' rather than in
--   fields of their own, each of which costs allocation at every update of
--   the state: an unused one alone made the compiler allocate 0.3% more
--   compiling T3294 at -O0 when this was added.
data RA_Context freeRegs
        = RA_Context
        {
        -- | the recorded assignments of the jump targets of the block being
        --   allocated, and for a target not yet reached those of the blocks
        --   it jumps to, as they are when the block starts
          rc_jumpLocs   :: [RegMap Loc]

        -- | the real register each vreg is moved into where it dies,
        --   in the block being allocated, or, for one moved into another vreg
        --   and not written again in the block, the first register the
        --   assignments in rc_jumpLocs give that one
        , rc_moveDests  :: MoveDests

        -- | the uniques of the vregs the block being allocated reads or
        --   writes, by their first-used registers
        , rc_claims     :: UniqFM RealReg [Unique]

        -- | 'GHC.CmmToAsm.Reg.Linear.wideMoveDests' of the procedure being
        --   allocated
        , rc_wideDests  :: UniqFM VirtualReg RealReg

        -- | for each vreg whose wide destination in 'rc_wideDests' is
        --   propagated from a vreg it is moved into where it dies, that vreg
        , rc_via        :: UniqFM VirtualReg VirtualReg

        -- | the jump targets of the block being allocated
        , rc_dests      :: [BlockId]

        -- | the last reads of the vregs live out of the block being
        --   allocated, built when the block first has to spill one or to
        --   free for a vreg the first register the assignments in rc_jumpLocs
        --   give it (allocRegsAndSpill_spill)
        , rc_lastReads  :: LastReads

        -- | the jump targets of each block of the procedure being allocated
        , rc_succs      :: LabelMap [BlockId]

        -- | the blocks of the procedure being allocated that are in no cycle
        --   and that more than one of its blocks jumps to
        , rc_acyclicJoins :: LabelSet

        -- | the registers live on entry to each block of the procedure being
        --   allocated
        , rc_live       :: BlockMap Regs

        -- | the real register each vreg is moved into where it dies, in each
        --   block of the procedure being allocated
        , rc_dying      :: LabelMap (UniqFM VirtualReg RealReg)

        -- | 'GHC.CmmToAsm.Reg.Linear.regClobbers' of the procedure being
        --   allocated on x86-64, whose fallback alone uses it, and Nothing
        --   elsewhere
        , rc_clobbers   :: Maybe (UniqFM VirtualReg Word64)

        -- | every allocatable register, from which each block's free
        --   registers on entry are derived
        , rc_initFree   :: freeRegs

        -- | Native code generator configuration
        , rc_config     :: !NCGConfig
        }

-- | The map of 'rc_moveDests', with the registers, by number,
--   it holds and those it holds for two or more vregs, for
--   'GHC.CmmToAsm.Reg.Linear.firstFreeInOrder' (one value, shared where the map
--   is empty: the masks held as a pair in a field of 'RA_Context' of their own
--   made the compiler allocate 0.076% more compiling T9961 at -O0, and so did
--   this value while 'mkMoveDests' took the block's list of moves; given the
--   map, it cost 0.016%, and nothing once OPAQUE as well, on GHC 10.1.20261109)
data MoveDests
        = MoveDests !(UniqFM VirtualReg RealReg)
                    {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- (given the map, not the list: given the list, the block's dying moves
-- no longer fused into the map, a thunk an instruction, and the compiler
-- ran 0.13% more instructions and allocated 0.27% more compiling T3294
-- at -O0; OPAQUE: its worker and wrapper allocated a MoveDests for each
-- block, the empty one too, and the compiler 0.016% more compiling T9961
-- at -O0)
mkMoveDests :: UniqFM VirtualReg RealReg -> MoveDests
{-# OPAQUE mkMoveDests #-}
mkMoveDests m
  | isNullUFM m = noMoveDests
  | otherwise   = nonDetStrictFoldUFM add (MoveDests m 0 0) m
    -- Setting bits commutes, so the fold's order does not matter
    -- (Note [Unique Determinism and code generation])
  where
    add :: RealReg -> MoveDests -> MoveDests
    add (RealRegSingle n) (MoveDests d a t)
      | testBit a n = MoveDests d a (setBit t n)
      | otherwise   = assertPpr (n < 64) (ppr n) $ MoveDests d (setBit a n) t

noMoveDests :: MoveDests
noMoveDests = MoveDests emptyUFM 0 0

-- | Whether the vreg with the given unique is live into the block.
liveInto :: Unique -> BlockMap Regs -> BlockId -> Bool
liveInto !u live b = elemUniqSet_Directly u (getRegs (mapFindWithDefault noRegs b live))

