
-----------------------------------------------------------------------------
--
-- The register allocator
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

{-
The algorithm is roughly:

  1) Compute strongly connected components of the basic block list.

  2) Compute liveness (mapping from pseudo register to
     point(s) of death?).

  3) Walk instructions in each basic block.  We keep track of
        (a) Free real registers (a bitmap?)
        (b) Current assignment of temporaries to machine registers and/or
            spill slots (call this the "assignment").
        (c) Partial mapping from basic block ids to a virt-to-loc mapping.
            When we first encounter a branch to a basic block,
            we fill in its entry in this table with the current mapping.

     For each instruction:
        (a) For each temporary *read* by the instruction:
            If the temporary does not have a real register allocation:
                - Allocate a real register from the free list.  If
                  the list is empty:
                  - Find a temporary to spill.  Pick one that is
                    not used in this instruction (ToDo: not
                    used for a while...)
                  - generate a spill instruction
                - If the temporary was previously spilled,
                  generate an instruction to read the temp from its spill loc.
            (optimisation: if we can see that a real register is going to
            be used soon, then don't use it for allocation).

        (b) For each real register clobbered by this instruction:
            If a temporary resides in it,
                If the temporary is live after this instruction,
                    Move the temporary to another (non-clobbered & free) reg,
                    or spill it to memory.  Mark the temporary as residing
                    in both memory and a register if it was spilled (it might
                    need to be read by this instruction).

            (ToDo: this is wrong for jump instructions?)

            We do this after step (a), because if we start with
               movq v1, %rsi
            which is an instruction that clobbers %rsi, if v1 currently resides
            in %rsi we want to get
               movq %rsi, %freereg
               movq %rsi, %rsi     -- will disappear
            instead of
               movq %rsi, %freereg
               movq %freereg, %rsi

        (c) Update the current assignment

        (d) If the instruction is a branch:
              if the destination block already has a register assignment,
                Generate a new block with fixup code and redirect the
                jump to the new block.
              else,
                Update the block id->assignment mapping with the current
                assignment.

        (e) Delete all register assignments for temps which are read
            (only) and die here.  Update the free register list.

        (f) Mark all registers clobbered by this instruction as not free,
            and mark temporaries which have been spilled due to clobbering
            as in memory (step (a) marks then as in both mem & reg).

        (g) For each temporary *written* by this instruction:
            Allocate a real register as for (b), spilling something
            else if necessary.
                - except when updating the assignment, drop any memory
                  locations that the temporary was previously in, since
                  they will be no longer valid after this instruction.

        (h) Delete all register assignments for temps which are
            written and die here (there should rarely be any).  Update
            the free register list.

        (i) Rewrite the instruction with the new mapping.

        (j) For each spilled reg known to be now dead, re-add its stack slot
            to the free list.

-}

module GHC.CmmToAsm.Reg.Linear (
        regAlloc,
        module  GHC.CmmToAsm.Reg.Linear.Base,
        module  GHC.CmmToAsm.Reg.Linear.Stats
  ) where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Linear.State
import GHC.CmmToAsm.Reg.Linear.Base
import GHC.CmmToAsm.Reg.Linear.StackMap
import GHC.CmmToAsm.Reg.Linear.FreeRegs
import GHC.CmmToAsm.Reg.Linear.Stats
import GHC.CmmToAsm.Reg.Linear.JoinToTargets
import qualified GHC.CmmToAsm.Reg.Linear.PPC     as PPC
import qualified GHC.CmmToAsm.Reg.Linear.X86     as X86
import qualified GHC.CmmToAsm.X86.Instr          as X86 (Instr)
import qualified GHC.CmmToAsm.Reg.Linear.X86_64  as X86_64
import qualified GHC.CmmToAsm.Reg.Linear.AArch64 as AArch64
import qualified GHC.CmmToAsm.Reg.Linear.RV64    as RV64
import qualified GHC.CmmToAsm.Reg.Linear.LA64    as LA64
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Utils
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.Platform.Reg
import GHC.Platform.Regs (globalRegMaybe)

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm

import GHC.Data.Graph.Directed
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSM
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

import Data.Containers.ListUtils
import Data.Maybe
import Data.List (find, sortOn)
import Data.Word (Word64)
import Control.Monad

-- -----------------------------------------------------------------------------
-- Top level of the register allocator

{- Note [Specialising the linear allocator]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The allocator is overloaded in the instructions ('Instruction') and in the free
registers ('FR'), so on its own it calls every instruction method through a
dictionary. On x86-64 it runs specialised to x86's instructions: 'regAlloc' has
a SPECIALISE pragma, and the specialiser copies what it calls, down to
'linearRegAlloc'' at x86-64's free registers. The code generator in
"GHC.CmmToAsm" is specialised for each target, and the specialisation's rule
fires in the copy for x86 only if 'regAlloc' is not inlined there first, hence
its NOINLINE [0], with which the rule fires from phase 0 on. A phase on the
SPECIALISE pragma alone does not suffice: cast worker/wrapper makes 'regAlloc'
an alias of its body, cast to 'UniqDSM', and the alias inlines from the
function's own phase, whatever its rules' (mkCastWrapperInlinePrag in
"GHC.Core.Opt.Simplify.Iteration"). 'linearRegAlloc' passes the other targets'
free registers to 'other', which is NOINLINE, so that they get no copy of the
allocator: the specialiser does not specialise a NOINLINE function.
'cmmTopLiveness', in "GHC.CmmToAsm.Reg.Liveness", is specialised to x86's
instructions likewise. Importing "GHC.CmmToAsm.X86" for its instance makes no
import cycle: of the register allocator's modules, the x86 code generator
imports only "GHC.CmmToAsm.Reg.Target".
-}

-- Allocate registers
{-# NOINLINE [0] regAlloc #-}  -- See Note [Specialising the linear allocator]
regAlloc
        :: Instruction instr
        => NCGConfig
        -> LabelSet
        -> LiveCmmDecl statics instr
        -> UniqDSM ( NatCmmDecl statics instr
                   , Maybe Int  -- number of extra stack slots required,
                                -- beyond maxSpillSlots
                   , Maybe RegAllocStats
                   )
{-# SPECIALISE regAlloc
      :: NCGConfig -> LabelSet -> LiveCmmDecl statics X86.Instr
      -> UniqDSM (NatCmmDecl statics X86.Instr, Maybe Int, Maybe RegAllocStats)
  #-}

regAlloc _ _ (CmmData sec d)
        = return
                ( CmmData sec d
                , Nothing
                , Nothing )

regAlloc _ _ (CmmProc (LiveInfo info _ _ _) lbl live [])
        = return ( CmmProc info lbl live (ListGraph [])
                 , Nothing
                 , Nothing )

regAlloc config cold (CmmProc static lbl live sccs)
        | LiveInfo info entry_ids@(first_id:_) block_live _ <- static
        = do
                -- do register allocation on each component.
                !(!final_blocks, !stats, !stack_use)
                        <- linearRegAlloc config cold entry_ids block_live sccs

                -- make sure the block that was first in the input list
                --      stays at the front of the output
                let !final_blocks' = sortOn ((/= first_id) . blockId) final_blocks

                let max_spill_slots = maxSpillSlots config
                    extra_stack
                      | stack_use > max_spill_slots
                      = Just $! stack_use - max_spill_slots
                      | otherwise
                      = Nothing

                return  ( CmmProc info lbl live (ListGraph final_blocks')
                        , extra_stack
                        , Just stats)

-- bogus. to make non-exhaustive match warning go away.
regAlloc _ _ (CmmProc _ _ _ _)
        = panic "RegAllocLinear.regAlloc: no match"


-- -----------------------------------------------------------------------------
-- Linear sweep to allocate registers


-- | Do register allocation on some basic blocks.
--   But be careful to allocate a block in an SCC only if it has
--   an entry in the block map or it is the first block.
--
linearRegAlloc
        :: forall instr. (Instruction instr)
        => NCGConfig
        -> LabelSet  -- ^ the blocks that follow a failed heap or stack check
        -> [BlockId] -- ^ entry points
        -> BlockMap Regs
              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)]
              -- ^ instructions annotated with "deaths"
        -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc config cold entry_ids block_live sccs
 = case platformArch platform of
      ArchX86        -> other $ (frInitFreeRegs platform :: X86.FreeRegs)
      ArchX86_64     -> go $ (frInitFreeRegs platform :: X86_64.FreeRegs)
      ArchS390X      -> panic "linearRegAlloc ArchS390X"
      ArchPPC        -> other $ (frInitFreeRegs platform :: PPC.FreeRegs)
      ArchARM _ _ _  -> panic "linearRegAlloc ArchARM"
      ArchAArch64    -> other $ (frInitFreeRegs platform :: AArch64.FreeRegs)
      ArchPPC_64 _   -> other $ (frInitFreeRegs platform :: PPC.FreeRegs)
      ArchAlpha      -> panic "linearRegAlloc ArchAlpha"
      ArchMipseb     -> panic "linearRegAlloc ArchMipseb"
      ArchMipsel     -> panic "linearRegAlloc ArchMipsel"
      ArchRISCV64    -> other (frInitFreeRegs platform :: RV64.FreeRegs)
      ArchLoongArch64 -> other $ (frInitFreeRegs platform :: LA64.FreeRegs)
      ArchJavaScript -> panic "linearRegAlloc ArchJavaScript"
      ArchWasm32     -> panic "linearRegAlloc ArchWasm32"
      ArchUnknown    -> panic "linearRegAlloc ArchUnknown"
 where
  go :: FR regs
     => regs -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)
  go f = linearRegAlloc' config f cold entry_ids block_live sccs
  -- See Note [Specialising the linear allocator]
  other :: FR regs
        => regs -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)
  other = go
  {-# NOINLINE other #-}
  platform = ncgPlatform config

-- | Constraints on the free-register and instruction instances used by the
-- linear allocator.
type RegAllocConstraint freeRegs instr =
        (FR freeRegs, Instruction instr)

linearRegAlloc'
        :: RegAllocConstraint freeRegs instr
        => NCGConfig
        -> freeRegs
        -> LabelSet                     -- ^ the blocks that follow a failed
                                        --   heap or stack check
        -> [BlockId]                    -- ^ entry points
        -> BlockMap Regs              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)] -- ^ instructions annotated with "deaths"
        -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc' config initFreeRegs cold entry_ids block_live sccs
 = UDSM $ \us -> do
    let !(!stack, !stats, !blocks, us') =
            runR ctx us
                $ linearRA_SCCs cold entry_ids block_live sccs
        platform = ncgPlatform config
        ctx = RA_Context
                { rc_jumpLocs  = []
                , rc_moveDests = emptyUFM
                , rc_wideDests = wideMoveDests platform cold block_live sccs
                  -- used by x86-64's fallback alone, and elsewhere a thunk
                  -- that would keep the SCCs alive
                , rc_clobbers  = case platformArch platform of
                    ArchX86_64 -> regClobbers platform sccs
                    _          -> emptyUFM
                , rc_initFree  = initFreeRegs
                , rc_config    = config }
     in DUniqResult (blocks, stats, getStackUse stack) us'

-- | For each virtual register, the real registers an instruction writes while
-- the virtual register is live past it, as bits by register number: a C call's
-- caller-saved registers, or R2's %r14 set for a jump, say. Given one of them,
-- the virtual register would have to move out of it at that instruction. A move
-- of the virtual register itself into one is no such write. The liveness pass
-- records, at each instruction writing a real register, the registers live
-- after it.
--
-- Every write counts, those in the blocks that follow a failed heap or stack
-- check too, which run seldom.  TODO: a write could count by how often its
-- block runs: see Note [Frequencies before register allocation] in
-- GHC.CmmToAsm.CFG.
regClobbers :: Instruction instr
            => Platform -> [SCC (LiveBasicBlock instr)]
            -> UniqFM VirtualReg Word64
regClobbers platform sccs
  = foldl' (\acc (BasicBlock _ instrs) -> foldl' ins acc instrs)
           emptyUFM (flattenSCCs sccs)
  where
    ins acc (LiveInstr (Instr i) (Just l))
      | hit /= 0
      = nonDetStrictFoldUniqSet clash acc (getRegs (liveAcross l))
      where
        RU rs ws = liveUsage l
        hit  = foldl' (\b r -> case regWithFormat_reg r of
                                 RegReal (RealRegSingle n) -> setBit b n
                                 _                         -> b) 0 ws
        src  = case takeRegRegMoveInstr platform i of
                 Just (RegVirtual v, RegReal _) -> Just v
                 _                              -> Nothing
        -- live past the instruction: live after it, and not only written
        -- by it
        clash r a = case regWithFormat_reg r of
          RegVirtual v
            | not (moved v)
            , not (has v ws) || has v rs
            , lookupWithDefaultUFM a 0 v .&. hit /= hit
            -> addToUFM_C (.|.) a v hit
          _ -> a
        moved v = case src of
          Just s  -> s == v
          Nothing -> False
        has v = any (\r -> case regWithFormat_reg r of
                              RegVirtual v' -> v' == v
                              _             -> False)
    ins acc _ = acc

-- | The virtual register the instruction moves where it dies, and the move's
-- destination, if it is such a move.
dyingMove :: Instruction instr
          => Platform -> instr -> Liveness -> Maybe (VirtualReg, Reg)
{-# INLINE dyingMove #-}  -- out of line it runs more instructions
dyingMove platform instr live
  | not (isEmptyUniqSet (getRegs (liveDieRead live)))
  , Just (RegVirtual vr, d) <- takeRegRegMoveInstr platform instr
  , RegVirtual vr `elemRegs` liveDieRead live
  = Just (vr, d)
  | otherwise
  = Nothing

-- | The moves of a block's virtual registers where they die, into real
-- registers or other virtual registers.
dyingMoves :: Instruction instr
           => Platform -> [LiveInstr instr] -> [(VirtualReg, Reg)]
{-# INLINE dyingMoves #-}  -- out of line it allocates more
dyingMoves platform instrs
  = [ m | LiveInstr (Instr instr) (Just live) <- instrs
        , Just m <- [dyingMove platform instr live] ]

-- | 'dyingMoves' into virtual registers, without the moves of a vreg the block
-- writes again after the move: a preference is looked up by the vreg, so such
-- a move's preference would reach the vreg's later value.
dyingMovesLast :: Instruction instr
               => Platform -> [LiveInstr instr] -> [(VirtualReg, VirtualReg)]
dyingMovesLast platform instrs = fst $ foldr step ([], emptyUniqSet) instrs
  where
    step (LiveInstr (Instr instr) (Just live)) (acc, written)
      | Just (vr, RegVirtual d) <- dyingMove platform instr live
      , not (vr `elementOfUniqSet` written)
      = ((vr, d) : acc, written')
      | otherwise = (acc, written')
      where written' = foldl' add written (writes (liveUsage live))
    step _ x = x
    add s r = case regWithFormat_reg r of
                RegVirtual v -> addOneToUniqSet s v
                _            -> s

-- | The real register each vreg is moved into where it dies in a block it is
-- live into, unless the block is one of the given ones (on x86-64, those that
-- follow a failed heap or stack check, 'checkFailures'), or that register is
-- live together with the vreg somewhere: at a block's entry, after an
-- instruction's writes, or where a vreg born in a block while the vreg is live
-- is moved into that register in the same block, where the later vreg dies. A
-- vreg that the vreg is moved into where it dies counts as the vreg in this
-- check, where the move is in a block some vreg is live into and not one of the
-- given ones. The walk of each block follows only those vregs and registers,
-- taking an instruction's writes from its register usage ('liveBorn' being
-- filled in only for the graph allocator and the liveness dump).
wideMoveDests :: Instruction instr
              => Platform -> LabelSet -> BlockMap Regs
              -> [SCC (LiveBasicBlock instr)]
              -> UniqFM VirtualReg RealReg
wideMoveDests platform cold block_live sccs
  | isNullUFM cands = cands
  | otherwise       = cands `minusUFM` foldl' blk emptyUFM blocks
  where
    blocks = flattenSCCs sccs
    live_in bid = mapFindWithDefault noRegs bid block_live
    -- a block some vreg is live into, and not one of the given ones
    warm bid live = uniqSetAny (isVirtualReg . regWithFormat_reg) (getRegs live)
                    && not (bid `setMember` cold)
    -- a vreg moved where it dies in a block it is live into: one computed in
    -- the block has the block's own move destination
    -- TODO: where several blocks give a vreg different destinations, the last
    -- block's wins: see Note [Frequencies before register allocation] in
    -- GHC.CmmToAsm.CFG.
    -- A destination no allocator hands out, Sp, Hp or BaseReg, and on AArch64,
    -- RV64 and LA64 those of the argument registers, R1 among them, stays a
    -- candidate here and in processBlock's move destinations, harmless but
    -- where it is the last block's and shadows a usable one or, on x86-64, is a
    -- vreg's only preference, which firstFreeInOrder's fallback counts as one.
    cands = listToUFM
      [ (vr, rr) | BasicBlock bid instrs <- blocks
                 , let live = live_in bid
                 , warm bid live
                 , (vr, RegReal rr) <- dyingMoves platform instrs
                 , RegVirtual vr `elemRegs` live ]
    dests = mkUniqSet (nonDetEltsUFM cands)
    -- each followed vreg to the candidates whose values it holds, with their
    -- destinations: a candidate itself, and each candidate moved into the
    -- vreg where it dies
    follow = plusUFM_C (++) (mapUFM_Directly (\u d -> [(u, d)]) cands)
           $ listToUFM_C (++)
      [ (b, [(getUnique a, d)])
      | BasicBlock bid instrs <- blocks
      , warm bid (live_in bid)
      , (a, RegVirtual b) <- dyingMoves platform instrs
      , Just d <- [lookupUFM cands a] ]
    -- bad: the candidates found live together with their destination;
    -- vs: the followed vregs live; ds: the destinations live
    blk bad (BasicBlock bid instrs)
      = let (bad', vs, ds) = born (bad, emptyUniqSet, emptyUniqSet)
                                  (live_in bid)
            -- the vregs the block moves into a destination where they die
            loc = listToUFM [ (vr, rr) | (vr, RegReal rr) <- dyingMoves platform instrs
                                       , rr `elementOfUniqSet` dests ]
        in go loc bad' vs ds instrs
    go _ !bad !_ !_ [] = bad
    go loc !bad !vs !ds (LiveInstr (Instr _) (Just l) : is)
      | quiet (liveDieRead l), not (any seen ws), quiet (liveDieWrite l)
      , isEmptyUniqSet vs || not (any (local loc) ws)
      = go loc bad vs ds is
      | otherwise
      = let !(vs1, ds1)       = dies (vs, ds) (liveDieRead l)
            bad1 | isEmptyUniqSet vs1 = bad
                 | otherwise          = foldl' (wanted loc vs1) bad ws
            !(bad', vs2, ds2) = foldl' (flip bear) (bad1, vs1, ds1) ws
            !(vs3, ds3)       = dies (vs2, ds2) (liveDieWrite l)
        in go loc bad' vs3 ds3 is
      where ws = writes (liveUsage l)
    go loc !bad !vs !ds (_ : is) = go loc bad vs ds is
    local loc r = case regWithFormat_reg r of
      RegVirtual v -> v `elemUFM` loc
      RegReal _    -> False
    -- a vreg born with a destination in loc: the live followed vregs'
    -- candidates with that destination are bad, but the vreg's own, which an
    -- update in place keeps in its register
    wanted loc vs bad r = case regWithFormat_reg r of
      RegVirtual v
        | Just w <- lookupUFM loc v -> clash w bad (delOneFromUniqSet vs v)
      _ -> bad
    -- none of the registers names a followed vreg or a destination
    quiet regs = not (uniqSetAny seen (getRegs regs))
    seen r = case regWithFormat_reg r of
      RegVirtual v -> v `elemUFM` follow
      RegReal rr   -> rr `elementOfUniqSet` dests
    dies acc regs = nonDetStrictFoldUniqSet die acc (getRegs regs)
    die r acc@(!vs, !ds) = case regWithFormat_reg r of
      RegVirtual v | v `elementOfUniqSet` vs  -> (delOneFromUniqSet vs v, ds)
      RegReal rr   | rr `elementOfUniqSet` ds -> (vs, delOneFromUniqSet ds rr)
      _                                       -> acc
    born acc regs = nonDetStrictFoldUniqSet bear acc (getRegs regs)
    bear r acc@(!bad, !vs, !ds) = case regWithFormat_reg r of
      RegVirtual v
        | Just cds <- lookupUFM follow v
        -> ( mark (`elementOfUniqSet` ds) bad cds, addOneToUniqSet vs v, ds )
      RegReal rr
        | rr `elementOfUniqSet` dests
        -> ( clash rr bad vs, vs, addOneToUniqSet ds rr )
      _ -> acc
    -- the candidates of the live followed vregs whose destination is the
    -- register
    clash rr bad vs
      = nonDetStrictFoldUniqSet
          (\v b -> case lookupUFM follow v of
                     Just cds -> mark (== rr) b cds
                     Nothing  -> b)
          bad vs
    -- a set with each candidate of a list whose destination passes p added
    mark p = foldl' (\ !b (c, d) -> if p d then addToUFM_Directly b c () else b)


-- | Allocate the SCCs in order, except that the blocks the garbage collector
-- returns to after a heap or stack check fails (the entry blocks in the given
-- set, those in a cycle excepted), and any SCC only they lead to, go after the
-- others. The first jump allocated into a block records where its live-in vregs
-- are, and later jumps into it move their values to match
-- (Note [The first jump into a block fixes its registers]). Such a block jumps
-- back to the block that made the check, which the hot path also enters;
-- allocated first, its reloads would choose that block's registers, and the hot
-- path would pay the moves at every iteration of a loop the check is in. An
-- entry is allocated at its turn, even before a deferred block that jumps to
-- it: this relies, as allocating a cyclic SCC's entries does, on no vreg being
-- live into an entry; a vreg that was would make the allocator panic where it
-- is read.
--
-- TODO: checkFailures counts as cold some blocks the hot path runs: see
-- Note [Frequencies before register allocation] in GHC.CmmToAsm.CFG.
linearRA_SCCs :: RegAllocConstraint freeRegs instr
              => LabelSet
              -> [BlockId]
              -> BlockMap Regs
              -> [SCC (LiveBasicBlock instr)]
              -> RegM freeRegs [NatBasicBlock instr]

linearRA_SCCs cold entry_ids block_live sccs
 = do   let -- a block the garbage collector returns to
            deferrable (AcyclicSCC (BasicBlock bid _))
              = bid `setMember` cold && bid `setMember` entries
            deferrable _ = False
            go acc deferred [] = return (acc, reverse deferred)
            go acc deferred (scc : rest)
              | deferrable scc = go acc (scc : deferred) rest
              | null deferred  = do acc' <- one acc scc
                                    go acc' deferred rest
              | otherwise = do
                  -- reached unless only deferred blocks lead to it
                  block_assig <- getBlockAssigR
                  if any (isReached entries block_assig . blockId)
                         (flattenSCC scc)
                    then do acc' <- one acc scc
                            go acc' deferred rest
                    else go acc (scc : deferred) rest
        (acc1, deferred) <- go [] [] sccs
        acc2 <- foldM one acc1 deferred
        return $ reverse acc2
  where
    -- looked up for every block, so a set rather than the list
    entries = setFromList entry_ids
    one acc (AcyclicSCC block) = do
        blocks' <- processBlock block_live block
        return (reverse blocks' ++ acc)
    one acc (CyclicSCC blocks) = do
        blockss' <- process entries block_live blocks
        return (reverse (concat blockss') ++ acc)

{- Note [The first jump into a block fixes its registers]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Of the jumps into a block, the first the allocator allocates records the block's
assignment, where every vreg live into it is (joinToTargets), and every later
jump into the block moves its values there, on the edge or in a fixup block. So
the order in which a block's predecessors are allocated decides where the moves
run. The SCCs come in dependency order, the path into a loop before the loop's
body, so that a loop's head takes the registers of the path into it, and its
back edge pays for any difference at every iteration; firstFreeInOrder's
preferences work to keep a loop's values in its head's registers.
The SCCs linearRA_SCCs defers are allocated after the others.

That first jump decides two more things. A vreg's first-used register
(lookupFirstUsed) is its register in the first assignment that, recorded for a
block it is live into, holds it in a register, not the first register it was
given, and it stands for the rest of the procedure. And a recorded location
keeps a value's stack slot: where the first jump into a loop's head had a
loop-carried value spilled, or in both a register and its slot, every later jump
holding it in a register only stores it on the edge, the back edge at every
iteration.
-}

{- from John Dias's patch 2008/10/16:
   The linear-scan allocator sometimes allocates a block
   before allocating one of its predecessors, which could lead to
   inconsistent allocations. Make it so a block is only allocated
   if a predecessor has set the "incoming" assignments for the block, or
   if it's the procedure's entry block.

   BL 2009/02: Careful. If the assignment for a block doesn't get set for
   some reason then this function will loop. We should probably do some
   more sanity checking to guard against this eventuality.
-}

process :: forall freeRegs instr. (RegAllocConstraint freeRegs instr)
        => LabelSet
        -> BlockMap Regs
        -> [GenBasicBlock (LiveInstr instr)]
        -> RegM freeRegs [[NatBasicBlock instr]]
process entry_ids block_live =
    \blocks -> go blocks [] (return []) False
  where
    go :: [GenBasicBlock (LiveInstr instr)]
       -> [GenBasicBlock (LiveInstr instr)]
       -> [[NatBasicBlock instr]]
       -> Bool
       -> RegM freeRegs [[NatBasicBlock instr]]
    go [] []         accum _madeProgress
      = return $ reverse accum

    go [] next_round accum madeProgress
      | not madeProgress
          {- BUGS: There are so many unreachable blocks in the code the warnings are overwhelming.
             pprTrace "RegAlloc.Linear.Main.process: no progress made, bailing out."
                (  text "Unreachable blocks:"
                $$ vcat (map ppr next_round)) -}
      = return $ reverse accum

      | otherwise
      = go next_round [] accum False

    go (b@(BasicBlock id _) : blocks) next_round accum madeProgress
      = do
          block_assig <- getBlockAssigR
          if isReached entry_ids block_assig id
            then do b' <- processBlock block_live b
                    go blocks next_round (b' : accum) True

            else do go blocks (b : next_round) accum madeProgress

-- | Whether a block can be allocated now: a jump to it has been allocated,
-- recording where its live-in vregs are, or it is an entry.
isReached :: LabelSet -> BlockAssignment -> BlockId -> Bool
isReached entry_ids block_assig bid
  = isJust (lookupBlockAssignment bid block_assig) || bid `setMember` entry_ids


-- | Do register allocation on this basic block
--
processBlock
        :: RegAllocConstraint freeRegs instr
        => BlockMap Regs              -- ^ live regs on entry to each basic block
        -> LiveBasicBlock instr         -- ^ block to do register allocation on
        -> RegM freeRegs [NatBasicBlock instr]   -- ^ block with registers allocated

processBlock block_live (BasicBlock id instrs)
 = do   -- pprTraceM "processBlock" $ text "" $$ ppr (BasicBlock id instrs)
        initBlock id block_live
        platform <- getPlatform
        bassig <- getBlockAssigR
        let recorded dest !ls
              | Just assig <- lookupBlockAssignment dest bassig = assig : ls
              | otherwise = ls
            !locs = foldr recorded []
              [ dest | LiveInstr (Instr instr) _ <- instrs
                     , dest <- jumpDestsOfInstr instr ]
            -- the entries of 'rc_moveDests' for the vregs moved into another
            -- vreg
            loop_moves
              | null locs = []
              | otherwise
              = [ (vr, r) | (vr, d) <- dyingMovesLast platform instrs
                          , r : _ <- [jumpLocRegs (getUnique d) locs] ]
        setBlockContextR locs $ listToUFM $
          loop_moves
          -- unlike dyingMovesLast's, these moves into real registers include
          -- those of a vreg the block writes again after the move, its later
          -- value then preferring that register too. As listToUFM keeps a
          -- vreg's last entry, such a move outranks the vreg's loop move, and
          -- where the block moves the vreg into two real registers, both its
          -- values prefer the second
          ++ [ (vr, rr) | (vr, RegReal rr) <- dyingMoves platform instrs ]

        (instrs', fixups)
                <- linearRA block_live id instrs
        -- pprTraceM "blockResult" $ ppr (instrs', fixups)
        return  $ BasicBlock id instrs' : fixups


-- | Set up the RegM state for the basic block with this BlockId: its
--      current reg assignment, and the freeregs on entry to it.
initBlock :: FR freeRegs
          => BlockId -> BlockMap Regs -> RegM freeRegs ()
initBlock id block_live
 = do   init_free   <- rc_initFree <$> getContextR
        block_assig <- getBlockAssigR
        let -- the assignment recorded on entry to this block, if a jump to
            -- it has been allocated, and none otherwise
            assig     = fromMaybe emptyRegMap
                                  (lookupBlockAssignment id block_assig)
            -- The registers free on entry to this block: every allocatable one
            -- but the real registers live into it and those holding the vregs
            -- of its assignment. Deriving them from the free registers of the
            -- block that first jumped here would leave a real register still
            -- allocated at that jump and not live into this one, R1 above all
            -- where it is allocatable, allocated until this block writes it and
            -- that value dies, so that a value it reloads cannot go back to
            -- that register and the block's jumps may need fixup moves
            -- (#27981). This relies on liveness listing in block_live every
            -- real register live into the block: one it missed would be handed
            -- to a vreg.
            hold (Loc loc _) fr = foldl' (flip frAllocateReg) fr (regsOfLoc loc)
            live_free = allocateRealRegs
                          (mapFindWithDefault noRegs id block_live) init_free
        setFreeRegsR $ nonDetStrictFoldUFM hold live_free assig
                         -- See Note [Unique Determinism and code generation]
        setAssigR assig


-- | Do allocation for a sequence of instructions.
linearRA
        :: forall freeRegs instr. (RegAllocConstraint freeRegs instr)
        => BlockMap Regs                      -- ^ map of what vregs are live on entry to each block.
        -> BlockId                              -- ^ id of the current block, for debugging.
        -> [LiveInstr instr]                    -- ^ liveness annotated instructions in this block.
        -> RegM freeRegs
                ( [instr]                       --   instructions after register allocation
                , [NatBasicBlock instr])        --   fresh blocks of fixup code.
linearRA block_live block_id = go [] []
  where
    go :: [instr]                              -- accumulator for instructions already processed.
       -> [NatBasicBlock instr]                -- accumulator for blocks of fixup code.
       -> [LiveInstr instr]                    -- liveness annotated instructions in this block.
       -> RegM freeRegs
               ( [instr]                       --   instructions after register allocation
               , [NatBasicBlock instr] )       --   fresh blocks of fixup code.
    go !accInstr !accFixups [] = do
        return ( reverse accInstr               -- instrs need to be returned in the correct order.
               , accFixups )                    -- it doesn't matter what order the fixup blocks are returned in.

    go accInstr accFixups (instr:instrs) = do
        (accInstr', new_fixups) <- raInsn block_live accInstr block_id instr
        go accInstr' (new_fixups ++ accFixups) instrs

-- | Do allocation for a single instruction.
raInsn
        :: RegAllocConstraint freeRegs instr
        => BlockMap Regs                         -- ^ map of what vregs are live on entry to each block.
        -> [instr]                              -- ^ accumulator for instructions already processed.
        -> BlockId                              -- ^ the id of the current block, for debugging
        -> LiveInstr instr                      -- ^ the instr to have its regs allocated, with liveness info.
        -> RegM freeRegs
                ( [instr]                       -- new instructions
                , [NatBasicBlock instr])        -- extra fixup blocks

raInsn _     new_instrs _ (LiveInstr ii Nothing)
        | Just n        <- takeDeltaInstr ii
        = do    setDeltaR n
                return (new_instrs, [])

raInsn _     new_instrs _ (LiveInstr ii@(Instr i) Nothing)
        | isMetaInstr ii
        = return (i : new_instrs, [])


raInsn block_live new_instrs id (LiveInstr (Instr instr) (Just live))
 = do
    platform <- getPlatform
    assig    <- getAssigR

    -- If we have a reg->reg move between virtual registers, where the
    -- src register is not live after this instruction, and the dst
    -- register does not already have an assignment,
    -- and the source register is assigned to a register, not to a spill slot,
    -- then we can eliminate the instruction.
    -- (we can't eliminate it if the source register is on the stack, because
    --  we do not want to use one spill slot for different virtual registers)
    case takeRegRegMoveInstr platform instr of
        Just (src,dst)  | Just fmt <- lookupReg src (liveDieRead live),
                          isVirtualReg dst,
                          not (dst `elemUFM` assig),
                          isRealReg src || isInReg src assig -> do
           case src of
              RegReal rr -> setAssigR (addToUFM assig dst (Loc (InReg rr) fmt))
                -- if src is a fixed reg, then we just map dest to this
                -- reg in the assignment.  src must be an allocatable reg,
                -- otherwise it wouldn't be in r_dying.
                -- A virtual src gives dst its location, whatever dst's
                -- preferences: firstFreeInOrder makes up for that only where
                -- src was allocated in this block ('rc_moveDests', its moves
                -- into vregs).
              _virt -> case lookupUFM assig src of
                         Nothing -> panic "raInsn"
                         Just loc ->
                           setAssigR (addToUFM (delFromUFM assig src) dst loc)

           -- we have eliminated this instruction
          {-
          freeregs <- getFreeRegsR
          assig <- getAssigR
          pprTrace "raInsn" (text "ELIMINATED: " <> docToSDoc (pprInstr instr)
                        $$ ppr r_dying <+> ppr w_dying $$ text (show freeregs) $$ ppr assig) $ do
          -}
           return (new_instrs, [])

        _ -> genRaInsn block_live new_instrs id instr live

raInsn _ _ _ instr
        = do
            platform <- getPlatform
            let instr' = fmap (pprInstr platform) instr
            pprPanic "raInsn" (text "no match for:" <> ppr instr')

-- ToDo: what can we do about
--
--     R1 = x
--     jump I64[x] // [R1]
--
-- where x is mapped to the same reg as R1.  We want to coalesce x and
-- R1, but the register allocator doesn't know whether x will be
-- assigned to again later, in which case x and R1 should be in
-- different registers.  Right now we assume the worst, and the
-- assignment to R1 will clobber x, so we'll spill x into another reg,
-- generating another reg->reg move.


isInReg :: Reg -> RegMap Loc -> Bool
isInReg src assig
  | Just (Loc (InReg _) _) <- lookupUFM assig src
  = True
  | otherwise
  = False


genRaInsn :: forall freeRegs instr.
             (RegAllocConstraint freeRegs instr)
          => BlockMap Regs
          -> [instr]
          -> BlockId
          -> instr
          -> Liveness
          -> RegM freeRegs ([instr], [NatBasicBlock instr])

genRaInsn block_live new_instrs block_id instr live = do
-- pprTraceM "genRaInsn" $ ppr (block_id, instr)
  platform <- getPlatform
  case liveUsage live of { RU read written ->
    do
    let dying = map regWithFormat_reg . nonDetEltsUniqSet . getRegs
        -- See Note [Unique Determinism and code generation]
        r_dying = dying (liveDieRead live)
        w_dying = dying (liveDieWrite live)
    let real_written = [ rr                          | RegWithFormat {regWithFormat_reg = RegReal rr} <- written ]
    let virt_written = [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt              <- written ]

    -- we don't need to do anything with real registers that are
    -- only read by this instr.  (the list is typically ~2 elements,
    -- so using nub isn't a problem).
    let virt_read :: [VirtualRegWithFormat]
        virt_read = nubOrdOn virtualRegWithFormat_reg [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt <- read ]

--     do
--         let real_read       = nub [ rr      | (RegReal rr) <- read]
--         freeregs <- getFreeRegsR
--         assig    <- getAssigR

--         pprTraceM "genRaInsn"
--                 (          text "block        = " <+> ppr block_id
--                         $$ text "instruction  = " <+> ppr instr
--                         $$ text "r_dying      = " <+> ppr r_dying
--                         $$ text "w_dying      = " <+> ppr w_dying
--                         $$ text "read         = " <+> ppr real_read    <+> ppr virt_read
--                         $$ text "written      = " <+> ppr real_written <+> ppr virt_written
--                         $$ text "freeregs     = " <+> ppr freeregs
--                         $$ text "assign       = " <+> ppr assig)

    -- (a), (b) allocate real regs for all regs read by this instruction.
    (r_spills, r_allocd) <-
        allocateRegsAndSpill True{-reading-} virt_read [] [] virt_read

    -- (c) save any temporaries which will be clobbered by this instruction
    clobber_saves <- saveClobberedTemps real_written r_dying

    -- (d) Update block map for new destinations
    -- NB. do this before removing dead regs from the assignment, because
    -- these dead regs might in fact be live in the jump targets (they're
    -- only dead in the code that follows in the current basic block).
    (fixup_blocks, adjusted_instr)
        <- joinToTargets block_live block_id instr

--     when (not $ null fixup_blocks) $ pprTraceM "genRA:FixBlocks" $ ppr fixup_blocks

    -- Debugging - show places where the reg alloc inserted
    -- assignment fixup blocks.
    -- when (not $ null fixup_blocks) $
    --    pprTrace "fixup_blocks" (ppr fixup_blocks) (return ())

    -- (e) Delete all register assignments for temps which are read
    --     (only) and die here.  Update the free register list.
    releaseRegs r_dying

    -- (f) Mark regs which are clobbered as unallocatable
    clobberRegs real_written

    -- (g) Allocate registers for temporaries *written* (only)
    (w_spills, w_allocd) <-
        allocateRegsAndSpill False{-writing-} virt_written [] [] virt_written

    -- (h) Release registers for temps which are written here and not
    -- used again.
    releaseRegs w_dying

    let
        -- (i) Patch the instruction
        patch_map :: UniqFM Reg Reg
        patch_map
                = toRegMap $ -- Cast key from VirtualReg to Reg
                             -- See Note [UniqFM and the register allocator]
                  listToUFM
                        [ (virtualRegWithFormat_reg vr, RegReal rr)
                        | (vr, rr) <- zip virt_read    r_allocd
                                   ++ zip virt_written w_allocd ]

        patched_instr :: instr
        patched_instr
                = patchRegsOfInstr platform adjusted_instr patchLookup

        patchLookup :: Reg -> Reg
        patchLookup x
                = case lookupUFM patch_map x of
                        Nothing -> x
                        Just y  -> y

    -- (j) free up stack slots for dead spilled regs
    -- TODO (can't be bothered right now)

    -- erase reg->reg moves where the source and destination are the same.
    --  If the src temp didn't die in this instr but happened to be allocated
    --  to the same real reg as the destination, then we can erase the move anyway.
    let squashed_instr  = case takeRegRegMoveInstr platform patched_instr of
                                Just (src, dst)
                                 | src == dst   -> []
                                _               -> [patched_instr]

    -- On the use of @reverse@ below.
    -- Since we can have spills and reloads produce multiple instructions
    -- we need to ensure they are emitted in the correct order.  We used to only
    -- emit single instructions in mkSpill/mkReload/mkRegRegMove.
    -- As such order of spills and reloads didn't matter.  However,  with
    -- multiple instructions potentially issued by those functions we need to be
    -- careful to not break execution order. Reversing the spills (clobber will
    -- also spill), will ensure they are emitted in the right order.
    --
    -- See also Ticket 19910 for changing the return type from [] to OrdList.

    -- For debugging, uncomment the follow line and the mkComment lines.
    -- u <- getUniqueR
    let code = concat [ --  mkComment (text "<genRaInsn(" <> ppr u <> text ")>")
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):squashed>")]
                        squashed_instr
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):w_spills>")
                      , reverse w_spills
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):r_spills>")
                      , reverse r_spills
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):clobber_saves>")
                      , reverse clobber_saves
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):new_instrs>")
                      , new_instrs
                        -- ,mkComment (text "</genRaInsn(" <> ppr u <> text ")>")
                      ]

--    pprTrace "patched-code" ((vcat $ map (docToSDoc . pprInstr) code)) $ do
--    pprTrace "patched-fixup" ((ppr fixup_blocks)) $ do

    return (code, fixup_blocks)

  }

-- -----------------------------------------------------------------------------
-- releaseRegs

releaseRegs :: FR freeRegs => [Reg] -> RegM freeRegs ()
releaseRegs regs = do
  assig <- getAssigR
  free <- getFreeRegsR

  let loop assig !free [] = do setAssigR assig; setFreeRegsR free; return ()
      loop assig !free (RegReal rr : rs) = loop assig (frReleaseReg rr free) rs
      loop assig !free (r:rs) =
         case lookupUFM assig r of
         Just (Loc (InBoth real _) _) ->
           loop (delFromUFM assig r)
                (frReleaseReg real free) rs
         Just (Loc (InReg real) _) ->
           loop (delFromUFM assig r)
                (frReleaseReg real free) rs
         _ ->
           loop (delFromUFM assig r) free rs
  loop assig free regs

-- -----------------------------------------------------------------------------
-- Clobber real registers

-- For each temp in a register that is going to be clobbered:
--      - if the temp dies after this instruction, do nothing
--      - otherwise, put it somewhere safe (another reg if possible,
--              otherwise spill and record InBoth in the assignment).
--      - for allocateRegs on the temps *read*,
--      - clobbered regs are allocatable.
--
--      for allocateRegs on the temps *written*,
--        - clobbered regs are not allocatable.
--

saveClobberedTemps
        :: forall instr freeRegs.
           (Instruction instr, FR freeRegs)
        => [RealReg]             -- ^ real registers clobbered by this instruction
        -> [Reg]                 -- ^ registers which are no longer live after this instruction,
                                 -- because read for the last time
        -> RegM freeRegs [instr] -- return: instructions to spill any temps that will
                                 -- be clobbered.

saveClobberedTemps [] _
        = return []

saveClobberedTemps clobbered dying
 = do
        assig   <- getAssigR
        (assig',instrs) <- nonDetStrictFoldUFM_DirectlyM maybe_spill (assig,[]) assig
        setAssigR assig'
        return $ -- mkComment (text "<saveClobberedTemps>") ++
                 instrs
--              ++ mkComment (text "</saveClobberedTemps>")
   where
     -- Unique represents the VirtualReg
     -- Here we separate the cases which we do want to spill from these we don't.
     maybe_spill :: Unique
                 -> (RegMap Loc,[instr])
                 -> Loc
                 -> RegM freeRegs (RegMap Loc,[instr])
     maybe_spill !temp !(assig,instrs) !loc =
        case loc of
                -- This is non-deterministic but we do not
                -- currently support deterministic code-generation.
                -- See Note [Unique Determinism and code generation]
                Loc (InReg reg) fmt
                    | any (realRegsAlias reg) clobbered
                    , temp `notElem` map getUnique dying
                    -> clobber temp (assig,instrs) (RealRegUsage reg fmt)
                _ -> return (assig,instrs)

     -- See Note [UniqFM and the register allocator]
     clobber :: Unique -> (RegMap Loc,[instr]) -> RealRegUsage -> RegM freeRegs (RegMap Loc,[instr])
     clobber temp (assig,instrs) (RealRegUsage reg fmt)
       = do config <- getConfig
            platform <- getPlatform

            freeRegs <- getFreeRegsR
            let regclass = targetClassOfRealReg platform reg
                freeRegs_thisClass = frGetFreeRegs regclass freeRegs

            case filter (`notElem` clobbered) freeRegs_thisClass of

              -- (1) we have a free reg of the right class that isn't
              -- clobbered by this instruction; use it to save the
              -- clobbered value.
              free@(_ : _) -> do
                  my_reg <- firstFreeInOrder temp free
                  setFreeRegsR (frAllocateReg my_reg freeRegs)

                  let new_assign = addToUFM_Directly assig temp (Loc (InReg my_reg) fmt)
                  let instr = mkRegRegMoveInstr config fmt
                                  (RegReal reg) (RegReal my_reg)

                  return (new_assign,(instr : instrs))

              -- (2) no free registers: spill the value
              [] -> do

                  (spill, slot)   <- spillR (RegWithFormat (RegReal reg) fmt) temp

                  -- record why this reg was spilled for profiling
                  recordSpill (SpillClobber temp)

                  let new_assign  = addToUFM_Directly assig temp (Loc (InBoth reg slot) fmt)

                  return (new_assign, (spill ++ instrs))




-- | Mark all these real regs as allocated,
--      and kick out their vreg assignments.
--
clobberRegs :: FR freeRegs => [RealReg] -> RegM freeRegs ()
clobberRegs []
        = return ()

clobberRegs clobbered
 = do   platform <- getPlatform
        freeregs <- getFreeRegsR
        setFreeRegsR $! frClobberRegs platform clobbered freeregs

        assig           <- getAssigR
        setAssigR $! clobber assig (nonDetUFMToList assig)
          -- This is non-deterministic but we do not
          -- currently support deterministic code-generation.
          -- See Note [Unique Determinism and code generation]

   where
        -- if the temp was InReg and clobbered, then we will have
        -- saved it in saveClobberedTemps above.  So the only case
        -- we have to worry about here is InBoth.  Note that this
        -- also catches temps which were loaded up during allocation
        -- of read registers, not just those saved in saveClobberedTemps.

        clobber :: RegMap Loc -> [(Unique,Loc)] -> RegMap Loc
        clobber assig []
                = assig

        clobber assig ((temp, Loc (InBoth reg slot) regFmt) : rest)
                | any (realRegsAlias reg) clobbered
                = clobber (addToUFM_Directly assig temp (Loc (InMem slot) regFmt)) rest

        clobber assig (_:rest)
                = clobber assig rest

-- -----------------------------------------------------------------------------
-- allocateRegsAndSpill

-- Why are we performing a spill?
data SpillLoc = ReadMem StackSlot Format -- reading from register only in memory
              | WriteNew                 -- writing to a new variable
              | WriteMem                 -- writing to register only in memory
-- Note that ReadNew is not valid, since you don't want to be reading
-- from an uninitialized register.  We also don't need the location of
-- the register in memory, since that will be invalidated by the write.
-- Technically, we could coalesce WriteNew and WriteMem into a single
-- entry as well. -- EZY

-- This function does several things:
--   For each temporary referred to by this instruction,
--   we allocate a real register (spilling another temporary if necessary).
--   We load the temporary up from memory if necessary.
--   We also update the register assignment in the process, and
--   the list of free registers and free stack slots.

allocateRegsAndSpill
        :: forall freeRegs instr. (FR freeRegs, Instruction instr)
        => Bool               -- True <=> reading (load up spilled regs)
        -> [VirtualRegWithFormat] -- don't push these out
        -> [instr]            -- spill insns
        -> [RealReg]          -- real registers allocated (accum.)
        -> [VirtualRegWithFormat] -- temps to allocate
        -> RegM freeRegs ( [instr] , [RealReg])

allocateRegsAndSpill _       _    spills alloc []
        = return (spills, reverse alloc)

allocateRegsAndSpill reading keep spills alloc (r@(VirtualRegWithFormat vr vrFmt):rs)
 = do   assig <- toVRegMap <$> getAssigR
        -- pprTraceM "allocateRegsAndSpill:assig" (ppr (r:rs) $$ ppr assig)
        -- See Note [UniqFM and the register allocator]
        let doSpill = allocRegsAndSpill_spill reading keep spills alloc r rs assig
        case lookupUFM assig vr of
                -- case (1a): already in a register
                Just (Loc (InReg my_reg) in_reg_fmt) -> do
                  -- (RF1) from Note [Allocated register formats]:
                  -- writes redefine the format the register is used at.
                  when (not reading && vrFmt /= in_reg_fmt) $
                    setAssigR $ toRegMap $
                      addToUFM assig vr (Loc (InReg my_reg) vrFmt)
                  allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

                -- case (1b): already in a register (and memory)
                Just (Loc (InBoth my_reg _) _) -> do
                  -- NB1. if we're writing this register, update its assignment to be
                  -- InReg, because the memory value is no longer valid.
                  -- NB2. This is why we must process written registers here, even if they
                  -- are also read by the same instruction.
                  when (not reading) $
                    setAssigR $ toRegMap $
                      addToUFM assig vr (Loc (InReg my_reg) vrFmt)
                  allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

                -- Not already in a register, so we need to find a free one...
                Just (Loc (InMem slot) memFmt)
                   | reading   -> doSpill (ReadMem slot memFmt)
                   | otherwise -> doSpill WriteMem
                Nothing | reading   ->
                   pprPanic "allocateRegsAndSpill: Cannot read from uninitialized register" (ppr vr)
                   -- NOTE: if the input to the NCG contains some
                   -- unreachable blocks with junk code, this panic
                   -- might be triggered.  Make sure you only feed
                   -- sensible code into the NCG.  In GHC.Cmm.Pipeline we
                   -- call removeUnreachableBlocks at the end for this
                   -- reason.

                        | otherwise -> doSpill WriteNew

{- Note [Register preferences in the linear allocator]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The preferred registers of a virtual register, the variable below, best first,
from which 'firstFreeInOrder' takes the first that is free:

 * The registers the variable has on entry to the jump targets of the current
   block whose assignments are already recorded, in the order of the block's
   jumps, such as a loop header jumped to from the loop's end: there the
   variable needs no fixup move at the jump (#27980).

 * The real register the current block moves the variable into where it dies,
   such as R1 before an evaluation: allocated there, the variable needs no move;
   or, where the block moves it into another variable, such as a loop variable
   at the loop's end, and does not write it again, the register a recorded jump
   target's assignment gives that one.

 * The register the variable has in the first assignment that, recorded for a
   block it is live into, holds it in a register, if any
   (Note [The first jump into a block fixes its registers]). This way, when we
   allocate for a loop, variables are likely to end up in the same registers at
   the end and start of the loop, avoiding redundant reg-reg moves. That buys
   nothing for a value the current block moves where it dies, which is live into
   no later block, hence the order.

 * The real register a block the variable is live into moves it into where it
   dies, such as R1 for a value evaluated some blocks after it is computed,
   unless that register is live together with the variable somewhere, as
   wideMoveDests counts it, or, on x86-64, the block follows a failed heap or
   stack check.

Preferring every register the variable was assigned to, not only the first, was
tried and turned out to barely matter.

Where none of them is free, the first free register is taken, on x86-64 in this
order: first those no instruction writes while the virtual register is live past
the write, such as %r14 and %rbx for a value live across a C call on System V,
so that the value need not move out of their way; and among those, as among the
rest, R1's register, %rbx, last, as an evaluation and a return write it: a value
given it may have to move out of their way, and a loop variable given it at the
loop's entry moves at every iteration (#27981). On x86-64, for a virtual
register with preferences, the free registers that a jump target's recorded
assignment gives any virtual register are left out where others remain, its own
there being among its preferences. This applies to the fallback only: a free
preferred register is taken even where a recorded jump target gives it to
another virtual register, as leaving those out too made more join fixups cyclic
and cost containers 0.13% and vector 0.11% more instructions (nofib's puzzle
2.7%). Leaving them out for one with no preferences too gains nothing measured
and costs compile time, the list of registers to leave out then being built for
every such allocation.
-}

-- | Of the given free registers of a class, the one to take for the
-- virtual register with the given unique: the first of its preferred
-- registers, best first, that is free, else on x86-64 the first free one in the
-- order Note [Register preferences in the linear allocator] gives, and
-- elsewhere the first free one.
firstFreeInOrder :: Unique -> [RealReg] -> RegM freeRegs RealReg
firstFreeInOrder _ [] = panic "firstFreeInOrder: no free register"
firstFreeInOrder u free@(r0 : _) = do
  platform <- getPlatform
  bassig <- getBlockAssigR
  RA_Context { rc_jumpLocs  = locs
             , rc_moveDests = moves
             , rc_wideDests = wide
             , rc_clobbers  = clobbers } <- getContextR
  let !jl = jumpLocRegs u locs
      !mv = lookupUFM_Directly moves u
      !fu = lookupFirstUsed u bassig
      !wd = lookupUFM_Directly wide u
      isFree r = r `elem` free
      -- the preference where it is free, else the next one
      pick (Just r) _ | isFree r = Just r
      pick _        k = k
      noPrefs = null jl && isNothing mv && isNothing fu && isNothing wd
      chosen = case find isFree jl of
                 Nothing -> pick mv (pick fu (pick wd Nothing))
                 found   -> found
  return $! case chosen of
    Just r -> r
    Nothing
      | ArchX86_64 <- platformArch platform
      , Just !r1 <- globalRegMaybe platform (VanillaReg 1)
      -> let !cl = lookupWithDefaultUFM_Directly clobbers 0 u
             kept (RealRegSingle n) = not (testBit cl n)
             -- the free ones to choose from: for one with preferences, those
             -- no jump target's assignment gives a virtual register if any
             -- remain, and all of them otherwise
             fs | noPrefs    = free
                | otherwise  = case filter (`notElem` reserved) free of
                                 []  -> free
                                 fs' -> fs'
             reserved = [ r | assig <- locs, Loc loc _ <- nonDetEltsUFM assig
                            , r <- regsOfLoc loc ]
             -- those no instruction writes while the vreg is live past it,
             -- then the rest, R1's last in each part
             part p = [ r | r <- fs, r /= r1, p r ]
                      ++ [ r1 | r1 `elem` fs, p r1 ]
             {-# INLINE part #-}  -- so that the lists fuse
         in fromMaybe r0 (listToMaybe (part kept ++ part (not . kept)))
      | otherwise
      -> r0

-- | 'jumpLocRegs' u locs: the registers the recorded jump-target assignments
-- locs give the virtual register with unique u, in order.
jumpLocRegs :: Unique -> [RegMap Loc] -> [RealReg]
{-# INLINE jumpLocRegs #-}  -- out of line it allocates more
jumpLocRegs u locs = foldr at [] locs
  where at assig !rs
          | Just (Loc loc _) <- lookupUFM_Directly assig u = regsOfLoc loc ++ rs
          | otherwise                                       = rs

-- reading is redundant with reason, but we keep it around because it's
-- convenient and it maintains the recursive structure of the allocator. -- EZY
allocRegsAndSpill_spill :: (FR freeRegs, Instruction instr)
                        => Bool
                        -> [VirtualRegWithFormat]
                        -> [instr]
                        -> [RealReg]
                        -> VirtualRegWithFormat
                        -> [VirtualRegWithFormat]
                        -> UniqFM VirtualReg Loc
                        -> SpillLoc
                        -> RegM freeRegs ([instr], [RealReg])
allocRegsAndSpill_spill reading keep spills alloc r@(VirtualRegWithFormat vr vrFmt) rs assig spill_loc
 = do   platform <- getPlatform
        freeRegs <- getFreeRegsR
        let regclass = classOfVirtualReg (platformArch platform) vr
            freeRegs_thisClass = frGetFreeRegs regclass freeRegs :: [RealReg]

        case freeRegs_thisClass of
         -- case (2): we have a free register
         _ : _ ->
           do   final_reg <- firstFreeInOrder (getUnique vr) freeRegs_thisClass

                spills'   <- loadTemp r spill_loc final_reg spills

                setAssigR $ toRegMap
                          $ (addToUFM assig vr $! newLocation spill_loc $ RealRegUsage final_reg vrFmt)
                setFreeRegsR $  frAllocateReg final_reg freeRegs

                allocateRegsAndSpill reading keep spills' (final_reg : alloc) rs


          -- case (3): we need to push something out to free up a register
         [] ->
           do   let inRegOrBoth (InReg _) = True
                    inRegOrBoth (InBoth _ _) = True
                    inRegOrBoth _ = False
                let candidates' :: UniqFM VirtualReg Loc
                    candidates' =
                      flip delListFromUFM (fmap virtualRegWithFormat_reg keep) $
                      filterUFM (inRegOrBoth . locWithFormat_loc) $
                      assig
                      -- This is non-deterministic but we do not
                      -- currently support deterministic code-generation.
                      -- See Note [Unique Determinism and code generation]
                let candidates = nonDetUFMToList candidates'

                -- the vregs we could kick out that are already in a slot
                let compat reg'
                      =  targetClassOfRealReg platform reg'
                      == regclass
                    candidates_inBoth :: [(Unique, RealRegUsage, StackSlot)]
                    candidates_inBoth
                        = [ (temp, RealRegUsage reg fmt, mem)
                          | (temp, Loc (InBoth reg mem) fmt) <- candidates
                          , compat reg ]

                -- the vregs we could kick out that are only in a reg
                --      this would require writing the reg to a new slot before using it.
                let candidates_inReg
                        = [ (temp, RealRegUsage reg fmt)
                          | (temp, Loc (InReg reg) fmt) <- candidates
                          , compat reg ]

                let result

                        -- we have a temporary that is in both register and mem,
                        -- just free up its register for use.
                        | (temp, (RealRegUsage cand_reg old_fmt), slot) : _ <- candidates_inBoth
                        = do    spills' <- loadTemp r spill_loc cand_reg spills
                                let assig1  = addToUFM_Directly assig temp $ Loc (InMem slot) old_fmt
                                let assig2  = addToUFM assig1 vr $! newLocation spill_loc (RealRegUsage cand_reg vrFmt)

                                setAssigR $ toRegMap assig2
                                allocateRegsAndSpill reading keep spills' (cand_reg:alloc) rs

                        -- otherwise, we need to spill a temporary that currently
                        -- resides in a register.
                        | (temp_to_push_out, RealRegUsage cand_reg old_reg_fmt) : _
                                        <- candidates_inReg
                        = do
                                -- Spill what's currently in the register, with the format of what's in the register.
                                (spill_store, slot) <- spillR (RegWithFormat (RegReal cand_reg) old_reg_fmt) temp_to_push_out

                                -- record that this temp was spilled
                                recordSpill (SpillAlloc temp_to_push_out)

                                -- Update the register assignment:
                                --  - the old data is now only in memory,
                                --  - the new data is now allocated to this register;
                                --    make sure to use the new format (#26542)
                                let assig1  = addToUFM_Directly assig temp_to_push_out $ Loc (InMem slot) old_reg_fmt
                                let assig2  = addToUFM assig1 vr $! newLocation spill_loc (RealRegUsage cand_reg vrFmt)
                                setAssigR $ toRegMap assig2

                                -- if need be, load up a spilled temp into the reg we've just freed up.
                                spills' <- loadTemp r spill_loc cand_reg spills

                                allocateRegsAndSpill reading keep
                                        (spill_store ++ spills')
                                        (cand_reg:alloc) rs


                        -- there wasn't anything to spill, so we're screwed.
                        | otherwise
                        = pprPanic ("RegAllocLinear.allocRegsAndSpill: no spill candidates\n")
                        $ vcat
                                [ text "allocating vreg:  " <> text (show vr)
                                , text "assignment:       " <> ppr assig
                                , text "format:           " <> ppr vrFmt
                                , text "freeRegs:         " <> text (showRegs freeRegs)
                                , text "initFreeRegs:     " <> text (showRegs (frInitFreeRegs platform `asTypeOf` freeRegs))
                                ]
                        where showRegs = show . map (\reg -> (reg, targetClassOfRealReg platform reg)) . allFreeRegs platform

                result


-- | Calculate a new location after a register has been loaded.
newLocation :: SpillLoc -> RealRegUsage -> Loc
-- if the tmp was read from a slot, then now its in a reg as well
newLocation (ReadMem slot memFmt) (RealRegUsage r _regFmt) =
  -- See Note [Use spilled format when reloading]
  Loc (InBoth r slot) memFmt

-- writes will always result in only the register being available
newLocation _ (RealRegUsage r regFmt) = Loc (InReg r) regFmt

-- | Load up a spilled temporary if we need to (read from memory).
loadTemp
        :: (Instruction instr)
        => VirtualRegWithFormat   -- the temp being loaded
        -> SpillLoc     -- the current location of this temp
        -> RealReg      -- the hreg to load the temp into
        -> [instr]
        -> RegM freeRegs [instr]

loadTemp (VirtualRegWithFormat vreg _fmt) (ReadMem slot memFmt) hreg spills
 = do
        -- See Note [Use spilled format when reloading]
        insn <- loadR (RegWithFormat (RegReal hreg) memFmt) slot
        recordSpill (SpillLoad $ getUnique vreg)
        return  $  {- mkComment (text "spill load") : -} insn ++ spills

loadTemp _ _ _ spills =
   return spills

{- Note [Allocated register formats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We uphold the following principle for the format at which we keep track of
alllocated registers:

  RF1. Writes redefine the format.

    When we write to a register 'r' at format 'fmt', we consider the register
    to hold that format going forwards.

    (In cases where a partial write is desired, the move instruction should
     specify that the destination format is the full register, even if, say,
     the instruction only writes to the low 64 bits of the register.
     See also Wrinkle [Don't allow scalar partial writes] in
     Note [Register formats in liveness analysis] in GHC.CmmToAsm.Reg.Liveness.)

  RF2. Reads from a register do not redefine its format.

    Generally speaking, as explained in Note [Register formats in liveness analysis]
    in GHC.CmmToAsm.Reg.Liveness, when computing the used format from a collection
    of reads, we take a least upper bound.

It is particularly important to get (RF1) correct, as otherwise we can end up in
the situation of T26411b, where code such as

  movsd .Ln6m(%rip),%v1
  shufpd $0,%v1,%v1

we start off with %v1 :: F64, but after shufpd (which broadcasts the low part
to the high part) we must consider that %v1 :: F64x2. If we fail to do that,
then we will silently discard the top bits in spill/reload operations.
-}

{- Note [Use spilled format when reloading]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We always reload at the full format that a register was spilled at. The rationale
is as follows:

  1. If later instructions only need the lower 64 bits of an XMM register,
     then we should have only spilled the lower 64 bits in the first place.
     (Whether this is true currently is another question.)
  2. If later instructions need say 128 bits, then we should immediately load
     the entire 128 bits, as this avoids multiple load instructions.

For (2), consider the situation of #26526, where we need to spill around a C
call (because we are using the System V ABI with no callee saved XMM registers).
Before register allocation, we have:

  vmovupd %v1 %v0
  call ...
  movsd   %v0 %v3
  movhlps %v0 %v4

The contents of %v0 need to be preserved across the call. We must spill %v0 at
format F64x2 (as later instructions need the entire 128 bits), and reload it
later. We thus expect something like:

  vmovupd %xmm1    %xmm0
  vmovupd %xmm0    72(%rsp) -- spill to preserve
  call ...
  vmovupd 72(%rsp) %xmm0    -- restore
  movsd   %xmm0    %xmm3
  movhlps %xmm0    %xmm4

This is certainly better than doing two loads from the stack, e.g.

  call ...
  movsd   72(%rsp) %xmm0 -- restore only lower 64 bits
  movsd   %xmm0    %xmm3
  vmovupd 72(%rsp) %xmm0 -- restore the full 128 bits
  movhlps %xmm0    %xmm4

The latter being especially risky because we don't want to believe %v0 is 'InBoth'
with format F64. The risk is that, when allocating registers for the 'VMOVUPD'
instruction, we think our data is already in a register and thus doesn't need to
be reloaded from memory, when in fact we have only loaded the lower 64 bits of
the data.
-}
