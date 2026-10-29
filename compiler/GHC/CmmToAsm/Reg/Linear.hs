
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
                    in memory only if it was spilled or was in memory too.

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

        (f) Mark all registers clobbered by this instruction as not free.

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
            WIP: not done; a StackMap never frees a slot.

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
import qualified GHC.Data.Word64Map as M
import GHC.Platform
import qualified GHC.Platform.X86_64 as X86_64 (globalRegMaybe)
import GHC.Exts (lazy)

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
allocator: the specialiser does not specialise a NOINLINE function. With 'go' at
every target, the compiler's code was 2.1 MB larger and it allocated 0.064% more
compiling !16269's CmmDiamondChain at -O0 and 0.052% more at -O and -O2, on GHC
10.1.20261108.
'cmmTopLiveness', in "GHC.CmmToAsm.Reg.Liveness", is specialised to x86's
instructions likewise. Importing "GHC.CmmToAsm.X86" for its instance makes no
import cycle: of the register allocator's modules, the x86 code generator
imports only "GHC.CmmToAsm.Reg.Target". Without the two SPECIALISE pragmas, the
compiler ran 3.3% more instructions and allocated 2.0% more compiling T3294 at
-O0, 1.2% and 0.66% more at -O and 1.2% and 0.64% more at -O2, and its code was
516 KB smaller; without the allocator's alone, 2.5% and 1.7% more at -O0,
0.89% and 0.55% at -O and 0.87% and 0.54% at -O2, and 455 KB, on GHC
10.1.20261108. Without the liveness pass's alone, the compiler ran 0.87% more
instructions and allocated 0.32% more compiling T3294 at -O0, 0.33% and 0.11%
more at -O and 0.32% and 0.11% more at -O2, and as much with INLINABLE in its
place, on GHC 10.1.20261108.
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
                --      stays at the front of the output, sorted only where it
                --      is not: sorting always made the compiler run 0.36% more
                --      instructions and allocate 0.48% more compiling T9961 at
                --      -O0, 0.11% and 0.14% more compiling !16269's
                --      CmmDiamondChain at -O and 0.10% and 0.14% more at -O2,
                --      on GHC 10.1.20261108
                let !final_blocks' = case final_blocks of
                      b : _ | blockId b == first_id -> final_blocks
                      _ -> sortOn ((/= first_id) . blockId) final_blocks

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
--   an entry in the block map or it is an entry block of the procedure.
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
                $ linearRA_SCCs cold entry_ids js block_live sccs
        -- each block's jump targets, read by the allocation order and the
        -- context alike
        js = jumpSuccs sccs
        platform = ncgPlatform config
        ctx = RA_Context
                { rc_jumpLocs  = []
                , rc_moveDests = mkMoveDests emptyUFM
                , rc_claims    = emptyUFM
                , rc_wideDests = wideMoveDests platform cold block_live sccs
                , rc_dests     = []
                , rc_succs     = js
                  -- built where read
                , rc_joins     = joinBlocks cold js
                , rc_live      = block_live
                  -- only the blocks with such moves, built where first read
                , rc_dying     = foldl' (dyingIn platform) mapEmpty (flattenSCCs sccs)
                  -- used by x86-64's fallback alone, and elsewhere a thunk that
                  -- would keep the SCCs alive (a plain map, the architecture
                  -- tested where it is used, made the compiler run 0.010% more
                  -- instructions compiling T3294 at -O0 and 0.004% and 0.003%
                  -- more at -O and -O2, the cause unidentified, on GHC
                  -- 10.1.20261108)
                , rc_clobbers  = case platformArch platform of
                    ArchX86_64 -> Just (regClobbers platform sccs)
                    _          -> Nothing
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
-- TODO: every write counts, those in the blocks that follow a failed heap or
-- stack check too, which run seldom; a write could count by how often its block
-- runs: see Note [Frequencies before register allocation] in GHC.CmmToAsm.CFG.
regClobbers :: Instruction instr
            => Platform -> [SCC (LiveBasicBlock instr)]
            -> UniqFM VirtualReg Word64
regClobbers platform sccs
  = foldl' (\acc (hit, s) -> nonDetStrictFoldUniqSet (mark hit) acc s)
           direct byHit
  where
    (direct, byHit) = foldl' (\m (BasicBlock _ instrs) -> foldl' ins m instrs)
                             (emptyUFM, []) (flattenSCCs sccs)
    -- each write's registers live past it: a small set marked at once, a vreg
    -- at a time, its mask looked up; a large one, as most divisions
    -- of !16269's CmmDiamondChain have, united with the others written with
    -- the same mask, the masks marked at the end, which costs less than
    -- a lookup for each vreg. Every set marked a vreg at a time, the compiler
    -- ran 2.4% more instructions compiling CmmDiamondChain at -O0 and 2.0%
    -- more at -O1 and -O2, but 0.12% fewer compiling T9961 at -O0; every set
    -- united, 0.49% fewer for CmmDiamondChain at -O0 but 0.09% more for T9961,
    -- allocating 0.29% more there, when this was added.
    ins (!d, !m) (LiveInstr (Instr i) (Just l))
      -- (the set is empty where the instruction writes no real register, which
      -- the test finds sooner than the mask: the mask first, the compiler ran
      -- 0.13% more instructions compiling T3294 at -O0 and 0.049% and 0.048%
      -- more at -O and -O2, on GHC 10.1.20261108)
      | not (isEmptyUniqSet (getRegs (liveAcross l)))
      -- (the members counted only up to the threshold: sizeUniqSet, counting
      -- them all, made the compiler run 0.29% more instructions compiling
      -- !16269's CmmDiamondChain at -O0 and 0.24% more at -O and -O2, on GHC
      -- 10.1.20261108; the 24, above which a set is large, is arbitrary, and
      -- may be revised where a case needs it: with no set large, compiling a
      -- block of 50 to 400 unsafe C calls ran 1.004 to 1.086 times the
      -- instructions, the excess growing as N squared)
      -- hit is not 0 here: the liveness pass leaves liveAcross empty unless
      -- the instruction writes a real register (liveness1U in
      -- GHC.CmmToAsm.Reg.Liveness), and x86-64's are numbered below 32
      = assertPpr (hit /= 0) (ppr (liveAcross l)) $
        if M.compareSize (ufmToIntMap (getUniqSet across)) 24 == GT
          then (d, add m)
          else (nonDetStrictFoldUniqSet (mark hit) d across, m)
      where
        RU rs ws = liveUsage l
        hit  = foldl' (\b r -> case regWithFormat_reg r of
                                 RegReal (RealRegSingle n) ->
                                   assertPpr (n < 64) (ppr n) $ setBit b n
                                 _                         -> b) 0 ws
        -- live past the instruction: live after it, and not only written
        -- by it; a move of the vreg itself into a real register is no
        -- clobber of it
        excl = [ v | Just (RegVirtual v, RegReal _) <- [takeRegRegMoveInstr platform i] ]
               ++ [ v | r <- ws, RegVirtual v <- [regWithFormat_reg r], not (has v rs) ]
        across = foldl' (\s v -> delOneFromUniqSet_Directly s (getUnique v))
                         (getRegs (liveAcross l)) excl
        -- (unionUniqSetsShared, not unionUniqSets: the liveness pass builds the
        -- sets live across consecutive instructions from one another, so they
        -- share all but a few paths, and the set they are united into stays the
        -- newest set itself while every older one is part of it: in a block of
        -- N unsafe C calls, each with the results of the calls before it live
        -- across it, a union walks a few paths, where unionUniqSets copied the
        -- whole set, O(N) a call. With unionUniqSets the compiler ran 3.5% more
        -- instructions compiling a block of 400 such calls at -O1, and 0.13% to
        -- 0.15% more, allocating 0.86% to 1.0% more, compiling a Cmm procedure
        -- of many basic blocks, on GHC 10.1.20261109)
        add [] = [(hit, across)]
        add (e@(h, s) : es)
          | h == hit  = let !s' = unionUniqSetsShared s across in (h, s') : es
          | otherwise = e : add es
        has v = any (\r -> case regWithFormat_reg r of
                              RegVirtual v' -> v' == v
                              _             -> False)
    ins dm _ = dm
    -- or's hit into the vreg's mask: or'ing is commutative, so the folds over
    -- unique sets that call mark give the same map in any order
    mark hit r a = case regWithFormat_reg r of
      RegVirtual v | lookupWithDefaultUFM a 0 v .&. hit /= hit
                   -> addToUFM_C (.|.) a v hit
      _            -> a

-- | The virtual register the instruction moves where it dies, and the move's
-- destination, if it is such a move.
dyingMove :: Instruction instr
          => Platform -> instr -> Liveness -> Maybe (VirtualReg, Reg)
-- INLINE: out of line it made the compiler run 0.11% more instructions
-- compiling T3294 at -O0 and 0.040% and 0.039% more at -O and -O2, on GHC
-- 10.1.20261108.
{-# INLINE dyingMove #-}
dyingMove platform instr live
  -- (the emptiness test first, most instructions killing no register they read:
  -- without it the compiler ran 0.036% more instructions compiling T3294 at -O0
  -- and 0.020% more compiling !16269's CmmDiamondChain at -O and -O2, on GHC
  -- 10.1.20261108)
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
-- INLINE: out of line it made the compiler allocate 0.59% more compiling T3294
-- at -O0, 0.20% more at -O and 0.19% more at -O2, and run 0.36%, 0.13% and
-- 0.13% more instructions, on GHC 10.1.20261108.
{-# INLINE dyingMoves #-}
dyingMoves platform instrs
  = [ m | LiveInstr (Instr instr) (Just live) <- instrs
        , Just m <- [dyingMove platform instr live] ]

-- | 'dyingMoves' into virtual registers, without the moves of a vreg the block
-- writes again after the move: a preference is looked up by the vreg, so such
-- a move's preference would reach the vreg's later value.
dyingMovesLast :: Instruction instr
               => Platform -> [LiveInstr instr] -> [(VirtualReg, VirtualReg)]
-- Listed in the order of the vregs' uniques, each vreg once, which the
-- caller's listToUFM does not depend on (Note [Unique order in the linear
-- allocator]).
dyingMovesLast platform instrs = nonDetEltsUFM (foldl' step emptyUFM instrs)
  where
    -- the moves so far, by the vreg moved, without those of a vreg written
    -- since
    step ms (LiveInstr (Instr instr) (Just live))
      | Just (vr, RegVirtual d) <- dyingMove platform instr live
      = addToUFM ms' vr (vr, d)
      | otherwise = ms'
      where ms' | isNullUFM ms = ms
                | otherwise    = foldl' unwrite ms (writes (liveUsage live))
    step ms _ = ms
    unwrite ms r = case regWithFormat_reg r of
      RegVirtual v | v `elemUFM` ms -> delFromUFM ms v
      _                             -> ms

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
    -- vreg's only preference, which fallbackReg counts as one: leaving those
    -- out changed no code measured and costs compile allocation.
    -- One walk finds them, and the moves of a vreg into another where it dies
    -- in the same blocks, of which follow takes those of candidates: a walk
    -- for each made the compiler run 0.39% more instructions compiling
    -- !16269's CmmDiamondChain at -O0 and 0.18% more compiling T12707 and
    -- T9961 at -O0 when this was added.
    (cands, vmoves) = foldl' blkMoves (emptyUFM, []) blocks
    blkMoves acc (BasicBlock bid instrs)
      | warm bid live = foldl' mv acc (dyingMoves platform instrs)
      | otherwise     = acc
      where
        live = live_in bid
        mv (!cs, !vm) (vr, RegReal rr)
          | RegVirtual vr `elemRegs` live = (addToUFM cs vr rr, vm)
        mv (!cs, !vm) (a, RegVirtual b)   = (cs, (a, b) : vm)
        mv csvm _                         = csvm
    -- The order in which the folds over and listings of unique sets and maps
    -- here visit their members does not reach the result: each builds a set,
    -- adds or deletes members or tests for one, and born's bear marks a
    -- followed vreg's candidates whose destination is a register born with it
    -- whichever of the two it visits first.
    --
    -- the candidates' destinations, as bits by register number
    dests :: Word64
    dests = nonDetStrictFoldUFM
              (\(RealRegSingle n) m -> assertPpr (n < 64) (ppr n) $ setBit m n)
              0 cands
    inDests (RealRegSingle n) = testBit dests n
    -- each followed vreg to the candidates whose values it holds, with their
    -- destinations: a candidate itself, and each candidate moved into the
    -- vreg where it dies
    follow = plusUFM_C (++) (mapUFM_Directly (\u d -> [(u, d)]) cands)
           $ listToUFM_C (++)
      [ (b, [(getUnique a, d)]) | (a, b) <- vmoves, Just d <- [lookupUFM cands a] ]
    -- bad: the candidates found live together with their destination;
    -- vs: the followed vregs live; ds: the destinations live
    blk bad (BasicBlock bid instrs)
      | not (touched live instrs) = bad
      | otherwise
      = let (bad', vs, ds) = born (bad, emptyUniqSet, 0 :: Word64) live
            -- the vregs the block moves into a destination where they die
            loc = listToUFM [ (vr, rr) | (vr, RegReal rr) <- dyingMoves platform instrs
                                       , inDests rr ]
        in go loc bad' vs ds instrs
      where live = live_in bid
    -- a block finds a candidate bad only where a destination is live into
    -- it or written in it, a move into one where a vreg dies included:
    -- elsewhere no destination is live during its walk, which then marks
    -- nothing; walking every block made the compiler run 3.8% more
    -- instructions compiling !16269's CmmDiamondChain at -O0 and 3.1% more at
    -- -O1 and -O2 when this was added
    touched live instrs
      =  any (\rr -> RegReal rr `elemRegs` live) destList
      || any (any isDest . writes . liveInstrUsage) instrs
    destList = bitsOf dests
    bitsOf 0 = []
    bitsOf m = RealRegSingle (countTrailingZeros m) : bitsOf (m .&. (m - 1))
    isDest r = case regWithFormat_reg r of
      RegReal rr -> inDests rr
      _          -> False
    go _ !bad !_ !_ [] = bad
    go loc !bad !vs !ds (LiveInstr (Instr _) (Just l) : is)
      | quiet (liveDieRead l), not (any seen ws), quiet (liveDieWrite l)
      , isEmptyUniqSet vs || not (any (local loc) ws)
      = go loc bad vs ds is
      | otherwise
      = let !(vs1, ds1)       = dies (vs, ds) (liveDieRead l)
            -- (no followed vreg live, no candidate wanted: the fold over the
            -- writes anyway made the compiler run 0.051% more instructions
            -- compiling nofib's puzzle at -O0 and 0.015% and 0.014% more at -O
            -- and -O2, on GHC 10.1.20261108)
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
      RegReal rr   -> inDests rr
    dies acc regs = nonDetStrictFoldUniqSet die acc (getRegs regs)
    die r acc@(!vs, !ds) = case regWithFormat_reg r of
      RegVirtual v | v `elementOfUniqSet` vs    -> (delOneFromUniqSet vs v, ds)
      RegReal (RealRegSingle n) | testBit ds n -> (vs, clearBit ds n)
      _                                        -> acc
    born acc regs = nonDetStrictFoldUniqSet bear acc (getRegs regs)
    bear r acc@(!bad, !vs, !ds) = case regWithFormat_reg r of
      RegVirtual v
        | Just cds <- lookupUFM follow v
        -> ( mark (\(RealRegSingle n) -> testBit ds n) bad cds
           , addOneToUniqSet vs v, ds )
      RegReal rr@(RealRegSingle n)
        | testBit dests n
        -> ( clash rr bad vs, vs, setBit ds n )
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


-- | Allocate the SCCs in order, except that, of the entry blocks other
-- than the procedure's first and outside any cycle, those the garbage
-- collector returns to after a heap or stack check fails (those in the
-- given set) and those that jump into a loop that calls nothing and whose
-- entry assignment is not yet recorded (#27800), and any SCC only they lead
-- to, go after the others. The first jump allocated into a block records
-- where its live-in vregs are, and later jumps into it move their values to
-- match (Note [The first jump into a block fixes its entry assignment] in
-- GHC.CmmToAsm.Reg.Linear.JoinToTargets). Where such a block jumps back to the
-- block that made the check, which the hot path also enters, allocating it
-- first would let its reloads choose that block's registers, and the hot path
-- would pay the moves at every iteration of a loop the check is in. An entry is
-- allocated at its turn, even before a deferred block that jumps to it: this
-- relies, as allocating a cyclic SCC's entries does, on no vreg being live into
-- an entry; a vreg that was would make the allocator panic where it is read.
--
-- TODO: checkFailures counts as cold some blocks the hot path runs: see
-- Note [Frequencies before register allocation] in GHC.CmmToAsm.CFG.
linearRA_SCCs :: RegAllocConstraint freeRegs instr
              => LabelSet
              -> [BlockId]
              -> BlockMap [BlockId]
              -> BlockMap Regs
              -> [SCC (LiveBasicBlock instr)]
              -> RegM freeRegs [NatBasicBlock instr]

linearRA_SCCs cold entry_ids succs block_live sccs
 = do   let -- a block the garbage collector returns to, or a return
            -- continuation that jumps into a loop with no call (#27800)
            deferrable assig (AcyclicSCC (BasicBlock bid _))
              = bid `setMember` entries && bid /= first_id
                && (bid `setMember` cold
                    || any (\d -> d `setMember` no_call_loops
                                  && isNothing (lookupBlockAssignment d assig))
                           (mapFindWithDefault [] bid succs))
            deferrable _ _ = False
            go acc deferred [] = return (acc, reverse deferred)
            go acc deferred (scc : rest) = do
              block_assig <- getBlockAssigR
              -- reached unless only deferred blocks lead to it
              if not (deferrable block_assig scc)
                 && (null deferred
                     || any (isReached entries block_assig . blockId)
                            (flattenSCC scc))
                then do acc' <- one acc scc
                        go acc' deferred rest
                else go acc (scc : deferred) rest
        (acc1, deferred) <- go [] [] sccs
        acc2 <- foldM one acc1 deferred
        return $ reverse acc2
  where
    -- looked up for every block, so a set rather than the list: with `elem` on
    -- the list the compiler ran 33% more instructions compiling T783 at -O0,
    -- 0.33% and 0.32% more compiling T3294 at -O and -O2, and 0.58% more
    -- compiling nofib's puzzle at -O2, on GHC 10.1.20261108
    entries = setFromList entry_ids
    first_id = case entry_ids of { b : _ -> b; [] -> panic "linearRA_SCCs" }
    no_call_loops = setFromList
      [ b | CyclicSCC bs <- sccs, not (any calls bs), BasicBlock b _ <- bs ]
    calls (BasicBlock _ is) = any clobbersMany is
    clobbersMany li
      = length [ () | RegWithFormat (RegReal _) _ <- writes (liveInstrUsage li) ] > 4
    one acc (AcyclicSCC block) = do
        blocks' <- processBlock succs block_live block
        return (reverse blocks' ++ acc)
    one acc (CyclicSCC blocks) = process entries succs block_live acc blocks

{- from John Dias's patch 2008/10/16:
   The linear-scan allocator sometimes allocates a block
   before allocating one of its predecessors, which could lead to
   inconsistent allocations. Make it so a block is only allocated
   if a predecessor has set the "incoming" assignments for the block, or
   if it is one of the procedure's entry blocks.

   A block whose assignment never gets set is dropped: process stops after
   a round in which it allocates no block.

   The first predecessor to be allocated sets the assignment for good:
   see Note [The first jump into a block fixes its entry assignment] in
   GHC.CmmToAsm.Reg.Linear.JoinToTargets.
-}

process :: forall freeRegs instr. (RegAllocConstraint freeRegs instr)
        => LabelSet
        -> BlockMap [BlockId]
        -> BlockMap Regs
        -> [NatBasicBlock instr]      -- the blocks allocated so far, last first
        -> [GenBasicBlock (LiveInstr instr)]
        -> RegM freeRegs [NatBasicBlock instr]
process entry_ids succs block_live =
    \acc blocks -> go blocks [] acc False
  where
    go :: [GenBasicBlock (LiveInstr instr)]
       -> [GenBasicBlock (LiveInstr instr)]
       -> [NatBasicBlock instr]
       -> Bool
       -> RegM freeRegs [NatBasicBlock instr]
    go [] []         accum _madeProgress
      = return accum

    go [] next_round accum madeProgress
      | not madeProgress
          {- BUGS: There are so many unreachable blocks in the code the warnings are overwhelming.
             pprTrace "RegAlloc.Linear.Main.process: no progress made, bailing out."
                (  text "Unreachable blocks:"
                $$ vcat (map ppr next_round)) -}
      = return accum

      | otherwise
      = go next_round [] accum False

    go (b@(BasicBlock id _) : blocks) next_round accum madeProgress
      = do
          block_assig <- getBlockAssigR
          if isReached entry_ids block_assig id
            then do b' <- processBlock succs block_live b
                    go blocks next_round (reverse b' ++ accum) True

            else do go blocks (b : next_round) accum madeProgress

-- | Whether a block can be allocated now: a jump to it has been allocated,
-- recording where its live-in vregs are, or it is an entry.
isReached :: LabelSet -> BlockAssignment -> BlockId -> Bool
isReached entry_ids block_assig bid
  = isJust (lookupBlockAssignment bid block_assig) || bid `setMember` entry_ids


-- | Each block's jump targets.
jumpSuccs :: Instruction instr
          => [SCC (LiveBasicBlock instr)] -> BlockMap [BlockId]
jumpSuccs sccs = mapFromList
  [ (bid, [ d | LiveInstr (Instr i) _ <- instrs, d <- jumpDestsOfInstr i ])
  | BasicBlock bid instrs <- flattenSCCs sccs ]

-- | Do register allocation on this basic block
--
processBlock
        :: RegAllocConstraint freeRegs instr
        => BlockMap [BlockId]         -- ^ each block's jump targets
        -> BlockMap Regs              -- ^ live regs on entry to each basic block
        -> LiveBasicBlock instr         -- ^ block to do register allocation on
        -> RegM freeRegs [NatBasicBlock instr]   -- ^ block with registers allocated

processBlock succs block_live (BasicBlock id instrs)
 = do   -- pprTraceM "processBlock" $ text "" $$ ppr (BasicBlock id instrs)
        initBlock id block_live
        platform <- getPlatform
        bassig <- getBlockAssigR
        let !dests = mapFindWithDefault [] id succs
            -- the jump targets' recorded assignments, and for one not yet
            -- reached, those of the blocks it jumps to, up to three jumps on,
            -- in the order a walk of every path first reaches them. The walk
            -- keeps, for each block it has walked on from with two jumps or
            -- more left, the most it had, and walks on from it again only
            -- with more: the list then loses only repeats, which no use of it
            -- counts, and a block with two jumps or more left is walked on from
            -- at most once for each such number, not once for each path to it,
            -- whose number grows exponentially with the jumps.
            -- Kept before the block's walk, the jumps left would skip a block
            -- reached again round a loop during that walk, and change the order
            recorded :: Int -> LabelMap Int -> [RegMap Loc] -> [BlockId]
                     -> (LabelMap Int, [RegMap Loc])
            recorded !_ !seen ls [] = (seen, ls)
            recorded depth seen ls (dest : dests)
              | Just assig <- lookupBlockAssignment dest bassig
              = recorded depth seen (assig : ls) dests
              | depth > 0, Just ds <- mapLookup dest succs
              , mapFindWithDefault 0 dest seen < depth
              , (seen', ls') <- recorded (depth - 1) seen ls ds
              = let seen'' | depth > 1 = mapInsert dest depth seen'
                           | otherwise = seen'
                in recorded depth seen'' ls' dests
              | otherwise = recorded depth seen ls dests
            -- (three jumps is arbitrary, and may be raised where a case needs
            -- more, at a cost to the compiler: raised to eight, it made
            -- the compiler run 0.66% more instructions compiling !16269's
            -- CmmDiamondChain at -O0, and lifted to 1000, compiling 50 to 400
            -- loops nested or in sequence ran 5 to 334 times the instructions,
            -- the cost growing as N squared)
            -- strict: built lazily, it made the compiler allocate 0.039% more
            -- compiling T9961 at -O0 and 0.016% more compiling !16269's
            -- CmmDiamondChain at -O and -O2, on GHC 10.1.20261108
            !locs = case recorded 3 mapEmpty [] dests of
                      (_, ls) -> reverse ls
            -- the entries of 'rc_moveDests' for the vregs moved into another
            -- vreg
            loop_moves
              -- (none without a recorded target, the block's moves unwalked:
              -- walking them anyway made the compiler run 0.44% more
              -- instructions and allocate 0.30% more compiling T3294 at -O0,
              -- 0.16% and 0.10% more at -O and 0.16% and 0.097% more at -O2, on
              -- GHC 10.1.20261108)
              | null locs = []
              | otherwise
              = [ (vr, r) | (vr, d) <- dyingMovesLast platform instrs
                          , r : _ <- [jumpLocRegs (getUnique d) locs] ]
            -- the vregs the block reads or writes, by their first-used
            -- registers (the usage liveness keeps, 'liveInstrUsage':
            -- computed again, it made the compiler run 0.28% more instructions
            -- and allocate 0.37% more compiling !16269's CmmDiamondChain at
            -- -O0 when this was added)
            claims = claimsOf bassig instrs
        setBlockContextR dests locs claims $ mkMoveDests $ listToUFM $
          loop_moves
          -- unlike dyingMovesLast's, these moves into real registers include
          -- those of a vreg the block writes again after the move, its later
          -- value then preferring that register too: leaving them out changed
          -- no code measured and costs compile time. As listToUFM keeps a
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
                         -- in any order: each register is allocated on its own
                         -- (Note [Unique Determinism and code generation])
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
        (accInstr', new_fixups) <- raInsn block_live accInstr block_id instr instrs
        go accInstr' (new_fixups ++ accFixups) instrs

-- | Do allocation for a single instruction.
raInsn
        :: RegAllocConstraint freeRegs instr
        => BlockMap Regs                         -- ^ map of what vregs are live on entry to each block.
        -> [instr]                              -- ^ accumulator for instructions already processed.
        -> BlockId                              -- ^ the id of the current block, for debugging
        -> LiveInstr instr                      -- ^ the instr to have its regs allocated, with liveness info.
        -> [LiveInstr instr]                    -- ^ the instructions after it in the block.
        -> RegM freeRegs
                ( [instr]                       -- new instructions
                , [NatBasicBlock instr])        -- extra fixup blocks

raInsn _     new_instrs _ (LiveInstr ii Nothing) _
        | Just n        <- takeDeltaInstr ii
        = do    setDeltaR n
                return (new_instrs, [])

raInsn _     new_instrs _ (LiveInstr ii@(Instr i) Nothing) _
        | isMetaInstr ii
        = return (i : new_instrs, [])


raInsn block_live new_instrs id (LiveInstr (Instr instr) (Just live)) rest
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
    -- Nor do we eliminate it where an assignment in rc_jumpLocs, a recorded
    -- jump target's or that of a block up to three jumps past a target not
    -- yet recorded, has dst in a free register other than src's and another
    -- vreg in src's: the move puts dst there, which a jump on the way there
    -- would otherwise have to swap with that vreg. Where none has a vreg in
    -- src's register, the jump moves dst on its own edge, where the move here
    -- would run on every path out of the block.
    RA_Context { rc_jumpLocs = locs } <- getContextR
    freeregs <- getFreeRegsR
    let wantedElsewhere src dst
          | null rs   = False
          | otherwise = not (any (`elem` srcRegs) rs) && any both locs
          where rs = jumpLocRegs (getUnique dst) locs
                -- one recorded assignment giving dst a free register and
                -- another vreg src's
                both tassig = case lookupUFM_Directly tassig (getUnique dst) of
                  Just (Loc l _) -> any isFree (regsOfLoc l) && taken tassig
                  Nothing        -> False
                taken = anyUFM (\ (Loc l _) -> any (`elem` srcRegs)
                                                   (regsOfLoc l))
                srcRegs = case src of
                  RegReal rr -> [rr]
                  _ | Just (Loc l _) <- lookupUFM assig src -> regsOfLoc l
                    | otherwise -> []
                isFree r = r `elem` frGetFreeRegs (targetClassOfRealReg platform r)
                                                  freeregs
    -- TODO: eliminating the move gives dst the register of src, even where
    -- dst is live into a block whose recorded entry assignment holds it
    -- in another register, unless src, allocated in this block, took that
    -- register (rc_moveDests); a jump into that block then needs a
    -- fixup move. Where that register is free and the assignment, one
    -- in rc_jumpLocs, holds another vreg in src's, the move is kept
    -- (wantedElsewhere). See
    -- Note [The first jump into a block fixes its entry assignment] in
    -- GHC.CmmToAsm.Reg.Linear.JoinToTargets.
    case takeRegRegMoveInstr platform instr of
        Just (src,dst)  | Just fmt <- lookupReg src (liveDieRead live),
                          isVirtualReg dst,
                          not (dst `elemUFM` assig),
                          isRealReg src || isInReg src assig,
                          not (wantedElsewhere src dst) -> do
           case src of
              RegReal rr -> setAssigR (addToUFM assig dst (Loc (InReg rr) fmt))
                -- if src is a fixed reg, then we just map dest to this
                -- reg in the assignment.  src must be an allocatable reg,
                -- otherwise it wouldn't be in r_dying.
              _virt -> case lookupUFM assig src of
                -- TODO: a virtual src gives dst its location, whatever dst's
                -- preferences: firstFreeInOrder makes up for that only where
                -- src was allocated in this block ('rc_moveDests', its moves
                -- into vregs).
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

        _ -> genRaInsn block_live new_instrs id instr rest live

raInsn _ _ _ instr _
        = do
            platform <- getPlatform
            let instr' = fmap (pprInstr platform) instr
            pprPanic "raInsn" (text "no match for:" <> ppr instr')

-- | The given real registers from the one paired with the last of the given
-- vregs whose unique is the given one, the pairs as zip makes them, or none.
lastPaired :: Unique -> [VirtualRegWithFormat] -> [RealReg] -> [RealReg]
lastPaired !u = go []
  where
    go !found (v : vs) rs@(_ : rs')
      | getUnique (virtualRegWithFormat_reg v) == u = go rs vs rs'
      | otherwise                                    = go found vs rs'
    go found _ _ = found

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
          -> [LiveInstr instr]
          -> Liveness
          -> RegM freeRegs ([instr], [NatBasicBlock instr])

genRaInsn block_live new_instrs block_id instr rest live = do
-- pprTraceM "genRaInsn" $ ppr (block_id, instr)
  platform <- getPlatform
  case liveUsage live of { RU read written ->
    do
    let real_written = [ rr                          | RegWithFormat {regWithFormat_reg = RegReal rr} <- written ]
    let virt_written = [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt              <- written ]

    -- we don't need to do anything with real registers that are
    -- only read by this instr.
    let virt_read :: [VirtualRegWithFormat]
        virt_read = [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt <- read ]

--     do
--         let real_read       = nub [ rr      | (RegReal rr) <- read]
--         freeregs <- getFreeRegsR
--         assig    <- getAssigR

--         pprTraceM "genRaInsn"
--                 (          text "block        = " <+> ppr block_id
--                         $$ text "instruction  = " <+> ppr instr
--                         $$ text "r_dying      = " <+> ppr (liveDieRead live)
--                         $$ text "w_dying      = " <+> ppr (liveDieWrite live)
--                         $$ text "read         = " <+> ppr real_read    <+> ppr virt_read
--                         $$ text "written      = " <+> ppr real_written <+> ppr virt_written
--                         $$ text "freeregs     = " <+> ppr freeregs
--                         $$ text "assign       = " <+> ppr assig)

    -- (a), (b) allocate real regs for all regs read by this instruction.
    (r_spills, r_allocd) <-
        allocateRegsAndSpill True{-reading-} virt_read [] [] virt_read

    -- (c) save any temporaries which will be clobbered by this instruction
    clobber_saves <- saveClobberedTemps real_written (liveDieRead live) rest

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
    releaseRegs (liveDieRead live)

    -- (f) Mark regs which are clobbered as unallocatable
    clobberRegs real_written

    -- (g) Allocate registers for temporaries *written* (only)
    (w_spills, w_allocd) <-
        allocateRegsAndSpill False{-writing-} virt_written [] [] virt_written

    -- (h) Release registers for temps which are written here and not
    -- used again.
    releaseRegs (liveDieWrite live)

    let
        -- (i) Patch the instruction
        -- Strict: squashed_instr and reload_into both evaluate it, and a lazy
        -- one is a thunk shared between them, built for every instruction,
        -- which made the compiler allocate 0.53% more compiling T3294 at -O0
        -- and 0.18% more at -O and -O2, on GHC 10.1.20261108.
        patched_instr :: instr
        !patched_instr
                = patchRegsOfInstr platform adjusted_instr patchLookup

        -- the real register of the last written pair whose vreg is x, else of
        -- the last read pair; x where none is
        patchLookup :: Reg -> Reg
        patchLookup x
          = let !u = getUnique x
            in case lastPaired u virt_written w_allocd of
                 r : _ -> RegReal r
                 []    -> case lastPaired u virt_read r_allocd of
                            r : _ -> RegReal r
                            []    -> x

    -- (j) free up stack slots for dead spilled regs
    -- TODO (can't be bothered right now)

    -- erase reg->reg moves where the source and destination are the same.
    --  If the src temp didn't die in this instr but happened to be allocated
    --  to the same real reg as the destination, then we can erase the move anyway.
    let squashed_instr  = case takeRegRegMoveInstr platform patched_instr of
                                Just (src, dst)
                                 | src == dst   -> []
                                _               -> [patched_instr]

    -- A vreg reloaded, in one instruction, only to be moved where it dies into
    -- a register that is free by then (one another vreg held until the clobber
    -- saves above, or one the move's destination vreg took with no store, free
    -- or held by a vreg that is in its slot too): reloaded straight into the
    -- move's destination, as where that register is free at the reload, and
    -- the move goes.  Nothing else changes, the register the reload took being
    -- freed with the vreg.
    -- (the reload and spill lists first: most instructions have neither; with
    -- the move first the compiler ran 0.11% and 0.075% more instructions
    -- compiling T9961 and T12707 at -O0 and 0.020% and 0.018% more compiling
    -- T12707 at -O and -O2, but 0.062% fewer compiling T3294 at -O0 and 0.014%
    -- and 0.015% fewer at -O and -O2, its allocation within 0.002%, on GHC
    -- 10.1.20261108)
    let reload_into = case (r_spills, w_spills, r_allocd, takeRegRegMoveInstr platform instr) of
          ([ld], [], [ry], Just (src@(RegVirtual _), _))
            | src `elemRegs` liveDieRead live
            , Just (RegReal ry', RegReal rx) <- takeRegRegMoveInstr platform patched_instr
            , ry' == ry, ry /= rx
            -> Just [patchRegsOfInstr platform ld
                       (\r -> if r == RegReal ry then RegReal rx else r)]
          _ -> Nothing

    -- On the use of @reverse@ (@onto@) below.
    -- Since we can have spills and reloads produce multiple instructions
    -- we need to ensure they are emitted in the correct order.  We used to only
    -- emit single instructions in mkSpill/mkReload/mkRegRegMove.
    -- As such order of spills and reloads didn't matter.  However,  with
    -- multiple instructions potentially issued by those functions we need to be
    -- careful to not break execution order. Reversing the spills (clobber will
    -- also spill), will ensure they are emitted in the right order.
    --
    -- See also Ticket 19910 for changing the return type from [] to OrdList.

    -- `onto xs acc` is reverse xs ++ acc: the lists joined strictly, innermost
    -- first, as most instructions have no spill, reload or save; appended
    -- lazily, each is a thunk an instruction, kept until the block's end, and
    -- the compiler allocated 1.8% more compiling T3294 at -O0 and 0.59% and
    -- 0.57% more at -O and -O2, and ran 1.2%, 0.41% and 0.40% more
    -- instructions, on GHC 10.1.20261111
    let onto :: [instr] -> [instr] -> [instr]
        onto xs acc = foldl' (flip (:)) acc xs
        !saved = clobber_saves `onto` new_instrs
        !code = case reload_into of
          Just ld -> ld `onto` saved
          Nothing -> let !spilled = w_spills `onto` (r_spills `onto` saved)
                     in squashed_instr `onto` spilled

--    pprTrace "patched-code" ((vcat $ map (docToSDoc . pprInstr) code)) $ do
--    pprTrace "patched-fixup" ((ppr fixup_blocks)) $ do

    return (code, fixup_blocks)

  }

-- -----------------------------------------------------------------------------
-- releaseRegs

releaseRegs :: FR freeRegs => Regs -> RegM freeRegs ()
-- nothing dies: the state stays as it is, unwritten, each write building a new
-- RA_State: written here and in saveClobberedTemps where nothing is clobbered,
-- the compiler allocated 2.3% more compiling T3294 at -O0 and 0.79% and 0.76%
-- more at -O and -O2, and ran 0.99%, 0.38% and 0.37% more instructions, and
-- written here alone, it allocated 2.3%, 0.78% and 0.76% more, on GHC
-- 10.1.20261108
releaseRegs regs
  | isEmptyUniqSet (getRegs regs) = return ()
  | otherwise = do
      assig <- getAssigR
      free <- getFreeRegsR

      -- each register is released on its own, so the fold's order does not
      -- matter (Note [Unique Determinism and code generation])
      let release r (assig', free') = case regWithFormat_reg r of
            RegReal rr -> let !f = frReleaseReg rr free' in (assig', f)
            v -> let !a = delFromUFM assig' v
                     !f = case lookupUFM assig' v of
                            Just (Loc (InBoth real _) _) -> frReleaseReg real free'
                            Just (Loc (InReg real) _)    -> frReleaseReg real free'
                            _                            -> free'
                 in (a, f)
      case nonDetStrictFoldUniqSet release (assig, free) (getRegs regs) of
        (assig', free') -> do setAssigR assig'; setFreeRegsR free'

-- -----------------------------------------------------------------------------
-- Clobber real registers

-- For each temp in a register that is going to be clobbered:
--      - if the temp dies after this instruction, do nothing
--      - otherwise, put it somewhere safe (another reg, not written
--              before the temp's next read, if possible, otherwise spill
--              and record InMem in the assignment).
--      - a temp in its stack slot too is recorded InMem there.
--      - for allocateRegs on the temps *read*,
--      - clobbered regs are allocatable.
--
--      for allocateRegs on the temps *written*,
--        - clobbered regs are not allocatable.
--

{- Note [Unique order in the linear allocator]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fold over the assignment visits its vregs in the order of their uniques, which
compares their tags first and then their numbers.

A vreg named in the Cmm keeps the unique of its local register, and the local
register of an Id keeps the Id's unique (idToReg in GHC.StgToCmm.Env), so its
tag is that of the pass that made the Id. -fobject-determinism renames every
local register of StgToCmm's output, in the order it visits them, with uniques
tagged 'Q', which sort before the instruction selector's 'n'. Among the tags
that vregs carry are, in their order (that renaming replaces 'B', 'g', 'r', 't'
and StgToCmm's own 'c'):

* 'B', Ids that CorePrep's eta-expansion makes, such as the arguments of a
  constructor worker's entry code at -O0;
* 'H', on i386 and 32-bit PowerPC, the high half of a 64-bit vreg
  (getHiVirtualRegFromLo in GHC.Platform.Reg);
* 'T', the temporaries of -fcmm-thread-sanitizer;
* 'c', StgToCmm's own temporaries, and a .cmm file's own registers;
* 'g', Ids made from the supply of the STG pipeline's passes (runStgM in
  GHC.Stg.Pipeline), such as the arguments that unarisation makes for an unboxed
  sum;
* 'n', the instruction selector's temporaries, and in a Haskell module the Cmm
  pipeline's, as for a safe foreign call;
* 'r', Ids whose uniques come from the name cache, which Tidy draws on for
  top-level Ids: the register that holds a top-level closure in its own code is
  one;
* 't', Ids made from the supplies of CorePrep, CoreToStg and GHC.Stg.EnforceEpt,
  among them CorePrep's clones of local Ids;
* 'u', the Cmm pipeline's temporaries in a .cmm file.

With -fobject-determinism, the order in a Haskell module is deterministic
except for the sanitizer's temporaries. A .cmm file's own registers
keep the 'c' uniques its parser draws, which nothing renames
(Note [Unique Determinism and code generation] in GHC.Types.Unique).

TODO: nothing in the allocator chose this order, but three places depend on it,
the first two taking it as a ranking:

* allocRegsAndSpill_spill chooses among the vregs in a register of the class
  needed, other than those being allocated for the instruction (keep). It evicts
  the first in the order of those in both a register and their slot or, if there
  are none, of those in a register only, whatever its next use (the ToDo of step
  (a) in the outline at the top of this module).
* saveClobberedTemps finds new registers for the values an instruction clobbers
  in the order, so an earlier value can take a free register that a later one
  prefers, and once the free registers that would suit them run out, the later
  values are spilled.
* makeRegMovementGraph lists a jump's moves in the order, which decides the
  order of joinToTargets' fixup moves and which move of a cycle goes through the
  stack.

No other fold over the assignment depends on the order.
-}

saveClobberedTemps
        :: forall instr freeRegs.
           (Instruction instr, FR freeRegs)
        => [RealReg]             -- ^ real registers clobbered by this instruction
        -> Regs                  -- ^ registers which are no longer live after this instruction,
                                 -- because read for the last time
        -> [LiveInstr instr]     -- ^ the instructions after this one in the block
        -> RegM freeRegs [instr] -- return: instructions to spill any temps that will
                                 -- be clobbered.

saveClobberedTemps [] _ _
        = return []

saveClobberedTemps clobbered dying rest
 = do
        assig   <- getAssigR
        -- saveClobberedTemps folds pick over the vregs in the order of their
        -- uniques, so the order decides which clobbered values move to free
        -- registers and which are spilled once those run out
        -- (Note [Unique order in the linear allocator])
        case nonDetStrictFoldUFM_Directly pick [] assig of
          -- nothing clobbered: the state unwritten, as in releaseRegs (written
          -- here alone, the compiler allocated 0.16% more compiling T9961 at
          -- -O0, 0.021% more compiling T12707 at -O and 0.017% more compiling
          -- !16269's CmmDiamondChain at -O2, on GHC 10.1.20261108)
          []      -> return []
          to_save -> do
            -- in the order of their uniques, as the fold visits them
            (assig', instrs)
              <- foldM (\acc (temp, loc) -> clobber temp acc loc)
                       (assig, []) (reverse to_save)
            setAssigR assig'
            return instrs
   where
     -- Unique represents the VirtualReg
     -- Here we separate the cases which we do want to spill from these we don't.
     pick :: Unique -> Loc -> [(Unique, Loc)] -> [(Unique, Loc)]
     pick !temp !loc acc =
        case loc of
                Loc (InReg reg) _
                    | any (realRegsAlias reg) clobbered
                    , not (temp `elemUniqSet_Directly` getRegs dying)
                    -> (temp, loc) : acc
                Loc (InBoth reg _) _
                    | any (realRegsAlias reg) clobbered
                    , not (temp `elemUniqSet_Directly` getRegs dying)
                    -> (temp, loc) : acc
                _ -> acc

     -- See Note [UniqFM and the register allocator]
     clobber :: Unique -> (RegMap Loc,[instr]) -> Loc -> RegM freeRegs (RegMap Loc,[instr])
     clobber temp (assig,instrs) (Loc (InReg reg) fmt)
       = do config <- getConfig
            platform <- getPlatform

            freeRegs <- getFreeRegsR
            let regclass = targetClassOfRealReg platform reg

            -- leaving out a register the block writes before it next reads the
            -- value: the value would move again, or be spilled there; where
            -- every free register is one, the value is spilled at once instead
            -- (#26631)
            case unwrittenBeforeRead temp rest
                   [ r | r <- frGetFreeRegs regclass freeRegs
                       , r `notElem` clobbered ] of

              -- (1) we have a free reg of the right class that isn't
              -- clobbered by this instruction, nor written before the value is
              -- next read; use it to save the
              -- clobbered value.
              free@(_ : _) -> do
                  my_reg <- firstFreeInOrder temp free
                  setFreeRegsR (frAllocateReg my_reg freeRegs)

                  let new_assign = addToUFM_Directly assig temp (Loc (InReg my_reg) fmt)
                  let instr = mkRegRegMoveInstr config fmt
                                  (RegReal reg) (RegReal my_reg)

                  return (new_assign,(instr : instrs))

              -- (2) no such free register: spill the value
              [] -> do

                  (spill, slot)   <- spillR (RegWithFormat (RegReal reg) fmt) temp

                  -- record why this reg was spilled for profiling
                  recordSpill (SpillClobber temp)

                  let new_assign  = addToUFM_Directly assig temp (Loc (InMem slot) fmt)

                  return (new_assign, (spill ++ instrs))
     clobber temp (assig,instrs) (Loc (InBoth _ slot) fmt)
       = return (addToUFM_Directly assig temp (Loc (InMem slot) fmt), instrs)
     clobber _ acc (Loc (InMem _) _) = return acc  -- pick gives none




-- | Of the given real registers, in their order, those that the given
-- instructions, the rest of a block, do not write before they read the
-- virtual register with the given unique; the walk stops once none is left.
unwrittenBeforeRead :: Unique -> [LiveInstr instr] -> [RealReg] -> [RealReg]
unwrittenBeforeRead !u = go
  where
    go _ [] = []
    go (LiveInstr _ (Just l) : is) cands
      | any isU rs           = cands
      | any written cands    = go is (filter (not . written) cands)
      | otherwise            = go is cands
      where RU rs ws = liveUsage l
            written r = any (isReal r) ws
    go (_ : is) cands = go is cands
    go [] cands       = cands
    isReal r w = case regWithFormat_reg w of
      RegReal r' -> r' == r
      _          -> False
    isU r = case regWithFormat_reg r of
      RegVirtual v -> getUnique v == u
      _            -> False

-- | Mark all these real regs as allocated.
--
clobberRegs :: FR freeRegs => [RealReg] -> RegM freeRegs ()
clobberRegs []
        = return ()

clobberRegs clobbered
 = do   platform <- getPlatform
        freeregs <- getFreeRegsR
        setFreeRegsR $! frClobberRegs platform clobbered freeregs

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
   variable needs no fixup move at the jump (#27980). For a jump target not yet
   reached, those of the blocks it jumps to are taken, up to three jumps on.

 * The real register the current block moves the variable into where it dies,
   such as R1 before an evaluation: allocated there, the variable needs no move;
   or, where the block moves it into another variable, such as a loop variable
   at the loop's end, and does not write it again, the first register that the
   assignments of the first bullet give that one.

 * The register the variable has in the first assignment that, recorded
   for a block it is live into, holds it in a register, if any
   (Note [The first jump into a block fixes its entry assignment] in
   GHC.CmmToAsm.Reg.Linear.JoinToTargets). This way, when we allocate for a
   loop, variables are likely to end up in the same registers at the end and
   start of the loop, avoiding redundant reg-reg moves. That buys nothing for
   a value the current block moves where it dies, which is live into no later
   block, hence the order. Nor does it for a value that reaches no block whose
   assignment is recorded, such as a reload into a variable that the code
   generator also uses for another value, live into the loop's head, where the
   value live into the block that recorded it reaches none of the blocks this
   value reaches either: there it comes after the wide destination; for a
   variable an earlier allocation gave the wide destination so (isStaleFirst),
   the free register of a recorded block the walk does reach takes this place.

 * The real register a block the variable is live into moves it into where it
   dies, such as R1 for a value evaluated some blocks after it is computed,
   unless that register is live together with the variable somewhere, as
   wideMoveDests counts it, or, on x86-64, the block follows a failed heap or
   stack check. It is left out also where another variable the current block
   reads or writes, with no location yet, has it as its first-used register.

Preferring every register the variable was assigned to, not only the first, was
tried and turned out to barely matter.

The fallback below leaves out, where others remain, the free registers the
current block moves another virtual register into where it dies: a variable
given one would keep the other from being computed in it, or have to move out of
its way.

On x86-64 the fallback also leaves out, where others remain, the free registers
that the blocks up to three jumps ahead that the variable is live into move
another virtual register into where it dies, the variable's own such
destinations excepted: given one, the variable would keep the other from
being computed in it, or have to move out of its way there (IntMap's union
and fromAscList in containers' benchmarks). Not for a variable live across
a C call, which leaves its register at the call anyway: a block after the
call, such as k-nucleotide's copy of a new key, would put it on an argument
register that the call's own setup needs. Nor where a jump target not yet
recorded that it is live into has other predecessors, those after a failed
heap or stack check not counted: its register there binds them all, and
a walk from one of them reaching a rare exit put megaparsec's manyTill
counter on a register its hot predecessor holds.

Where none of them is free, the allocator takes a free register: the first, and
on x86-64 ('fallbackReg') the first in this order: first those no instruction
writes while the virtual register is live past the write, such as %r14 and %rbx
for a value live across a C call on System V, so that the value need not move
out of their way; and among those, as among the rest, R1's register, %rbx, last,
as an evaluation and a return write it: a value given it may have to move out of
their way, and a loop variable given it at the loop's entry moves at every
iteration (#27981). On x86-64, for a virtual register with preferences, the free
registers that one of the assignments the block's jumps lead to (rc_jumpLocs:
a recorded jump target's, or that of a block up to three jumps past a target
not yet recorded) gives any virtual register are left out where others remain,
its own there being among its preferences. This applies to the fallback only: a
free preferred register is taken even where one of those assignments gives it to
another virtual register, as leaving those out too made more join fixups cyclic
and cost containers 0.13% and vector 0.11% more instructions (nofib's puzzle
2.7%). Leaving them out for one with no preferences too gains nothing measured
and costs compile time, the list of registers to leave out then being built for
every such allocation.

TODO: the first-used preference is keyed by virtual register, and a Cmm local
is one virtual register over its whole procedure, however many live ranges
it has. So the register taken for one live range becomes the preference of
another. Stack layout makes such ranges: it reloads a local that is live across
a call into the same local in the call's continuation. Where a walk from the
block finds that the value live into the block recorded with that register
reaches none of the blocks this value reaches, the vreg's free wide destination,
or the register of a recorded block it reaches, is taken first (stale, reached
in firstFreeInOrder).
-}

-- | Of the given free registers of a class, the one to take for the
-- virtual register with the given unique: the first of its preferred
-- registers, best first, that is free, else the first free one in the order
-- Note [Register preferences in the linear allocator] gives.
firstFreeInOrder :: Unique -> [RealReg] -> RegM freeRegs RealReg
firstFreeInOrder u free = do
  bassig <- getBlockAssigR
  cur <- getAssigR :: RegM freeRegs (RegMap Loc)
  ctx@RA_Context { rc_jumpLocs  = locs
                 , rc_moveDests = MoveDests moves allMoved movedTwice
                 , rc_claims    = claims
                 , rc_wideDests = wide
                 , rc_dests     = dests
                 , rc_succs     = succs
                 , rc_live      = live
                 , rc_dying     = dying } <- getContextR
  let !jl = jumpLocRegs u locs
      mv = lookupUFM_Directly moves u
      !firstUsed = lookupFirstUsed u bassig
      -- the wide destination, where it and the first-used register differ,
      -- both are free and no other vreg of the block claims it (claimed),
      -- and a walk from the current block's jump targets through the blocks
      -- the vreg is live into, stopping at those whose assignment is
      -- recorded, ends without reaching one but reaches a block that moves
      -- the vreg into the wide destination where it dies, while the value
      -- live into a block recorded with the vreg in the first-used register
      -- reaches none of the blocks the walk reached: then the two are other
      -- values, and the wide destination is this value's own (not where
      -- blocksHolding gives up, as for reached below)
      stale = case firstUsed of
             Just r0
               | Just w <- lookupUFM_Directly wide u, w /= r0, isFree w, isFree r0
               , not (claimed w)
               , (True, Nothing, region) <- staleWalk u live succs bassig dests
               , any (movesInto w) (setElems region)
               , Just holders <- blocksHolding u r0 bassig
               , not (any (meets region) holders)
               -> Just w
             _ -> Nothing
      -- for a vreg an earlier allocation gave stale's register
      -- (isStaleFirst), where the walk reaches a recorded block holding
      -- the vreg in another free register and the value live into a block
      -- recorded with the vreg in the first-used register reaches neither
      -- that block nor the blocks the walk passed, such as a reload of
      -- a stack slot in another continuation that reaches a join an earlier
      -- one recorded, that block's register
      reached = case firstUsed of
             Just r0
               | isStaleFirst u bassig
               , (True, Just b, region) <- staleWalk u live succs bassig dests
               , Just assig <- lookupBlockAssignment b bassig
               , Just (Loc l _) <- lookupUFM_Directly assig u
               , r : _ <- regsOfLoc l, r /= r0, isFree r
               , Just holders <- blocksHolding u r0 bassig
               , not (any (meets (setInsert b region)) holders)
               -> Just r
             _ -> Nothing
      -- the first-used register, but none where stale gives a register,
      -- and reached's where it gives one
      !fu | isJust stale   = Nothing
          | isJust reached = reached
          | otherwise      = firstUsed
      movesInto w b = case mapLookup b dying of
        Just (DyingMoves _ ds) -> lastDest u ds == Just w
        Nothing -> False
      meets = staleMeets u live succs bassig
      -- a vreg already given a location
      assigned v = isJust (lookupUFM_Directly cur v)
      -- a wide destination another vreg of the block has as its first-used
      -- register is left out
      claimed r = any (\v -> v /= u && not (assigned v))
                      (lookupWithDefaultUFM claims [] r)
      wdFree = case lookupUFM_Directly wide u of
                 Just r | isFree r, not (claimed r) -> Just r
                 _                                  -> Nothing
      isFree r = r `elem` free
      -- the preference where it is free, else the next one
      pick (Just r) _ | isFree r = Just r
      pick _        k = k
      -- the registers the current block moves other vregs into where they
      -- die: given one, the vreg would keep the other from being computed in
      -- it, or have to move out of its way
      -- (made from per-block masks: a fold over moves at each choice made
      -- the compiler run 2.3% more instructions compiling a block of 400
      -- C calls, on GHC 10.1.20261109)
      others = case mv of
        Just (RealRegSingle n)
          | not (testBit movedTwice n) -> clearBit allMoved n
        _                              -> allMoved
      -- (the list itself where the block moves no other vreg: filtered there
      -- too, it made the compiler run 0.17% more instructions and allocate
      -- 0.37% more compiling T3294 at -O0, 0.091% and 0.17% more at -O and
      -- 0.090% and 0.17% more at -O2, on GHC 10.1.20261108)
      free' | others == 0 = free
            | otherwise   = case filter (\(RealRegSingle n) -> not (testBit others n)) free of
                              [] -> free
                              fs -> fs
      noPrefs = null jl && isNothing mv && isNothing fu
                && maybe True claimed (lookupUFM_Directly wide u)
      chosen = case find isFree jl of
                 Nothing -> pick mv (pick fu wdFree)
                 found   -> found
  case stale of
    Just w | chosen == Just w -> setBlockAssigR (markStaleFirst u bassig)
    _                         -> return ()
  return $! case chosen of
    Just r  -> r
    Nothing -> case rc_clobbers ctx of
      Just clobbers -> fallbackReg ctx clobbers bassig noPrefs u free'
      Nothing       -> case free' of
        r : _ -> r
        []    -> panic "firstFreeInOrder: no free register"

-- | Of the vregs the given instructions read or write, those with a first-used
-- register in the given block assignment, by that register, each once (the
-- order of each register's list is not deterministic, and 'firstFreeInOrder'
-- only tests whether any of them is one it counts). Top-level, so that
-- a block's context holds a thunk of the block assignment and the instructions
-- and nothing else: built in processBlock, a form of it allocated a closure
-- for every block, those that test no claim included.
claimsOf :: BlockAssignment -> [LiveInstr instr] -> UniqFM RealReg [Unique]
claimsOf bassig instrs
  = nonDetStrictFoldUFM_Directly
      (\u rr m -> addToUFM_C (\us _ -> u : us) m rr [u]) emptyUFM byVreg
  where
    byVreg :: UniqFM VirtualReg RealReg
    byVreg = listToUFM_Directly
               [ (u, rr) | li@(LiveInstr (Instr _) _) <- instrs
                         , let RU rs ws = liveInstrUsage li
                         , r <- rs ++ ws
                         , RegVirtual v <- [regWithFormat_reg r]
                         , let u = getUnique v
                         , Just rr <- [lookupFirstUsed u bassig] ]

-- | On x86-64, of the given free registers, the one 'firstFreeInOrder' takes
-- for the virtual register with the given unique where none of its preferences
-- is free, given regClobbers' map ('rc_clobbers'), the block assignment
-- and whether it has none: the first in the order
-- Note [Register preferences in the linear allocator] gives.
fallbackReg :: RA_Context freeRegs -> UniqFM VirtualReg Word64
            -> BlockAssignment -> Bool -> Unique -> [RealReg] -> RealReg
fallbackReg RA_Context { rc_jumpLocs = locs, rc_dests = dests, rc_succs = succs
                       , rc_joins = joins, rc_live = live, rc_dying = dying }
            clobbers bassig noPrefs u free@(r0 : _)
  = let !r1 = r1Reg
        !cl = lookupWithDefaultUFM_Directly clobbers 0 u
        kept (RealRegSingle n) = not (testBit cl n)
        -- the free ones to choose from: for one with preferences, those no
        -- assignment in rc_jumpLocs gives a virtual register if any remain, and
        -- all of them otherwise
        fs | noPrefs || reserved == 0 = free
           | otherwise = case filter unreserved free of
                           []  -> free
                           fs' -> fs'
        -- and of those, the ones the blocks up to three jumps ahead do not
        -- move another vreg into where it dies while the vreg is live into
        -- them, if any remain: given one, the vreg would keep that one from
        -- being computed in it, or have to move out of its way. Not for a vreg
        -- live across a C call, which leaves its register there anyway, nor
        -- where a jump target not yet recorded that the vreg is live into has
        -- other predecessors, which then have to give it the same register
        !ahead | cl .&. cCallClobbers == cCallClobbers = 0
               | otherwise = movedAhead
        !wanted = ahead .&. complement (ahead `shiftR` 32)
        ok (RealRegSingle n) = not (testBit wanted n)
        joinNext = any (\b -> b `setMember` joins && liveInto u live b
                              && isNothing (lookupBlockAssignment b bassig))
                       dests
        -- the real registers that the blocks up to three jumps ahead of the
        -- current one, those the vreg is live into, move vregs into where they
        -- die, as bits by register number, and above them, shifted by 32, those
        -- they move the vreg itself into (three jumps, bounding the walk,
        -- is arbitrary, and may be raised where a case needs more at little
        -- cost to the compiler: lifted to 1000, compiling 50 to 400 loops in
        -- sequence ran 1.01 to 1.11 times the instructions, the excess growing
        -- as N squared)
        movedAhead :: Word64
        movedAhead = fst (aheadWalks u live succs dying 3 0 mapEmpty dests)
        unreserved (RealRegSingle n) = not (testBit reserved n)
        -- (the registers visited in the order of the vregs' uniques: setting
        -- their bits gives the same mask in any order)
        reserved = foldl' (nonDetStrictFoldUFM reserve) (0 :: Word64) locs
        reserve (Loc loc _) b
          = foldl' (\b' (RealRegSingle n) ->
                      assertPpr (n < 64) (ppr n) $ setBit b' n)
                   b (regsOfLoc loc)
        -- those no instruction writes while the vreg is live past it, then the
        -- rest, R1's last in each part
        part p = [ r | r <- fs, p r, r /= r1 ] ++ [ r1 | r1 `elem` fs, p r1 ]
        -- INLINE so that the lists fuse: let-bound and used more than once,
        -- part is otherwise not inlined and builds both lists, which made the
        -- compiler allocate 0.66% more compiling T3294 at -O0, 0.22% more at -O
        -- and 0.21% more at -O2, on GHC 10.1.20261108
        {-# INLINE part #-}
        -- The choice stays the list's own box, not a new one per call. So
        -- r /= r1 comes last in part: tested first, against a literal, it
        -- leaves the element to be rebuilt. And rd is returned under lazy:
        -- the result being a small product, demand analysis would otherwise
        -- unbox rd (Note [Unboxed demand on function bodies returning small
        -- products] in GHC.Types.Demand), and worker/wrapper box it anew.
        -- Together they save 0.070% of T3294's compile allocation at -O0,
        -- 0.023% at -O and 0.022% at -O2, on GHC 10.1.20261108; neither alone
        -- saved any at -O0, on GHC 10.1.20261103.
        rd = fromMaybe r0 (listToMaybe (part kept ++ part (not . kept)))
    in if ok rd then lazy rd
       else case listToMaybe (part (\r -> ok r && kept r)
                              ++ part (\r -> ok r && not (kept r))) of
              Just r | not joinNext -> r
              _                     -> lazy rd
fallbackReg _ _ _ _ _ [] = panic "fallbackReg: no free register"

-- | R1's register on x86-64, which an evaluation and a return write.
r1Reg :: RealReg
r1Reg = fromMaybe (panic "r1Reg: R1 has no register")
                  (X86_64.globalRegMaybe (VanillaReg 1))

-- | The registers of x86-64 that a C call writes on both System V and Windows,
-- rax, rcx, rdx and r8 to r11, as bits by register number: a vreg whose
-- clobbering writes include all of them is live across a C call.
cCallClobbers :: Word64
cCallClobbers = foldl' setBit 0 [0, 2, 3, 8, 9, 10, 11]

-- | fallbackReg's walk: from the given blocks, up to the given number
-- of jumps, through the blocks the vreg with the given unique is live into,
-- the registers their moves into real registers where vregs die go to, as bits
-- by register number, and above them, shifted by 32, those of the vreg's own
-- such moves, or'ed into the given ones. The walk keeps, beside them, for each
-- block it walks on from with two jumps or more left, the most jumps it had
-- left there, set before that walk, and walks on from the block again only
-- with more: a walk with fewer jumps reaches only blocks the walk with more
-- reaches, and or'ing is idempotent and commutative, so the result is the
-- same, and a block is walked on from with two jumps or more left at most once
-- for each number of them, not once for each path to it, whose number grows
-- exponentially with the jumps. Top-level: local to fallbackReg, the walk made
-- the compiler allocate 0.18% more compiling T3294 at -O0, 0.059% more at -O
-- and 0.055% more at -O2, on GHC 10.1.20261108.
aheadWalks :: Unique -> BlockMap Regs -> LabelMap [BlockId]
           -> LabelMap DyingMoves
           -> Int -> Word64 -> LabelMap Int -> [BlockId]
           -> (Word64, LabelMap Int)
aheadWalks !_ _ _ _ !_ !acc !seen [] = (acc, seen)
aheadWalks u live succs dying d acc seen (b : bs)
  | not (liveInto u live b) = aheadWalks u live succs dying d acc seen bs
  | otherwise
  = let !acc' = case mapLookup b dying of
                  Just (DyingMoves m ds)
                    -> assertPpr (m `shiftR` 32 == 0) (ppr m) $
                       acc .|. m .|. lastDestBit u ds
                  Nothing                -> acc
    in if d <= 1 || mapFindWithDefault 0 b seen >= d
       then aheadWalks u live succs dying d acc' seen bs
       else case aheadWalks u live succs dying (d - 1) acc'
                            (if d > 2 then mapInsert b d seen else seen)
                            (mapFindWithDefault [] b succs) of
              (acc'', seen') -> aheadWalks u live succs dying d acc'' seen' bs

-- | The blocks that two blocks or more jump to, the jumps from the given
-- blocks not counted (on x86-64, those that follow a failed heap or stack
-- check).
joinBlocks :: LabelSet -> BlockMap [BlockId] -> LabelSet
joinBlocks cold js = jumpedToTwice (\b -> not (b `setMember` cold)) js

-- | The blocks that two blocks or more of those passing the test jump to. The
-- fold visits the blocks in the order of their labels, which does not change
-- the set.
jumpedToTwice :: (BlockId -> Bool) -> BlockMap [BlockId] -> LabelSet
jumpedToTwice from js = case mapFoldlWithKey step (Seen setEmpty setEmpty) js of
  Seen _ twice -> twice
  where
    step seen@(Seen once0 _) b ds
      | from b    = targets once0 seen ds
      | otherwise = seen
    -- each block's targets counted once, those it jumps to twice included:
    -- tested against the targets of the blocks before it
    targets _ seen [] = seen
    targets once0 (Seen once twice) (d : more)
      | d `setMember` once0 = targets once0 (Seen once (setInsert d twice)) more
      | otherwise           = targets once0 (Seen (setInsert d once) twice) more
{-# INLINE jumpedToTwice #-}

-- | The blocks jumped to by some block, and by two or more.
data Seen = Seen !LabelSet !LabelSet

-- | The block's moves into real registers where vregs die, added to the map
-- where it has any.
dyingIn :: Instruction instr => Platform -> LabelMap DyingMoves
        -> LiveBasicBlock instr -> LabelMap DyingMoves
dyingIn platform acc (BasicBlock b instrs)
  = case [ (vr, rr) | (vr, RegReal rr) <- dyingMoves platform instrs ] of
      []  -> acc
      ms  -> mapInsert b (DyingMoves (foldl' (\m (_, RealRegSingle n) ->
                                                assertPpr (n < 64) (ppr n) $
                                                setBit m n)
                                             0 ms) ms) acc

-- | The register of the last of the moves given that moves the vreg with the
-- given unique, as 'listToUFM' keeps a vreg's last entry.
lastDest :: Unique -> [(VirtualReg, RealReg)] -> Maybe RealReg
lastDest u = foldl' (\acc (v, r) -> if getUnique v == u then Just r else acc) Nothing

-- | As 'lastDest', the register as a bit above 32 by its number, or 0.
lastDestBit :: Unique -> [(VirtualReg, RealReg)] -> Word64
lastDestBit u ds
  = maybe 0 (\(RealRegSingle n) -> assertPpr (n < 32) (ppr n) $ bit (n + 32))
          (lastDest u ds)

-- | What 'liveWalk' does at a block: stop with a result, take the block
-- without its successors, or take it and queue them.
data Step r = Done r | Skip | Expand

-- | The walk of 'staleWalk' and 'staleMeets': breadth first from the given
-- blocks through the blocks the vreg with the given unique is live into, each
-- once, the step given at each, and giving up after 64; the results at the
-- walk's end and at that cap given the blocks visited. The 64, bounding the
-- walk, is arbitrary, and may be raised where a case needs more at little
-- cost to the compiler: lifted to 1000, it changed the compile cost and code
-- of programs of 50 to 400 loops, calls, cases or diamonds by under 0.1%.
liveWalk :: Unique -> BlockMap Regs -> LabelMap [BlockId]
         -> (LabelSet -> r) -> (LabelSet -> r)
         -> (BlockId -> LabelSet -> Step r) -> [BlockId] -> r
{-# INLINE liveWalk #-}
liveWalk !u live succs atEnd atCap visit start = go (0 :: Int) setEmpty start []
  where
    go !_ !seen [] [] = atEnd seen
    go n seen [] later = go n seen (reverse later) []
    go n seen (b : bs) later
      | b `setMember` seen || not (liveInto u live b) = go n seen bs later
      | n >= 64 = atCap seen
      | otherwise = case visit b seen of
          Done r -> r
          Skip   -> go (n + 1) (setInsert b seen) bs later
          Expand -> go (n + 1) (setInsert b seen) bs
                       (reverse (mapFindWithDefault [] b succs) ++ later)

-- | firstFreeInOrder's walk: from the given blocks through the blocks the vreg
-- with the given unique is live into, stopping at those whose assignment is
-- recorded; whether the walk ended, the recorded block it reached if any, and
-- the blocks it visited. Out of line, as firstFreeInOrder rarely takes it:
-- inlined, with staleMeets, it made the compiler allocate 0.028% more compiling
-- !16269's CmmDiamondChain at -O0 and 0.023% more at -O and -O2, on GHC
-- 10.1.20261108.
staleWalk :: Unique -> BlockMap Regs -> LabelMap [BlockId]
          -> BlockAssignment -> [BlockId] -> (Bool, Maybe BlockId, LabelSet)
{-# NOINLINE staleWalk #-}
staleWalk u live succs bassig
  = liveWalk u live succs (\seen -> (True, Nothing, seen))
                          (\seen -> (False, Nothing, seen)) visit
  where
    visit b seen
      | Just _ <- lookupBlockAssignment b bassig = Done (True, Just b, seen)
      | otherwise                                = Expand

-- | firstFreeInOrder's test of whether the value live into the recorded block
-- fb reaches a block of the region, by 'liveWalk' from fb, going no further
-- than the other recorded blocks and giving up as 'staleWalk' does.
staleMeets :: Unique -> BlockMap Regs -> LabelMap [BlockId]
           -> BlockAssignment -> LabelSet -> BlockId -> Bool
{-# NOINLINE staleMeets #-}
staleMeets u live succs bassig region fb
  = liveWalk u live succs (const False) (const True) visit [fb]
  where
    visit b _
      | b `setMember` region                              = Done True
      | b /= fb, Just _ <- lookupBlockAssignment b bassig = Skip
      | otherwise                                         = Expand

-- | 'jumpLocRegs' u locs: the registers the assignments locs give the virtual
-- register with unique u, in order: in rc_jumpLocs, those of the recorded jump
-- targets and, past a target not yet recorded, of the blocks up to three jumps
-- on.
jumpLocRegs :: Unique -> [RegMap Loc] -> [RealReg]
-- INLINE: out of line it made the compiler allocate 0.030% more compiling
-- nofib's puzzle at -O0 and 0.008% and 0.009% more at -O and -O2, and run
-- 0.013% more instructions compiling T3294 at -O0 and 0.008% more at -O2, on
-- GHC 10.1.20261108.
{-# INLINE jumpLocRegs #-}
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
           do   let compat reg'
                      =  targetClassOfRealReg platform reg'
                      == regclass
                    keep_us = map (getUnique . virtualRegWithFormat_reg) keep
                    evictable temp reg = compat reg && temp `notElem` keep_us
                    -- the first vreg we could kick out that is already in
                    -- a slot, and the first that is only in a reg, which
                    -- would require writing the reg to a new slot before
                    -- using it
                    pick temp (Loc l fmt) acc@(both, only) = case l of
                      InBoth reg mem
                        | Nothing <- both, evictable temp reg
                        -> (Just (temp, RealRegUsage reg fmt, mem), only)
                      InReg reg
                        | Nothing <- only, evictable temp reg
                        -> (both, Just (temp, RealRegUsage reg fmt))
                      _ -> acc
                    -- the fold visits the vregs in the order of their uniques,
                    -- and that order picks the vreg evicted below: of those in
                    -- a register and not in keep whose register is of the class
                    -- needed (compat), the first in both a register and a slot,
                    -- else the first in a register only (Note [Unique order in
                    -- the linear allocator])
                    !(first_inBoth, first_inReg)
                      = nonDetStrictFoldUFM_Directly pick (Nothing, Nothing)
                                                     assig

                let result

                        -- we have a temporary that is in both register and mem,
                        -- just free up its register for use.
                        | Just (temp, (RealRegUsage cand_reg old_fmt), slot) <- first_inBoth
                        = do    spills' <- loadTemp r spill_loc cand_reg spills
                                let assig1  = addToUFM_Directly assig temp $ Loc (InMem slot) old_fmt
                                let assig2  = addToUFM assig1 vr $! newLocation spill_loc (RealRegUsage cand_reg vrFmt)

                                setAssigR $ toRegMap assig2
                                allocateRegsAndSpill reading keep spills' (cand_reg:alloc) rs

                        -- otherwise, we need to spill a temporary that currently
                        -- resides in a register.
                        | Just (temp_to_push_out, RealRegUsage cand_reg old_reg_fmt)
                                        <- first_inReg
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
