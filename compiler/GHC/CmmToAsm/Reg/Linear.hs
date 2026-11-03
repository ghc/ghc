
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
import GHC.Utils.Monad (anyM)
import qualified GHC.Data.Word64Map as M
import GHC.Platform
import qualified GHC.Platform.X86_64 as X86_64 (globalRegMaybe)
import GHC.Exts (lazy)

import Data.Containers.ListUtils
import Data.Maybe
import Data.List (find, sortOn)
import Data.Array.IArray (Array, listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import qualified Data.IntMap.Strict as IM
import Data.Ord (Down (..))
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Data.Word (Word64)
import Control.Monad
import GHC.CmmToAsm.CFG (BlockFreqs, blockFreq, blockFreqAt, blockIndex, blockCount)

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
        -> (BlockId -> BlockFreqs)
        -> LiveCmmDecl statics instr
        -> UniqDSM ( NatCmmDecl statics instr
                   , Maybe Int  -- number of extra stack slots required,
                                -- beyond maxSpillSlots
                   , Maybe RegAllocStats
                   )
{-# SPECIALISE regAlloc
      :: NCGConfig -> LabelSet -> (BlockId -> BlockFreqs)
      -> LiveCmmDecl statics X86.Instr
      -> UniqDSM (NatCmmDecl statics X86.Instr, Maybe Int, Maybe RegAllocStats)
  #-}

regAlloc _ _ _ (CmmData sec d)
        = return
                ( CmmData sec d
                , Nothing
                , Nothing )

regAlloc _ _ _ (CmmProc (LiveInfo info _ _ _) lbl live [])
        = return ( CmmProc info lbl live (ListGraph [])
                 , Nothing
                 , Nothing )

regAlloc config cold freqsFrom (CmmProc static lbl live sccs)
        | LiveInfo info entry_ids@(first_id:_) block_live _ <- static
        -- lazy: banged, so that at -O0 it was the static noBlockFreqs and no
        -- thunk a procedure, it made the compiler run 0.015% and 0.010% more
        -- instructions compiling T9961 and T12707 at -O0, and moved no figure
        -- by 0.005% at -O or -O2, on GHC 10.1.20261108
        , let freqs = freqsFrom first_id
        = do
                -- do register allocation on each component.
                !(!final_blocks, !stats, !stack_use)
                        <- linearRegAlloc config cold freqs entry_ids block_live
                                          sccs

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
regAlloc _ _ _ (CmmProc _ _ _ _)
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
        -> BlockFreqs -- ^ each block's estimated frequency
        -> [BlockId] -- ^ entry points
        -> BlockMap Regs
              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)]
              -- ^ instructions annotated with "deaths"
        -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc config cold freqs entry_ids block_live sccs
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
  go f = linearRegAlloc' config f cold freqs entry_ids block_live sccs
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
        -> BlockFreqs                   -- ^ each block's estimated frequency
        -> [BlockId]                    -- ^ entry points
        -> BlockMap Regs              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)] -- ^ instructions annotated with "deaths"
        -> UniqDSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc' config initFreeRegs cold !freqs entry_ids block_live sccs
 = UDSM $ \us -> do
    let !(!stack, !stats, !blocks, us') =
            runR ctx us
                $ linearRA_SCCs cold freqs entry_ids js block_live sccs
        -- each block's jump targets, read by the allocation order and the
        -- context alike
        js = jumpSuccs sccs
        platform = ncgPlatform config
        wv = wideMoveDests platform cold freqs block_live sccs
        ctx = RA_Context
                { rc_jumpLocs  = []
                , rc_moveDests = emptyUFM
                , rc_claims    = []
                , rc_wideDests = fst wv
                , rc_via       = snd wv
                , rc_block     = panic "linearRegAlloc': rc_block read before a block"
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
-- Every write counts, those in the blocks that follow a failed heap or stack
-- check too, which run seldom.  TODO: a write could count by how often its
-- block runs: see Note [Frequencies before register allocation] in
-- GHC.CmmToAsm.CFG.
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
      , hit /= 0 = if M.compareSize (ufmToIntMap (getUniqSet across)) 24 == GT
                     then (d, add m)
                     else (nonDetStrictFoldUniqSet (mark hit) d across, m)
      where
        RU rs ws = liveUsage l
        hit  = foldl' (\b r -> case regWithFormat_reg r of
                                 RegReal (RealRegSingle n) -> setBit b n
                                 _                         -> b) 0 ws
        -- live past the instruction: live after it, and not only written
        -- by it; a move of the vreg itself into a real register is no
        -- clobber of it
        excl = [ v | Just (RegVirtual v, RegReal _) <- [takeRegRegMoveInstr platform i] ]
               ++ [ v | r <- ws, RegVirtual v <- [regWithFormat_reg r], not (has v rs) ]
        across = foldl' (\s v -> delOneFromUniqSet_Directly s (getUnique v))
                         (getRegs (liveAcross l)) excl
        add [] = [(hit, across)]
        add (e@(h, s) : es)
          | h == hit  = let !s' = unionUniqSets s across in (h, s') : es
          | otherwise = e : add es
        has v = any (\r -> case regWithFormat_reg r of
                              RegVirtual v' -> v' == v
                              _             -> False)
    ins dm _ = dm
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
-- given ones. A vreg moved so into a vreg with a register here has that
-- register too, up to four such moves back, so that its value is computed
-- there, unless the check leaves it out; the second map gives each vreg moved
-- so, left out or not, the vreg it is moved into. The walk of each block
-- follows only those vregs and registers, taking an instruction's writes from
-- its register usage ('liveBorn' being filled in only for the graph allocator
-- and the liveness dump).
wideMoveDests :: Instruction instr
              => Platform -> LabelSet -> BlockFreqs -> BlockMap Regs
              -> [SCC (LiveBasicBlock instr)]
              -> (UniqFM VirtualReg RealReg, UniqFM VirtualReg VirtualReg)
wideMoveDests platform cold freqs block_live sccs
  -- (no candidates, no walk: the walk over every block, which then marks
  -- nothing, made the compiler allocate 0.015% more compiling T9961 at -O0, and
  -- moved no figure by 0.005% at -O or -O2, on GHC 10.1.20261108)
  | isNullUFM cands = (cands, via)
  | otherwise       = (cands `minusUFM` foldl' blk emptyUFM blocks, via)
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
    -- vreg's only preference, which fallbackReg counts as one.
    -- Four moves back, bounding back's passes, is arbitrary, and may be raised
    -- where a case needs more at little cost to the compiler: lifted to 1000,
    -- compiling a loop that rotates 50 to 400 variables ran 1.02 to 1.19 times
    -- the instructions, the excess growing as N squared.
    (cands, via) = back (4 :: Int) (direct, emptyUFM)
    direct = mapUFM fst directAt
    -- One walk finds them, with the block of the move, and the moves of a vreg
    -- into another where it dies in the same blocks, which back and follow
    -- take: a walk for each made the compiler run 0.46% more instructions
    -- compiling !16269's CmmDiamondChain at -O0, 0.39% more at -O and 0.38%
    -- more at -O2, on GHC 10.1.20261108.
    (directAt, vmovesR) = foldl' blkMoves (emptyUFM, []) blocks
    blkMoves acc (BasicBlock bid instrs)
      | warm bid live = foldl' mv acc (dyingMoves platform instrs)
      | otherwise     = acc
      where
        live = live_in bid
        mv (!cs, !vm) (vr, RegReal rr)
          | RegVirtual vr `elemRegs` live = (addToUFM cs vr (rr, bid), vm)
        mv (!cs, !vm) (a, RegVirtual b)   = (cs, (a, b) : vm)
        mv csvm _                         = csvm
    -- a vreg moved where it dies into a candidate, the move coalescing the two,
    -- has the candidate's destination too, so that the value is computed there
    -- from the start, and the candidate as the vreg it is moved into: up to
    -- four such moves back
    back :: Int -> (UniqFM VirtualReg RealReg, UniqFM VirtualReg VirtualReg)
         -> (UniqFM VirtualReg RealReg, UniqFM VirtualReg VirtualReg)
    back 0 cv = cv
    back n cv@(c, w)
      | isNullUFM c = cv
      | null new    = cv
      | otherwise   = back (n - 1)
                        ( plusUFM c (listToUFM [ (a, d) | (a, _, d) <- new ])
                        , plusUFM w (listToUFM [ (a, b) | (a, b, _) <- new ]) )
      where new = [ (a, b, d) | (a, b) <- vmoves, not (a `elemUFM` c)
                              , Just d <- [lookupUFM c b] ]
    -- the moves of vregs into vregs where they die, in the blocks warm takes,
    -- in the walk's order, of which back keeps a vreg's last
    vmoves = reverse vmovesR
    dests = mkUniqSet (nonDetEltsUFM cands)
    -- each followed vreg to the candidates whose values it holds, with their
    -- destinations: a candidate itself, and each candidate moved into the
    -- vreg where it dies
    follow = plusUFM_C (++) (mapUFM_Directly (\u d -> [(u, d)]) cands)
           $ listToUFM_C (++)
      [ (b, [(getUnique a, d)])
      | (a, b) <- vmoves, Just d <- [lookupUFM cands a] ]
    -- bad: the candidates found live together with their destination;
    -- vs: the followed vregs live; ds: the destinations live
    blk bad (BasicBlock bid instrs)
      | not (touched bid instrs) = bad
      | otherwise
      = let (bad', vs, ds) = born (bad, emptyUniqSet, emptyUniqSet)
                                  (live_in bid)
            -- the vregs the block moves into a destination where they die
            loc = listToUFM [ (vr, rr) | (vr, RegReal rr) <- dyingMoves platform instrs
                                       , rr `elementOfUniqSet` dests ]
        in go loc bad' vs ds instrs
    -- a block finds a candidate bad only where a destination is live into
    -- it or written in it, a move into one where a vreg dies included, or,
    -- where candidates are propagated, where it writes a vreg with a
    -- destination directly (wantedDirect): elsewhere no destination is live
    -- during its walk and no such vreg is born, so that it marks nothing;
    -- walking every block made the compiler run 3.8% more instructions
    -- compiling !16269's CmmDiamondChain at -O0 and 3.1% more at -O1 and -O2
    -- when this was added
    touched bid instrs
      =  any (\rr -> RegReal rr `elemRegs` live_in bid) destList
      || any writesDest instrs
      || not (isNullUFM via) && any writesDirect instrs
    destList = nonDetEltsUniqSet dests
    writesDest (LiveInstr (Instr _) (Just l)) = any isDest (writes (liveUsage l))
    writesDest _ = False
    isDest r = case regWithFormat_reg r of
      RegReal rr -> rr `elementOfUniqSet` dests
      _          -> False
    writesDirect (LiveInstr (Instr _) (Just l)) = any isDirect (writes (liveUsage l))
    writesDirect _ = False
    isDirect r = case regWithFormat_reg r of
      RegVirtual v -> v `elemUFM` direct
      _            -> False
    go _ !bad !_ !_ [] = bad
    go loc !bad !vs !ds (LiveInstr (Instr _) (Just l) : is)
      = let !(vs1, ds1)       = dies (vs, ds) (liveDieRead l)
            -- (no followed vreg live, no candidate wanted: the folds over the
            -- writes anyway made the compiler run 0.051% more instructions
            -- compiling nofib's puzzle at -O0 and 0.015% and 0.014% more at -O
            -- and -O2, on GHC 10.1.20261108)
            bad1 | isEmptyUniqSet vs1 = bad
                 | otherwise          = foldl' (wantedDirect vs1)
                                          (foldl' (wanted loc vs1) bad ws) ws
            !(bad', vs2, ds2) = foldl' (flip bear) (bad1, vs1, ds1) ws
            !(vs3, ds3)       = dies (vs2, ds2) (liveDieWrite l)
        in go loc bad' vs3 ds3 is
      where ws = writes (liveUsage l)
    go loc !bad !vs !ds (_ : is) = go loc bad vs ds is
    -- a vreg born with a destination in loc: the live followed vregs'
    -- candidates with that destination are bad, but the vreg's own, which an
    -- update in place keeps in its register
    wanted loc vs bad r = case regWithFormat_reg r of
      RegVirtual v
        | Just w <- lookupUFM loc v -> clash w bad (delOneFromUniqSet vs v)
      _ -> bad
    -- a propagated candidate yields its destination to a vreg with that
    -- destination directly, born while it is live, where the direct vreg's move
    -- into the register is estimated to run more often than the move into it at
    -- the end of the candidate's chain, by the block frequencies, which inherit
    -- static prediction's errors on tag tests (Note [Static prediction of tag
    -- tests] in GHC.CmmToAsm.CFG)
    wantedDirect vs bad r = case regWithFormat_reg r of
      RegVirtual v
        | Just w <- lookupUFM direct v
        -> nonDetStrictFoldUniqSet
             (\x b -> if x /= v && x `elemUFM` via && lookupUFM cands x == Just w
                         && hotter v x
                        then addToUFM b x () else b)
             bad vs
      _ -> bad
    hotter d p = moveFreq d > moveFreq (chainEnd p)
    moveFreq u = maybe 0 (blockFreq freqs . snd) (lookupUFM directAt u)
    chainEnd p = maybe p chainEnd (lookupUFM via p)
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
-- Where the blocks' frequencies are estimated (on x86-64, under
-- -fcmm-static-pred), the SCCs go in 'hotFirst''s order instead: the
-- procedure's first block first, then hotter first among those that can be
-- allocated, another entry as hot as the hottest of itself and the blocks it
-- jumps to, estimates counted in factors of 1.2 (the floor of their logarithm
-- to that base); and an entry other than the procedure's first block set aside
-- while a block it jumps to, not yet reached, runs more than 1.2 times as
-- often as the entry, other paths bringing it at least a sixth of its runs:
-- allocated first, the entry would choose the registers of a block those paths
-- run, as a block the garbage collector returns to or a continuation entering
-- a loop would (GHC #27800). The estimate of a tag test is static prediction's
-- (Note [Static prediction of tag tests]), which can leave an unannotated test
-- of a pointer's tag at 50/50; the factor damps what that makes of the paths
-- around an evaluation.
--
-- TODO: without the estimate, checkFailures counts as cold some blocks the hot
-- path runs: see Note [Frequencies before register allocation] in
-- GHC.CmmToAsm.CFG.
linearRA_SCCs :: RegAllocConstraint freeRegs instr
              => LabelSet
              -> BlockFreqs
              -> [BlockId]
              -> BlockMap [BlockId]
              -> BlockMap Regs
              -> [SCC (LiveBasicBlock instr)]
              -> RegM freeRegs [NatBasicBlock instr]

linearRA_SCCs cold freqs entry_ids succs block_live sccs
 | blockCount freqs > 0
 = reverse <$> foldM one [] (hotFirst freqs entries first_id succs sccs)
 | otherwise
 = do   let -- a block the garbage collector returns to, or a return
            -- continuation that jumps into a loop with no call (GHC #27800)
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
              if deferrable block_assig scc
                then go acc (scc : deferred) rest
                -- reached unless only deferred blocks lead to it
                else if null deferred
                        || any (isReached entries block_assig . blockId)
                               (flattenSCC scc)
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
    one acc (CyclicSCC blocks) = do
        blockss' <- process entries succs block_live blocks
        return (reverse (concat blockss') ++ acc)

-- | The SCCs in the order linearRA_SCCs allocates them where it has the blocks'
-- estimated frequencies: hotter first among those it can allocate, an entry,
-- or an SCC a jump from an allocated one reaches, an SCC's heat being its
-- hottest block's estimate in factors of 1.2 (the floor of its logarithm to
-- that base), and ties going to the earlier in the given order (dependency
-- order, Note [The first jump into a block fixes its registers]).  Static
-- prediction can leave an unannotated test of a pointer's tag 50/50 (Note
-- [Static prediction of tag tests]), so the paths around an evaluation can
-- come out about as hot, and the given order decides between them, as it did
-- without the estimate: Data.IntSet's fromAscList loop otherwise took its
-- head's registers from the procedure's entry path, the arguments' (0.75),
-- before the join of its evaluations' continuations (0.716), and paid two
-- moves an element on its back edge. The procedure's first block, where no
-- loop holds it, counts as hotter than any other: it jumps with the arguments
-- where the calling convention puts them, which a loop calling a function again
-- takes again, where a continuation's values are where its evaluation left
-- them (Data.Tree's and Data.Map's folds paid three instructions a node when
-- the continuation went first). An entry other than the first block, where
-- no loop holds it, counts as hot as the hottest of itself and the blocks
-- it jumps to: a continuation reloads its values, and the block it jumps to
-- takes its registers, which may be a path's that the estimate calls hotter
-- but where those values are spilled, as where a loop's every iteration calls
-- (nofib's fannkuch-redux at -O1 stored eight values on its edge an iteration
-- when the path went first); ranked above every SCC but the first block, the
-- entries cost Data.IntSet's mapMonotonic at -O2 two instructions an element,
-- its loop's continuation going after colder ones. An entry other than the
-- first block is set aside while a block it jumps to, not yet reached, runs
-- more than 1.2 times as often as it, and tested again whenever one of its
-- targets is reached. Where nothing can be allocated, the entry set aside
-- that the estimate calls hottest is, ties to the earlier, or where none is,
-- the earliest SCC left: two continuations joining wait for each other, and
-- the hotter's registers are the join's (Data.IntMap's mapKeysMonotonic at
-- -O1 paid an instruction an element when the colder went first). A cyclic SCC
-- holding a block whose first jump target in it and another differ in estimate
-- by a factor of 1.2, and entered at one block, reached from outside it or an
-- entry, goes hottest block first ('hotOrder'), so that the hotter side of
-- the branch fixes its join's registers and the colder side pays the moves:
-- megaparsec's skipManyTill ran 5% fewer instructions, and nofib's wheel-sieve1
-- 1% fewer. Entered at more blocks, its joins also meet registers fixed apart
-- from the order, an outside predecessor's at a block reached from outside and
-- none at an entry, into which no vreg is live, and the order would rank such
-- blocks by an entry's heat, the estimate's guess for the call or evaluation
-- returning to it: so ordered, nofib's puzzle at -O2, whose loops are entered
-- at twenty blocks and at four, ran no faster, and the compiler ran 0.15% more
-- instructions compiling it, when this was added, on GHC 10.1.20261108. The
-- order depends on the jumps alone, not on what allocation records, so it is
-- computed before allocation. NOINLINE: inlined, its local thunks are allocated
-- for every procedure, at -O0 too, where it is not called, and the compiler
-- allocated 0.064% more compiling T9961 at -O0 and 0.042%, 0.012% and 0.011%
-- more compiling T12707 at -O0, -O and -O2, though it ran 0.13% fewer
-- instructions for T9961 at -O0 and 0.076%, 0.019% and 0.018% fewer for T12707,
-- the cause of that unidentified, on GHC 10.1.20261108.
{-# NOINLINE hotFirst #-}
hotFirst :: forall b. BlockFreqs -> LabelSet -> BlockId -> BlockMap [BlockId]
         -> [SCC (GenBasicBlock b)] -> [SCC (GenBasicBlock b)]
hotFirst freqs entries first_id succs sccs
  = runST $ do
      -- (unchecked reads and writes, unsafeRead, unsafeWrite and unsafeAt here
      -- and unsafeRead in hotOrder: readArray, writeArray and (!) in their
      -- place, while hotOrder kept its heap in an STUArray, made the compiler
      -- run 0.080% more instructions and allocate 0.036% more compiling T12707
      -- at -O, and run 0.091% more compiling nofib's puzzle at -O2, on GHC
      -- 10.1.20261108)
      -- each block's SCC, four times its index, plus 1 once a jump reaches it,
      -- 3 where the first jump is from the block's own SCC
      blockScc <- newArray (0, max 0 (nb - 1)) (-4) :: ST s (STUArray s Int Int)
      known <- foldM (\ok (i, scc) ->
                        foldM (\ok' blk -> case blockIndex freqs (blockId blk) of
                                  j | j < 0     -> return False
                                    | otherwise -> unsafeWrite blockScc j (4 * i)
                                                   >> return ok')
                              ok (flattenSCC scc))
                     True (zip [0 ..] sccs)
      if not known then return sccs  -- not expected: the CFG holds every block
      else do
        -- 0 not yet reached, 1 in the heap, 2 allocated, 3 set aside; the
        -- heap's size at k
        status <- newArray (0, k) (0 :: Int) :: ST s (STUArray s Int Int)
        -- a binary heap: a Data.Set on (Down heat, i), the same order, made the
        -- compiler run 0.066% more instructions and allocate 0.11% more
        -- compiling T12707 at -O, and 0.079% and 0.11% more compiling nofib's
        -- puzzle at -O2, and nothing at -O0, where hotFirst is not called, on
        -- GHC 10.1.20261108
        heap <- newArray (0, k - 1) 0 :: ST s (STUArray s Int Int)
        -- the set-aside entries to test again when a block is reached
        waitOn <- newSTRef (IM.empty :: IM.IntMap [Int])
        asideRef <- newSTRef ([] :: [Int])
        let better i j = let hi = unsafeAt heat i; hj = unsafeAt heat j
                         in hi > hj || (hi == hj && i < j)
            swap a c = do x <- unsafeRead heap a
                          y <- unsafeRead heap c
                          unsafeWrite heap a y
                          unsafeWrite heap c x
            up 0 = return ()
            up c = do let p = (c - 1) `quot` 2
                      x <- unsafeRead heap c
                      y <- unsafeRead heap p
                      when (better x y) $ swap c p >> up p
            down n p = do
              let l = 2 * p + 1
                  r = l + 1
              if l >= n then return () else do
                x <- unsafeRead heap l
                c <- if r < n then do y <- unsafeRead heap r
                                      return (if better y x then r else l)
                              else return l
                cv <- unsafeRead heap c
                pv <- unsafeRead heap p
                when (better cv pv) $ swap c p >> down n c
            push i = do
              unsafeWrite status i 1
              n <- unsafeRead status k
              unsafeWrite heap n i
              unsafeWrite status k (n + 1)
              up n
            pop = do
              n <- unsafeRead status k
              top <- unsafeRead heap 0
              unsafeRead heap (n - 1) >>= unsafeWrite heap 0
              unsafeWrite status k (n - 1)
              down (n - 1) 0
              return top
            isReached' d = case blockIndex freqs d of
              j | j < 0     -> return False
                | otherwise -> (\v -> v .&. 1 /= 0) <$> unsafeRead blockScc j
            -- whether an SCC is an entry set aside now
            waitsNow i = case sccArr ! i of
              AcyclicSCC (BasicBlock bid _)
                | bid `setMember` entries, bid /= first_id
                -> let f = 1.2 * blockFreq freqs bid
                   in anyM (\d -> if blockFreq freqs d > f
                                    then not <$> isReached' d
                                    else return False)
                           (mapFindWithDefault [] bid succs)
              _ -> return False
            setAside i = do
              unsafeWrite status i 3
              readSTRef asideRef >>= writeSTRef asideRef . (i :)
              forM_ (mapFindWithDefault [] (sccBlockId i) succs) $ \d -> do
                let j = blockIndex freqs d
                when (j >= 0) $ do
                  v <- unsafeRead blockScc j
                  when (v .&. 1 == 0) $ readSTRef waitOn >>= \wm ->
                    writeSTRef waitOn $! IM.insertWith (++) j [i] wm
            -- allocate SCC i, giving it as it goes in the order: a cyclic one
            -- holding a block whose first jump target in it and another differ
            -- in estimate by a factor of 1.2 as 'hotOrder' orders it
            allocate i = do
              unsafeWrite status i 2
              case sccArr ! i of
                scc@(AcyclicSCC blk) -> do
                  forM_ (mapFindWithDefault [] (blockId blk) succs) $ \d -> do
                    let j = blockIndex freqs d
                    when (j >= 0) $ unsafeRead blockScc j >>= reach i j
                  return scc
                scc@(CyclicSCC blks) -> do
                  imb <- foldM (\a blk ->
                                  case mapFindWithDefault [] (blockId blk) succs of
                                    [d] -> one i d >> return a
                                    [d1, d2] | not a -> two i d1 d2
                                    ds -> walk i (-1) a ds)
                               False blks
                  if imb then hotOrder freqs entries succs blockScc blks
                         else return scc
            -- the one jump target of a block of SCC i reached
            {-# INLINE one #-}
            one i d = let j = blockIndex freqs d
                      in when (j >= 0) $ unsafeRead blockScc j >>= reach i j
            -- the two jump targets of a block of SCC i reached, and whether
            -- both are in SCC i and differ in estimate by a factor of 1.2;
            -- reach leaves the SCC each element of blockScc holds as it was
            {-# INLINE two #-}
            two i d1 d2 = do
              let j1 = blockIndex freqs d1
                  j2 = blockIndex freqs d2
              when (j1 >= 0) $ unsafeRead blockScc j1 >>= reach i j1
              when (j2 >= 0) $ unsafeRead blockScc j2 >>= reach i j2
              if j1 < 0 || j2 < 0 then return False else do
                v1 <- unsafeRead blockScc j1
                v2 <- unsafeRead blockScc j2
                let f1 = blockFreqAt freqs j1
                    f2 = blockFreqAt freqs j2
                return $! v1 `shiftR` 2 == i && v2 `shiftR` 2 == i
                          && (f1 > 1.2 * f2 || f2 > 1.2 * f1)
            -- the jump targets of a block of SCC i reached, and whether the
            -- first in SCC i and another differ in estimate by a factor of 1.2,
            -- r the first one's index (-1 none yet)
            walk !_ !_ !a [] = return a
            walk i r a (d : ds) = case blockIndex freqs d of
              j | j < 0     -> walk i r a ds
                | otherwise -> do
                    v <- unsafeRead blockScc j
                    reach i j v
                    if v `shiftR` 2 /= i then walk i r a ds
                    else if r < 0 then walk i j a ds
                    else let f = blockFreqAt freqs j
                             g = blockFreqAt freqs r
                         in walk i r (a || f > 1.2 * g || g > 1.2 * f) ds
            -- the block at index j, whose entry is v, reached by a jump from
            -- SCC i
            {-# INLINE reach #-}
            reach i j v = when (v .&. 1 == 0) $ do
              let s = v `shiftR` 2
              unsafeWrite blockScc j (if s == i then v + 3 else v + 1)
              when (s >= 0) $ do
                st <- unsafeRead status s
                when (st == 0) $ push s
              wm <- readSTRef waitOn
              case IM.lookup j wm of
                Nothing -> return ()
                Just ws -> do
                  writeSTRef waitOn $! IM.delete j wm
                  forM_ ws $ \w -> do
                    st <- unsafeRead status w
                    when (st == 3) $ do
                      stillWaits <- waitsNow w
                      unless stillWaits $ push w
            -- the earliest SCC not yet reached, from a cursor that only
            -- advances, as an SCC's status never returns to 0
            unreached c | c >= k    = return Nothing
                        | otherwise = do st <- unsafeRead status c
                                         if st == 0 then return (Just c)
                                                    else unreached (c + 1)
            go out cur = do
              n <- unsafeRead status k
              if n > 0 then do
                i <- pop
                w <- waitsNow i
                if w then setAside i >> go out cur
                     else allocate i >>= \scc -> go (scc : out) cur
              else do
                u <- unreached cur
                aside <- filterM (\i -> (== 3) <$> unsafeRead status i) =<< readSTRef asideRef
                writeSTRef asideRef aside
                let hotter j i = blockFreq freqs (sccBlockId j)
                                 > blockFreq freqs (sccBlockId i)
                    -- the hotter, ties to the earlier
                    pref i j | hotter j i || (not (hotter i j) && j < i) = j
                             | otherwise                                = i
                    pick = case aside of
                      a : as -> Just (foldl' pref a as)
                      []     -> u
                case pick of
                  Just i  -> do scc <- allocate i
                                go (scc : out) (maybe cur id u)
                  Nothing -> return (reverse out)
        forM_ (setElems entries) $ \e -> do
          let j = blockIndex freqs e
          when (j >= 0) $ do
            v <- unsafeRead blockScc j
            let s = v `shiftR` 2
            when (s >= 0) $ do st <- unsafeRead status s
                               when (st == 0) $ push s
        go [] 0
  where
    k = length sccs
    nb = blockCount freqs
    sccArr = listArray (0, k - 1) sccs :: Array Int (SCC (GenBasicBlock b))
    sccBlockId i = case flattenSCC (sccArr ! i) of
      blk : _ -> blockId blk
      []      -> panic "hotFirst: an empty SCC"
    -- the first block hotter than any other, another entry by the hottest of
    -- itself and its jump targets, the others by their hottest block's
    -- estimate, in factors of 1.2
    heat :: UArray Int Int
    heat = listArray (0, k - 1)
      [ case scc of
          AcyclicSCC blk
            | blockId blk == first_id           -> maxBound
            | blockId blk `setMember` entries
            -> bucket $ foldl' (\a d -> max a (blockFreq freqs d))
                               (blockFreq freqs (blockId blk))
                               (mapFindWithDefault [] (blockId blk) succs)
          _ -> bucket $ foldl' (\a blk -> max a (blockFreq freqs (blockId blk)))
                               0 (flattenSCC scc)
      | scc <- sccs ]

-- | An estimated frequency in factors of 1.2, the floor of its logarithm to
-- that base, so that blocks of about the same heat count as equal.
bucket :: Double -> Int
bucket h | h > 0     = floor (log h / log 1.2)
         | otherwise = minBound

-- | The blocks of a cyclic SCC of hotFirst's, every one of which a jump has
-- reached (blockScc's entries), in the order of allocation: hottest reached
-- block first, by 'bucket', ties to the earlier in the given order, each
-- block's jump targets in the SCC reached as it goes, where exactly one block
-- was reached from outside the SCC or is an entry; given as they are otherwise.
hotOrder :: forall s b. BlockFreqs -> LabelSet -> BlockMap [BlockId]
         -> STUArray s Int Int -> [GenBasicBlock b]
         -> ST s (SCC (GenBasicBlock b))
hotOrder freqs entries succs blockScc blks = do
    -- the blocks reached from outside the SCC and the entries, counted to 2
    let starts !k [] = return k
        starts k (blk : rest) = do
          v <- unsafeRead blockScc (blockIndex freqs (blockId blk))
          if v .&. 3 == 1 || blockId blk `setMember` entries
            then if k > 0 then return 2 else starts (1 :: Int) rest
            else starts k rest
    ns <- starts 0 blks
    if ns /= 1 then return (CyclicSCC blks) else do
      -- the blocks reached from outside the SCC, and the entries, first
      starts0 <- filterM (\x -> do
                   let blk = barr ! x
                   v <- unsafeRead blockScc (blockIndex freqs (blockId blk))
                   return (v .&. 3 == 1 || blockId blk `setMember` entries))
                 [0 .. n - 1]
      let -- each block's position in the SCC, by its index in the CFG
          posOf = IM.fromList [ (blockIndex freqs (blockId blk), x)
                              | (x, blk) <- zip [0 ..] blks ]
          -- a Data.Set on (Down heat, x), over one SCC's blocks: a binary heap
          -- in an STUArray, as hotFirst's, made the compiler run at most 0.001%
          -- fewer instructions and allocate at most 0.005% less (T12707 at -O),
          -- on GHC 10.1.20261108
          key x = (Down (bucket (blockFreq freqs (blockId (barr ! x)))), x)
          push a@(!h, !sn) x
            | x `IS.member` sn = a
            | otherwise        = (Set.insert (key x) h, IS.insert x sn)
          loop (h, sn) acc = case Set.minView h of
            Nothing -> reverse acc
            Just ((_, x), h') ->
              let ts = [ y | d <- mapFindWithDefault [] (blockId (barr ! x))
                                                     succs
                           , Just y <- [IM.lookup (blockIndex freqs d) posOf] ]
              in loop (foldl' push (h', sn) ts) (x : acc)
          order = loop (foldl' push (Set.empty, IS.empty) starts0) []
      return $ if null order then CyclicSCC blks
                             else CyclicSCC (map (barr !) order)
  where
    n = length blks
    barr = listArray (0, n - 1) blks :: Array Int (GenBasicBlock b)

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
        -> BlockMap [BlockId]
        -> BlockMap Regs
        -> [GenBasicBlock (LiveInstr instr)]
        -> RegM freeRegs [[NatBasicBlock instr]]
process entry_ids succs block_live =
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
            then do b' <- processBlock succs block_live b
                    go blocks next_round (b' : accum) True

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
        let -- the jump targets' recorded assignments, and for one not yet
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
            !locs = case recorded 3 mapEmpty []
                                  (mapFindWithDefault [] id succs) of
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
            -- the first-used registers of the vregs the block reads or writes
            -- (the usage liveness keeps, 'liveInstrUsage':
            -- computed again, it made the compiler run 0.28% more instructions
            -- and allocate 0.37% more compiling !16269's CmmDiamondChain at
            -- -O0 when this was added)
            claims = [ (rr, u) | li@(LiveInstr (Instr _) _) <- instrs
                               , let RU rs ws = liveInstrUsage li
                               , r <- rs ++ ws
                               , RegVirtual v <- [regWithFormat_reg r]
                               , let u = getUnique v
                               , Just rr <- [lookupFirstUsed u bassig] ]
        setBlockContextR id locs claims $ listToUFM $
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
    -- Nor do we eliminate it where a jump target of the block whose assignment
    -- is recorded has dst in a free register other than src's and another
    -- vreg in src's: the move puts dst there, which the jump would otherwise
    -- have to swap with that vreg. Where no target has a vreg in src's
    -- register, the jump moves dst on its own edge, where the move here would
    -- run on every path out of the block.
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
    let dying = map regWithFormat_reg . nonDetEltsUniqSet . getRegs
        -- See Note [Unique Determinism and code generation]
        r_dying = dying (liveDieRead live)
        w_dying = dying (liveDieWrite live)
    let real_written = [ rr                          | RegWithFormat {regWithFormat_reg = RegReal rr} <- written ]
    let virt_written = [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt              <- written ]

    -- we don't need to do anything with real registers that are
    -- only read by this instr.
    let virt_read :: [VirtualRegWithFormat]
        virt_read = case [ VirtualRegWithFormat vr fmt | RegWithFormat (RegVirtual vr) fmt <- read ] of
          -- the list itself where it holds at most two registers, distinct:
          -- nubOrdOn always, master's, made the compiler run 0.46% more
          -- instructions and allocate 0.89% more compiling T3294 at -O0, 0.17%
          -- and 0.29% more at -O and 0.16% and 0.28% more at -O2, on GHC
          -- 10.1.20261108
          vs@[]  -> vs
          vs@[_] -> vs
          vs@[a, b] | virtualRegWithFormat_reg a /= virtualRegWithFormat_reg b -> vs
          vs -> nubOrdOn virtualRegWithFormat_reg vs

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
    clobber_saves <- saveClobberedTemps real_written r_dying rest

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
        -- Strict: squashed_instr and reload_into both evaluate it, and a lazy
        -- one is a thunk shared between them, built for every instruction,
        -- which made the compiler allocate 0.53% more compiling T3294 at -O0
        -- and 0.18% more at -O and -O2, on GHC 10.1.20261108.
        patched_instr :: instr
        !patched_instr
                = patchRegsOfInstr platform adjusted_instr patchLookup

        -- the real register of the last pair whose vreg is x, the written
        -- pairs after the read ones, as listToUFM keeps a key's last entry;
        -- x where none is
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
    -- saves above, or one the move's destination vreg took without evicting
    -- another): reloaded straight into the move's destination, as where that
    -- register is free at the reload, and the move goes.  Nothing else
    -- changes, the register the reload took being freed with the vreg.
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
                        maybe squashed_instr (const []) reload_into
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):w_spills>")
                      , reverse w_spills
                        -- ,mkComment (text "<genRaInsn(" <> ppr u <> text "):r_spills>")
                      , maybe (reverse r_spills) reverse reload_into
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
-- nothing dies: the state stays as it is, unwritten, each write building a new
-- RA_State: written here and in saveClobberedTemps where nothing is clobbered,
-- the compiler allocated 2.3% more compiling T3294 at -O0 and 0.79% and 0.76%
-- more at -O and -O2, and ran 0.99%, 0.38% and 0.37% more instructions, and
-- written here alone, it allocated 2.3%, 0.78% and 0.76% more, on GHC
-- 10.1.20261108
releaseRegs [] = return ()
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
--      - otherwise, put it somewhere safe (another reg, not written
--              before the temp's next read, if possible, otherwise spill
--              and record InBoth in the assignment).
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
        -> [LiveInstr instr]     -- ^ the instructions after this one in the block
        -> RegM freeRegs [instr] -- return: instructions to spill any temps that will
                                 -- be clobbered.

saveClobberedTemps [] _ _
        = return []

saveClobberedTemps clobbered dying rest
 = do
        assig   <- getAssigR
        -- This is non-deterministic but we do not
        -- currently support deterministic code-generation.
        -- See Note [Unique Determinism and code generation]
        case nonDetStrictFoldUFM_Directly pick [] assig of
          -- nothing clobbered: the state unwritten, as in releaseRegs (written
          -- here alone, the compiler allocated 0.16% more compiling T9961 at
          -- -O0, 0.021% more compiling T12707 at -O and 0.017% more compiling
          -- !16269's CmmDiamondChain at -O2, on GHC 10.1.20261108)
          []      -> return []
          to_save -> do
            -- in the order of their uniques, as the fold visits them
            (assig', instrs)
              <- foldM (\acc (temp, usage) -> clobber temp acc usage)
                       (assig, []) (reverse to_save)
            setAssigR assig'
            return instrs
   where
     -- Unique represents the VirtualReg
     -- Here we separate the cases which we do want to spill from these we don't.
     dying_us = map getUnique dying
     pick :: Unique -> Loc -> [(Unique, RealRegUsage)]
          -> [(Unique, RealRegUsage)]
     pick !temp !loc acc =
        case loc of
                Loc (InReg reg) fmt
                    | any (realRegsAlias reg) clobbered
                    , temp `notElem` dying_us
                    -> (temp, RealRegUsage reg fmt) : acc
                _ -> acc

     -- See Note [UniqFM and the register allocator]
     clobber :: Unique -> (RegMap Loc,[instr]) -> RealRegUsage -> RegM freeRegs (RegMap Loc,[instr])
     clobber temp (assig,instrs) (RealRegUsage reg fmt)
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

                  let new_assign  = addToUFM_Directly assig temp (Loc (InBoth reg slot) fmt)

                  return (new_assign, (spill ++ instrs))




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
        setAssigR $! nonDetStrictFoldUFM_Directly clobber assig assig
          -- This is non-deterministic but we do not
          -- currently support deterministic code-generation.
          -- See Note [Unique Determinism and code generation]

   where
        -- if the temp was InReg and clobbered, then we will have
        -- saved it in saveClobberedTemps above.  So the only case
        -- we have to worry about here is InBoth.  Note that this
        -- also catches temps which were loaded up during allocation
        -- of read registers, not just those saved in saveClobberedTemps.

        clobber :: Unique -> Loc -> RegMap Loc -> RegMap Loc
        clobber temp (Loc (InBoth reg slot) regFmt) assig
                | any (realRegsAlias reg) clobbered
                = addToUFM_Directly assig temp (Loc (InMem slot) regFmt)
        clobber _ _ assig
                = assig

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
   at the loop's end, and does not write it again, the register a recorded jump
   target's assignment gives that one.

 * The register the variable has in the first assignment that, recorded for a
   block it is live into, holds it in a register, if any
   (Note [The first jump into a block fixes its registers]). This way, when we
   allocate for a loop, variables are likely to end up in the same registers at
   the end and start of the loop, avoiding redundant reg-reg moves. That buys
   nothing for a value the current block moves where it dies, which is live into
   no later block, hence the order. Nor does it for a value that reaches no block
   whose assignment is recorded, such as a reload into a variable that the code
   generator also uses for another value, live into the loop's head, where the
   value live into the block that recorded it reaches none of the blocks this
   value reaches either: there it comes after the wide destination.

 * For a variable whose wide destination, below, is propagated from another
   variable it is moved into where it dies, the register that variable has in
   its first assignment: the propagated destination is where that variable
   goes later, such as a loop variable at the loop's exit, and its first
   assignment where it is, such as at the loop's head.

 * The real register a block the variable is live into moves it into where it
   dies, such as R1 for a value evaluated some blocks after it is computed, or,
   for a variable moved where it dies into another variable that has such a
   register, up to four such moves back, that register, so that the value is
   computed where it goes; unless that register is live together with the
   variable somewhere, as wideMoveDests counts it, or, on x86-64, the block
   follows a failed heap or stack check. It is left out also where another
   variable the current block reads or writes, with no location yet, has it as
   its first-used register.

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
registers that a jump target's recorded assignment gives any virtual register
are left out where others remain, its own there being among its preferences.
This applies to the fallback only: a free preferred register is taken even where
a recorded jump target gives it to another virtual register, as leaving those
out too made more join fixups cyclic and cost containers 0.13% and vector 0.11%
more instructions (nofib's puzzle 2.7%). Leaving them out for one with no
preferences too gains nothing measured and costs compile time, the list of
registers to leave out then being built for every such allocation.
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
                 , rc_moveDests = moves
                 , rc_claims    = claims
                 , rc_wideDests = wide
                 , rc_via       = via
                 , rc_block     = blk
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
      -- values, and the wide destination is this value's own
      stale = case firstUsed of
             Just r0
               | Just w <- lookupUFM_Directly wide u, w /= r0, isFree w, isFree r0
               -- (the walk skipped where a jump target's register other than w
               -- is free and taken: without this the compiler allocated 0.012%
               -- more compiling T783 at -O0 and 0.008% more compiling !16269's
               -- CmmDiamondChain at -O and -O2, on GHC 10.1.20261108)
               , maybe True (== w) found
               , not (claimed w)
               , (True, Nothing, region) <- staleWalk u live succs bassig (succsOf blk)
               , any (movesInto w) (setElems region)
               , not (any (meets region) (blocksHolding u r0 bassig))
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
               , (True, Just b, region) <- staleWalk u live succs bassig (succsOf blk)
               , Just assig <- lookupBlockAssignment b bassig
               , Just (Loc l _) <- lookupUFM_Directly assig u
               , r : _ <- regsOfLoc l, r /= r0, isFree r
               , not (any (meets (setInsert b region)) (blocksHolding u r0 bassig))
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
      succsOf b = mapFindWithDefault [] b succs
      -- a vreg already given a location
      assigned v = isJust (lookupUFM_Directly cur v)
      -- a wide destination another vreg of the block has as its first-used
      -- register is left out
      claimed r = any (\(r', v) -> r' == r && v /= u && not (assigned v)) claims
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
      others = nonDetStrictFoldUFM_Directly
                 (\v (RealRegSingle n) b -> if v == u then b else setBit b n)
                 (0 :: Word64) moves
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
      -- for a vreg whose wide destination is propagated from a vreg it is
      -- moved into where it dies, that vreg's first-used register where it is
      -- free: the propagated destination is where that vreg goes later, its
      -- first-used register where it is
      viaFree = case lookupUFM_Directly via u of
                  Just y | isJust (lookupUFM_Directly wide u)
                         , Just r <- lookupFirstUsed (getUnique y) bassig
                         , isFree r
                         -> Just r
                  _      -> Nothing
      -- the first of the jump targets' registers that is free
      found = find isFree jl
      chosen = case found of
                 Nothing -> pick mv (pick fu (pick viaFree wdFree))
                 _       -> found
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

-- | On x86-64, of the given free registers, the one 'firstFreeInOrder' takes
-- for the virtual register with the given unique where none of its preferences
-- is free, given regClobbers' map ('rc_clobbers'), the block assignment
-- and whether it has none: the first in the order
-- Note [Register preferences in the linear allocator] gives.
fallbackReg :: RA_Context freeRegs -> UniqFM VirtualReg Word64
            -> BlockAssignment -> Bool -> Unique -> [RealReg] -> RealReg
fallbackReg RA_Context { rc_jumpLocs = locs, rc_block = blk, rc_succs = succs
                       , rc_joins = joins, rc_live = live, rc_dying = dying }
            clobbers bassig noPrefs u free@(r0 : _)
  = let !r1 = r1Reg
        !cl = lookupWithDefaultUFM_Directly clobbers 0 u
        kept (RealRegSingle n) = not (testBit cl n)
        -- the free ones to choose from: for one with preferences, those no jump
        -- target's assignment gives a virtual register if any remain, and all
        -- of them otherwise
        fs | noPrefs   = free
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
                       (succsOf blk)
        -- the real registers that the blocks up to three jumps ahead of the
        -- current one, those the vreg is live into, move vregs into where they
        -- die, as bits by register number, and above them, shifted by 32, those
        -- they move the vreg itself into (three jumps, bounding the walk,
        -- is arbitrary, and may be raised where a case needs more at little
        -- cost to the compiler: lifted to 1000, compiling 50 to 400 loops in
        -- sequence ran 1.01 to 1.11 times the instructions, the excess growing
        -- as N squared)
        movedAhead :: Word64
        movedAhead = fst (aheadWalks u live succs dying 3 0 mapEmpty
                                     (succsOf blk))
        succsOf b = mapFindWithDefault [] b succs
        unreserved (RealRegSingle n) = not (testBit reserved n)
        reserved = foldl' (\b (RealRegSingle n) -> setBit b n) (0 :: Word64)
                     [ r | assig <- locs, Loc loc _ <- nonDetEltsUFM assig
                         , r <- regsOfLoc loc ]
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
        -- products] in GHC.Core.Opt.DmdAnal), and worker/wrapper box it anew.
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
                  Just (DyingMoves m ds) -> acc .|. m .|. lastDestBit u ds
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

-- | The blocks that two blocks or more of those passing the test jump to
jumpedToTwice :: (BlockId -> Bool) -> BlockMap [BlockId] -> LabelSet
jumpedToTwice from js = case mapFoldlWithKey step (Seen setEmpty setEmpty) js of
  Seen _ twice -> twice
  where
    step seen b ds
      | from b    = targets seen [] ds
      | otherwise = seen
    -- each block's targets counted once, those it jumps to twice included
    targets seen _ [] = seen
    targets seen@(Seen once twice) prev (d : more)
      | d `elem` prev      = targets seen prev more
      | d `setMember` once = targets (Seen once (setInsert d twice)) (d : prev) more
      | otherwise          = targets (Seen (setInsert d once) twice) (d : prev) more
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
      ms  -> mapInsert b (DyingMoves (foldl' (\m (_, RealRegSingle n) -> setBit m n) 0 ms) ms) acc

-- | The register of the last of the moves given that moves the vreg with the
-- given unique, as 'listToUFM' keeps a vreg's last entry.
lastDest :: Unique -> [(VirtualReg, RealReg)] -> Maybe RealReg
lastDest u = foldl' (\acc (v, r) -> if getUnique v == u then Just r else acc) Nothing

-- | As 'lastDest', the register as a bit above 32 by its number, or 0.
lastDestBit :: Unique -> [(VirtualReg, RealReg)] -> Word64
lastDestBit u ds = maybe 0 (\(RealRegSingle n) -> bit (n + 32)) (lastDest u ds)

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
      | n >= 64 = atCap seen
      | b `setMember` seen || not (liveInto u live b) = go n seen bs later
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

-- | 'jumpLocRegs' u locs: the registers the recorded jump-target assignments
-- locs give the virtual register with unique u, in order.
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
                    -- This is non-deterministic but we do not
                    -- currently support deterministic code-generation.
                    -- See Note [Unique Determinism and code generation]
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
