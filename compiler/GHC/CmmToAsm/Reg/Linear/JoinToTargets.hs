-- | Handles joining of a jump instruction to its targets.

--      The first time we encounter a jump to a particular basic block, we
--      record the assignment of temporaries.  The next time we encounter a
--      jump to the same block, we compare our current assignment to the
--      stored one.  They might be different if spilling has occurred in one
--      branch; so some fixup code will be required to match up the assignments.
--
module GHC.CmmToAsm.Reg.Linear.JoinToTargets (joinToTargets) where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Linear.State
import GHC.CmmToAsm.Reg.Linear.Base
import GHC.CmmToAsm.Reg.Linear.FreeRegs
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Types

import GHC.Platform.Reg

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Data.Graph.Directed
import GHC.Data.Maybe
import GHC.Utils.Panic
import GHC.Utils.Monad (concatMapM)
import GHC.Types.Unique
import GHC.Types.Unique.FM

import GHC.Utils.Outputable
import GHC.CmmToAsm.Format
import GHC.Types.Unique.Set

import Data.Coerce (coerce)

-- | For a jump instruction at the end of a block, generate fixup code so its
--      vregs are in the correct regs for its destination.
--
joinToTargets
        :: (FR freeRegs, Instruction instr)
        => BlockMap Regs -- ^ maps the unique of the blockid to the set of vregs
                                        --      that are known to be live on the entry to each block.

        -> BlockId                      -- ^ id of the current block
        -> instr                        -- ^ branch instr on the end of the source block.

        -> RegM freeRegs ([NatBasicBlock instr] -- fresh blocks of fixup code.
                         , instr)               -- the original branch
                                                -- instruction, but maybe
                                                -- patched to jump
                                                -- to a fixup block first.

joinToTargets block_live id instr

        -- we only need to worry about jump instructions.
        | not $ isJumpishInstr instr
        = return ([], instr)

        | otherwise
        = joinToTargets' block_live [] id instr (jumpDestsOfInstr instr)

-----
joinToTargets'
        :: (FR freeRegs, Instruction instr)
        => BlockMap Regs -- ^ maps the unique of the blockid to the set of vregs
                                        --      that are known to be live on the entry to each block.

        -> [NatBasicBlock instr]        -- ^ acc blocks of fixup code.

        -> BlockId                      -- ^ id of the current block
        -> instr                        -- ^ branch instr on the end of the source block.

        -> [BlockId]                    -- ^ branch destinations still to consider.

        -> RegM freeRegs ([NatBasicBlock instr], instr)

-- no more targets to consider. all done.
joinToTargets' _          new_blocks _ instr []
        = return (new_blocks, instr)

-- handle a branch target.
joinToTargets' block_live new_blocks block_id instr (dest:dests)
 = do
        -- get the map of where the vregs are stored on entry to each basic block.
        block_assig     <- getBlockAssigR

        -- get the assignment on entry to the branch instruction.
        assig           <- getAssigR

        -- adjust the current assignment to remove any vregs that are not live
        -- on entry to the destination block.
        let live_set            = expectJust $ mapLookup dest block_live
        let still_live uniq _   = uniq `elemUniqSet_Directly` getRegs live_set
        let adjusted_assig      = filterUFM_Directly still_live assig

        -- and free up those registers which are now free.
        let to_free =
                [ r     | (reg, Loc loc _locFmt) <- nonDetUFMToList assig
                        -- The registers are released into a bitmap of
                        -- free registers, so the order does not matter
                        -- (Note [Unique Determinism and code generation])
                        , not (elemUniqSet_Directly reg $ getRegs live_set)
                        , r          <- regsOfLoc loc ]

        case lookupBlockAssignment  dest block_assig of
         Nothing
          -> joinToTargets_first
                        block_live new_blocks block_id instr dest dests
                        block_assig adjusted_assig to_free

         Just (_, dest_assig)
          -> joinToTargets_again
                        block_live new_blocks block_id instr dest dests
                        adjusted_assig dest_assig


{- Note [The first jump into a block fixes its entry assignment]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Of the jumps into a block, the first that the allocator processes records the
block's entry assignment (joinToTargets_first): where each vreg live into the
block is, InReg, InMem or InBoth, and which registers are free. Every jump into
the block that is processed later is made to conform to that assignment by fixup
code (joinToTargets_again), whatever its own assignment and however often its
edge runs. Hence:

* TODO: where a loop head's entry assignment comes from a preheader that has
  spilled a loop-carried vreg, it records the vreg InMem. The body then reloads
  the vreg at its first read, and every back edge stores it again once the body
  has written it.

* TODO #27800: a return continuation is an entry block, and the allocator,
  going in SCC order, processes it before the loops it jumps into. So where the
  procedure's entry and such a continuation enter one loop at two different
  blocks, the two record incompatible assignments, and the loop's own edges get
  fixup code that runs on every iteration. Taking the entry first and then,
  after each SCC, an SCC holding a block whose entry assignment is recorded was
  tried on a development branch and showed no gain.

* TODO: where a failed heap or stack check returns from the garbage collector
  to a block of its own (GHC.StgToCmm.Heap), that block is an entry block that
  jumps back into the checking block. The order in which the allocator takes the
  blocks, from sccBlocks in GHC.CmmToAsm.Reg.Liveness, depends on no frequency,
  so the return block can come first and record the assignment that the hot
  path's jump into the checking block must then conform to.

The assignment a jump arrives with is chosen without regard to the entry
assignment already recorded for its target: see the TODOs in joinToTargets_first
(#27981) and in findPrefRealReg in GHC.CmmToAsm.Reg.Linear (#27980).
-}

-- this is the first time we jumped to this block.
joinToTargets_first :: (FR freeRegs, Instruction instr)
                    => BlockMap Regs
                    -> [NatBasicBlock instr]
                    -> BlockId
                    -> instr
                    -> BlockId
                    -> [BlockId]
                    -> BlockAssignment freeRegs
                    -> RegMap Loc
                    -> [RealReg]
                    -> RegM freeRegs ([NatBasicBlock instr], instr)
joinToTargets_first block_live new_blocks block_id instr dest dests
        block_assig src_assig
        to_free

 = do   -- free up the regs that are not live on entry to this block.
        -- TODO #27981: to_free holds only registers that hold vregs. So a real
        -- register still allocated at the jump, such as R1 on x86-64 where
        -- it is read after a conditional branch, stays allocated in the free
        -- registers recorded for the block, even when it is not live into it.
        freeregs        <- getFreeRegsR
        let freeregs' = foldl' (flip frReleaseReg) freeregs to_free

        -- remember the current assignment on entry to this block: see
        -- Note [The first jump into a block fixes its entry assignment]
        setBlockAssigR (updateBlockAssignment dest (freeregs', src_assig) block_assig)

        joinToTargets' block_live new_blocks block_id instr dests

-- we've jumped to this block before
joinToTargets_again :: (Instruction instr, FR freeRegs)
                    => BlockMap Regs
                    -> [NatBasicBlock instr]
                    -> BlockId
                    -> instr
                    -> BlockId
                    -> [BlockId]
                    -> UniqFM Reg Loc
                    -> UniqFM Reg Loc
                    -> RegM freeRegs ([NatBasicBlock instr], instr)
joinToTargets_again
    block_live new_blocks block_id instr dest dests
    src_assig dest_assig

        -- the assignments already match, no problem.
        | equalIgnoringFormats
            (nonDetUFMToList dest_assig)
            (nonDetUFMToList src_assig)
        -- Both lists are in the order of the uniques, so
        -- the comparison does not depend on that order
        -- (Note [Unique Determinism and code generation])
        = joinToTargets' block_live new_blocks block_id instr dests

        -- assignments don't match, need fixup code
        | otherwise
        = do

                -- make a graph of what things need to be moved where.
                let graph = makeRegMovementGraph src_assig dest_assig

                -- look for cycles in the graph. This can happen if regs need to be swapped.
                -- Note that we depend on the fact that this function does a
                --      bottom up traversal of the tree-like portions of the graph.
                --
                --  eg, if we have
                --      R1 -> R2 -> R3
                --
                --  ie move value in R1 to R2 and value in R2 to R3.
                --
                -- We need to do the R2 -> R3 move before R1 -> R2.
                --
                let sccs  = movementGraphSCCs graph

              -- debugging
                {-
                pprTrace
                        ("joinToTargets: making fixup code")
                        (vcat   [ text "        in block: "     <> ppr block_id
                                , text " jmp instruction: "     <> ppr instr
                                , text "  src assignment: "     <> ppr src_assig
                                , text " dest assignment: "     <> ppr dest_assig
                                , text "  movement graph: "     <> ppr graph
                                , text "   sccs of graph: "     <> ppr sccs
                                , text ""])
                        (return ())
                -}
                delta           <- getDeltaR
                fixUpInstrs     <- concatMapM (handleComponent delta instr) sccs

                -- make a new basic block containing the fixup code.
                --      A the end of the current block we will jump to the fixup one,
                --      then that will jump to our original destination.
                --      The fixup code gets a block of its own even where the
                --      jump is unconditional; putting the moves before such
                --      a jump instead was tried on a development branch and
                --      showed no gain.
                fixup_block_id <- mkBlockId <$> getUniqueR
                let block = BasicBlock fixup_block_id
                                $ fixUpInstrs ++ mkJumpInstr dest

                -- if we didn't need any fixups, then don't include the block
                case fixUpInstrs of
                 []     -> joinToTargets' block_live new_blocks block_id instr dests

                 -- patch the original branch instruction so it goes to our
                 --     fixup block instead.
                 _      -> let  instr'  =  patchJumpInstr instr
                                            (\bid -> if bid == dest
                                                        then fixup_block_id
                                                        else bid) -- no change!

                           in do
                                {- --debugging
                                pprTrace "FixUpEdge info:"
                                    (
                                    text "inBlock:" <> ppr block_id $$
                                    text "instr:" <> ppr instr $$
                                    text "instr':" <> ppr instr' $$
                                    text "fixup_block_id':" <>
                                        ppr fixup_block_id $$
                                    text "dest:" <> ppr dest
                                    ) (return ())
                                -}
                                recordFixupBlock block_id fixup_block_id dest
                                joinToTargets' block_live (block : new_blocks)
                                               block_id instr' dests


-- | Construct a graph of register\/spill movements.
--
--      We cut some corners by not handling memory-to-memory moves.
--      This shouldn't happen because every temporary gets its own stack slot.
--      BUG #27638: since #26668 a temporary can have more than one stack slot,
--      and a move between two of them panics in makeMove: see handleComponent.
--
makeRegMovementGraph :: RegMap Loc -> RegMap Loc -> [Node Loc Unique]
makeRegMovementGraph adjusted_assig dest_assig
 = [ node       | (vreg, src) <- nonDetUFMToList adjusted_assig
                    -- The list's order, by unique, decides the order of the
                    -- fixup moves and which move of a cycle goes through the
                    -- stack: see Note [Unique order in the linear allocator] in
                    -- GHC.CmmToAsm.Reg.Linear
                    -- source reg might not be needed at the dest:
                , Just loc <- [lookupUFM_Directly dest_assig vreg]
                , node <- expandNode vreg src loc ]


-- | Expand out the destination, so InBoth destinations turn into
--      a combination of InReg and InMem.

--      The InBoth handling is a little tricky here.  If the destination is
--      InBoth, then we must ensure that the value ends up in both locations.
--      An InBoth  destination must conflict with an InReg or InMem source, so
--      we expand an InBoth destination as necessary.
--
--      An InBoth source is slightly different: we only care about the register
--      that the source value is in, so that we can move it to the destinations.
--
expandNode
        :: a
        -> Loc -- ^ source of move
        -> Loc -- ^ destination of move
        -> [Node Loc a]
expandNode vreg src@(Loc srcLoc srcFmt) dst@(Loc dstLoc dstFmt) =
  case (srcLoc, dstLoc) of
    (InReg srcReg, InBoth dstReg dstMem)
        | srcReg == dstReg
        -> [DigraphNode vreg src [Loc (InMem dstMem) dstFmt]]
        | otherwise
        -> [DigraphNode vreg src [Loc (InReg dstReg) dstFmt
                                 ,Loc (InMem dstMem) dstFmt]]
    (InMem srcMem, InBoth dstReg dstMem)
        | srcMem == dstMem
        -> [DigraphNode vreg src [Loc (InReg dstReg) dstFmt]]
        | otherwise
        -> [DigraphNode vreg src [Loc (InReg dstReg) dstFmt
                                 ,Loc (InMem dstMem) dstFmt]]
    (InBoth _ srcMem, InMem dstMem)
        | srcMem == dstMem
        -> [] -- guaranteed to be true
    (InBoth srcReg _, InReg dstReg)
        | srcReg == dstReg
        -> []
    -- TODO: where dst is InBoth with the source's slot, as when a vreg is
    -- InBoth at both ends of the jump, this stores the value into a slot that
    -- already holds it.
    (InBoth srcReg _, _)
        -> expandNode vreg (Loc (InReg srcReg) srcFmt) dst
    _
      | srcLoc == dstLoc
      -> []
      | otherwise
      -> [DigraphNode vreg src [dst]]


-- | Generate fixup code for a particular component in the move graph
--      This component tells us what values need to be moved to what
--      destinations. We have eliminated any possibility of single-node
--      cycles in expandNode above.
--
handleComponent
        :: Instruction instr
        => Int -> instr -> SCC (Node Loc Unique)
        -> RegM freeRegs [instr]

-- If the graph is acyclic then we won't get the swapping problem below.
--      In this case we can just do the moves directly, and avoid having to
--      go via a spill slot.
--
handleComponent delta _  (AcyclicSCC (DigraphNode vreg src dsts))
        = concatMapM (makeMove delta vreg src) dsts


-- Handle some cyclic moves.
--      This can happen if we have two regs that need to be swapped.
--      eg:
--           vreg   source loc   dest loc
--          (vreg1, InReg r1,    [InReg r2])
--          (vreg2, InReg r2,    [InReg r1])
--
--      To avoid needing a temp register, we spill the first move's source
--      reg into its vreg's slot, do the other moves (handling any cycle left
--      among them the same way), and reload the slot into the first move's
--      destination reg last.
--
--      There can be no cycle through a memory location, as a stack slot is
--      allocated for one virtual register only. But since #26668 a vreg can
--      have more than one slot: getStackSlotFor gives it a new one where its
--      format has outgrown the old.
--
--      BUG #27638: a jump can then need a move from one of a vreg's slots to
--      another, which makeMove does not handle: it panics. And where the first
--      move goes to an InBoth destination, the pattern below leaves out the
--      destination's slot, which is then written only if it is the slot spillR
--      picks; otherwise the slot keeps a stale value, which is wrong code where
--      it is reloaded. No program reaching either is known.
--
handleComponent delta instr
        (CyclicSCC ((DigraphNode vreg (Loc (InReg sreg) scls) ((Loc (InReg dreg) dcls: _))) : rest))
        -- dest list may have more than one element, if the reg is also InMem.
 = do
        -- spill the source into its slot
        (instrSpill, slot)
                        <- spillR (RegWithFormat (RegReal sreg) scls) vreg

        -- reload into destination reg
        instrLoad       <- loadR (RegWithFormat (RegReal dreg) dcls) slot

        remainingFixUps <- mapM (handleComponent delta instr)
                                (movementGraphSCCs rest)

        -- make sure to do all the reloads after all the spills,
        --      so we don't end up clobbering the source values.
        return (instrSpill ++ concat remainingFixUps ++ instrLoad)

handleComponent _ _ (CyclicSCC _)
 = panic "Register Allocator: handleComponent cyclic"

-- Helper functions that use the @Ord (IgnoreFormat Loc)@ instance.

equalIgnoringFormats :: [(Unique, Loc)] -> [(Unique, Loc)] -> Bool
equalIgnoringFormats =
  coerce $ (==) @[(Unique, IgnoreFormat Loc)]
movementGraphSCCs :: [Node Loc Unique] -> [SCC (Node Loc Unique)]
movementGraphSCCs =
  coerce $ stronglyConnCompFromEdgedVerticesOrdR @(IgnoreFormat Loc) @Unique

-- | Move a vreg between these two locations.
--
makeMove
    :: Instruction instr
    => Int           -- ^ current C stack delta
    -> Unique        -- ^ unique of the vreg that we're moving
    -> Loc -- ^ source location
    -> Loc -- ^ destination location
    -> RegM freeRegs [instr]  -- ^ move instruction

makeMove delta vreg (Loc src _srcFmt) (Loc dst dstFmt)
 = do config <- getConfig
      case (src, dst) of
          (InReg s, InReg d) ->
              do recordSpill (SpillJoinRR vreg)
                 return $ [mkRegRegMoveInstr config dstFmt (RegReal s) (RegReal d)]
          (InMem s, InReg d) ->
              do recordSpill (SpillJoinRM vreg)
                 return $ mkLoadInstr config (RegWithFormat (RegReal d) dstFmt) delta s
          (InReg s, InMem d) ->
              do recordSpill (SpillJoinRM vreg)
                 return $ mkSpillInstr config (RegWithFormat (RegReal s) dstFmt) delta d
          _ ->
              -- we don't handle memory to memory moves.
              -- they shouldn't happen because we don't share
              -- stack slots between vregs.
              -- BUG #27638: they can happen, between two slots of one vreg, and
              -- the compiler then panics here: see handleComponent.
              pprPanic "makeMove: we don't handle mem->mem moves"
                 (ppr vreg <+> parens (ppr src) <+> parens (ppr dst))
