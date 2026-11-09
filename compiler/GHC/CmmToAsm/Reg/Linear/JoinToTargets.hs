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
import GHC.CmmToAsm.Reg.Linear.FreeRegs (FR (..))
import GHC.CmmToAsm.Reg.Target (targetClassOfReg, targetMkVirtualReg)
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Types

import GHC.Platform (platformWordSizeInBytes)
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
import GHC.Data.Word64Map.Strict (isSubmapOfBy)

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

        -- BUG: on x86 a floating-point == or /= test's block ends in two jumps
        -- that name one target, genCondBranch's parity jump and a conditional
        -- or unconditional one, and each is joined on its own. Where the target
        -- needs a fixup block, each jump gets one, with the same moves, and
        -- both are reachable. This costs only code size, on x86 and x86-64.
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

        case lookupBlockAssignment  dest block_assig of
         -- this is the first time we jumped to this block:
         -- remember the current assignment on entry to it: see
         -- Note [The first jump into a block fixes its entry assignment]
         Nothing
          -> do setBlockAssigR
                  (updateBlockAssignment dest adjusted_assig block_assig)
                joinToTargets' block_live new_blocks block_id instr dests

         Just dest_assig
          -> joinToTargets_again
                        block_live new_blocks block_id instr dest dests
                        assig dest_assig


{- Note [The first jump into a block fixes its entry assignment]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Of the jumps into a block, the first that the allocator processes records the
block's entry assignment (joinToTargets'): where each vreg live into the block
is, InReg, InMem or InBoth. Every jump into the block that is processed later is
made to conform to that assignment by fixup code (joinToTargets_again), whatever
its own assignment and however often its edge runs. So the order in which a
block's predecessors are allocated decides where the moves run. The SCCs come
in dependency order, the path into a loop before the loop's body, so that a
loop's head takes the registers of the path into it, and its back edge pays
for any difference at every iteration; firstFreeInOrder's preferences work to
keep a loop's values in its head's registers. The SCCs linearRA_SCCs defers are
allocated after the others. Hence:

* TODO: where a loop head's entry assignment comes from a preheader that has
  spilled a loop-carried vreg, it records the vreg InMem. The body then reloads
  the vreg at its first read, and every back edge stores it again once the body
  has written it. Likewise where the preheader has it InBoth, every back edge
  that holds it in a register only stores it.

* TODO #27800: a return continuation is an entry block, and the allocator, going
  in SCC order, processes it before a loop it jumps into where the loop calls
  something (linearRA_SCCs defers one that jumps into a loop that calls nothing
  and whose entry assignment is not yet recorded). So where the procedure's
  entry and such a continuation enter one loop at two different blocks, the
  two record incompatible assignments, and the loop's own edges get fixup code
  that runs on every iteration. Taking the entry first and then, after each
  SCC, an SCC holding a block whose entry assignment is recorded was tried on a
  development branch and showed no gain.

That first jump decides one more thing. A vreg's first-used register
(lookupFirstUsed) is its register in the first assignment that, recorded for a
block it is live into, holds it in a register, not the first register it was
given, and it stands for the rest of the procedure.
-}

-- we've jumped to this block before
joinToTargets_again :: (FR freeRegs, Instruction instr)
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

        -- every vreg of the recorded assignment is where it records it
        | recordedLocsHold dest_assig src_assig
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
                -- The moves between two slots come first: no other move reads
                -- the slot one writes or writes the slot it reads, both being
                -- one vreg's, and before the other moves every register free
                -- at the jump holds nothing (makeMove).
                fixUpInstrs     <- case filter slotToSlot sccs of
                  []    -> concatMapM (handleComponent delta) sccs
                  slots -> concatMapM (handleComponent delta)
                             (slots ++ filter (not . slotToSlot) sccs)

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
                                -- BUG: instr' jumps to the fixup block
                                -- wherever instr jumped to dest, but dests
                                -- still holds dest's later occurrences, a jump
                                -- table's other entries for it. Each makes the
                                -- same moves again, into a block nothing jumps
                                -- to, which gets an edge in the CFG and is laid
                                -- out and emitted. This costs only code size.
                                recordFixupBlock block_id fixup_block_id dest
                                joinToTargets' block_live (block : new_blocks)
                                               block_id instr' dests


-- | Construct a graph of register\/spill movements.
--
--      A move from memory to memory is made through a register (makeMove).
--      BUG: since #26668 a temporary can have more than one stack slot,
--      and a jump into a block that holds it in another of its slots only
--      needs a move between two of them, which makeMove makes through a
--      register free at the jump, and where there is none, on x86 and x86-64
--      by mkSlotSlotMoveInstr, a word at a time through one register saved and
--      given back; on AArch64, the one other target with formats wider than
--      a slot, it panics. Nothing keeps one free on any target: the assignment
--      at the jump can hold every allocatable register, on x86-64 the eleven
--      integer registers (rax, rbx, rcx, rdx, rsi, rdi, r8 to r11 and r14) and
--      the sixteen vector registers.
--
makeRegMovementGraph :: RegMap Loc -> RegMap Loc -> [Node Loc Unique]
makeRegMovementGraph adjusted_assig dest_assig
 = [ node       | (vreg, src) <- nonDetUFMToList adjusted_assig
                    -- The list's order, by unique, decides the order
                    -- of the fixup moves and which move of a cycle is
                    -- made first, by a swap or through the stack: see
                    -- Note [Unique order in the linear allocator] in
                    -- GHC.CmmToAsm.Reg.Linear
                    -- source reg might not be needed at the dest,
                    -- adjusted_assig being the whole assignment at the jump:
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
expandNode vreg (Loc srcLoc srcFmt) (Loc dstLoc dstFmt)
  | srcLoc == dstLoc = []
  | otherwise
  = case toReg ++ toMem of
      []   -> []
      dsts -> [DigraphNode vreg (Loc from srcFmt) dsts]
  where
    -- the value is moved from the source's register where it has one
    from = case srcLoc of
      InBoth r _ -> InReg r
      _          -> srcLoc
    -- a destination register or slot that is the source's holds the value
    -- already
    toReg = [ Loc (InReg r) dstFmt | r <- regsOfLoc dstLoc
                                   , r `notElem` regsOfLoc srcLoc ]
    toMem = [ Loc (InMem m) dstFmt | Just m <- [slotOf dstLoc]
                                   , slotOf srcLoc /= Just m ]
    slotOf (InMem s)    = Just s
    slotOf (InBoth _ s) = Just s
    slotOf (InReg _)    = Nothing


-- | Generate fixup code for a particular component in the move graph
--      This component tells us what values need to be moved to what
--      destinations. We have eliminated any possibility of single-node
--      cycles in expandNode above.
--
handleComponent
        :: (FR freeRegs, Instruction instr)
        => Int -> SCC (Node Loc Unique)
        -> RegM freeRegs [instr]

-- If the graph is acyclic then we won't get the swapping problem below.
--      In this case we can just do the moves directly, and avoid having to
--      go via a spill slot.
--
-- From a slot to a register and another slot (expandNode), the value is
--      stored into the slot from the register it was loaded into, at the
--      narrower of the two formats, as makeMove moves it between two slots,
--      and with no free register.  The node is in no cycle, its source slot
--      being its vreg's own, and no other move writes the register or the slot.
handleComponent delta
        (AcyclicSCC (DigraphNode vreg src@(Loc (InMem _) srcFmt)
                                 [dst@(Loc (InReg r) dstFmt), Loc (InMem d) _]))
 = do load  <- makeMove delta vreg src dst
      store <- makeMove delta vreg (Loc (InReg r) dstFmt)
                        (Loc (InMem d) (narrowerFormat srcFmt dstFmt))
      return (load ++ store)

handleComponent delta (AcyclicSCC (DigraphNode vreg src dsts))
        = concatMapM (makeMove delta vreg src) dsts


-- Handle some cyclic moves.
--      This can happen if we have two regs that need to be swapped.
--      eg:
--           vreg   source loc   dest loc
--          (vreg1, InReg r1,    [InReg r2])
--          (vreg2, InReg r2,    [InReg r1])
--
--      Where one instruction swaps two registers of the format, we swap the
--      first source with its destination: the value the destination held,
--      the source of another move of the cycle, is then in the source
--      register, so the remaining moves are a cycle one shorter, until
--      the last swap places two values.  Otherwise, to avoid needing a temp
--      register, we spill the first move's source reg into its vreg's slot,
--      do the other moves (handling any cycle left among them the same way),
--      and reload the slot into the first move's destination reg last.
--
--      There can be no cycle through a memory location, as a stack slot is
--      allocated for one virtual register only. A virtual register does get
--      a new slot where its format outgrows the old one (getStackSlotFor,
--      #26668), but no other move writes the slot it is moved from, so its move
--      between the two slots is in no cycle.
--
--      BUG: a jump can then need a move from one of a vreg's slots to another,
--      which makeMove makes through a free register, panicking on AArch64
--      where none is (see makeRegMovementGraph).
--
--      BUG: where the first move goes to an InBoth destination and no one
--      instruction swaps two registers of the format (a floating-point or
--      vector format on x86 and x86-64, any format on the other targets), the
--      pattern below leaves out the destination's slot, which is then written
--      only if it is the slot spillR picks; otherwise the slot keeps a stale
--      value, which is wrong code where it is reloaded, on i386, x86-64 and
--      AArch64, the targets whose code generators take vectors. No program is
--      known to leave a stale slot.
--
handleComponent delta
        (CyclicSCC ((DigraphNode vreg src@(Loc (InReg sreg) scls) ((Loc (InReg dreg) dcls: more))) : rest))
        -- dest list may have more than one element, if the reg is also InMem.
 = do
    config <- getConfig
    case mkRegRegSwapInstr config scls (RegReal sreg) (RegReal dreg) of
      Just swap -> do
        stores <- concatMapM (makeMove delta vreg src) more
        recordSpill (SpillJoinRR vreg)
        -- the value dreg held is now in sreg, so it needs no move into sreg
        let moved (DigraphNode v (Loc (InReg r) fmt) ds)
              | r == dreg = DigraphNode v (Loc (InReg sreg) fmt)
                                        (filter (not . inSreg) ds)
            moved n = n
            inSreg (Loc (InReg r) _) = r == sreg
            inSreg _                 = False
        remainingFixUps <- mapM (handleComponent delta)
                                (movementGraphSCCs (map moved rest))
        return (stores ++ swap : concat remainingFixUps)
      Nothing -> do
        -- spill the source into its slot
        (instrSpill, slot)
                        <- spillR (RegWithFormat (RegReal sreg) scls) vreg

        -- reload into destination reg
        instrLoad       <- loadR (RegWithFormat (RegReal dreg) dcls) slot

        remainingFixUps <- mapM (handleComponent delta)
                                (movementGraphSCCs rest)

        -- make sure to do all the reloads after all the spills,
        --      so we don't end up clobbering the source values.
        return (instrSpill ++ concat remainingFixUps ++ instrLoad)

handleComponent _ (CyclicSCC _)
 = panic "Register Allocator: handleComponent cyclic"

-- | Whether the second assignment has every vreg of the first at the first's
-- location, whatever its formats.
recordedLocsHold :: RegMap Loc -> RegMap Loc -> Bool
recordedLocsHold dest src
  = isSubmapOfBy (\(Loc l1 _) (Loc l2 _) -> l1 == l2)
                 (ufmToIntMap dest) (ufmToIntMap src)

-- | Whether the component is a move between two slots, which expandNode
-- gives a node of its own and makeMove makes through a register.
slotToSlot :: SCC (Node Loc Unique) -> Bool
slotToSlot (AcyclicSCC (DigraphNode _ (Loc (InMem _) _) [Loc (InMem _) _]))
  = True
slotToSlot _ = False

-- A helper function that uses the @Ord (IgnoreFormat Loc)@ instance.

movementGraphSCCs :: [Node Loc Unique] -> [SCC (Node Loc Unique)]
movementGraphSCCs =
  coerce $ stronglyConnCompFromEdgedVerticesOrdR @(IgnoreFormat Loc) @Unique

-- | Move a vreg between these two locations.
--
makeMove
    :: (FR freeRegs, Instruction instr)
    => Int           -- ^ current C stack delta
    -> Unique        -- ^ unique of the vreg that we're moving
    -> Loc -- ^ source location
    -> Loc -- ^ destination location
    -> RegM freeRegs [instr]  -- ^ move instruction

makeMove delta vreg (Loc src srcFmt) (Loc dst dstFmt)
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
          -- A vreg that outgrows its slot gets a new one (getStackSlotFor,
          -- #26668), so on two paths to a join it can be in memory only, in
          -- two slots (#27638).  Where it goes into no register at the join
          -- (handleComponent), it is moved through a register that holds
          -- nothing at the jump, at the narrower of the two formats.  At most
          -- a word can go through an integer register.  Where no such register
          -- is spare, it is moved by mkSlotSlotMoveInstr, through a register
          -- saved and given back, where the target's instance defines the
          -- method (x86 and x86-64), and otherwise makeMove panics.
          (InMem s, InMem d) ->
              do platform <- getPlatform
                 free     <- getFreeRegsR
                 let fmt = narrowerFormat srcFmt dstFmt
                     spare f = [ (r, f)
                               | r <- frGetFreeRegs (targetClassOfReg platform
                                        (RegVirtual (targetMkVirtualReg platform vreg f)))
                                        free ]
                     viaInt | formatInBytes fmt <= platformWordSizeInBytes platform
                            = spare (intFormat (formatToWidth fmt))
                            | otherwise
                            = []
                 recordSpill (SpillJoinRM vreg)
                 recordSpill (SpillJoinRM vreg)
                 case spare fmt ++ viaInt of
                   (r, f) : _ ->
                     return $ mkLoadInstr config (RegWithFormat (RegReal r) f) delta s
                           ++ mkSpillInstr config (RegWithFormat (RegReal r) f) delta d
                   -- BUG: on AArch64, panics where no register is free at the
                   -- jump: see makeRegMovementGraph.
                   []
                     | Just move <- mkSlotSlotMoveInstr config fmt delta s d
                     -> return move
                     | otherwise
                     -> pprPanic "makeMove: no free register for a mem->mem move"
                          (ppr vreg <+> parens (ppr src) <+> parens (ppr dst))
          _ ->
              -- expandNode leaves no InBoth location
              pprPanic "makeMove: InBoth"
                 (ppr vreg <+> parens (ppr src) <+> parens (ppr dst))

-- | The narrower of a vreg's formats in two slots: no more is in the source
-- slot or fits in the destination slot.
narrowerFormat :: Format -> Format -> Format
narrowerFormat srcFmt dstFmt
  | formatInBytes srcFmt <= formatInBytes dstFmt = srcFmt
  | otherwise                                    = dstFmt
