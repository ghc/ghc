
-- | Handles joining of a jump instruction to its targets.

--      The first time we encounter a jump to a particular basic block, we
--      record the assignment of temporaries.  The next time we encounter a
--      jump to the same block, we compare our current assignment to the
--      stored one.  They might be different if spilling has occurred in one
--      branch; so some fixup code will be required to match up the assignments.
--
module RegAlloc.Linear.JoinToTargets (joinToTargets) where

import RegAlloc.Linear.State
import RegAlloc.Linear.Base
import RegAlloc.Linear.FreeRegs
import RegAlloc.Liveness
import Instruction
import Reg

import BlockId
import Digraph
import DynFlags
import Outputable
import Unique
import UniqFM
import UniqSet


-- | For a jump instruction at the end of a block, generate fixup code so its
--      vregs are in the correct regs for its destination.
--
joinToTargets
        :: (FR freeRegs, Instruction instr)
        => BlockMap RegSet              -- ^ maps the unique of the blockid to the set of vregs
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
        => BlockMap RegSet              -- ^ maps the unique of the blockid to the set of vregs
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
        let Just live_set       = mapLookup dest block_live
        let still_live uniq _   = uniq `elemUniqSet_Directly` live_set
        let adjusted_assig      = filterUFM_Directly still_live assig

        -- and free up those registers which are now free.
        let to_free =
                [ r     | (reg, loc) <- ufmToList assig
                        , not (elemUniqSet_Directly reg live_set)
                        , r          <- regsOfLoc loc ]

        case mapLookup dest block_assig of
         Nothing
          -> joinToTargets_first
                        block_live new_blocks block_id instr dest dests
                        block_assig adjusted_assig to_free

         Just (_, dest_assig)
          -> joinToTargets_again
                        block_live new_blocks block_id instr dest dests
                        adjusted_assig dest_assig


-- this is the first time we jumped to this block.
joinToTargets_first :: (FR freeRegs, Instruction instr)
                    => BlockMap RegSet
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

 = do   dflags <- getDynFlags
        let platform = targetPlatform dflags

        -- free up the regs that are not live on entry to this block.
        freeregs        <- getFreeRegsR
        let freeregs' = foldr (frReleaseReg platform) freeregs to_free

        -- remember the current assignment on entry to this block.
        setBlockAssigR (mapInsert dest (freeregs', src_assig) block_assig)

        joinToTargets' block_live new_blocks block_id instr dests


-- we've jumped to this block before
joinToTargets_again :: (Instruction instr, FR freeRegs)
                    => BlockMap RegSet
                    -> [NatBasicBlock instr]
                    -> BlockId
                    -> instr
                    -> BlockId
                    -> [BlockId]
                    -> UniqFM Loc
                    -> UniqFM Loc
                    -> RegM freeRegs ([NatBasicBlock instr], instr)
joinToTargets_again
    block_live new_blocks block_id instr dest dests
    src_assig dest_assig

        -- the assignments already match, no problem.
        | ufmToList dest_assig == ufmToList src_assig
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
                let sccs  = stronglyConnCompFromEdgedVerticesR graph

{-              -- debugging
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
                fixUpInstrs_    <- mapM (handleComponent delta instr) sccs
                let fixUpInstrs = concat fixUpInstrs_

                -- make a new basic block containing the fixup code.
                --      A the end of the current block we will jump to the fixup one,
                --      then that will jump to our original destination.
                fixup_block_id <- getUniqueR
                let block = BasicBlock (mkBlockId fixup_block_id)
                                $ fixUpInstrs ++ mkJumpInstr dest

{-              pprTrace
                        ("joinToTargets: fixup code is:")
                        (vcat   [ ppr block
                                , text ""])
                        (return ())
-}
                -- if we didn't need any fixups, then don't include the block
                case fixUpInstrs of
                 []     -> joinToTargets' block_live new_blocks block_id instr dests

                 -- patch the original branch instruction so it goes to our
                 --     fixup block instead.
                 _      -> let  instr'  =  patchJumpInstr instr
                                                (\bid -> if bid == dest
                                                                then mkBlockId fixup_block_id
                                                                else bid) -- no change!

                           in   joinToTargets' block_live (block : new_blocks) block_id instr' dests


-- | Construct a graph of register\/spill movements.
--
--      Cyclic components seem to occur only very rarely.
--
--      We cut some corners by not handling memory-to-memory moves.
--      This shouldn't happen because every temporary gets its own stack slot.
--
makeRegMovementGraph :: RegMap Loc -> RegMap Loc -> [(Unique, Loc, [Loc])]
makeRegMovementGraph adjusted_assig dest_assig
 = [ node       | (vreg, src) <- ufmToList adjusted_assig
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
        -> Loc                  -- ^ source of move
        -> Loc                  -- ^ destination of move
        -> [(a, Loc, [Loc])]

expandNode vreg loc@(InReg src) (InBoth dst mem)
        | src == dst = [(vreg, loc, [InMem mem])]
        | otherwise  = [(vreg, loc, [InReg dst, InMem mem])]

expandNode vreg loc@(InMem src) (InBoth dst mem)
        | src == mem = [(vreg, loc, [InReg dst])]
        | otherwise  = [(vreg, loc, [InReg dst, InMem mem])]

expandNode _        (InBoth _ src) (InMem dst)
        | src == dst = [] -- guaranteed to be true

expandNode _        (InBoth src _) (InReg dst)
        | src == dst = []

expandNode vreg     (InBoth src _) dst
        = expandNode vreg (InReg src) dst

expandNode vreg src dst
        | src == dst = []
        | otherwise  = [(vreg, src, [dst])]


-- | Generate fixup code for a particular component in the move graph
--      This component tells us what values need to be moved to what
--      destinations. We have eliminated any possibility of single-node
--      cycles in expandNode above.
--
handleComponent
        :: Instruction instr
        => Int -> instr -> SCC (Unique, Loc, [Loc])
        -> RegM freeRegs [instr]

-- If the graph is acyclic then we won't get the swapping problem below.
--      In this case we can just do the moves directly, and avoid having to
--      go via a spill slot.
--
handleComponent delta _  (AcyclicSCC (vreg, src, dsts))
        = mapM (makeMove delta vreg src) dsts


-- Handle some cyclic moves.
--      This can happen if we have two regs that need to be swapped.
--      eg:
--           vreg   source loc   dest loc
--          (vreg1, InReg r1,    [InReg r2])
--          (vreg2, InReg r2,    [InReg r1])
--
--      To avoid needing temp register, we just spill all the source regs, then
--      reaload them into their destination regs.
--
--      Note that we can not have cycles that involve memory locations as
--      sources as single destination because memory locations (stack slots)
--      are allocated exclusively for a virtual register and therefore can not
--      require a fixup.
--
handleComponent delta instr
        (CyclicSCC ((vreg, InReg sreg, (InReg dreg: _)) : rest))
        -- dest list may have more than one element, if the reg is also InMem.
 = do
        -- spill the source into its slot
        (instrSpill, slot)
                        <- spillR (RegReal sreg) vreg

        -- reload into destination reg
        instrLoad       <- loadR (RegReal dreg) slot

        remainingFixUps <- mapM (handleComponent delta instr)
                                (stronglyConnCompFromEdgedVerticesR rest)

        -- make sure to do all the reloads after all the spills,
        --      so we don't end up clobbering the source values.
        return ([instrSpill] ++ concat remainingFixUps ++ [instrLoad])

handleComponent _ _ (CyclicSCC _)
 = panic "Register Allocator: handleComponent cyclic"


-- | Move a vreg between these two locations.
--
makeMove
    :: Instruction instr
    => Int      -- ^ current C stack delta.
    -> Unique   -- ^ unique of the vreg that we're moving.
    -> Loc      -- ^ source location.
    -> Loc      -- ^ destination location.
    -> RegM freeRegs instr  -- ^ move instruction.

makeMove delta vreg src dst
 = do dflags <- getDynFlags
      let platform = targetPlatform dflags

      case (src, dst) of
          (InReg s, InReg d) ->
              do recordSpill (SpillJoinRR vreg)
                 return $ mkRegRegMoveInstr platform (RegReal s) (RegReal d)
          (InMem s, InReg d) ->
              do recordSpill (SpillJoinRM vreg)
                 return $ mkLoadInstr dflags (RegReal d) delta s
          (InReg s, InMem d) ->
              do recordSpill (SpillJoinRM vreg)
                 return $ mkSpillInstr dflags (RegReal s) delta d
          _ ->
              -- we don't handle memory to memory moves.
              -- they shouldn't happen because we don't share
              -- stack slots between vregs.
              panic ("makeMove " ++ show vreg ++ " (" ++ show src ++ ") ("
                  ++ show dst ++ ")"
                  ++ " we don't handle mem->mem moves.")

