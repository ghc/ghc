
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import qualified GHC.CmmToAsm.Reg.Linear.X86_64  as X86_64
import qualified GHC.CmmToAsm.Reg.Linear.AArch64 as AArch64
import qualified GHC.CmmToAsm.Reg.Linear.RV64    as RV64
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Utils
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Platform.Reg
import GHC.Platform.Reg.Class (RegClass(..))

import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm hiding (RegSet)

import GHC.Data.Graph.Directed
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

import Data.Maybe
import Data.List (partition, nub)
import Control.Monad

-- -----------------------------------------------------------------------------
-- Top level of the register allocator

-- Allocate registers
regAlloc
        :: Instruction instr
        => NCGConfig
        -> LiveCmmDecl statics instr
        -> UniqSM ( NatCmmDecl statics instr
                  , Maybe Int  -- number of extra stack slots required,
                               -- beyond maxSpillSlots
                  , Maybe RegAllocStats
                  )

regAlloc _ (CmmData sec d)
        = return
                ( CmmData sec d
                , Nothing
                , Nothing )

regAlloc _ (CmmProc (LiveInfo info _ _ _) lbl live [])
        = return ( CmmProc info lbl live (ListGraph [])
                 , Nothing
                 , Nothing )

regAlloc config (CmmProc static lbl live sccs)
        | LiveInfo info entry_ids@(first_id:_) block_live _ <- static
        = do
                -- do register allocation on each component.
                !(!final_blocks, !stats, !stack_use)
                        <- linearRegAlloc config entry_ids block_live sccs

                -- make sure the block that was first in the input list
                --      stays at the front of the output
                let !(!(!first':_), !rest')
                                = partition ((== first_id) . blockId) final_blocks

                let max_spill_slots = maxSpillSlots config
                    extra_stack
                      | stack_use > max_spill_slots
                      = Just $! stack_use - max_spill_slots
                      | otherwise
                      = Nothing

                return  ( CmmProc info lbl live (ListGraph (first' : rest'))
                        , extra_stack
                        , Just stats)

-- bogus. to make non-exhaustive match warning go away.
regAlloc _ (CmmProc _ _ _ _)
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
        -> [BlockId] -- ^ entry points
        -> BlockMap RegSet
              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)]
              -- ^ instructions annotated with "deaths"
        -> UniqSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc config entry_ids block_live sccs
 = case platformArch platform of
      ArchX86        -> go $ (frInitFreeRegs platform :: X86.FreeRegs)
      ArchX86_64     -> go $ (frInitFreeRegs platform :: X86_64.FreeRegs)
      ArchS390X      -> panic "linearRegAlloc ArchS390X"
      ArchPPC        -> go $ (frInitFreeRegs platform :: PPC.FreeRegs)
      ArchARM _ _ _  -> panic "linearRegAlloc ArchARM"
      ArchAArch64    -> go $ (frInitFreeRegs platform :: AArch64.FreeRegs)
      ArchPPC_64 _   -> go $ (frInitFreeRegs platform :: PPC.FreeRegs)
      ArchAlpha      -> panic "linearRegAlloc ArchAlpha"
      ArchMipseb     -> panic "linearRegAlloc ArchMipseb"
      ArchMipsel     -> panic "linearRegAlloc ArchMipsel"
      ArchRISCV64    -> go (frInitFreeRegs platform :: RV64.FreeRegs)
      ArchLoongArch64-> panic "linearRegAlloc ArchLoongArch64"
      ArchJavaScript -> panic "linearRegAlloc ArchJavaScript"
      ArchWasm32     -> panic "linearRegAlloc ArchWasm32"
      ArchUnknown    -> panic "linearRegAlloc ArchUnknown"
 where
  go :: (FR regs, Outputable regs)
     => regs -> UniqSM ([NatBasicBlock instr], RegAllocStats, Int)
  go f = linearRegAlloc' config f entry_ids block_live sccs
  platform = ncgPlatform config

-- | Constraints on the instruction instances used by the
-- linear allocator.
type OutputableRegConstraint freeRegs instr =
        (FR freeRegs, Outputable freeRegs, Instruction instr)

linearRegAlloc'
        :: OutputableRegConstraint freeRegs instr
        => NCGConfig
        -> freeRegs
        -> [BlockId]                    -- ^ entry points
        -> BlockMap RegSet              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)] -- ^ instructions annotated with "deaths"
        -> UniqSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc' config initFreeRegs entry_ids block_live sccs
 = do   us      <- getUniqueSupplyM
        let !(_, !stack, !stats, !blocks) =
                runR config emptyBlockAssignment initFreeRegs emptyRegMap emptyStackMap us
                    $ linearRA_SCCs entry_ids block_live [] sccs
        return  (blocks, stats, getStackUse stack)


linearRA_SCCs :: OutputableRegConstraint freeRegs instr
              => [BlockId]
              -> BlockMap RegSet
              -> [NatBasicBlock instr]
              -> [SCC (LiveBasicBlock instr)]
              -> RegM freeRegs [NatBasicBlock instr]

linearRA_SCCs _ _ blocksAcc []
        = return $ reverse blocksAcc

linearRA_SCCs entry_ids block_live blocksAcc (AcyclicSCC block : sccs)
 = do   blocks' <- processBlock block_live block
        linearRA_SCCs entry_ids block_live
                ((reverse blocks') ++ blocksAcc)
                sccs

linearRA_SCCs entry_ids block_live blocksAcc (CyclicSCC blocks : sccs)
 = do
        blockss' <- process entry_ids block_live blocks
        linearRA_SCCs entry_ids block_live
                (reverse (concat blockss') ++ blocksAcc)
                sccs

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

process :: forall freeRegs instr. (OutputableRegConstraint freeRegs instr)
        => [BlockId]
        -> BlockMap RegSet
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
          if isJust (lookupBlockAssignment id block_assig) || id `elem` entry_ids
            then do b' <- processBlock block_live b
                    go blocks next_round (b' : accum) True

            else do go blocks (b : next_round) accum madeProgress


-- | Do register allocation on this basic block
--
processBlock
        :: OutputableRegConstraint freeRegs instr
        => BlockMap RegSet              -- ^ live regs on entry to each basic block
        -> LiveBasicBlock instr         -- ^ block to do register allocation on
        -> RegM freeRegs [NatBasicBlock instr]   -- ^ block with registers allocated

processBlock block_live (BasicBlock id instrs)
 = do   -- pprTraceM "processBlock" $ text "" $$ ppr (BasicBlock id instrs)
        initBlock id block_live

        (instrs', fixups)
                <- linearRA block_live id instrs
        -- pprTraceM "blockResult" $ ppr (instrs', fixups)
        return  $ BasicBlock id instrs' : fixups


-- | Load the freeregs and current reg assignment into the RegM state
--      for the basic block with this BlockId.
initBlock :: FR freeRegs
          => BlockId -> BlockMap RegSet -> RegM freeRegs ()
initBlock id block_live
 = do   platform    <- getPlatform
        block_assig <- getBlockAssigR
        case lookupBlockAssignment id block_assig of
                -- no prior info about this block: we must consider
                -- any fixed regs to be allocated, but we can ignore
                -- virtual regs (presumably this is part of a loop,
                -- and we'll iterate again).  The assignment begins
                -- empty.
                Nothing
                 -> do  -- pprTrace "initFreeRegs" (text $ show initFreeRegs) (return ())
                        case mapLookup id block_live of
                          Nothing ->
                            setFreeRegsR    (frInitFreeRegs platform)
                          Just live ->
                            setFreeRegsR $ foldl' (flip $ frAllocateReg platform) (frInitFreeRegs platform)
                                                  [ r | RegReal r <- nonDetEltsUniqSet live ]
                            -- See Note [Unique Determinism and code generation]
                        setAssigR       emptyRegMap

                -- load info about register assignments leading into this block.
                Just (freeregs, assig)
                 -> do  setFreeRegsR    freeregs
                        setAssigR       assig


-- | Do allocation for a sequence of instructions.
linearRA
        :: forall freeRegs instr. (OutputableRegConstraint freeRegs instr)
        => BlockMap RegSet                      -- ^ map of what vregs are live on entry to each block.
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
        :: OutputableRegConstraint freeRegs instr
        => BlockMap RegSet                      -- ^ map of what vregs are love on entry to each block.
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
    assig    <- getAssigR :: RegM freeRegs (UniqFM Reg Loc)

    -- If we have a reg->reg move between virtual registers, where the
    -- src register is not live after this instruction, and the dst
    -- register does not already have an assignment,
    -- and the source register is assigned to a register, not to a spill slot,
    -- then we can eliminate the instruction.
    -- (we can't eliminate it if the source register is on the stack, because
    --  we do not want to use one spill slot for different virtual registers)
    case takeRegRegMoveInstr instr of
        Just (src,dst)  | src `elementOfUniqSet` (liveDieRead live),
                          isVirtualReg dst,
                          not (dst `elemUFM` assig),
                          isRealReg src || isInReg src assig -> do
           case src of
              (RegReal rr) -> setAssigR (addToUFM assig dst (InReg rr))
                -- if src is a fixed reg, then we just map dest to this
                -- reg in the assignment.  src must be an allocatable reg,
                -- otherwise it wouldn't be in r_dying.
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

        _ -> genRaInsn block_live new_instrs id instr
                        (nonDetEltsUniqSet $ liveDieRead live)
                        (nonDetEltsUniqSet $ liveDieWrite live)
                        -- See Note [Unique Determinism and code generation]

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
isInReg src assig | Just (InReg _) <- lookupUFM assig src = True
                  | otherwise = False


genRaInsn :: forall freeRegs instr.
             (OutputableRegConstraint freeRegs instr)
          => BlockMap RegSet
          -> [instr]
          -> BlockId
          -> instr
          -> [Reg]
          -> [Reg]
          -> RegM freeRegs ([instr], [NatBasicBlock instr])

genRaInsn block_live new_instrs block_id instr r_dying w_dying = do
-- pprTraceM "genRaInsn" $ ppr (block_id, instr)
  platform <- getPlatform
  case regUsageOfInstr platform instr of { RU read written ->
    do
    let real_written    = [ rr  | (RegReal     rr) <- written ] :: [RealReg]
    let virt_written    = [ vr  | (RegVirtual  vr) <- written ]

    -- we don't need to do anything with real registers that are
    -- only read by this instr.  (the list is typically ~2 elements,
    -- so using nub isn't a problem).
    let virt_read       = nub [ vr      | (RegVirtual vr) <- read ] :: [VirtualReg]

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
                        [ (t, RegReal r)
                                | (t, r) <- zip virt_read    r_allocd
                                         ++ zip virt_written w_allocd ]

        patched_instr :: instr
        patched_instr
                = patchRegsOfInstr adjusted_instr patchLookup

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
    let squashed_instr  = case takeRegRegMoveInstr patched_instr of
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
  platform <- getPlatform
  assig <- getAssigR
  free <- getFreeRegsR

  let loop assig !free [] = do setAssigR assig; setFreeRegsR free; return ()
      loop assig !free (RegReal rr : rs) = loop assig (frReleaseReg platform rr free) rs
      loop assig !free (r:rs) =
         case lookupUFM assig r of
         Just (InBoth real _) -> loop (delFromUFM assig r)
                                      (frReleaseReg platform real free) rs
         Just (InReg real)    -> loop (delFromUFM assig r)
                                      (frReleaseReg platform real free) rs
         _                    -> loop (delFromUFM assig r) free rs
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
        => [RealReg]            -- real registers clobbered by this instruction
        -> [Reg]                -- registers which are no longer live after this insn
        -> RegM freeRegs [instr]         -- return: instructions to spill any temps that will
                                -- be clobbered.

saveClobberedTemps [] _
        = return []

saveClobberedTemps clobbered dying
 = do
        assig   <- getAssigR :: RegM freeRegs (UniqFM Reg Loc)
        (assig',instrs) <- nonDetStrictFoldUFM_DirectlyM maybe_spill (assig,[]) assig
        setAssigR assig'
        return $ -- mkComment (text "<saveClobberedTemps>") ++
                 instrs
--              ++ mkComment (text "</saveClobberedTemps>")
   where
     -- Unique represents the VirtualReg
     -- Here we separate the cases which we do want to spill from these we don't.
     maybe_spill :: Unique -> (RegMap Loc,[instr]) -> (Loc) -> RegM freeRegs (RegMap Loc,[instr])
     maybe_spill !temp !(assig,instrs) !loc =
        case loc of
                -- This is non-deterministic but we do not
                -- currently support deterministic code-generation.
                -- See Note [Unique Determinism and code generation]
                InReg reg
                    | any (realRegsAlias reg) clobbered
                    , temp `notElem` map getUnique dying
                    -> clobber temp (assig,instrs) (reg)
                _ -> return (assig,instrs)


     -- See Note [UniqFM and the register allocator]
     clobber :: Unique -> (RegMap Loc,[instr]) -> (RealReg) -> RegM freeRegs (RegMap Loc,[instr])
     clobber temp (assig,instrs) (reg)
       = do platform <- getPlatform

            freeRegs <- getFreeRegsR
            let regclass = targetClassOfRealReg platform reg
                freeRegs_thisClass = frGetFreeRegs platform regclass freeRegs

            case filter (`notElem` clobbered) freeRegs_thisClass of

              -- (1) we have a free reg of the right class that isn't
              -- clobbered by this instruction; use it to save the
              -- clobbered value.
              (my_reg : _) -> do
                  setFreeRegsR (frAllocateReg platform my_reg freeRegs)

                  let new_assign = addToUFM_Directly assig temp (InReg my_reg)
                  let instr = mkRegRegMoveInstr platform
                                  (RegReal reg) (RegReal my_reg)

                  return (new_assign,(instr : instrs))

              -- (2) no free registers: spill the value
              [] -> do
                  (spill, slot)   <- spillR (RegReal reg) temp

                  -- record why this reg was spilled for profiling
                  recordSpill (SpillClobber temp)

                  let new_assign  = addToUFM_Directly assig temp (InBoth reg slot)

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

        let gpRegs  = frGetFreeRegs platform RcInteger freeregs :: [RealReg]
            fltRegs = frGetFreeRegs platform RcFloat   freeregs :: [RealReg]
            dblRegs = frGetFreeRegs platform RcDouble  freeregs :: [RealReg]

        let extra_clobbered = [ r | r <- clobbered
                                  , r `elem` (gpRegs ++ fltRegs ++ dblRegs) ]

        setFreeRegsR $! foldl' (flip $ frAllocateReg platform) freeregs extra_clobbered

        -- setFreeRegsR $! foldl' (flip $ frAllocateReg platform) freeregs clobbered

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

        clobber assig ((temp, InBoth reg slot) : rest)
                | any (realRegsAlias reg) clobbered
                = clobber (addToUFM_Directly assig temp (InMem slot)) rest

        clobber assig (_:rest)
                = clobber assig rest

-- -----------------------------------------------------------------------------
-- allocateRegsAndSpill

-- Why are we performing a spill?
data SpillLoc = ReadMem StackSlot  -- reading from register only in memory
              | WriteNew           -- writing to a new variable
              | WriteMem           -- writing to register only in memory
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
        => Bool                 -- True <=> reading (load up spilled regs)
        -> [VirtualReg]         -- don't push these out
        -> [instr]              -- spill insns
        -> [RealReg]            -- real registers allocated (accum.)
        -> [VirtualReg]         -- temps to allocate
        -> RegM freeRegs ( [instr] , [RealReg])

allocateRegsAndSpill _       _    spills alloc []
        = return (spills, reverse alloc)

allocateRegsAndSpill reading keep spills alloc (r:rs)
 = do   assig <- toVRegMap <$> getAssigR
        -- pprTraceM "allocateRegsAndSpill:assig" (ppr (r:rs) $$ ppr assig)
        -- See Note [UniqFM and the register allocator]
        let doSpill = allocRegsAndSpill_spill reading keep spills alloc r rs assig
        case lookupUFM assig r of
                -- case (1a): already in a register
                Just (InReg my_reg) ->
                        allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

                -- case (1b): already in a register (and memory)
                -- NB1. if we're writing this register, update its assignment to be
                -- InReg, because the memory value is no longer valid.
                -- NB2. This is why we must process written registers here, even if they
                -- are also read by the same instruction.
                Just (InBoth my_reg _)
                 -> do  when (not reading) (setAssigR $ toRegMap (addToUFM assig r (InReg my_reg)))
                        allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

                -- Not already in a register, so we need to find a free one...
                Just (InMem slot) | reading   -> doSpill (ReadMem slot)
                                  | otherwise -> doSpill WriteMem
                Nothing | reading   ->
                   pprPanic "allocateRegsAndSpill: Cannot read from uninitialized register" (ppr r)
                   -- NOTE: if the input to the NCG contains some
                   -- unreachable blocks with junk code, this panic
                   -- might be triggered.  Make sure you only feed
                   -- sensible code into the NCG.  In GHC.Cmm.Pipeline we
                   -- call removeUnreachableBlocks at the end for this
                   -- reason.

                        | otherwise -> doSpill WriteNew

-- | Given a virtual reg find a preferred real register.
-- The preferred register is simply the first one the variable
-- was assigned to (if any). This way when we allocate for a loop
-- variables are likely to end up in the same registers at the
-- end and start of the loop, avoiding redundant reg-reg moves.
-- Note: I tried returning a list of past assignments, but that
-- turned out to barely matter.
findPrefRealReg :: VirtualReg -> RegM freeRegs (Maybe RealReg)
findPrefRealReg vreg = do
  bassig <- getBlockAssigR :: RegM freeRegs (BlockAssignment freeRegs)
  return $ lookupFirstUsed vreg bassig

-- reading is redundant with reason, but we keep it around because it's
-- convenient and it maintains the recursive structure of the allocator. -- EZY
allocRegsAndSpill_spill :: (FR freeRegs, Instruction instr)
                        => Bool
                        -> [VirtualReg]
                        -> [instr]
                        -> [RealReg]
                        -> VirtualReg
                        -> [VirtualReg]
                        -> UniqFM VirtualReg Loc
                        -> SpillLoc
                        -> RegM freeRegs ([instr], [RealReg])
allocRegsAndSpill_spill reading keep spills alloc r rs assig spill_loc
 = do   platform <- getPlatform
        freeRegs <- getFreeRegsR
        let freeRegs_thisClass  = frGetFreeRegs platform (classOfVirtualReg r) freeRegs :: [RealReg]

        -- Can we put the variable into a register it already was?
        pref_reg <- findPrefRealReg r

        case freeRegs_thisClass of
         -- case (2): we have a free register
         (first_free : _) ->
           do   let !final_reg
                        | Just reg <- pref_reg
                        , reg `elem` freeRegs_thisClass
                        = reg
                        | otherwise
                        = first_free
                spills'   <- loadTemp r spill_loc final_reg spills

                setAssigR $ toRegMap
                          $ (addToUFM assig r $! newLocation spill_loc final_reg)
                setFreeRegsR $  frAllocateReg platform final_reg freeRegs

                allocateRegsAndSpill reading keep spills' (final_reg : alloc) rs


          -- case (3): we need to push something out to free up a register
         [] ->
           do   let inRegOrBoth (InReg _) = True
                    inRegOrBoth (InBoth _ _) = True
                    inRegOrBoth _ = False
                let candidates' :: UniqFM VirtualReg Loc
                    candidates' =
                      flip delListFromUFM keep $
                      filterUFM inRegOrBoth $
                      assig
                      -- This is non-deterministic but we do not
                      -- currently support deterministic code-generation.
                      -- See Note [Unique Determinism and code generation]
                let candidates = nonDetUFMToList candidates'

                -- the vregs we could kick out that are already in a slot
                let candidates_inBoth :: [(Unique, RealReg, StackSlot)]
                    candidates_inBoth
                        = [ (temp, reg, mem)
                          | (temp, InBoth reg mem) <- candidates
                          , targetClassOfRealReg platform reg == classOfVirtualReg r ]

                -- the vregs we could kick out that are only in a reg
                --      this would require writing the reg to a new slot before using it.
                let candidates_inReg
                        = [ (temp, reg)
                          | (temp, InReg reg) <- candidates
                          , targetClassOfRealReg platform reg == classOfVirtualReg r ]

                let result

                        -- we have a temporary that is in both register and mem,
                        -- just free up its register for use.
                        | (temp, my_reg, slot) : _      <- candidates_inBoth
                        = do    spills' <- loadTemp r spill_loc my_reg spills
                                let assig1  = addToUFM_Directly assig temp (InMem slot)
                                let assig2  = addToUFM assig1 r $! newLocation spill_loc my_reg

                                setAssigR $ toRegMap assig2
                                allocateRegsAndSpill reading keep spills' (my_reg:alloc) rs

                        -- otherwise, we need to spill a temporary that currently
                        -- resides in a register.
                        | (temp_to_push_out, (my_reg :: RealReg)) : _
                                        <- candidates_inReg
                        = do
                                (spill_store, slot) <- spillR (RegReal my_reg) temp_to_push_out

                                -- record that this temp was spilled
                                recordSpill (SpillAlloc temp_to_push_out)

                                -- update the register assignment
                                let assig1  = addToUFM_Directly assig temp_to_push_out   (InMem slot)
                                let assig2  = addToUFM assig1 r                 $! newLocation spill_loc my_reg
                                setAssigR $ toRegMap assig2

                                -- if need be, load up a spilled temp into the reg we've just freed up.
                                spills' <- loadTemp r spill_loc my_reg spills

                                allocateRegsAndSpill reading keep
                                        (spill_store ++ spills')
                                        (my_reg:alloc) rs


                        -- there wasn't anything to spill, so we're screwed.
                        | otherwise
                        = pprPanic ("RegAllocLinear.allocRegsAndSpill: no spill candidates\n")
                        $ vcat
                                [ text "allocating vreg:  " <> text (show r)
                                , text "assignment:       " <> ppr assig
                                , text "freeRegs:         " <> text (show freeRegs)
                                , text "initFreeRegs:     " <> text (show (frInitFreeRegs platform `asTypeOf` freeRegs)) ]

                result


-- | Calculate a new location after a register has been loaded.
newLocation :: SpillLoc -> RealReg -> Loc
-- if the tmp was read from a slot, then now its in a reg as well
newLocation (ReadMem slot) my_reg = InBoth my_reg slot
-- writes will always result in only the register being available
newLocation _ my_reg = InReg my_reg

-- | Load up a spilled temporary if we need to (read from memory).
loadTemp
        :: (Instruction instr)
        => VirtualReg   -- the temp being loaded
        -> SpillLoc     -- the current location of this temp
        -> RealReg      -- the hreg to load the temp into
        -> [instr]
        -> RegM freeRegs [instr]

loadTemp vreg (ReadMem slot) hreg spills
 = do
        insn <- loadR (RegReal hreg) slot
        recordSpill (SpillLoad $ getUnique vreg)
        return  $  {- mkComment (text "spill load") : -} insn ++ spills

loadTemp _ _ _ spills =
   return spills
