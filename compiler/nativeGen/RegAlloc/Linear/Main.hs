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

module RegAlloc.Linear.Main (
        regAlloc,
        module  RegAlloc.Linear.Base,
        module  RegAlloc.Linear.Stats
  ) where

#include "HsVersions.h"


import RegAlloc.Linear.State
import RegAlloc.Linear.Base
import RegAlloc.Linear.StackMap
import RegAlloc.Linear.FreeRegs
import RegAlloc.Linear.Stats
import RegAlloc.Linear.JoinToTargets
import qualified RegAlloc.Linear.PPC.FreeRegs    as PPC
import qualified RegAlloc.Linear.SPARC.FreeRegs  as SPARC
import qualified RegAlloc.Linear.X86.FreeRegs    as X86
import qualified RegAlloc.Linear.X86_64.FreeRegs as X86_64
import TargetReg
import RegAlloc.Liveness
import Instruction
import Reg

import BlockId
import Cmm hiding (RegSet)

import Digraph
import DynFlags
import Unique
import UniqSet
import UniqFM
import UniqSupply
import Outputable
import Platform

import Data.Maybe
import Data.List
import Control.Monad

-- -----------------------------------------------------------------------------
-- Top level of the register allocator

-- Allocate registers
regAlloc
        :: (Outputable instr, Instruction instr)
        => DynFlags
        -> LiveCmmDecl statics instr
        -> UniqSM ( NatCmmDecl statics instr
                  , Maybe Int  -- number of extra stack slots required,
                               -- beyond maxSpillSlots
                  , Maybe RegAllocStats)

regAlloc _ (CmmData sec d)
        = return
                ( CmmData sec d
                , Nothing
                , Nothing )

regAlloc _ (CmmProc (LiveInfo info _ _ _) lbl live [])
        = return ( CmmProc info lbl live (ListGraph [])
                 , Nothing
                 , Nothing )

regAlloc dflags (CmmProc static lbl live sccs)
        | LiveInfo info (Just first_id) (Just block_live) _     <- static
        = do
                -- do register allocation on each component.
                (final_blocks, stats, stack_use)
                        <- linearRegAlloc dflags first_id block_live sccs

                -- make sure the block that was first in the input list
                --      stays at the front of the output
                let ((first':_), rest')
                                = partition ((== first_id) . blockId) final_blocks

                let max_spill_slots = maxSpillSlots dflags
                    extra_stack
                      | stack_use > max_spill_slots
                      = Just (stack_use - max_spill_slots)
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
        :: (Outputable instr, Instruction instr)
        => DynFlags
        -> BlockId                      -- ^ the first block
        -> BlockMap RegSet              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)] -- ^ instructions annotated with "deaths"
        -> UniqSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc dflags first_id block_live sccs
 = let platform = targetPlatform dflags
   in case platformArch platform of
      ArchX86       -> linearRegAlloc' dflags (frInitFreeRegs platform :: X86.FreeRegs)    first_id block_live sccs
      ArchX86_64    -> linearRegAlloc' dflags (frInitFreeRegs platform :: X86_64.FreeRegs) first_id block_live sccs
      ArchSPARC     -> linearRegAlloc' dflags (frInitFreeRegs platform :: SPARC.FreeRegs)  first_id block_live sccs
      ArchPPC       -> linearRegAlloc' dflags (frInitFreeRegs platform :: PPC.FreeRegs)    first_id block_live sccs
      ArchARM _ _ _ -> panic "linearRegAlloc ArchARM"
      ArchPPC_64    -> panic "linearRegAlloc ArchPPC_64"
      ArchAlpha     -> panic "linearRegAlloc ArchAlpha"
      ArchMipseb    -> panic "linearRegAlloc ArchMipseb"
      ArchMipsel    -> panic "linearRegAlloc ArchMipsel"
      ArchJavaScript -> panic "linearRegAlloc ArchJavaScript"
      ArchUnknown   -> panic "linearRegAlloc ArchUnknown"

linearRegAlloc'
        :: (FR freeRegs, Outputable instr, Instruction instr)
        => DynFlags
        -> freeRegs
        -> BlockId                      -- ^ the first block
        -> BlockMap RegSet              -- ^ live regs on entry to each basic block
        -> [SCC (LiveBasicBlock instr)] -- ^ instructions annotated with "deaths"
        -> UniqSM ([NatBasicBlock instr], RegAllocStats, Int)

linearRegAlloc' dflags initFreeRegs first_id block_live sccs
 = do   us      <- getUs
        let (_, stack, stats, blocks) =
                runR dflags emptyBlockMap initFreeRegs emptyRegMap (emptyStackMap dflags) us
                    $ linearRA_SCCs first_id block_live [] sccs
        return  (blocks, stats, getStackUse stack)


linearRA_SCCs :: (FR freeRegs, Instruction instr, Outputable instr)
              => BlockId
              -> BlockMap RegSet
              -> [NatBasicBlock instr]
              -> [SCC (LiveBasicBlock instr)]
              -> RegM freeRegs [NatBasicBlock instr]

linearRA_SCCs _ _ blocksAcc []
        = return $ reverse blocksAcc

linearRA_SCCs first_id block_live blocksAcc (AcyclicSCC block : sccs)
 = do   blocks' <- processBlock block_live block
        linearRA_SCCs first_id block_live
                ((reverse blocks') ++ blocksAcc)
                sccs

linearRA_SCCs first_id block_live blocksAcc (CyclicSCC blocks : sccs)
 = do
        blockss' <- process first_id block_live blocks [] (return []) False
        linearRA_SCCs first_id block_live
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

process :: (FR freeRegs, Instruction instr, Outputable instr)
        => BlockId
        -> BlockMap RegSet
        -> [GenBasicBlock (LiveInstr instr)]
        -> [GenBasicBlock (LiveInstr instr)]
        -> [[NatBasicBlock instr]]
        -> Bool
        -> RegM freeRegs [[NatBasicBlock instr]]

process _ _ [] []         accum _
        = return $ reverse accum

process first_id block_live [] next_round accum madeProgress
        | not madeProgress

          {- BUGS: There are so many unreachable blocks in the code the warnings are overwhelming.
             pprTrace "RegAlloc.Linear.Main.process: no progress made, bailing out."
                (  text "Unreachable blocks:"
                $$ vcat (map ppr next_round)) -}
        = return $ reverse accum

        | otherwise
        = process first_id block_live
                  next_round [] accum False

process first_id block_live (b@(BasicBlock id _) : blocks)
        next_round accum madeProgress
 = do
        block_assig <- getBlockAssigR

        if isJust (mapLookup id block_assig)
             || id == first_id
         then do
                b'  <- processBlock block_live b
                process first_id block_live blocks
                        next_round (b' : accum) True

         else   process first_id block_live blocks
                        (b : next_round) accum madeProgress


-- | Do register allocation on this basic block
--
processBlock
        :: (FR freeRegs, Outputable instr, Instruction instr)
        => BlockMap RegSet              -- ^ live regs on entry to each basic block
        -> LiveBasicBlock instr         -- ^ block to do register allocation on
        -> RegM freeRegs [NatBasicBlock instr]   -- ^ block with registers allocated

processBlock block_live (BasicBlock id instrs)
 = do   initBlock id block_live
        (instrs', fixups)
                <- linearRA block_live [] [] id instrs
        return  $ BasicBlock id instrs' : fixups


-- | Load the freeregs and current reg assignment into the RegM state
--      for the basic block with this BlockId.
initBlock :: FR freeRegs
          => BlockId -> BlockMap RegSet -> RegM freeRegs ()
initBlock id block_live
 = do   dflags <- getDynFlags
        let platform = targetPlatform dflags
        block_assig     <- getBlockAssigR
        case mapLookup id block_assig of
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
                            setFreeRegsR $ foldr (frAllocateReg platform) (frInitFreeRegs platform) [ r | RegReal r <- uniqSetToList live ]
                        setAssigR       emptyRegMap

                -- load info about register assignments leading into this block.
                Just (freeregs, assig)
                 -> do  setFreeRegsR    freeregs
                        setAssigR       assig


-- | Do allocation for a sequence of instructions.
linearRA
        :: (FR freeRegs, Outputable instr, Instruction instr)
        => BlockMap RegSet                      -- ^ map of what vregs are live on entry to each block.
        -> [instr]                              -- ^ accumulator for instructions already processed.
        -> [NatBasicBlock instr]                -- ^ accumulator for blocks of fixup code.
        -> BlockId                              -- ^ id of the current block, for debugging.
        -> [LiveInstr instr]                    -- ^ liveness annotated instructions in this block.

        -> RegM freeRegs
                ( [instr]                       --   instructions after register allocation
                , [NatBasicBlock instr])        --   fresh blocks of fixup code.


linearRA _          accInstr accFixup _ []
        = return
                ( reverse accInstr              -- instrs need to be returned in the correct order.
                , accFixup)                     -- it doesn't matter what order the fixup blocks are returned in.


linearRA block_live accInstr accFixups id (instr:instrs)
 = do
        (accInstr', new_fixups) <- raInsn block_live accInstr id instr

        linearRA block_live accInstr' (new_fixups ++ accFixups) id instrs


-- | Do allocation for a single instruction.
raInsn
        :: (FR freeRegs, Outputable instr, Instruction instr)
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

raInsn _     new_instrs _ (LiveInstr ii Nothing)
        | isMetaInstr ii
        = return (new_instrs, [])


raInsn block_live new_instrs id (LiveInstr (Instr instr) (Just live))
 = do
    assig    <- getAssigR

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
                        (uniqSetToList $ liveDieRead live)
                        (uniqSetToList $ liveDieWrite live)

raInsn _ _ _ instr
        = pprPanic "raInsn" (text "no match for:" <> ppr instr)

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


genRaInsn :: (FR freeRegs, Instruction instr, Outputable instr)
          => BlockMap RegSet
          -> [instr]
          -> BlockId
          -> instr
          -> [Reg]
          -> [Reg]
          -> RegM freeRegs ([instr], [NatBasicBlock instr])

genRaInsn block_live new_instrs block_id instr r_dying w_dying = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  case regUsageOfInstr platform instr of { RU read written ->
    do
    let real_written    = [ rr  | (RegReal     rr) <- written ]
    let virt_written    = [ vr  | (RegVirtual  vr) <- written ]

    -- we don't need to do anything with real registers that are
    -- only read by this instr.  (the list is typically ~2 elements,
    -- so using nub isn't a problem).
    let virt_read       = nub [ vr      | (RegVirtual vr) <- read ]

    -- debugging
{-    freeregs <- getFreeRegsR
    assig    <- getAssigR
    pprDebugAndThen (defaultDynFlags Settings{ sTargetPlatform=platform }) trace "genRaInsn"
        (ppr instr
                $$ text "r_dying      = " <+> ppr r_dying
                $$ text "w_dying      = " <+> ppr w_dying
                $$ text "virt_read    = " <+> ppr virt_read
                $$ text "virt_written = " <+> ppr virt_written
                $$ text "freeregs     = " <+> text (show freeregs)
                $$ text "assig        = " <+> ppr assig)
        $ do
-}

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
        patch_map
                = listToUFM
                        [ (t, RegReal r)
                                | (t, r) <- zip virt_read    r_allocd
                                         ++ zip virt_written w_allocd ]

        patched_instr
                = patchRegsOfInstr adjusted_instr patchLookup

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

    let code = squashed_instr ++ w_spills ++ reverse r_spills
                ++ clobber_saves ++ new_instrs

--    pprTrace "patched-code" ((vcat $ map (docToSDoc . pprInstr) code)) $ do
--    pprTrace "pached-fixup" ((ppr fixup_blocks)) $ do

    return (code, fixup_blocks)

  }

-- -----------------------------------------------------------------------------
-- releaseRegs

releaseRegs :: FR freeRegs => [Reg] -> RegM freeRegs ()
releaseRegs regs = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  assig <- getAssigR
  free <- getFreeRegsR
  let loop _     free _ | free `seq` False = undefined
      loop assig free [] = do setAssigR assig; setFreeRegsR free; return ()
      loop assig free (RegReal rr : rs) = loop assig (frReleaseReg platform rr free) rs
      loop assig free (r:rs) =
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
        :: (Outputable instr, Instruction instr, FR freeRegs)
        => [RealReg]            -- real registers clobbered by this instruction
        -> [Reg]                -- registers which are no longer live after this insn
        -> RegM freeRegs [instr]         -- return: instructions to spill any temps that will
                                -- be clobbered.

saveClobberedTemps [] _
        = return []

saveClobberedTemps clobbered dying
 = do
        assig   <- getAssigR
        let to_spill
                = [ (temp,reg)
                        | (temp, InReg reg) <- ufmToList assig
                        , any (realRegsAlias reg) clobbered
                        , temp `notElem` map getUnique dying  ]

        (instrs,assig') <- clobber assig [] to_spill
        setAssigR assig'
        return instrs

   where
     clobber assig instrs []
            = return (instrs, assig)

     clobber assig instrs ((temp, reg) : rest)
       = do dflags <- getDynFlags
            let platform = targetPlatform dflags

            freeRegs <- getFreeRegsR
            let regclass = targetClassOfRealReg platform reg
                freeRegs_thisClass = frGetFreeRegs platform regclass freeRegs

            case filter (`notElem` clobbered) freeRegs_thisClass of

              -- (1) we have a free reg of the right class that isn't
              -- clobbered by this instruction; use it to save the
              -- clobbered value.
              (my_reg : _) -> do
                  setFreeRegsR (frAllocateReg platform my_reg freeRegs)

                  let new_assign = addToUFM assig temp (InReg my_reg)
                  let instr = mkRegRegMoveInstr platform
                                  (RegReal reg) (RegReal my_reg)

                  clobber new_assign (instr : instrs) rest

              -- (2) no free registers: spill the value
              [] -> do
                  (spill, slot)   <- spillR (RegReal reg) temp
     
                  -- record why this reg was spilled for profiling
                  recordSpill (SpillClobber temp)
     
                  let new_assign  = addToUFM assig temp (InBoth reg slot)
     
                  clobber new_assign (spill : instrs) rest



-- | Mark all these real regs as allocated,
--      and kick out their vreg assignments.
--
clobberRegs :: FR freeRegs => [RealReg] -> RegM freeRegs ()
clobberRegs []
        = return ()

clobberRegs clobbered
 = do   dflags <- getDynFlags
        let platform = targetPlatform dflags

        freeregs        <- getFreeRegsR
        setFreeRegsR $! foldr (frAllocateReg platform) freeregs clobbered

        assig           <- getAssigR
        setAssigR $! clobber assig (ufmToList assig)

   where
        -- if the temp was InReg and clobbered, then we will have
        -- saved it in saveClobberedTemps above.  So the only case
        -- we have to worry about here is InBoth.  Note that this
        -- also catches temps which were loaded up during allocation
        -- of read registers, not just those saved in saveClobberedTemps.

        clobber assig []
                = assig

        clobber assig ((temp, InBoth reg slot) : rest)
                | any (realRegsAlias reg) clobbered
                = clobber (addToUFM assig temp (InMem slot)) rest

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
        :: (FR freeRegs, Outputable instr, Instruction instr)
        => Bool                 -- True <=> reading (load up spilled regs)
        -> [VirtualReg]         -- don't push these out
        -> [instr]              -- spill insns
        -> [RealReg]            -- real registers allocated (accum.)
        -> [VirtualReg]         -- temps to allocate
        -> RegM freeRegs ( [instr] , [RealReg])

allocateRegsAndSpill _       _    spills alloc []
        = return (spills, reverse alloc)

allocateRegsAndSpill reading keep spills alloc (r:rs)
 = do   assig <- getAssigR
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
                 -> do  when (not reading) (setAssigR (addToUFM assig r (InReg my_reg)))
                        allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

                -- Not already in a register, so we need to find a free one...
                Just (InMem slot) | reading   -> doSpill (ReadMem slot)
                                  | otherwise -> doSpill WriteMem
                Nothing | reading   ->
                   pprPanic "allocateRegsAndSpill: Cannot read from uninitialized register" (ppr r)
                   -- NOTE: if the input to the NCG contains some
                   -- unreachable blocks with junk code, this panic
                   -- might be triggered.  Make sure you only feed
                   -- sensible code into the NCG.  In CmmPipeline we
                   -- call removeUnreachableBlocks at the end for this
                   -- reason.

                        | otherwise -> doSpill WriteNew


-- reading is redundant with reason, but we keep it around because it's
-- convenient and it maintains the recursive structure of the allocator. -- EZY
allocRegsAndSpill_spill :: (FR freeRegs, Instruction instr, Outputable instr)
                        => Bool
                        -> [VirtualReg]
                        -> [instr]
                        -> [RealReg]
                        -> VirtualReg
                        -> [VirtualReg]
                        -> UniqFM Loc
                        -> SpillLoc
                        -> RegM freeRegs ([instr], [RealReg])
allocRegsAndSpill_spill reading keep spills alloc r rs assig spill_loc
 = do   dflags <- getDynFlags
        let platform = targetPlatform dflags
        freeRegs                <- getFreeRegsR
        let freeRegs_thisClass  = frGetFreeRegs platform (classOfVirtualReg r) freeRegs

        case freeRegs_thisClass of

         -- case (2): we have a free register
         (my_reg : _) ->
           do   spills'   <- loadTemp r spill_loc my_reg spills

                setAssigR       (addToUFM assig r $! newLocation spill_loc my_reg)
                setFreeRegsR $  frAllocateReg platform my_reg freeRegs

                allocateRegsAndSpill reading keep spills' (my_reg : alloc) rs


          -- case (3): we need to push something out to free up a register
         [] ->
           do   let keep' = map getUnique keep

                -- the vregs we could kick out that are already in a slot
                let candidates_inBoth
                        = [ (temp, reg, mem)
                                | (temp, InBoth reg mem) <- ufmToList assig
                                , temp `notElem` keep'
                                , targetClassOfRealReg platform reg == classOfVirtualReg r ]

                -- the vregs we could kick out that are only in a reg
                --      this would require writing the reg to a new slot before using it.
                let candidates_inReg
                        = [ (temp, reg)
                                | (temp, InReg reg)     <- ufmToList assig
                                , temp `notElem` keep'
                                , targetClassOfRealReg platform reg == classOfVirtualReg r ]

                let result

                        -- we have a temporary that is in both register and mem,
                        -- just free up its register for use.
                        | (temp, my_reg, slot) : _      <- candidates_inBoth
                        = do    spills' <- loadTemp r spill_loc my_reg spills
                                let assig1  = addToUFM assig temp (InMem slot)
                                let assig2  = addToUFM assig1 r $! newLocation spill_loc my_reg

                                setAssigR assig2
                                allocateRegsAndSpill reading keep spills' (my_reg:alloc) rs

                        -- otherwise, we need to spill a temporary that currently
                        -- resides in a register.
                        | (temp_to_push_out, (my_reg :: RealReg)) : _
                                        <- candidates_inReg
                        = do
                                (spill_insn, slot) <- spillR (RegReal my_reg) temp_to_push_out
                                let spill_store  = (if reading then id else reverse)
                                                        [ -- COMMENT (fsLit "spill alloc")
                                                           spill_insn ]

                                -- record that this temp was spilled
                                recordSpill (SpillAlloc temp_to_push_out)

                                -- update the register assignment
                                let assig1  = addToUFM assig temp_to_push_out   (InMem slot)
                                let assig2  = addToUFM assig1 r                 $! newLocation spill_loc my_reg
                                setAssigR assig2

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
                                , text "assignment:       " <> text (show $ ufmToList assig)
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
        :: (Outputable instr, Instruction instr)
        => VirtualReg   -- the temp being loaded
        -> SpillLoc     -- the current location of this temp
        -> RealReg      -- the hreg to load the temp into
        -> [instr]
        -> RegM freeRegs [instr]

loadTemp vreg (ReadMem slot) hreg spills
 = do
        insn <- loadR (RegReal hreg) slot
        recordSpill (SpillLoad $ getUnique vreg)
        return  $  {- COMMENT (fsLit "spill load") : -} insn : spills

loadTemp _ _ _ spills =
   return spills

