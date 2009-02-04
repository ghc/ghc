{-# OPTIONS -fno-warn-missing-signatures #-}
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
	(a) For each real register clobbered by this instruction:
	    If a temporary resides in it,
		If the temporary is live after this instruction,
		    Move the temporary to another (non-clobbered & free) reg,
		    or spill it to memory.  Mark the temporary as residing
		    in both memory and a register if it was spilled (it might
		    need to be read by this instruction).
	    (ToDo: this is wrong for jump instructions?)

	(b) For each temporary *read* by the instruction:
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

	(c) Update the current assignment

	(d) If the intstruction is a branch:
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
	module	RegAlloc.Linear.Base,
	module	RegAlloc.Linear.Stats
  ) where

#include "HsVersions.h"


import RegAlloc.Linear.State
import RegAlloc.Linear.Base
import RegAlloc.Linear.StackMap
import RegAlloc.Linear.FreeRegs
import RegAlloc.Linear.Stats
import RegAlloc.Linear.JoinToTargets

import BlockId
import MachRegs
import MachInstrs
import RegAllocInfo
import RegLiveness
import Cmm hiding (RegSet)

import Digraph
import Unique
import UniqSet
import UniqFM
import UniqSupply
import Outputable
import FastString

import Data.Maybe
import Data.List
import Control.Monad

#include "../includes/MachRegs.h"


-- -----------------------------------------------------------------------------
-- Top level of the register allocator

-- Allocate registers
regAlloc 
	:: LiveCmmTop
	-> UniqSM (NatCmmTop, Maybe RegAllocStats)

regAlloc (CmmData sec d) 
 	= return
		( CmmData sec d
		, Nothing )
	
regAlloc (CmmProc (LiveInfo info _ _) lbl params (ListGraph []))
	= return ( CmmProc info lbl params (ListGraph [])
		 , Nothing )
	
regAlloc (CmmProc static lbl params (ListGraph comps))
	| LiveInfo info (Just first_id) block_live	<- static
	= do	
 		-- do register allocation on each component.
		(final_blocks, stats)
			<- linearRegAlloc first_id block_live 
			$ map (\b -> case b of 
					BasicBlock _ [b]	-> AcyclicSCC b
					BasicBlock _ bs		-> CyclicSCC  bs)
			$ comps

		-- make sure the block that was first in the input list
		--	stays at the front of the output
		let ((first':_), rest')
				= partition ((== first_id) . blockId) final_blocks

		return	( CmmProc info lbl params (ListGraph (first' : rest'))
			, Just stats)
	
-- bogus. to make non-exhaustive match warning go away.
regAlloc (CmmProc _ _ _ _)
	= panic "RegAllocLinear.regAlloc: no match"


-- -----------------------------------------------------------------------------
-- Linear sweep to allocate registers


-- | Do register allocation on some basic blocks.
--   But be careful to allocate a block in an SCC only if it has
--   an entry in the block map or it is the first block.
--
linearRegAlloc
	:: BlockId                      -- ^ the first block
        -> BlockMap RegSet		-- ^ live regs on entry to each basic block
	-> [SCC LiveBasicBlock]		-- ^ instructions annotated with "deaths"
	-> UniqSM ([NatBasicBlock], RegAllocStats)

linearRegAlloc first_id block_live sccs
 = do	us	<- getUs
 	let (_, _, stats, blocks) =
		runR emptyBlockMap initFreeRegs emptyRegMap emptyStackMap us
			$ linearRA_SCCs first_id block_live [] sccs

	return	(blocks, stats)

linearRA_SCCs _ _ blocksAcc []
	= return $ reverse blocksAcc

linearRA_SCCs first_id block_live blocksAcc (AcyclicSCC block : sccs) 
 = do	blocks'	<- processBlock block_live block
	linearRA_SCCs first_id block_live 
		((reverse blocks') ++ blocksAcc)
		sccs

linearRA_SCCs first_id block_live blocksAcc (CyclicSCC blocks : sccs) 
 = do	let process [] []         accum = return $ reverse accum
            process [] next_round accum = process next_round [] accum
            process (b@(BasicBlock id _) : blocks) next_round accum =
              do block_assig <- getBlockAssigR
                 if isJust (lookupBlockEnv block_assig id) || id == first_id
                  then do b'  <- processBlock block_live b
                          process blocks next_round (b' : accum)
                  else process blocks (b : next_round) accum
        blockss' <- process blocks [] (return [])
	linearRA_SCCs first_id block_live
		(reverse (concat blockss') ++ blocksAcc)
		sccs
		

-- | Do register allocation on this basic block
--
processBlock
	:: BlockMap RegSet 		-- ^ live regs on entry to each basic block
	-> LiveBasicBlock 		-- ^ block to do register allocation on
	-> RegM [NatBasicBlock]		-- ^ block with registers allocated

processBlock block_live (BasicBlock id instrs)
 = do 	initBlock id
 	(instrs', fixups)
		<- linearRA block_live [] [] id instrs

	return	$ BasicBlock id instrs' : fixups


-- | Load the freeregs and current reg assignment into the RegM state
--	for the basic block with this BlockId.
initBlock :: BlockId -> RegM ()
initBlock id
 = do	block_assig	<- getBlockAssigR
  	case lookupBlockEnv block_assig id of
	        -- no prior info about this block: assume everything is
	        -- free and the assignment is empty.
	 	Nothing
		 -> do	setFreeRegsR	initFreeRegs
		 	setAssigR	emptyRegMap

		-- load info about register assignments leading into this block.
		Just (freeregs, assig)
		 -> do	setFreeRegsR 	freeregs
			setAssigR	assig


-- | Do allocation for a sequence of instructions.
linearRA
	:: BlockMap RegSet		-- ^ map of what vregs are live on entry to each block.
	-> [Instr] 			-- ^ accumulator for instructions already processed.
	-> [NatBasicBlock] 		-- ^ accumulator for blocks of fixup code.
	-> BlockId			-- ^ id of the current block, for debugging.
	-> [LiveInstr]			-- ^ liveness annotated instructions in this block.

	-> RegM ( [Instr]		--   instructions after register allocation
		, [NatBasicBlock])	--   fresh blocks of fixup code.


linearRA _          accInstr accFixup _ []
	= return 
		( reverse accInstr	-- instrs need to be returned in the correct order.
		, accFixup)		-- it doesn't matter what order the fixup blocks are returned in.


linearRA block_live accInstr accFixups id (instr:instrs)
 = do
 	(accInstr', new_fixups) 
		<- raInsn block_live accInstr id instr

	linearRA block_live accInstr' (new_fixups ++ accFixups) id instrs


-- | Do allocation for a single instruction.
raInsn  
	:: BlockMap RegSet		-- ^ map of what vregs are love on entry to each block.
	-> [Instr]			-- ^ accumulator for instructions already processed.
	-> BlockId			-- ^ the id of the current block, for debugging
	-> LiveInstr 			-- ^ the instr to have its regs allocated, with liveness info.
	-> RegM 
		( [Instr]		-- new instructions
		, [NatBasicBlock])	-- extra fixup blocks

raInsn _     new_instrs _ (Instr (COMMENT _) Nothing)
 = return (new_instrs, [])

raInsn _     new_instrs _ (Instr (DELTA n) Nothing)  
 = do
    setDeltaR n
    return (new_instrs, [])

raInsn block_live new_instrs id (Instr instr (Just live))
 = do
    assig    <- getAssigR

    -- If we have a reg->reg move between virtual registers, where the
    -- src register is not live after this instruction, and the dst
    -- register does not already have an assignment,
    -- and the source register is assigned to a register, not to a spill slot,
    -- then we can eliminate the instruction.
    -- (we can't eliminitate it if the source register is on the stack, because
    --  we do not want to use one spill slot for different virtual registers)
    case isRegRegMove instr of
	Just (src,dst)	| src `elementOfUniqSet` (liveDieRead live), 
		  	  isVirtualReg dst,
		 	  not (dst `elemUFM` assig),
		 	  Just (InReg _) <- (lookupUFM assig src) -> do
	   case src of
	      RealReg i -> setAssigR (addToUFM assig dst (InReg i))
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


raInsn _ _ id instr
	= pprPanic "raInsn" (text "no match for:" <> ppr instr)




genRaInsn block_live new_instrs block_id instr r_dying w_dying =
    case regUsage instr              of { RU read written ->
    case partition isRealReg written of { (real_written1,virt_written) ->
    do
    let 
	real_written = [ r | RealReg r <- real_written1 ]

	-- we don't need to do anything with real registers that are
	-- only read by this instr.  (the list is typically ~2 elements,
	-- so using nub isn't a problem).
	virt_read = nub (filter isVirtualReg read)
    -- in

    -- (a) save any temporaries which will be clobbered by this instruction
    clobber_saves <- saveClobberedTemps real_written r_dying


{-  freeregs <- getFreeRegsR
    assig <- getAssigR
    pprTrace "raInsn" 
    	(docToSDoc (pprInstr instr) $$ ppr r_dying <+> ppr w_dying $$ ppr virt_read <+> ppr virt_written 
		$$ text (show freeregs) $$ ppr assig) 
		$ do
-}

    -- (b), (c) allocate real regs for all regs read by this instruction.
    (r_spills, r_allocd) <- 
	allocateRegsAndSpill True{-reading-} virt_read [] [] virt_read

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
	patch_map = listToUFM   [ (t,RealReg r) | 
				  (t,r) <- zip virt_read r_allocd
					  ++ zip virt_written w_allocd ]

	patched_instr = patchRegs adjusted_instr patchLookup
	patchLookup x = case lookupUFM patch_map x of
				Nothing -> x
				Just y  -> y
    -- in

    -- pprTrace "patched" (docToSDoc (pprInstr patched_instr)) $ do

    -- (j) free up stack slots for dead spilled regs
    -- TODO (can't be bothered right now)

    -- erase reg->reg moves where the source and destination are the same.
    --	If the src temp didn't die in this instr but happened to be allocated
    --	to the same real reg as the destination, then we can erase the move anyway.
	squashed_instr	= case isRegRegMove patched_instr of
				Just (src, dst)
				 | src == dst	-> []
				_		-> [patched_instr]

    return (squashed_instr ++ w_spills ++ reverse r_spills
		 ++ clobber_saves ++ new_instrs,
	    fixup_blocks)
  }}

-- -----------------------------------------------------------------------------
-- releaseRegs

releaseRegs regs = do
  assig <- getAssigR
  free <- getFreeRegsR
  loop assig free regs 
 where
  loop _     free _ | free `seq` False = undefined
  loop assig free [] = do setAssigR assig; setFreeRegsR free; return ()
  loop assig free (RealReg r : rs) = loop assig (releaseReg r free) rs
  loop assig free (r:rs) = 
     case lookupUFM assig r of
	Just (InBoth real _) -> loop (delFromUFM assig r) (releaseReg real free) rs
	Just (InReg real) -> loop (delFromUFM assig r) (releaseReg real free) rs
	_other            -> loop (delFromUFM assig r) free rs

-- -----------------------------------------------------------------------------
-- Clobber real registers

{-
For each temp in a register that is going to be clobbered:
  - if the temp dies after this instruction, do nothing
  - otherwise, put it somewhere safe (another reg if possible,
    otherwise spill and record InBoth in the assignment).

for allocateRegs on the temps *read*,
  - clobbered regs are allocatable.

for allocateRegs on the temps *written*, 
  - clobbered regs are not allocatable.
-}

saveClobberedTemps
   :: [RegNo]		   -- real registers clobbered by this instruction
   -> [Reg]		   -- registers which are no longer live after this insn
   -> RegM [Instr] 	   -- return: instructions to spill any temps that will
		 	   -- be clobbered.

saveClobberedTemps [] _ = return [] -- common case
saveClobberedTemps clobbered dying =  do
  assig <- getAssigR
  let
	to_spill  = [ (temp,reg) | (temp, InReg reg) <- ufmToList assig,
				   reg `elem` clobbered,
				   temp `notElem` map getUnique dying  ]
  -- in
  (instrs,assig') <- clobber assig [] to_spill
  setAssigR assig'
  return instrs
 where
  clobber assig instrs [] = return (instrs,assig)
  clobber assig instrs ((temp,reg):rest)
    = do
	--ToDo: copy it to another register if possible
	(spill,slot) <- spillR (RealReg reg) temp
	recordSpill (SpillClobber temp)

	let new_assign	= addToUFM assig temp (InBoth reg slot)
	clobber new_assign (spill : COMMENT (fsLit "spill clobber") : instrs) rest

clobberRegs :: [RegNo] -> RegM ()
clobberRegs [] = return () -- common case
clobberRegs clobbered = do
  freeregs <- getFreeRegsR
--  setFreeRegsR $! foldr grabReg freeregs clobbered
  setFreeRegsR $! foldr allocateReg freeregs clobbered

  assig <- getAssigR
  setAssigR $! clobber assig (ufmToList assig)
 where
    -- if the temp was InReg and clobbered, then we will have
    -- saved it in saveClobberedTemps above.  So the only case
    -- we have to worry about here is InBoth.  Note that this
    -- also catches temps which were loaded up during allocation
    -- of read registers, not just those saved in saveClobberedTemps.
  clobber assig [] = assig
  clobber assig ((temp, InBoth reg slot) : rest)
	| reg `elem` clobbered
	= clobber (addToUFM assig temp (InMem slot)) rest
  clobber assig (_:rest)
	= clobber assig rest 

-- -----------------------------------------------------------------------------
-- allocateRegsAndSpill

-- This function does several things:
--   For each temporary referred to by this instruction,
--   we allocate a real register (spilling another temporary if necessary).
--   We load the temporary up from memory if necessary.
--   We also update the register assignment in the process, and
--   the list of free registers and free stack slots.

allocateRegsAndSpill
	:: Bool			-- True <=> reading (load up spilled regs)
	-> [Reg]		-- don't push these out
	-> [Instr]		-- spill insns
	-> [RegNo]		-- real registers allocated (accum.)
	-> [Reg]		-- temps to allocate
	-> RegM ([Instr], [RegNo])

allocateRegsAndSpill _       _    spills alloc []
  = return (spills,reverse alloc)

allocateRegsAndSpill reading keep spills alloc (r:rs) = do
  assig <- getAssigR
  case lookupUFM assig r of
  -- case (1a): already in a register
     Just (InReg my_reg) ->
	allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

  -- case (1b): already in a register (and memory)
  -- NB1. if we're writing this register, update its assignemnt to be
  -- InReg, because the memory value is no longer valid.
  -- NB2. This is why we must process written registers here, even if they
  -- are also read by the same instruction.
     Just (InBoth my_reg _) -> do
	when (not reading) (setAssigR (addToUFM assig r (InReg my_reg)))
   	allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

  -- Not already in a register, so we need to find a free one...
     loc -> do
	freeregs <- getFreeRegsR

        case getFreeRegs (regClass r) freeregs of

      	-- case (2): we have a free register
      	  my_reg:_ -> {- pprTrace "alloc" (ppr r <+> ppr my_reg <+> ppr freeClass) $ -}
	    do
    	    spills'   <- loadTemp reading r loc my_reg spills
	    let new_loc 
		 | Just (InMem slot) <- loc, reading = InBoth my_reg slot
		 | otherwise		             = InReg my_reg
	    setAssigR (addToUFM assig r $! new_loc)
	    setFreeRegsR $ allocateReg my_reg freeregs
    	    allocateRegsAndSpill reading keep spills' (my_reg:alloc) rs

        -- case (3): we need to push something out to free up a register
          [] -> do
	    let
	      keep' = map getUnique keep
	      candidates1 = [ (temp,reg,mem)
			    | (temp, InBoth reg mem) <- ufmToList assig,
			      temp `notElem` keep', regClass (RealReg reg) == regClass r ]
	      candidates2 = [ (temp,reg)
			    | (temp, InReg reg) <- ufmToList assig,
			      temp `notElem` keep', regClass (RealReg reg) == regClass r  ]
	    -- in
	    ASSERT2(not (null candidates1 && null candidates2), 
		    text (show freeregs) <+> ppr r <+> ppr assig) do

	    case candidates1 of

	     -- we have a temporary that is in both register and mem,
	     -- just free up its register for use.
	     -- 
	     (temp,my_reg,slot):_ -> do
    	        spills' <- loadTemp reading r loc my_reg spills
    	        let	
	          assig1  = addToUFM assig temp (InMem slot)
	          assig2  = addToUFM assig1 r (InReg my_reg)
 	        -- in
		setAssigR assig2
		allocateRegsAndSpill reading keep spills' (my_reg:alloc) rs

	     -- otherwise, we need to spill a temporary that currently
	     -- resides in a register.


	     [] -> do

		-- TODO: plenty of room for optimisation in choosing which temp
	        -- to spill.  We just pick the first one that isn't used in 
	        -- the current instruction for now.

	        let (temp_to_push_out, my_reg) 
			= case candidates2 of
				[]	-> panic $ "RegAllocLinear.allocRegsAndSpill: no spill candidates"
					++ "assignment: " ++ show (ufmToList assig) ++ "\n"
				(x:_)	-> x
				
    	        (spill_insn, slot) <- spillR (RealReg my_reg) temp_to_push_out
		let spill_store	 = (if reading then id else reverse)
					[ COMMENT (fsLit "spill alloc") 
					, spill_insn ]

		-- record that this temp was spilled
		recordSpill (SpillAlloc temp_to_push_out)

		-- update the register assignment
    	        let assig1  = addToUFM assig temp_to_push_out	(InMem slot)
	        let assig2  = addToUFM assig1 r			(InReg my_reg)
	        setAssigR assig2

		-- if need be, load up a spilled temp into the reg we've just freed up.
    	        spills' <- loadTemp reading r loc my_reg spills

 	        allocateRegsAndSpill reading keep
			(spill_store ++ spills')
		 	(my_reg:alloc) rs


-- | Load up a spilled temporary if we need to.
loadTemp
	:: Bool
	-> Reg 		-- the temp being loaded
	-> Maybe Loc	-- the current location of this temp
	-> RegNo	-- the hreg to load the temp into
	-> [Instr]
	-> RegM [Instr]

loadTemp True vreg (Just (InMem slot)) hreg spills
 = do
 	insn <- loadR (RealReg hreg) slot
	recordSpill (SpillLoad $ getUnique vreg)
	return	$  COMMENT (fsLit "spill load") : insn : spills

loadTemp _ _ _ _ spills =
   return spills

