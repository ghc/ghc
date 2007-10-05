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

module RegAllocLinear (
  	regAlloc,
	RegAllocStats, pprStats
  ) where

#include "HsVersions.h"

import MachRegs
import MachInstrs
import RegAllocInfo
import RegLiveness
import Cmm hiding (RegSet)

import Digraph
import Unique		( Uniquable(getUnique), Unique )
import UniqSet
import UniqFM
import UniqSupply
import Outputable
import State

import Data.Maybe
import Data.List
import Control.Monad
import Data.Word
import Data.Bits


-- -----------------------------------------------------------------------------
-- The free register set

-- This needs to be *efficient*

{- Here's an inefficient 'executable specification' of the FreeRegs data type:
type FreeRegs = [RegNo]

noFreeRegs = 0
releaseReg n f = if n `elem` f then f else (n : f)
initFreeRegs = allocatableRegs
getFreeRegs cls f = filter ( (==cls) . regClass . RealReg ) f
allocateReg f r = filter (/= r) f
-}

#if defined(powerpc_TARGET_ARCH)

-- The PowerPC has 32 integer and 32 floating point registers.
-- This is 32bit PowerPC, so Word64 is inefficient - two Word32s are much
-- better.
-- Note that when getFreeRegs scans for free registers, it starts at register
-- 31 and counts down. This is a hack for the PowerPC - the higher-numbered
-- registers are callee-saves, while the lower regs are caller-saves, so it
-- makes sense to start at the high end.
-- Apart from that, the code does nothing PowerPC-specific, so feel free to
-- add your favourite platform to the #if (if you have 64 registers but only
-- 32-bit words).

data FreeRegs = FreeRegs !Word32 !Word32
	      deriving( Show )	-- The Show is used in an ASSERT

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0

releaseReg :: RegNo -> FreeRegs -> FreeRegs
releaseReg r (FreeRegs g f)
    | r > 31    = FreeRegs g (f .|. (1 `shiftL` (fromIntegral r - 32)))
    | otherwise = FreeRegs (g .|. (1 `shiftL` fromIntegral r)) f
    
initFreeRegs :: FreeRegs
initFreeRegs = foldr releaseReg noFreeRegs allocatableRegs

getFreeRegs :: RegClass -> FreeRegs -> [RegNo]	-- lazilly
getFreeRegs cls (FreeRegs g f)
    | RcDouble <- cls = go f (0x80000000) 63
    | RcInteger <- cls = go g (0x80000000) 31
    | otherwise = pprPanic "RegAllocLinear.getFreeRegs: Bad cls" (ppr cls)
    where
        go _ 0 _ = []
        go x m i | x .&. m /= 0 = i : (go x (m `shiftR` 1) $! i-1)
                 | otherwise    = go x (m `shiftR` 1) $! i-1

allocateReg :: RegNo -> FreeRegs -> FreeRegs
allocateReg r (FreeRegs g f) 
    | r > 31    = FreeRegs g (f .&. complement (1 `shiftL` (fromIntegral r - 32)))
    | otherwise = FreeRegs (g .&. complement (1 `shiftL` fromIntegral r)) f

#else

-- If we have less than 32 registers, or if we have efficient 64-bit words,
-- we will just use a single bitfield.

#if defined(alpha_TARGET_ARCH)
type FreeRegs = Word64
#else
type FreeRegs = Word32
#endif

noFreeRegs :: FreeRegs
noFreeRegs = 0

releaseReg :: RegNo -> FreeRegs -> FreeRegs
releaseReg n f = f .|. (1 `shiftL` n)

initFreeRegs :: FreeRegs
initFreeRegs = foldr releaseReg noFreeRegs allocatableRegs

getFreeRegs :: RegClass -> FreeRegs -> [RegNo]	-- lazilly
getFreeRegs cls f = go f 0
  where go 0 _ = []
        go n m 
	  | n .&. 1 /= 0 && regClass (RealReg m) == cls
	  = m : (go (n `shiftR` 1) $! (m+1))
	  | otherwise
	  = go (n `shiftR` 1) $! (m+1)
	-- ToDo: there's no point looking through all the integer registers
	-- in order to find a floating-point one.

allocateReg :: RegNo -> FreeRegs -> FreeRegs
allocateReg r f = f .&. complement (1 `shiftL` fromIntegral r)

#endif

-- -----------------------------------------------------------------------------
-- The assignment of virtual registers to stack slots

-- We have lots of stack slots. Memory-to-memory moves are a pain on most
-- architectures. Therefore, we avoid having to generate memory-to-memory moves
-- by simply giving every virtual register its own stack slot.

-- The StackMap stack map keeps track of virtual register - stack slot
-- associations and of which stack slots are still free. Once it has been
-- associated, a stack slot is never "freed" or removed from the StackMap again,
-- it remains associated until we are done with the current CmmProc.

type StackSlot = Int
data StackMap = StackMap [StackSlot] (UniqFM StackSlot)

emptyStackMap :: StackMap
emptyStackMap = StackMap [0..maxSpillSlots] emptyUFM

getStackSlotFor :: StackMap -> Unique -> (StackMap,Int)
getStackSlotFor (StackMap [] _) _
	= panic "RegAllocLinear.getStackSlotFor: out of stack slots"

getStackSlotFor fs@(StackMap (freeSlot:stack') reserved) reg =
    case lookupUFM reserved reg of
    	Just slot -> (fs,slot)
    	Nothing -> (StackMap stack' (addToUFM reserved reg freeSlot), freeSlot)

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
	= return
		( CmmProc info lbl params (ListGraph [])
		, Nothing )
	
regAlloc (CmmProc static lbl params (ListGraph comps))
	| LiveInfo info (Just first_id) block_live	<- static
	= do	
 		-- do register allocation on each component.
		(final_blocks, stats)
			<- linearRegAlloc block_live 
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

data Loc = InReg   {-# UNPACK #-} !RegNo
	 | InMem   {-# UNPACK #-} !Int		-- stack slot
	 | InBoth  {-# UNPACK #-} !RegNo
		   {-# UNPACK #-} !Int		-- stack slot
  deriving (Eq, Show, Ord)

{- 
A temporary can be marked as living in both a register and memory
(InBoth), for example if it was recently loaded from a spill location.
This makes it cheap to spill (no save instruction required), but we
have to be careful to turn this into InReg if the value in the
register is changed.

This is also useful when a temporary is about to be clobbered.  We
save it in a spill location, but mark it as InBoth because the current
instruction might still want to read it.
-}

instance Outputable Loc where
  ppr l = text (show l)


-- | Do register allocation on some basic blocks.
--
linearRegAlloc
	:: BlockMap RegSet		-- ^ live regs on entry to each basic block
	-> [SCC LiveBasicBlock]		-- ^ instructions annotated with "deaths"
	-> UniqSM ([NatBasicBlock], RegAllocStats)

linearRegAlloc block_live sccs
 = do	us	<- getUs
 	let (_, _, stats, blocks) =
		runR emptyBlockMap initFreeRegs emptyRegMap emptyStackMap us
			$ linearRA_SCCs block_live [] sccs

	return	(blocks, stats)

linearRA_SCCs _ blocksAcc []
	= return $ reverse blocksAcc

linearRA_SCCs block_live blocksAcc (AcyclicSCC block : sccs) 
 = do	blocks'	<- processBlock block_live block
	linearRA_SCCs block_live 
		((reverse blocks') ++ blocksAcc)
		sccs

linearRA_SCCs block_live blocksAcc (CyclicSCC blocks : sccs) 
 = do	blockss' <- mapM (processBlock block_live) blocks
	linearRA_SCCs block_live
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
		<- linearRA block_live [] [] instrs

	return	$ BasicBlock id instrs' : fixups


-- | Load the freeregs and current reg assignment into the RegM state
--	for the basic block with this BlockId.
initBlock :: BlockId -> RegM ()
initBlock id
 = do	block_assig	<- getBlockAssigR
  	case lookupUFM block_assig id of
	        -- no prior info about this block: assume everything is
	        -- free and the assignment is empty.
	 	Nothing
		 -> do	setFreeRegsR	initFreeRegs
		 	setAssigR	emptyRegMap

		-- load info about register assignments leading into this block.
		Just (freeregs, assig)
		 -> do	setFreeRegsR 	freeregs
			setAssigR	assig


linearRA
	:: BlockMap RegSet
	-> [Instr] -> [NatBasicBlock] -> [LiveInstr]
	-> RegM ([Instr], [NatBasicBlock])

linearRA _          instr_acc fixups []
	= return (reverse instr_acc, fixups)

linearRA block_live instr_acc fixups (instr:instrs)
 = do	(instr_acc', new_fixups) <- raInsn block_live instr_acc instr
	linearRA block_live instr_acc' (new_fixups++fixups) instrs

-- -----------------------------------------------------------------------------
-- Register allocation for a single instruction

type BlockAssignment = BlockMap (FreeRegs, RegMap Loc)

raInsn  :: BlockMap RegSet		-- Live temporaries at each basic block
	-> [Instr]			-- new instructions (accum.)
	-> LiveInstr 			-- the instruction (with "deaths")
	-> RegM (
	     [Instr],			-- new instructions
	     [NatBasicBlock]		-- extra fixup blocks
	   )

raInsn _     new_instrs (Instr (COMMENT _) Nothing)
 = return (new_instrs, [])

raInsn _     new_instrs (Instr (DELTA n) Nothing)  
 = do
    setDeltaR n
    return (new_instrs, [])

raInsn block_live new_instrs (Instr instr (Just live))
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

	   -- we have elimianted this instruction
	   {-
	   freeregs <- getFreeRegsR
    	   assig <- getAssigR
	   pprTrace "raInsn" (text "ELIMINATED: " <> docToSDoc (pprInstr instr) $$ ppr r_dying <+> ppr w_dying $$ text (show freeregs) $$ ppr assig) $ do
	   -}
	   return (new_instrs, [])

	_ -> genRaInsn block_live new_instrs instr 
			(uniqSetToList $ liveDieRead live) 
			(uniqSetToList $ liveDieWrite live)


raInsn _ _ li
	= pprPanic "raInsn" (text "no match for:" <> ppr li)


genRaInsn block_live new_instrs instr r_dying w_dying =
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

    {-
    freeregs <- getFreeRegsR
    assig <- getAssigR
    pprTrace "raInsn" (docToSDoc (pprInstr instr) $$ ppr r_dying <+> ppr w_dying $$ ppr virt_read <+> ppr virt_written $$ text (show freeregs) $$ ppr assig) $ do
    -}

    -- (b), (c) allocate real regs for all regs read by this instruction.
    (r_spills, r_allocd) <- 
	allocateRegsAndSpill True{-reading-} virt_read [] [] virt_read

    -- (d) Update block map for new destinations
    -- NB. do this before removing dead regs from the assignment, because
    -- these dead regs might in fact be live in the jump targets (they're
    -- only dead in the code that follows in the current basic block).
    (fixup_blocks, adjusted_instr)
	<- joinToTargets block_live [] instr (jumpDests instr [])

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
	clobber new_assign (spill : COMMENT FSLIT("spill clobber") : instrs) rest

clobberRegs :: [RegNo] -> RegM ()
clobberRegs [] = return () -- common case
clobberRegs clobbered = do
  freeregs <- getFreeRegsR
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
      	  my_reg:_ -> do
    	    spills'   <- loadTemp reading r loc my_reg spills
	    let new_loc 
		 | Just (InMem slot) <- loc, reading = InBoth my_reg slot
		 | otherwise		             = InReg my_reg
	    setAssigR (addToUFM assig r $! new_loc)
	    setFreeRegsR (allocateReg my_reg freeregs)
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

	        let (temp_to_push_out, my_reg) = myHead "regalloc" candidates2

    	        (spill_insn, slot) <- spillR (RealReg my_reg) temp_to_push_out
		let spill_store	 = (if reading then id else reverse)
					[ COMMENT FSLIT("spill alloc") 
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
	return	$  COMMENT FSLIT("spill load") : insn : spills

loadTemp _ _ _ _ spills =
   return spills


myHead s [] = panic s
myHead _ (x:_) = x

-- -----------------------------------------------------------------------------
-- Joining a jump instruction to its targets

-- The first time we encounter a jump to a particular basic block, we
-- record the assignment of temporaries.  The next time we encounter a
-- jump to the same block, we compare our current assignment to the
-- stored one.  They might be different if spilling has occrred in one
-- branch; so some fixup code will be required to match up the
-- assignments.

joinToTargets
	:: BlockMap RegSet
	-> [NatBasicBlock]
	-> Instr
	-> [BlockId]
	-> RegM ([NatBasicBlock], Instr)

joinToTargets _          new_blocks instr []
  = return (new_blocks, instr)

joinToTargets block_live new_blocks instr (dest:dests) = do
  block_assig <- getBlockAssigR
  assig <- getAssigR
  let
	-- adjust the assignment to remove any registers which are not
	-- live on entry to the destination block.
	adjusted_assig = filterUFM_Directly still_live assig

	live_set = lookItUp "joinToTargets" block_live dest
	still_live uniq _ = uniq `elemUniqSet_Directly` live_set

	-- and free up those registers which are now free.
	to_free =
	  [ r | (reg, loc) <- ufmToList assig, 
		not (elemUniqSet_Directly reg live_set), 
		r <- regsOfLoc loc ]

	regsOfLoc (InReg r)    = [r]
	regsOfLoc (InBoth r _) = [r]
	regsOfLoc (InMem _)    = []
  -- in
  case lookupUFM block_assig dest of
	-- Nothing <=> this is the first time we jumped to this
	-- block.
	Nothing -> do
	  freeregs <- getFreeRegsR
	  let freeregs' = foldr releaseReg freeregs to_free 
	  setBlockAssigR (addToUFM block_assig dest 
				(freeregs',adjusted_assig))
	  joinToTargets block_live new_blocks instr dests

	Just (_, dest_assig)

	   -- the assignments match
	   | ufmToList dest_assig == ufmToList adjusted_assig
	   -> joinToTargets block_live new_blocks instr dests

	   -- need fixup code
	   | otherwise
	   -> do
	       delta <- getDeltaR
	       
               let graph = makeRegMovementGraph adjusted_assig dest_assig
	       let sccs  = stronglyConnCompR graph
	       fixUpInstrs <- mapM (handleComponent delta instr) sccs

	       block_id <- getUniqueR
	       let block = BasicBlock (BlockId block_id) $
	               concat fixUpInstrs ++ mkBranchInstr dest

	       let instr' = patchJump instr dest (BlockId block_id)

	       joinToTargets block_live (block : new_blocks) instr' dests


-- | Construct a graph of register/spill movements.
--
-- 	We cut some corners by
-- 	a) not handling cyclic components
-- 	b) not handling memory-to-memory moves.
--
-- 	Cyclic components seem to occur only very rarely,
-- 	and we don't need memory-to-memory moves because we
-- 	make sure that every temporary always gets its own
-- 	stack slot.

makeRegMovementGraph :: RegMap Loc -> RegMap Loc -> [(Unique, Loc, [Loc])]
makeRegMovementGraph adjusted_assig dest_assig
 = let
 	mkNodes src vreg
	 = expandNode vreg src
	 $ lookupWithDefaultUFM_Directly
           	dest_assig
                (panic "RegisterAlloc.joinToTargets")
		vreg

   in	[ node 	| (vreg, src) <- ufmToList adjusted_assig
 		, node <- mkNodes src vreg ]

-- The InBoth handling is a little tricky here.  If
-- the destination is InBoth, then we must ensure that
-- the value ends up in both locations.  An InBoth
-- destination must conflict with an InReg or InMem
-- source, so we expand an InBoth destination as
-- necessary.  An InBoth source is slightly different:
-- we only care about the register that the source value
-- is in, so that we can move it to the destinations.

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


-- | Make a move instruction between these two locations so we
--	can join together allocations for different basic blocks.
--
makeMove :: Int -> Unique -> Loc -> Loc -> RegM Instr
makeMove _     vreg (InReg src) (InReg dst)
 = do	recordSpill (SpillJoinRR vreg)
 	return	$ mkRegRegMoveInstr (RealReg src) (RealReg dst)

makeMove delta vreg (InMem src) (InReg dst)
 = do	recordSpill (SpillJoinRM vreg)
 	return	$ mkLoadInstr (RealReg dst) delta src

makeMove delta vreg (InReg src) (InMem dst)
 = do	recordSpill (SpillJoinRM vreg)
	return	$ mkSpillInstr (RealReg src) delta dst

makeMove _     vreg src dst
	= panic $ "makeMove " ++ show vreg ++ " (" ++ show src ++ ") ("
		++ show dst ++ ")"
		++ " (workaround: use -fviaC)"


-- we have eliminated any possibility of single-node cylces
-- in expandNode above.
handleComponent :: Int -> Instr -> SCC (Unique, Loc, [Loc]) -> RegM [Instr]
handleComponent delta _  (AcyclicSCC (vreg,src,dsts))
	 = mapM (makeMove delta vreg src) dsts

-- we can not have cycles that involve memory
-- locations as source nor as single destination
-- because memory locations (stack slots) are
-- allocated exclusively for a virtual register and
-- therefore can not require a fixup
handleComponent delta instr (CyclicSCC ((vreg, (InReg sreg),dsts):rest))
 = do
	spill_id <- getUniqueR
	(_, slot) 		<- spillR (RealReg sreg) spill_id
	remainingFixUps 	<- mapM (handleComponent delta instr) (stronglyConnCompR rest)
	restoreAndFixInstr 	<- getRestoreMoves dsts slot
	return ([instr] ++ concat remainingFixUps ++ restoreAndFixInstr)

	where
	getRestoreMoves [r@(InReg reg), mem@(InMem _)] slot
	 = do
		restoreToReg 	<- loadR (RealReg reg) slot
		moveInstr	<- makeMove delta vreg r mem
		return $ [COMMENT FSLIT("spill join move"), restoreToReg, moveInstr]

	getRestoreMoves [InReg reg] slot
		= loadR (RealReg reg) slot >>= return . (:[])

	getRestoreMoves [InMem _] _ 	= panic "getRestoreMoves can not handle memory only restores"
	getRestoreMoves _ _ 		= panic "getRestoreMoves unknown case"


handleComponent _ _ (CyclicSCC _)
 = panic "Register Allocator: handleComponent cyclic"



-- -----------------------------------------------------------------------------
-- The register allocator's monad.  

-- Here we keep all the state that the register allocator keeps track
-- of as it walks the instructions in a basic block.

data RA_State 
  = RA_State {
	ra_blockassig :: BlockAssignment,
		-- The current mapping from basic blocks to 
		-- the register assignments at the beginning of that block.
	ra_freeregs   :: {-#UNPACK#-}!FreeRegs,	-- free machine registers
	ra_assig      :: RegMap Loc,	-- assignment of temps to locations
	ra_delta      :: Int,		-- current stack delta
	ra_stack      :: StackMap,	-- free stack slots for spilling
	ra_us         :: UniqSupply,    -- unique supply for generating names
	                                -- for fixup blocks.

	-- Record why things were spilled, for -ddrop-asm-stats.
	-- 	Just keep a list here instead of a map of regs -> reasons.
	-- 	We don't want to slow down the allocator if we're not going to emit the stats.
	ra_spills     :: [SpillReason]
  }

newtype RegM a = RegM { unReg :: RA_State -> (# RA_State, a #) }


instance Monad RegM where
  m >>= k   =  RegM $ \s -> case unReg m s of { (# s, a #) -> unReg (k a) s }
  return a  =  RegM $ \s -> (# s, a #)

runR :: BlockAssignment -> FreeRegs -> RegMap Loc -> StackMap -> UniqSupply
  -> RegM a -> (BlockAssignment, StackMap, RegAllocStats, a)
runR block_assig freeregs assig stack us thing =
  case unReg thing (RA_State{ ra_blockassig=block_assig, ra_freeregs=freeregs,
			ra_assig=assig, ra_delta=0{-???-}, ra_stack=stack,
			ra_us = us, ra_spills = [] }) of
	(# state'@RA_State{ ra_blockassig=block_assig, ra_stack=stack' }, returned_thing #)
		-> (block_assig, stack', makeRAStats state', returned_thing)

spillR :: Reg -> Unique -> RegM (Instr, Int)
spillR reg temp = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  let (stack',slot) = getStackSlotFor stack temp
      instr  = mkSpillInstr reg delta slot
  in
  (# s{ra_stack=stack'}, (instr,slot) #)

loadR :: Reg -> Int -> RegM Instr
loadR reg slot = RegM $ \ s@RA_State{ra_delta=delta} ->
  (# s, mkLoadInstr reg delta slot #)

getFreeRegsR :: RegM FreeRegs
getFreeRegsR = RegM $ \ s@RA_State{ra_freeregs = freeregs} ->
  (# s, freeregs #)

setFreeRegsR :: FreeRegs -> RegM ()
setFreeRegsR regs = RegM $ \ s ->
  (# s{ra_freeregs = regs}, () #)

getAssigR :: RegM (RegMap Loc)
getAssigR = RegM $ \ s@RA_State{ra_assig = assig} ->
  (# s, assig #)

setAssigR :: RegMap Loc -> RegM ()
setAssigR assig = RegM $ \ s ->
  (# s{ra_assig=assig}, () #)

getBlockAssigR :: RegM BlockAssignment
getBlockAssigR = RegM $ \ s@RA_State{ra_blockassig = assig} ->
  (# s, assig #)

setBlockAssigR :: BlockAssignment -> RegM ()
setBlockAssigR assig = RegM $ \ s ->
  (# s{ra_blockassig = assig}, () #)

setDeltaR :: Int -> RegM ()
setDeltaR n = RegM $ \ s ->
  (# s{ra_delta = n}, () #)

getDeltaR :: RegM Int
getDeltaR = RegM $ \s -> (# s, ra_delta s #)

getUniqueR :: RegM Unique
getUniqueR = RegM $ \s ->
  case splitUniqSupply (ra_us s) of
    (us1, us2) -> (# s{ra_us = us2}, uniqFromSupply us1 #)

-- | Record that a spill instruction was inserted, for profiling.
recordSpill :: SpillReason -> RegM ()
recordSpill spill
 	= RegM $ \s -> (# s { ra_spills = spill : ra_spills s}, () #)

-- -----------------------------------------------------------------------------

-- | Reasons why instructions might be inserted by the spiller.
--	Used when generating stats for -ddrop-asm-stats.
--
data SpillReason
	= SpillAlloc	!Unique	-- ^ vreg was spilled to a slot so we could use its
				--	current hreg for another vreg
	| SpillClobber	!Unique	-- ^ vreg was moved because its hreg was clobbered
	| SpillLoad	!Unique	-- ^ vreg was loaded from a spill slot

	| SpillJoinRR	!Unique	-- ^ reg-reg move inserted during join to targets
	| SpillJoinRM	!Unique	-- ^ reg-mem move inserted during join to targets


-- | Used to carry interesting stats out of the register allocator.
data RegAllocStats
	= RegAllocStats
	{ ra_spillInstrs	:: UniqFM [Int] }


-- | Make register allocator stats from its final state.
makeRAStats :: RA_State -> RegAllocStats
makeRAStats state
	= RegAllocStats
	{ ra_spillInstrs	= binSpillReasons (ra_spills state) }


-- | Build a map of how many times each reg was alloced, clobbered, loaded etc.
binSpillReasons
	:: [SpillReason] -> UniqFM [Int]

binSpillReasons reasons
	= addListToUFM_C
		(zipWith (+))
		emptyUFM
		(map (\reason -> case reason of
			SpillAlloc r	-> (r, [1, 0, 0, 0, 0])
			SpillClobber r	-> (r, [0, 1, 0, 0, 0])
			SpillLoad r	-> (r, [0, 0, 1, 0, 0])
			SpillJoinRR r	-> (r, [0, 0, 0, 1, 0])
			SpillJoinRM r	-> (r, [0, 0, 0, 0, 1])) reasons)


-- | Count reg-reg moves remaining in this code.
countRegRegMovesNat :: NatCmmTop -> Int
countRegRegMovesNat cmm
	= execState (mapGenBlockTopM countBlock cmm) 0
 where
 	countBlock b@(BasicBlock _ instrs)
	 = do	mapM_ countInstr instrs
	 	return	b

	countInstr instr
		| Just _	<- isRegRegMove instr
		= do	modify (+ 1)
			return instr

		| otherwise
		=	return instr


-- | Pretty print some RegAllocStats
pprStats :: [NatCmmTop] -> [RegAllocStats] -> SDoc
pprStats code statss
 = let	-- sum up all the instrs inserted by the spiller
 	spills		= foldl' (plusUFM_C (zipWith (+)))
 				emptyUFM
			$ map ra_spillInstrs statss

	spillTotals	= foldl' (zipWith (+))
				[0, 0, 0, 0, 0]
			$ eltsUFM spills

	-- count how many reg-reg-moves remain in the code
	moves		= sum $ map countRegRegMovesNat code

	pprSpill (reg, spills)
		= parens $ (hcat $ punctuate (text ", ")  (doubleQuotes (ppr reg) : map ppr spills))

   in	(  text "-- spills-added-total"
	$$ text "--    (allocs, clobbers, loads, joinRR, joinRM, reg_reg_moves_remaining)"
	$$ (parens $ (hcat $ punctuate (text ", ") (map ppr spillTotals ++ [ppr moves])))
	$$ text ""
	$$ text "-- spills-added"
   	$$ text "--    (reg_name, allocs, clobbers, loads, joinRR, joinRM)"
	$$ (vcat $ map pprSpill
   		 $ ufmToList spills)
	$$ text "")


-- -----------------------------------------------------------------------------
-- Utils

#ifdef DEBUG
my_fromJust s p Nothing  = pprPanic ("fromJust: " ++ s) p
my_fromJust _ _ (Just x) = x
#else
my_fromJust _ _ = fromJust
#endif

lookItUp :: Uniquable b => String -> UniqFM a -> b -> a
lookItUp str fm x = my_fromJust str (ppr (getUnique x)) (lookupUFM fm x)
