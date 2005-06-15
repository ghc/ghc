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

module RegisterAlloc (
  	regAlloc
  ) where

#include "HsVersions.h"

import PprMach
import MachRegs
import MachInstrs
import RegAllocInfo
import Cmm

import Digraph
import Unique		( Uniquable(getUnique), Unique )
import UniqSet
import UniqFM
import Outputable

#ifndef DEBUG
import Maybe		( fromJust )
#endif
import List		( nub, partition )
import Monad		( when )
import DATA_WORD
import DATA_BITS

-- -----------------------------------------------------------------------------
-- Some useful types

type RegSet = UniqSet Reg

type RegMap a = UniqFM a
emptyRegMap = emptyUFM

type BlockMap a = UniqFM a
emptyBlockMap = emptyUFM

-- A basic block where the isntructions are annotated with the registers
-- which are no longer live in the *next* instruction in this sequence.
-- (NB. if the instruction is a jump, these registers might still be live
-- at the jump target(s) - you have to check the liveness at the destination
-- block to find out).
type AnnBasicBlock 
	= GenBasicBlock (Instr,
			 [Reg],		-- registers read (only) which die
			 [Reg])		-- registers written which die

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

noFreeRegs = FreeRegs 0 0
releaseReg r (FreeRegs g f)
    | r > 31    = FreeRegs g (f .|. (1 `shiftL` (fromIntegral r - 32)))
    | otherwise = FreeRegs (g .|. (1 `shiftL` fromIntegral r)) f
    
initFreeRegs :: FreeRegs
initFreeRegs = foldr releaseReg noFreeRegs allocatableRegs

getFreeRegs cls (FreeRegs g f)
    | RcDouble <- cls = go f (0x80000000) 63
    | RcInteger <- cls = go g (0x80000000) 31
    where
        go x 0 i = []
        go x m i | x .&. m /= 0 = i : (go x (m `shiftR` 1) $! i-1)
                 | otherwise    = go x (m `shiftR` 1) $! i-1

allocateReg (FreeRegs g f) r
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
  where go 0 m = []
        go n m 
	  | n .&. 1 /= 0 && regClass (RealReg m) == cls
	  = m : (go (n `shiftR` 1) $! (m+1))
	  | otherwise
	  = go (n `shiftR` 1) $! (m+1)
	-- ToDo: there's no point looking through all the integer registers
	-- in order to find a floating-point one.

allocateReg :: FreeRegs -> RegNo -> FreeRegs
allocateReg f r = f .&. complement (1 `shiftL` fromIntegral r)
#endif

-- -----------------------------------------------------------------------------
-- The free list of stack slots

-- This doesn't need to be so efficient.  It also doesn't really need to be
-- maintained as a set, so we just use an ordinary list (lazy, because it
-- contains all the possible stack slots and there are lots :-).

type StackSlot = Int
type FreeStack = [StackSlot]

completelyFreeStack :: FreeStack
completelyFreeStack = [0..maxSpillSlots]

getFreeStackSlot :: FreeStack -> (FreeStack,Int)
getFreeStackSlot (slot:stack) = (stack,slot)

freeStackSlot :: FreeStack -> Int -> FreeStack
freeStackSlot stack slot = slot:stack


-- -----------------------------------------------------------------------------
-- Top level of the register allocator

regAlloc :: NatCmmTop -> NatCmmTop
regAlloc (CmmData sec d) = CmmData sec d
regAlloc (CmmProc info lbl params [])
  = CmmProc info lbl params []  -- no blocks to run the regalloc on
regAlloc (CmmProc info lbl params blocks@(first:rest))
  = -- pprTrace "Liveness" (ppr block_live) $
    CmmProc info lbl params (first':rest')
  where
    first_id               = blockId first
    sccs	           = sccBlocks blocks
    (ann_sccs, block_live) = computeLiveness sccs
    final_blocks	   = linearRegAlloc block_live ann_sccs
    ((first':_),rest')	   = partition ((== first_id) . blockId) final_blocks


sccBlocks :: [NatBasicBlock] -> [SCC NatBasicBlock]
sccBlocks blocks = stronglyConnComp graph
  where
	getOutEdges :: [Instr] -> [BlockId]
	getOutEdges instrs = foldr jumpDests [] instrs

	graph = [ (block, getUnique id, map getUnique (getOutEdges instrs))
		| block@(BasicBlock id instrs) <- blocks ]


-- -----------------------------------------------------------------------------
-- Computing liveness

computeLiveness
   :: [SCC NatBasicBlock]
   -> ([SCC AnnBasicBlock],	-- instructions annotated with list of registers
				-- which are "dead after this instruction".
       BlockMap RegSet)		-- blocks annontated with set of live registers
				-- on entry to the block.

  -- NOTE: on entry, the SCCs are in "reverse" order: later blocks may transfer
  -- control to earlier ones only.  The SCCs returned are in the *opposite* 
  -- order, which is exactly what we want for the next pass.
	
computeLiveness sccs
  = livenessSCCs emptyBlockMap [] sccs
  where
  livenessSCCs 
	 :: BlockMap RegSet 
	 -> [SCC AnnBasicBlock]		-- accum
	 -> [SCC NatBasicBlock]
	 -> ([SCC AnnBasicBlock], BlockMap RegSet)

  livenessSCCs blockmap done [] = (done, blockmap)
  livenessSCCs blockmap done
	(AcyclicSCC (BasicBlock block_id instrs) : sccs) =
	  {- pprTrace "live instrs" (ppr (getUnique block_id) $$
				  vcat (map (\(instr,regs) -> docToSDoc (pprInstr instr) $$ ppr regs) instrs')) $ 
	  -}
	  livenessSCCs blockmap'
		(AcyclicSCC (BasicBlock block_id instrs'):done) sccs
	where (live,instrs') = liveness emptyUniqSet blockmap []
					(reverse instrs)
	      blockmap' = addToUFM blockmap block_id live
	-- TODO: cope with recursive blocks
  
  liveness :: RegSet			-- live regs
	   -> BlockMap RegSet   	-- live regs on entry to other BBs
	   -> [(Instr,[Reg],[Reg])]   	-- instructions (accum)
	   -> [Instr]			-- instructions
	   -> (RegSet, [(Instr,[Reg],[Reg])])

  liveness liveregs blockmap done []  = (liveregs, done)
  liveness liveregs blockmap done (instr:instrs) 
 	= liveness liveregs2 blockmap ((instr,r_dying,w_dying):done) instrs
	where 
	      RU read written = regUsage instr

	      -- registers that were written here are dead going backwards.
	      -- registers that were read here are live going backwards.
	      liveregs1 = (liveregs `delListFromUniqSet` written)
				    `addListToUniqSet` read

	      -- union in the live regs from all the jump destinations of this
	      -- instruction.
	      targets = jumpDests instr [] -- where we go from here
	      liveregs2 = unionManyUniqSets 
			    (liveregs1 : map (lookItUp "liveness" blockmap) 
						targets)

	      -- registers that are not live beyond this point, are recorded
	      --  as dying here.
	      r_dying  = [ reg | reg <- read, reg `notElem` written,
			         not (elementOfUniqSet reg liveregs) ]

	      w_dying = [ reg | reg <- written,
			        not (elementOfUniqSet reg liveregs) ]

-- -----------------------------------------------------------------------------
-- Linear sweep to allocate registers

data Loc = InReg   {-# UNPACK #-} !RegNo
	 | InMem   {-# UNPACK #-} !Int		-- stack slot
	 | InBoth  {-# UNPACK #-} !RegNo
		   {-# UNPACK #-} !Int		-- stack slot
  deriving (Eq, Show)

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

#ifdef DEBUG
instance Outputable Loc where
  ppr l = text (show l)
#endif

linearRegAlloc
   :: BlockMap RegSet		-- live regs on entry to each basic block
   -> [SCC AnnBasicBlock]	-- instructions annotated with "deaths"
   -> [NatBasicBlock]
linearRegAlloc block_live sccs = linearRA_SCCs emptyBlockMap sccs
  where
  linearRA_SCCs
	:: BlockAssignment
	-> [SCC AnnBasicBlock]
	-> [NatBasicBlock]
  linearRA_SCCs block_assig [] = []
  linearRA_SCCs block_assig 
	(AcyclicSCC (BasicBlock id instrs) : sccs) 
	= BasicBlock id instrs' : linearRA_SCCs block_assig' sccs
    where
	(block_assig',(instrs',fixups)) = 
	   case lookupUFM block_assig id of
		-- no prior info about this block: assume everything is
		-- free and the assignment is empty.
		Nothing -> 
		   runR block_assig initFreeRegs 
				emptyRegMap completelyFreeStack $
			linearRA [] [] instrs 
		Just (freeregs,stack,assig) -> 
		   runR block_assig freeregs assig stack $
			linearRA [] [] instrs 

  linearRA :: [Instr] -> [NatBasicBlock] -> [(Instr,[Reg],[Reg])]
	-> RegM ([Instr], [NatBasicBlock])
  linearRA instr_acc fixups [] = 
    return (reverse instr_acc, fixups)
  linearRA instr_acc fixups (instr:instrs) = do
    (instr_acc', new_fixups) <- raInsn block_live instr_acc instr
    linearRA instr_acc' (new_fixups++fixups) instrs

-- -----------------------------------------------------------------------------
-- Register allocation for a single instruction

type BlockAssignment = BlockMap (FreeRegs, FreeStack, RegMap Loc)

raInsn  :: BlockMap RegSet		-- Live temporaries at each basic block
	-> [Instr]			-- new instructions (accum.)
	-> (Instr,[Reg],[Reg])		-- the instruction (with "deaths")
	-> RegM (
	     [Instr],			-- new instructions
	     [NatBasicBlock]		-- extra fixup blocks
	   )

raInsn block_live new_instrs (instr@(DELTA n), _, _) = do
    setDeltaR n
    return (new_instrs, [])

raInsn block_live new_instrs (instr, r_dying, w_dying) = do
    assig    <- getAssigR

    -- If we have a reg->reg move between virtual registers, where the
    -- src register is not live after this instruction, and the dst
    -- register does not already have an assignment, then we can
    -- eliminate the instruction.
    case isRegRegMove instr of
	Just (src,dst)
		| src `elem` r_dying, 
		  isVirtualReg dst,
		  Just loc <- lookupUFM assig src,
	 	  not (dst `elemUFM` assig) -> do
			setAssigR (addToUFM (delFromUFM assig src) dst loc)
			return (new_instrs, [])

	other -> genRaInsn block_live new_instrs instr r_dying w_dying


genRaInsn block_live new_instrs instr r_dying w_dying = do
    let 
	RU read written = regUsage instr

	(real_written1,virt_written) = partition isRealReg written

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

    return (patched_instr : w_spills ++ reverse r_spills
		 ++ clobber_saves ++ new_instrs,
	    fixup_blocks)

-- -----------------------------------------------------------------------------
-- releaseRegs

releaseRegs regs = do
  assig <- getAssigR
  free <- getFreeRegsR
  loop assig free regs 
 where
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
      (spill,slot) <- spillR (RealReg reg)
      clobber (addToUFM assig temp (InBoth reg slot)) (spill:instrs) rest

clobberRegs :: [RegNo] -> RegM ()
clobberRegs [] = return () -- common case
clobberRegs clobbered = do
  freeregs <- getFreeRegsR
  setFreeRegsR (foldl allocateReg freeregs clobbered)
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
  clobber assig (entry:rest)
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

allocateRegsAndSpill reading keep spills alloc []
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
     Just (InBoth my_reg mem) -> do
	when (not reading) (setAssigR (addToUFM assig r (InReg my_reg)))
   	allocateRegsAndSpill reading keep spills (my_reg:alloc) rs

  -- Not already in a register, so we need to find a free one...
     loc -> do
	freeregs <- getFreeRegsR

        case getFreeRegs (regClass r) freeregs of

      	-- case (2): we have a free register
      	  my_reg:_ -> do
    	    spills'   <- do_load reading loc my_reg spills
	    let new_loc 
		 | Just (InMem slot) <- loc, reading = InBoth my_reg slot
		 | otherwise		             = InReg my_reg
	    setAssigR (addToUFM assig r $! new_loc)
	    setFreeRegsR (allocateReg freeregs my_reg)
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
    	        spills' <- do_load reading loc my_reg spills
    	        let	
	          assig1  = addToUFM assig temp (InMem slot)
	          assig2  = addToUFM assig1 r (InReg my_reg)
 	        -- in
		setAssigR assig2
		allocateRegsAndSpill reading keep spills' (my_reg:alloc) rs

	     -- otherwise, we need to spill a temporary that currently
	     -- resides in a register.
	     [] -> do
	        let
	          (temp_to_push_out, my_reg) = myHead "regalloc" candidates2
	          -- TODO: plenty of room for optimisation in choosing which temp
	          -- to spill.  We just pick the first one that isn't used in 
	          -- the current instruction for now.
	        -- in
    	        (spill_insn,slot) <- spillR (RealReg my_reg)
    	        let	
	          assig1  = addToUFM assig temp_to_push_out (InMem slot)
	          assig2  = addToUFM assig1 r (InReg my_reg)
 	        -- in
	        setAssigR assig2
    	        spills' <- do_load reading loc my_reg spills
    	        allocateRegsAndSpill reading keep (spill_insn:spills')
		 	(my_reg:alloc) rs
  where
	-- load up a spilled temporary if we need to
	do_load True (Just (InMem slot)) reg spills = do
           insn <- loadR (RealReg reg) slot
	   return (insn : spills)
	do_load _ _ _ spills = 
	   return spills

myHead s [] = panic s
myHead s (x:xs) = x

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

joinToTargets block_live new_blocks instr []
  = return (new_blocks, instr)
joinToTargets block_live new_blocks instr (dest:dests) = do
  block_assig <- getBlockAssigR
  assig <- getAssigR
  let
	-- adjust the assignment to remove any registers which are not
	-- live on entry to the destination block.
	adjusted_assig = filterUFM_Directly still_live assig
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
 	  stack <- getStackR
	  setBlockAssigR (addToUFM block_assig dest 
				(freeregs',stack,adjusted_assig))
	  joinToTargets block_live new_blocks instr dests

	Just (freeregs,stack,dest_assig)
	   | ufmToList dest_assig == ufmToList adjusted_assig
	   -> -- ok, the assignments match
	     joinToTargets block_live new_blocks instr dests
	   | otherwise
	   -> -- need fixup code
	     panic "joinToTargets: ToDo: need fixup code"
  where
	live_set = lookItUp "joinToTargets" block_live dest

-- -----------------------------------------------------------------------------
-- The register allocator's monad.  

-- Here we keep all the state that the register allocator keeps track
-- of as it walks the instructions in a basic block.

data RA_State 
  = RA_State {
	ra_blockassig :: BlockAssignment,
		-- The current mapping from basic blocks to 
		-- the register assignments at the beginning of that block.
	ra_freeregs   :: FreeRegs,	-- free machine registers
	ra_assig      :: RegMap Loc,	-- assignment of temps to locations
	ra_delta      :: Int,		-- current stack delta
	ra_stack      :: FreeStack	-- free stack slots for spilling
  }

newtype RegM a = RegM { unReg :: RA_State -> (# RA_State, a #) }

instance Monad RegM where
  m >>= k   =  RegM $ \s -> case unReg m s of { (# s, a #) -> unReg (k a) s }
  return a  =  RegM $ \s -> (# s, a #)

runR :: BlockAssignment -> FreeRegs -> RegMap Loc -> FreeStack -> RegM a ->
  (BlockAssignment, a)
runR block_assig freeregs assig stack thing =
  case unReg thing (RA_State{ ra_blockassig=block_assig, ra_freeregs=freeregs,
			ra_assig=assig, ra_delta=0{-???-}, ra_stack=stack }) of
	(# RA_State{ ra_blockassig=block_assig }, returned_thing #)
		-> (block_assig, returned_thing)

spillR :: Reg -> RegM (Instr, Int)
spillR reg = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  let (stack',slot) = getFreeStackSlot stack
      instr  = mkSpillInstr reg delta slot
  in
  (# s{ra_stack=stack'}, (instr,slot) #)

loadR :: Reg -> Int -> RegM Instr
loadR reg slot = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  (# s, mkLoadInstr reg delta slot #)

freeSlotR :: Int -> RegM ()
freeSlotR slot = RegM $ \ s@RA_State{ra_stack=stack} ->
  (# s{ra_stack=freeStackSlot stack slot}, () #)

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

getStackR :: RegM FreeStack
getStackR = RegM $ \ s@RA_State{ra_stack = stack} ->
  (# s, stack #)

setStackR :: FreeStack -> RegM ()
setStackR stack = RegM $ \ s ->
  (# s{ra_stack=stack}, () #)

getBlockAssigR :: RegM BlockAssignment
getBlockAssigR = RegM $ \ s@RA_State{ra_blockassig = assig} ->
  (# s, assig #)

setBlockAssigR :: BlockAssignment -> RegM ()
setBlockAssigR assig = RegM $ \ s ->
  (# s{ra_blockassig = assig}, () #)

setDeltaR :: Int -> RegM ()
setDeltaR n = RegM $ \ s ->
  (# s{ra_delta = n}, () #)

-- -----------------------------------------------------------------------------
-- Utils

#ifdef DEBUG
my_fromJust s p Nothing  = pprPanic ("fromJust: " ++ s) p
my_fromJust s p (Just x) = x
#else
my_fromJust _ _ = fromJust
#endif

lookItUp :: Uniquable b => String -> UniqFM a -> b -> a
lookItUp str fm x = my_fromJust str (ppr (getUnique x)) (lookupUFM fm x)
