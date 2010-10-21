-- | State monad for the linear register allocator.

-- 	Here we keep all the state that the register allocator keeps track
-- 	of as it walks the instructions in a basic block.

module RegAlloc.Linear.State (
	RA_State(..),
	RegM,
	runR,

	spillR,
	loadR,

	getFreeRegsR,
	setFreeRegsR,

	getAssigR,
	setAssigR,
	
	getBlockAssigR,
	setBlockAssigR,
	
	setDeltaR,
	getDeltaR,
	
	getUniqueR,
	
	recordSpill
)
where

import RegAlloc.Linear.Stats
import RegAlloc.Linear.StackMap
import RegAlloc.Linear.Base
import RegAlloc.Linear.FreeRegs
import RegAlloc.Liveness
import Instruction
import Reg

import Unique
import UniqSupply


-- | The RegM Monad
instance Monad RegM where
  m >>= k   =  RegM $ \s -> case unReg m s of { (# s, a #) -> unReg (k a) s }
  return a  =  RegM $ \s -> (# s, a #)


-- | Run a computation in the RegM register allocator monad.
runR 	:: BlockAssignment 
	-> FreeRegs 
	-> RegMap Loc
	-> StackMap 
	-> UniqSupply
  	-> RegM a 
	-> (BlockAssignment, StackMap, RegAllocStats, a)

runR block_assig freeregs assig stack us thing =
  case unReg thing 
  	(RA_State
		{ ra_blockassig = block_assig
		, ra_freeregs	= freeregs
		, ra_assig	= assig
		, ra_delta	= 0{-???-}
		, ra_stack	= stack
		, ra_us 	= us
		, ra_spills 	= [] }) 
   of
	(# state'@RA_State
		{ ra_blockassig = block_assig
		, ra_stack	= stack' }
		, returned_thing #)
		
	 -> 	(block_assig, stack', makeRAStats state', returned_thing)


-- | Make register allocator stats from its final state.
makeRAStats :: RA_State -> RegAllocStats
makeRAStats state
	= RegAllocStats
	{ ra_spillInstrs	= binSpillReasons (ra_spills state) }


spillR 	:: Instruction instr
	=> Reg -> Unique -> RegM (instr, Int)

spillR reg temp = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  let (stack',slot) = getStackSlotFor stack temp
      instr  = mkSpillInstr reg delta slot
  in
  (# s{ra_stack=stack'}, (instr,slot) #)


loadR 	:: Instruction instr
	=> Reg -> Int -> RegM instr

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
  case takeUniqFromSupply (ra_us s) of
    (uniq, us) -> (# s{ra_us = us}, uniq #)


-- | Record that a spill instruction was inserted, for profiling.
recordSpill :: SpillReason -> RegM ()
recordSpill spill
 	= RegM $ \s -> (# s { ra_spills = spill : ra_spills s}, () #)
