-- | State monad for the linear register allocator.

-- 	Here we keep all the state that the register allocator keeps track
-- 	of as it walks the instructions in a basic block.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

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
import RegAlloc.Liveness
import Instruction
import Reg

import Platform
import Unique
import UniqSupply


-- | The RegM Monad
instance Monad (RegM freeRegs) where
  m >>= k   =  RegM $ \s -> case unReg m s of { (# s, a #) -> unReg (k a) s }
  return a  =  RegM $ \s -> (# s, a #)


-- | Run a computation in the RegM register allocator monad.
runR 	:: BlockAssignment freeRegs
	-> freeRegs 
	-> RegMap Loc
	-> StackMap 
	-> UniqSupply
  	-> RegM freeRegs a 
	-> (BlockAssignment freeRegs, StackMap, RegAllocStats, a)

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
makeRAStats :: RA_State freeRegs -> RegAllocStats
makeRAStats state
	= RegAllocStats
	{ ra_spillInstrs	= binSpillReasons (ra_spills state) }


spillR :: Instruction instr
       => Platform -> Reg -> Unique -> RegM freeRegs (instr, Int)

spillR platform reg temp = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  let (stack',slot) = getStackSlotFor stack temp
      instr  = mkSpillInstr platform reg delta slot
  in
  (# s{ra_stack=stack'}, (instr,slot) #)


loadR :: Instruction instr
      => Platform -> Reg -> Int -> RegM freeRegs instr

loadR platform reg slot = RegM $ \ s@RA_State{ra_delta=delta} ->
  (# s, mkLoadInstr platform reg delta slot #)

getFreeRegsR :: RegM freeRegs freeRegs
getFreeRegsR = RegM $ \ s@RA_State{ra_freeregs = freeregs} ->
  (# s, freeregs #)

setFreeRegsR :: freeRegs -> RegM freeRegs ()
setFreeRegsR regs = RegM $ \ s ->
  (# s{ra_freeregs = regs}, () #)

getAssigR :: RegM freeRegs (RegMap Loc)
getAssigR = RegM $ \ s@RA_State{ra_assig = assig} ->
  (# s, assig #)

setAssigR :: RegMap Loc -> RegM freeRegs ()
setAssigR assig = RegM $ \ s ->
  (# s{ra_assig=assig}, () #)

getBlockAssigR :: RegM freeRegs (BlockAssignment freeRegs)
getBlockAssigR = RegM $ \ s@RA_State{ra_blockassig = assig} ->
  (# s, assig #)

setBlockAssigR :: BlockAssignment freeRegs -> RegM freeRegs ()
setBlockAssigR assig = RegM $ \ s ->
  (# s{ra_blockassig = assig}, () #)

setDeltaR :: Int -> RegM freeRegs ()
setDeltaR n = RegM $ \ s ->
  (# s{ra_delta = n}, () #)

getDeltaR :: RegM freeRegs Int
getDeltaR = RegM $ \s -> (# s, ra_delta s #)

getUniqueR :: RegM freeRegs Unique
getUniqueR = RegM $ \s ->
  case takeUniqFromSupply (ra_us s) of
    (uniq, us) -> (# s{ra_us = us}, uniq #)


-- | Record that a spill instruction was inserted, for profiling.
recordSpill :: SpillReason -> RegM freeRegs ()
recordSpill spill
 	= RegM $ \s -> (# s { ra_spills = spill : ra_spills s}, () #)
