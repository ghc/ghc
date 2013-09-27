-- | State monad for the linear register allocator.

--      Here we keep all the state that the register allocator keeps track
--      of as it walks the instructions in a basic block.

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

import DynFlags
import Unique
import UniqSupply

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))


-- | The register allocator monad type.
newtype RegM freeRegs a
        = RegM { unReg :: RA_State freeRegs -> (# RA_State freeRegs, a #) }

instance Functor (RegM freeRegs) where
      fmap = liftM

instance Applicative (RegM freeRegs) where
      pure = return
      (<*>) = ap

instance Monad (RegM freeRegs) where
  m >>= k   =  RegM $ \s -> case unReg m s of { (# s, a #) -> unReg (k a) s }
  return a  =  RegM $ \s -> (# s, a #)

instance HasDynFlags (RegM a) where
    getDynFlags = RegM $ \s -> (# s, ra_DynFlags s #)


-- | Run a computation in the RegM register allocator monad.
runR    :: DynFlags
        -> BlockAssignment freeRegs
        -> freeRegs
        -> RegMap Loc
        -> StackMap
        -> UniqSupply
        -> RegM freeRegs a
        -> (BlockAssignment freeRegs, StackMap, RegAllocStats, a)

runR dflags block_assig freeregs assig stack us thing =
  case unReg thing
        (RA_State
                { ra_blockassig = block_assig
                , ra_freeregs   = freeregs
                , ra_assig      = assig
                , ra_delta      = 0{-???-}
                , ra_stack      = stack
                , ra_us         = us
                , ra_spills     = []
                , ra_DynFlags   = dflags })
   of
        (# state'@RA_State
                { ra_blockassig = block_assig
                , ra_stack      = stack' }
                , returned_thing #)

         ->     (block_assig, stack', makeRAStats state', returned_thing)


-- | Make register allocator stats from its final state.
makeRAStats :: RA_State freeRegs -> RegAllocStats
makeRAStats state
        = RegAllocStats
        { ra_spillInstrs        = binSpillReasons (ra_spills state) }


spillR :: Instruction instr
       => Reg -> Unique -> RegM freeRegs (instr, Int)

spillR reg temp = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack} ->
  let dflags = ra_DynFlags s
      (stack',slot) = getStackSlotFor stack temp
      instr  = mkSpillInstr dflags reg delta slot
  in
  (# s{ra_stack=stack'}, (instr,slot) #)


loadR :: Instruction instr
      => Reg -> Int -> RegM freeRegs instr

loadR reg slot = RegM $ \ s@RA_State{ra_delta=delta} ->
  let dflags = ra_DynFlags s
  in (# s, mkLoadInstr dflags reg delta slot #)

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

