{-# LANGUAGE CPP, PatternSynonyms #-}

#if !defined(GHC_LOADED_INTO_GHCI)
{-# LANGUAGE UnboxedTuples #-}
#endif

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

        recordSpill,
        recordFixupBlock
)
where

import GhcPrelude

import RegAlloc.Linear.Stats
import RegAlloc.Linear.StackMap
import RegAlloc.Linear.Base
import RegAlloc.Liveness
import Instruction
import Reg
import BlockId

import DynFlags
import Unique
import UniqSupply

import Control.Monad (liftM, ap)

-- Avoids using unboxed tuples when loading into GHCi
#if !defined(GHC_LOADED_INTO_GHCI)

type RA_Result freeRegs a = (# RA_State freeRegs, a #)

pattern RA_Result :: a -> b -> (# a, b #)
pattern RA_Result a b = (# a, b #)
{-# COMPLETE RA_Result #-}
#else

data RA_Result freeRegs a = RA_Result {-# UNPACK #-} !(RA_State freeRegs) !a

#endif

-- | The register allocator monad type.
newtype RegM freeRegs a
        = RegM { unReg :: RA_State freeRegs -> RA_Result freeRegs a }

instance Functor (RegM freeRegs) where
      fmap = liftM

instance Applicative (RegM freeRegs) where
      pure a  =  RegM $ \s -> RA_Result s a
      (<*>) = ap

instance Monad (RegM freeRegs) where
  m >>= k   =  RegM $ \s -> case unReg m s of { RA_Result s a -> unReg (k a) s }

instance HasDynFlags (RegM a) where
    getDynFlags = RegM $ \s -> RA_Result s (ra_DynFlags s)


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
                , ra_DynFlags   = dflags
                , ra_fixups     = [] })
   of
        RA_Result state returned_thing
         ->     (ra_blockassig state, ra_stack state, makeRAStats state, returned_thing)


-- | Make register allocator stats from its final state.
makeRAStats :: RA_State freeRegs -> RegAllocStats
makeRAStats state
        = RegAllocStats
        { ra_spillInstrs        = binSpillReasons (ra_spills state)
        , ra_fixupList          = ra_fixups state }


spillR :: Instruction instr
       => Reg -> Unique -> RegM freeRegs (instr, Int)

spillR reg temp = RegM $ \ s@RA_State{ra_delta=delta, ra_stack=stack0} ->
  let dflags = ra_DynFlags s
      (stack1,slot) = getStackSlotFor stack0 temp
      instr  = mkSpillInstr dflags reg delta slot
  in
  RA_Result s{ra_stack=stack1} (instr,slot)


loadR :: Instruction instr
      => Reg -> Int -> RegM freeRegs instr

loadR reg slot = RegM $ \ s@RA_State{ra_delta=delta} ->
  let dflags = ra_DynFlags s
  in RA_Result s (mkLoadInstr dflags reg delta slot)

getFreeRegsR :: RegM freeRegs freeRegs
getFreeRegsR = RegM $ \ s@RA_State{ra_freeregs = freeregs} ->
  RA_Result s freeregs

setFreeRegsR :: freeRegs -> RegM freeRegs ()
setFreeRegsR regs = RegM $ \ s ->
  RA_Result s{ra_freeregs = regs} ()

getAssigR :: RegM freeRegs (RegMap Loc)
getAssigR = RegM $ \ s@RA_State{ra_assig = assig} ->
  RA_Result s assig

setAssigR :: RegMap Loc -> RegM freeRegs ()
setAssigR assig = RegM $ \ s ->
  RA_Result s{ra_assig=assig} ()

getBlockAssigR :: RegM freeRegs (BlockAssignment freeRegs)
getBlockAssigR = RegM $ \ s@RA_State{ra_blockassig = assig} ->
  RA_Result s assig

setBlockAssigR :: BlockAssignment freeRegs -> RegM freeRegs ()
setBlockAssigR assig = RegM $ \ s ->
  RA_Result s{ra_blockassig = assig} ()

setDeltaR :: Int -> RegM freeRegs ()
setDeltaR n = RegM $ \ s ->
  RA_Result s{ra_delta = n} ()

getDeltaR :: RegM freeRegs Int
getDeltaR = RegM $ \s -> RA_Result s (ra_delta s)

getUniqueR :: RegM freeRegs Unique
getUniqueR = RegM $ \s ->
  case takeUniqFromSupply (ra_us s) of
    (uniq, us) -> RA_Result s{ra_us = us} uniq


-- | Record that a spill instruction was inserted, for profiling.
recordSpill :: SpillReason -> RegM freeRegs ()
recordSpill spill
    = RegM $ \s -> RA_Result (s { ra_spills = spill : ra_spills s }) ()

-- | Record a created fixup block
recordFixupBlock :: BlockId -> BlockId -> BlockId -> RegM freeRegs ()
recordFixupBlock from between to
    = RegM $ \s -> RA_Result (s { ra_fixups = (from,between,to) : ra_fixups s }) ()
