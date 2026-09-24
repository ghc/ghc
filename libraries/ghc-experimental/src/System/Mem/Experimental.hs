module System.Mem.Experimental
  (
  -- *  Garbage collection
    performDeadlockDetection

  -- *  Allocation counter and limits
  , setGlobalAllocationLimitHandler
  , AllocationLimitKillBehaviour(..)
  , getAllocationCounterFor
  , setAllocationCounterFor
  , enableAllocationLimitFor
  , disableAllocationLimitFor
  )
  where
import GHC.Internal.AllocationLimitHandler
import GHC.Internal.System.Mem
