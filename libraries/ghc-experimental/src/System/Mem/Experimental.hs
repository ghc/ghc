module System.Mem.Experimental
  ( setGlobalAllocationLimitHandler
  , AllocationLimitKillBehaviour(..)
  , getAllocationCounterFor
  , setAllocationCounterFor
  , enableAllocationLimitFor
  , disableAllocationLimitFor
  )
  where
import GHC.Internal.AllocationLimitHandler
