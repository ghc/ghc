module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled(
    peekStgTSOProfInfo
  , peekTopCCS
) where

import Prelude
import Foreign
import GHC.Exts.Heap.ProfInfo.Types

-- | This implementation is used when PROFILING is undefined.
-- It always returns 'Nothing', because there is no profiling info available.
peekStgTSOProfInfo :: (Ptr a -> IO (Maybe CostCentreStack)) -> Ptr tsoPtr -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo _ _ = return Nothing

peekTopCCS :: Ptr a -> IO (Maybe CostCentreStack)
peekTopCCS _ = return Nothing
