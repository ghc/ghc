{-# LANGUAGE CPP, DeriveGeneric #-}
module GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled(
    peekStgTSOProfInfo
) where

import Prelude
import Foreign
import GHC.Exts.Heap.ProfInfo.Types

-- | This implementation is used when PROFILING is undefined.
-- It always returns 'Nothing', because there is no profiling info available.
peekStgTSOProfInfo :: Ptr tsoPtr -> IO (Maybe StgTSOProfInfo)
peekStgTSOProfInfo _ = return Nothing
