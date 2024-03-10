{-# LANGUAGE CPP #-}

module GHC.Exts.Heap.ProfInfo.PeekProfInfo (module Reexport) where

-- See [hsc and CPP workaround]

#if defined(PROFILING)
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled as Reexport
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled ()
  -- See Note [No way-dependent imports] in GHC.Exts.Heap.Closures

#else
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled as Reexport
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled ()
  -- See Note [No way-dependent imports] in GHC.Exts.Heap.Closures

#endif
