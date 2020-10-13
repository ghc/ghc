{-# LANGUAGE CPP #-}

module GHC.Exts.Heap.FFIClosures (module Reexport) where

#if defined(PROFILING)
import GHC.Exts.Heap.FFIClosures_ProfilingEnabled as Reexport
import GHC.Exts.Heap.FFIClosures_ProfilingDisabled ()
#else
import GHC.Exts.Heap.FFIClosures_ProfilingDisabled as Reexport
import GHC.Exts.Heap.FFIClosures_ProfilingEnabled ()
#endif
