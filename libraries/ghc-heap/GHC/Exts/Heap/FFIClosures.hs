{-# LANGUAGE CPP #-}

module GHC.Exts.Heap.FFIClosures (module Reexport) where

-- NOTE [hsc and CPP workaround]
--
-- # Problem
--
-- Often, .hsc files are used to get the correct offsets of C struct fields.
-- Those structs may be affected by CPP directives e.g. profiled vs not profiled
-- closure headers is affected by the PROFILED cpp define. Since we are building
-- multiple variants of the RTS, we must support all possible offsets e.g. by
-- running hsc2hs with cpp defines corresponding to each RTS flavour. The
-- problem is that GHC's build system runs hsc2hs *only once* per .hsc file
-- without properly setting cpp defines. This results in the same (probably
-- incorrect) offsets into our C structs.
--
--
-- # Workaround
--
-- To work around this issue, we create multiple .hsc files each manually
-- defining thir cpp defines (see e.g. FFIClosures_ProfilingDisabled.hsc and
-- FFIClosures_ProfilingEnabled.hsc). Then we rely on cpp defines working
-- correctly in .hs files and use CPP to switch on which .hsc module to
-- re-export (see below). In each case we import the desired .hsc module as
-- `Reexport` and we import `()` (i.e. nothing) from all other .hsc variants
-- just so that the build system sees all .hsc file as dependencies.
--
--
-- # Future Work
--
-- - Duplication of the code in the .hsc files could be reduced simply by
--   placing the code in a single .hsc.in file and `#include`ing it from each
--   .hsc file. The .hsc files would only be responsible for setting the correct
--   cpp defines. This currently doesn't work as hadrian doesn't know to copy
--   the .hsc.in file to the build directory.
-- - The correct solution would be for the build system to run `hsc2hs` with the
--   correct cpp defines once per RTS flavour.
--

#if defined(PROFILING)
import GHC.Exts.Heap.FFIClosures_ProfilingEnabled as Reexport
import GHC.Exts.Heap.FFIClosures_ProfilingDisabled ()
#else
import GHC.Exts.Heap.FFIClosures_ProfilingDisabled as Reexport
import GHC.Exts.Heap.FFIClosures_ProfilingEnabled ()
#endif
