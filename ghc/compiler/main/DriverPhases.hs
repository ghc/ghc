-----------------------------------------------------------------------------
-- $Id: DriverPhases.hs,v 1.17 2002/03/29 21:39:37 sof Exp $
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

#include "../includes/config.h"

module DriverPhases (
   Phase(..),
   startPhase,		-- :: String -> Phase
   phaseInputExt, 	-- :: Phase -> String

   haskellish_file, haskellish_suffix,
   haskellish_src_file, haskellish_src_suffix,
   hsbootish_file, hsbootish_suffix,
   objish_file, objish_suffix,
   cish_file, cish_suffix
 ) where

import DriverUtil

-----------------------------------------------------------------------------
-- Phases

{-
   Phase of the           | Suffix saying | Flag saying   | (suffix of)
   compilation system     | ``start here''| ``stop after''| output file
   
   literate pre-processor | .lhs          | -             | -
   C pre-processor (opt.) | -             | -E            | -
   Haskell compiler       | .hs           | -C, -S        | .hc, .s
   C compiler (opt.)      | .hc or .c     | -S            | .s
   assembler              | .s  or .S     | -c            | .o
   linker                 | other         | -             | a.out
-}

data Phase 
	= MkDependHS	-- haskell dependency generation
	| Unlit
	| Cpp
	| HsPp
	| Hsc
	| HsBoot
	| Cc
	| HCc		-- Haskellised C (as opposed to vanilla C) compilation
	| Mangle	-- assembly mangling, now done by a separate script.
	| SplitMangle	-- after mangler if splitting
	| SplitAs
	| As
	| Ln
#ifdef ILX
        | Ilx2Il
	| Ilasm
#endif
  deriving (Eq, Show)

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase "lhs"   = Unlit
startPhase "hs"    = Cpp
startPhase "hscpp" = HsPp
startPhase "hspp"  = Hsc
startPhase "hcr"   = Hsc
startPhase "hs-boot" = HsBoot
startPhase "hc"    = HCc
startPhase "c"     = Cc
startPhase "cpp"   = Cc
startPhase "C"     = Cc
startPhase "cc"    = Cc
startPhase "cxx"   = Cc
startPhase "raw_s" = Mangle
startPhase "s"     = As
startPhase "S"     = As
startPhase "o"     = Ln
startPhase _       = Ln	   -- all unknown file types

-- the output suffix for a given phase is uniquely determined by
-- the input requirements of the next phase.
phaseInputExt Unlit       = "lhs"
phaseInputExt Cpp         = "lpp"	-- intermediate only
phaseInputExt HsPp        = "hscpp"
phaseInputExt Hsc         = "hspp"
phaseInputExt HCc         = "hc"
phaseInputExt Cc          = "c"
phaseInputExt Mangle      = "raw_s"
phaseInputExt SplitMangle = "split_s"	-- not really generated
phaseInputExt As          = "s"
phaseInputExt SplitAs     = "split_s"   -- not really generated
phaseInputExt Ln          = "o"
phaseInputExt MkDependHS  = "dep"
phaseInputExt HsBoot      = "hs-boot"
#ifdef ILX
phaseInputExt Ilx2Il      = "ilx"
phaseInputExt Ilasm       = "il"
#endif

haskellish_suffix     = (`elem` [ "hs", "lhs", "hspp", "hscpp", "hcr", "hc", "raw_s" ])
haskellish_src_suffix = (`elem` [ "hs", "lhs", "hspp", "hscpp", "hcr"])
cish_suffix           = (`elem` [ "c", "cpp", "C", "cc", "cxx", "s", "S" ])
hsbootish_suffix      = (`elem` [ "hs-boot" ])

#if mingw32_TARGET_OS || cygwin32_TARGET_OS
objish_suffix     = (`elem` [ "o", "O", "obj", "OBJ" ])
#else
objish_suffix     = (`elem` [ "o" ])
#endif

haskellish_file     = haskellish_suffix     . getFileSuffix
haskellish_src_file = haskellish_src_suffix . getFileSuffix
cish_file           = cish_suffix           . getFileSuffix
objish_file         = objish_suffix         . getFileSuffix
hsbootish_file      = hsbootish_suffix      . getFileSuffix
