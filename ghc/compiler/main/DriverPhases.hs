-----------------------------------------------------------------------------
-- $Id: DriverPhases.hs,v 1.26 2003/06/05 10:11:22 simonmar Exp $
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

#include "../includes/config.h"

module DriverPhases (
   Phase(..),
   happensBefore,
   startPhase,		-- :: String -> Phase
   phaseInputExt, 	-- :: Phase -> String

   haskellish_file, haskellish_suffix,
   haskellish_src_file, haskellish_src_suffix,
   objish_file, objish_suffix,
   cish_file, cish_suffix,
   isExtCore_file, extcoreish_suffix,
   isSourceFile         -- :: FilePath -> Bool
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
	= Unlit
	| Cpp
	| HsPp
	| Hsc
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

-- Partial ordering on phases: we want to know which phases will occur before 
-- which others.  This is used for sanity checking, to ensure that the
-- pipeline will stop at some point (see DriverPipeline.runPipeline).
x `happensBefore` y 
	| x `elem` haskell_pipe = y `elem` tail (dropWhile (/= x) haskell_pipe)
	| x `elem` c_pipe       = y `elem` tail (dropWhile (/= x) c_pipe)
	| otherwise = False

haskell_pipe = [Unlit,Cpp,HsPp,Hsc,HCc,Mangle,SplitMangle,As,SplitAs,Ln]
c_pipe       = [Cc,As,Ln]

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase "lhs"   = Unlit
startPhase "hs"    = Cpp
startPhase "hscpp" = HsPp
startPhase "hspp"  = Hsc
startPhase "hcr"   = Hsc
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
#ifdef ILX
phaseInputExt Ilx2Il      = "ilx"
phaseInputExt Ilasm       = "il"
#endif

haskellish_suffix     = (`elem` [ "hs", "lhs", "hspp", "hscpp", "hcr", "hc", "raw_s" ])
haskellish_src_suffix = (`elem` [ "hs", "lhs", "hspp", "hscpp", "hcr"])
cish_suffix           = (`elem` [ "c", "cpp", "C", "cc", "cxx", "s", "S" ])
extcoreish_suffix     = (`elem` [ "hcr" ])

-- Use the appropriate suffix for the system on which 
-- the GHC-compiled code will run
#if mingw32_TARGET_OS || cygwin32_TARGET_OS
objish_suffix     = (`elem` [ "o", "O", "obj", "OBJ" ])
#else
objish_suffix     = (`elem` [ "o" ])
#endif

haskellish_file     = haskellish_suffix     . getFileSuffix
haskellish_src_file = haskellish_src_suffix . getFileSuffix
cish_file           = cish_suffix           . getFileSuffix
isExtCore_file      = extcoreish_suffix     . getFileSuffix
objish_file         = objish_suffix         . getFileSuffix

isSourceFile :: FilePath -> Bool
isSourceFile   f    =
   haskellish_file f ||
   cish_file   f
