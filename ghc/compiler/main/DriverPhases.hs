-----------------------------------------------------------------------------
-- $Id: DriverPhases.hs,v 1.31 2005/01/18 13:51:28 simonmar Exp $
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

#include "../includes/ghcconfig.h"

module DriverPhases (
   Phase(..),
   happensBefore,
   startPhase,		-- :: String -> Phase
   phaseInputExt, 	-- :: Phase -> String

   isHaskellishFilename, 
   isHaskellSrcFilename,
   isObjectFilename,
   isCishFilename,
   isExtCoreFilename,
   isDynLibFilename,
   isHaskellUserSrcFilename,
   isSourceFilename         -- :: FilePath -> Bool
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
	| CmmCpp	-- pre-process Cmm source
	| Cmm		-- parse & compile Cmm code
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
	| x `elem` cmm_pipe     = y `elem` tail (dropWhile (/= x) cmm_pipe)
	| x `elem` c_pipe       = y `elem` tail (dropWhile (/= x) c_pipe)
	| otherwise = False

haskell_post_hsc = [HCc,Mangle,SplitMangle,As,SplitAs,Ln]
haskell_pipe = Unlit : Cpp : HsPp : Hsc : haskell_post_hsc
cmm_pipe     = CmmCpp : Cmm : haskell_post_hsc
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
startPhase "cmm"   = CmmCpp
startPhase "cmmcpp" = Cmm
startPhase _       = Ln	   -- all unknown file types

-- This is used to determine the extension for the output from the
-- current phase (if it generates a new file).  The extension depends
-- on the next phase in the pipeline.
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
phaseInputExt CmmCpp	  = "cmm"
phaseInputExt Cmm	  = "cmmcpp"
#ifdef ILX
phaseInputExt Ilx2Il      = "ilx"
phaseInputExt Ilasm       = "il"
#endif

haskellish_suffixes          = [ "hs", "lhs", "hspp", "hscpp", "hcr", "hc", "raw_s", "cmm" ]
haskellish_src_suffixes      = [ "hs", "lhs", "hspp", "hscpp", "hcr", "cmm" ]
cish_suffixes                = [ "c", "cpp", "C", "cc", "cxx", "s", "S" ]
extcoreish_suffixes          = [ "hcr" ]
haskellish_user_src_suffixes = [ "hs", "lhs" ]

-- Use the appropriate suffix for the system on which 
-- the GHC-compiled code will run
#if mingw32_TARGET_OS || cygwin32_TARGET_OS
objish_suffixes     = [ "o", "O", "obj", "OBJ" ]
#else
objish_suffixes     = [ "o" ]
#endif

#ifdef mingw32_TARGET_OS
dynlib_suffixes = ["dll", "DLL"]
#elif defined(darwin_TARGET_OS)
dynlib_suffixes = ["dylib"]
#else
dynlib_suffixes = ["so"]
#endif

isHaskellishFilename     f = getFileSuffix f `elem` haskellish_suffixes
isHaskellSrcFilename     f = getFileSuffix f `elem` haskellish_src_suffixes
isCishFilename           f = getFileSuffix f `elem` cish_suffixes
isExtCoreFilename        f = getFileSuffix f `elem` extcoreish_suffixes
isObjectFilename         f = getFileSuffix f `elem` objish_suffixes
isHaskellUserSrcFilename f = getFileSuffix f `elem` haskellish_user_src_suffixes
isDynLibFilename	 f = getFileSuffix f `elem` dynlib_suffixes

isSourceFilename :: FilePath -> Bool
isSourceFilename f  =
   isHaskellishFilename f ||
   isCishFilename f
