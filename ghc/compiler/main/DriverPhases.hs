-----------------------------------------------------------------------------
-- $Id: DriverPhases.hs,v 1.34 2005/01/31 16:59:37 simonpj Exp $
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

module DriverPhases (
   HscSource(..), isHsBoot, hscSourceString,
   HscTarget(..), Phase(..),
   happensBefore, eqPhase, anyHsc, isStopLn,
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
import Panic		( panic )

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

data HscSource
   = HsSrcFile | HsBootFile | ExtCoreFile
     deriving( Eq, Ord, Show )
	-- Ord needed for the finite maps we build in CompManager


hscSourceString :: HscSource -> String
hscSourceString HsSrcFile   = ""
hscSourceString HsBootFile  = "[boot]"
hscSourceString ExtCoreFile = "[ext core]"

isHsBoot :: HscSource -> Bool
isHsBoot HsBootFile = True
isHsBoot other      = False

data HscTarget
  = HscC
  | HscAsm
  | HscJava
  | HscILX
  | HscInterpreted
  | HscNothing
  deriving (Eq, Show)

data Phase 
	= Unlit HscSource
	| Cpp   HscSource
	| HsPp  HscSource
	| Hsc   HscSource
	| Cc
	| HCc		-- Haskellised C (as opposed to vanilla C) compilation
	| Mangle	-- assembly mangling, now done by a separate script.
	| SplitMangle	-- after mangler if splitting
	| SplitAs
	| As
	| CmmCpp	-- pre-process Cmm source
	| Cmm		-- parse & compile Cmm code
#ifdef ILX
        | Ilx2Il
	| Ilasm
#endif

	-- The final phase is a pseudo-phase that tells the pipeline to stop.
	-- There is no runPhase case for it.
	| StopLn	-- Stop, but linking will follow, so generate .o file

  deriving (Show)

anyHsc :: Phase
anyHsc = Hsc (panic "anyHsc")

isStopLn :: Phase -> Bool
isStopLn StopLn = True
isStopLn other  = False

eqPhase :: Phase -> Phase -> Bool
-- Equality of constructors, ignoring the HscSource field
-- NB: the HscSource field can be 'bot'; see anyHsc above
eqPhase (Unlit _)   (Unlit _) 	= True
eqPhase (Cpp   _)   (Cpp   _) 	= True
eqPhase (HsPp  _)   (HsPp  _) 	= True
eqPhase (Hsc   _)   (Hsc   _) 	= True
eqPhase Cc	    Cc	        = True
eqPhase HCc	    HCc		= True
eqPhase Mangle	    Mangle    	= True
eqPhase SplitMangle SplitMangle = True
eqPhase SplitAs	    SplitAs 	= True
eqPhase As	    As 		= True
eqPhase CmmCpp	    CmmCpp 	= True
eqPhase Cmm	    Cmm 	= True
eqPhase StopLn	    StopLn	= True
eqPhase _	    _		= False

-- Partial ordering on phases: we want to know which phases will occur before 
-- which others.  This is used for sanity checking, to ensure that the
-- pipeline will stop at some point (see DriverPipeline.runPipeline).
StopLn `happensBefore` y = False
x      `happensBefore` y = after_x `eqPhase` y || after_x `happensBefore` y
	where
	  after_x = nextPhase x

nextPhase :: Phase -> Phase
-- A conservative approximation the next phase, used in happensBefore
nextPhase (Unlit sf)	= Cpp  sf
nextPhase (Cpp   sf)	= HsPp sf
nextPhase (HsPp  sf)	= Hsc  sf
nextPhase (Hsc   sf)	= HCc
nextPhase HCc		= Mangle
nextPhase Mangle	= SplitMangle
nextPhase SplitMangle	= As
nextPhase As		= SplitAs
nextPhase SplitAs	= StopLn
nextPhase Cc		= As
nextPhase CmmCpp	= Cmm
nextPhase Cmm		= HCc
nextPhase StopLn	= panic "nextPhase: nothing after StopLn"

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase "lhs"      = Unlit HsSrcFile
startPhase "lhs-boot" = Unlit HsBootFile
startPhase "hs"       = Cpp   HsSrcFile
startPhase "hs-boot"  = Cpp   HsBootFile
startPhase "hscpp"    = HsPp  HsSrcFile
startPhase "hspp"     = Hsc   HsSrcFile
startPhase "hcr"      = Hsc   ExtCoreFile
startPhase "hc"       = HCc
startPhase "c"        = Cc
startPhase "cpp"      = Cc
startPhase "C"        = Cc
startPhase "cc"       = Cc
startPhase "cxx"      = Cc
startPhase "raw_s"    = Mangle
startPhase "s"        = As
startPhase "S"        = As
startPhase "o"        = StopLn
startPhase "cmm"      = CmmCpp
startPhase "cmmcpp"   = Cmm
startPhase _          = StopLn	   -- all unknown file types

-- This is used to determine the extension for the output from the
-- current phase (if it generates a new file).  The extension depends
-- on the next phase in the pipeline.
phaseInputExt (Unlit HsSrcFile)   = "lhs"
phaseInputExt (Unlit HsBootFile)  = "lhs-boot"
phaseInputExt (Unlit ExtCoreFile) = "lhcr"
phaseInputExt (Cpp   _)  	  = "lpp"	-- intermediate only
phaseInputExt (HsPp  _)		  = "hscpp"	-- intermediate only
phaseInputExt (Hsc   _)  	  = "hspp"	-- intermediate only
	-- NB: as things stand, phaseInputExt (Hsc x) must not evaluate x
	--     because runPipeline uses the StopBefore phase to pick the
	--     output filename.  That could be fixed, but watch out.
phaseInputExt HCc         	  = "hc"  
phaseInputExt Cc          	  = "c"
phaseInputExt Mangle      	  = "raw_s"
phaseInputExt SplitMangle 	  = "split_s"	-- not really generated
phaseInputExt As          	  = "s"
phaseInputExt SplitAs     	  = "split_s"   -- not really generated
phaseInputExt CmmCpp	  	  = "cmm"
phaseInputExt Cmm	  	  = "cmmcpp"
phaseInputExt StopLn          	  = "o"
#ifdef ILX
phaseInputExt Ilx2Il      	  = "ilx"
phaseInputExt Ilasm       	  = "il"
#endif

haskellish_src_suffixes      = haskellish_user_src_suffixes ++
			       [ "hspp", "hscpp", "hcr", "cmm" ]
haskellish_suffixes          = haskellish_src_suffixes ++ ["hc", "raw_s"]
cish_suffixes                = [ "c", "cpp", "C", "cc", "cxx", "s", "S" ]
extcoreish_suffixes          = [ "hcr" ]
haskellish_user_src_suffixes = [ "hs", "lhs", "hs-boot", "lhs-boot" ]	-- Will not be deleted as temp files

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
