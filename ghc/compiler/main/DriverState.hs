-----------------------------------------------------------------------------
-- $Id: DriverState.hs,v 1.4 2000/10/11 16:26:04 simonmar Exp $
--
-- Settings for the driver
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module DriverState where

#include "HsVersions.h"

import CmStaticInfo
import CmdLineOpts
import DriverUtil
import Util
import Config
import Array

import Exception
import IOExts

import System
import IO
import List
import Char  
import Monad

-----------------------------------------------------------------------------
-- Driver state

-- certain flags can be specified on a per-file basis, in an OPTIONS
-- pragma at the beginning of the source file.  This means that when
-- compiling mulitple files, we have to restore the global option
-- settings before compiling a new file.  
--
-- The DriverState record contains the per-file-mutable state.

data DriverState = DriverState {

	-- are we runing cpp on this file?
	cpp_flag 		:: Bool,

	-- misc
	stolen_x86_regs		:: Int,
	cmdline_hc_includes	:: [String],

	-- options for a particular phase
	opt_L			:: [String],
	opt_P			:: [String],
	opt_c			:: [String],
	opt_a			:: [String],
	opt_m			:: [String]
   }

initDriverState = DriverState {
	cpp_flag		= False,
	stolen_x86_regs		= 4,
	cmdline_hc_includes	= [],
	opt_L			= [],
	opt_P			= [],
	opt_c			= [],
	opt_a			= [],
	opt_m			= [],
   }
	
GLOBAL_VAR(driver_state, initDriverState, DriverState)

readState :: (DriverState -> a) -> IO a
readState f = readIORef driver_state >>= return . f

updateState :: (DriverState -> DriverState) -> IO ()
updateState f = readIORef driver_state >>= writeIORef driver_state . f

addOpt_L     a = updateState (\s -> s{opt_L      =  a : opt_L      s})
addOpt_P     a = updateState (\s -> s{opt_P      =  a : opt_P      s})
addOpt_c     a = updateState (\s -> s{opt_c      =  a : opt_c      s})
addOpt_a     a = updateState (\s -> s{opt_a      =  a : opt_a      s})
addOpt_m     a = updateState (\s -> s{opt_m      =  a : opt_m      s})

addCmdlineHCInclude a = 
   updateState (\s -> s{cmdline_hc_includes =  a : cmdline_hc_includes s})

	-- we add to the options from the front, so we need to reverse the list
getOpts :: (DriverState -> [a]) -> IO [a]
getOpts opts = readState opts >>= return . reverse

-----------------------------------------------------------------------------
-- non-configured things

cHaskell1Version = "5" -- i.e., Haskell 98

-----------------------------------------------------------------------------
-- Global compilation flags

-- location of compiler-related files
GLOBAL_VAR(topDir,  clibdir, String)
GLOBAL_VAR(inplace, False,   Bool)

-- Cpp-related flags
hs_source_cpp_opts = global
	[ "-D__HASKELL1__="++cHaskell1Version
	, "-D__GLASGOW_HASKELL__="++cProjectVersionInt				
	, "-D__HASKELL98__"
	, "-D__CONCURRENT_HASKELL__"
	]

-- Verbose
GLOBAL_VAR(verbose, False, Bool)
is_verbose = do v <- readIORef verbose; if v then return "-v" else return ""

-- where to keep temporary files
GLOBAL_VAR(v_TmpDir,       cDEFAULT_TMPDIR,  String   )

-- Keep output from intermediate phases
GLOBAL_VAR(keep_hi_diffs, 	False, 		Bool)
GLOBAL_VAR(keep_hc_files,	False,		Bool)
GLOBAL_VAR(keep_s_files,	False,		Bool)
GLOBAL_VAR(keep_raw_s_files,	False,		Bool)
GLOBAL_VAR(keep_tmp_files, 	False, 		Bool)

-- Misc
GLOBAL_VAR(scale_sizes_by,      1.0,		Double)
GLOBAL_VAR(dry_run, 		False,		Bool)
#if !defined(HAVE_WIN32_DLL_SUPPORT) || defined(DONT_WANT_WIN32_DLL_SUPPORT)
GLOBAL_VAR(static, 		True,		Bool)
#else
GLOBAL_VAR(static,              False,          Bool)
#endif
GLOBAL_VAR(recomp,  		True,		Bool)
GLOBAL_VAR(collect_ghc_timing, 	False,		Bool)
GLOBAL_VAR(do_asm_mangling,	True,		Bool)
GLOBAL_VAR(excess_precision,	False,		Bool)

-----------------------------------------------------------------------------
-- Splitting object files (for libraries)

GLOBAL_VAR(split_object_files,	False,		Bool)
GLOBAL_VAR(split_prefix,	"",		String)
GLOBAL_VAR(n_split_files,	0,		Int)
	
can_split :: Bool
can_split =  prefixMatch "i386" cTARGETPLATFORM
	  || prefixMatch "alpha" cTARGETPLATFORM
	  || prefixMatch "hppa" cTARGETPLATFORM
	  || prefixMatch "m68k" cTARGETPLATFORM
	  || prefixMatch "mips" cTARGETPLATFORM
	  || prefixMatch "powerpc" cTARGETPLATFORM
	  || prefixMatch "rs6000" cTARGETPLATFORM
	  || prefixMatch "sparc" cTARGETPLATFORM

-----------------------------------------------------------------------------
-- Compiler output options

GLOBAL_VAR(hsc_lang, if cGhcWithNativeCodeGen == "YES" && 
			 (prefixMatch "i386" cTARGETPLATFORM ||
			  prefixMatch "sparc" cTARGETPLATFORM)
			then  HscAsm
			else  HscC, 
	   HscLang)

GLOBAL_VAR(output_dir,  Nothing, Maybe String)
GLOBAL_VAR(output_suf,  Nothing, Maybe String)
GLOBAL_VAR(output_file, Nothing, Maybe String)
GLOBAL_VAR(output_hi,   Nothing, Maybe String)

GLOBAL_VAR(ld_inputs,	[],      [String])

odir_ify :: String -> IO String
odir_ify f = do
  odir_opt <- readIORef output_dir
  case odir_opt of
	Nothing -> return f
	Just d  -> return (newdir d f)

osuf_ify :: String -> IO String
osuf_ify f = do
  osuf_opt <- readIORef output_suf
  case osuf_opt of
	Nothing -> return f
	Just s  -> return (newsuf s f)

-----------------------------------------------------------------------------
-- Hi Files

GLOBAL_VAR(produceHi,    	True,	Bool)
GLOBAL_VAR(hi_on_stdout, 	False,	Bool)
GLOBAL_VAR(hi_suf,          	"hi",	String)

-----------------------------------------------------------------------------
-- Warnings & sanity checking

-- Warning packages that are controlled by -W and -Wall.  The 'standard'
-- warnings that you get all the time are
-- 	   
-- 	   -fwarn-overlapping-patterns
-- 	   -fwarn-missing-methods
--	   -fwarn-missing-fields
--	   -fwarn-deprecations
-- 	   -fwarn-duplicate-exports
-- 
-- these are turned off by -Wnot.


standardWarnings  = [ "-fwarn-overlapping-patterns"
		    , "-fwarn-missing-methods"
		    , "-fwarn-missing-fields"
		    , "-fwarn-deprecations"
		    , "-fwarn-duplicate-exports"
		    ]
minusWOpts    	  = standardWarnings ++ 
		    [ "-fwarn-unused-binds"
		    , "-fwarn-unused-matches"
		    , "-fwarn-incomplete-patterns"
		    , "-fwarn-unused-imports"
		    ]
minusWallOpts 	  = minusWOpts ++
		    [ "-fwarn-type-defaults"
		    , "-fwarn-name-shadowing"
		    , "-fwarn-missing-signatures"
		    , "-fwarn-hi-shadowing"
		    ]

data WarningState = W_default | W_ | W_all | W_not
GLOBAL_VAR(warning_opt, W_default, WarningState)

-----------------------------------------------------------------------------
-- Compiler optimisation options

GLOBAL_VAR(opt_level, 0, Int)

setOptLevel :: String -> IO ()
setOptLevel ""  	    = do { writeIORef opt_level 1; go_via_C }
setOptLevel "not" 	    = writeIORef opt_level 0
setOptLevel [c] | isDigit c = do
   let level = ord c - ord '0'
   writeIORef opt_level level
   when (level >= 1) go_via_C
setOptLevel s = unknownFlagErr ("-O"++s)

go_via_C = do
   l <- readIORef hsc_lang
   case l of { HscAsm -> writeIORef hsc_lang HscC; 
	       _other -> return () }

GLOBAL_VAR(opt_minus_o2_for_C, False, Bool)

GLOBAL_VAR(opt_MaxSimplifierIterations, 4,     Int)
GLOBAL_VAR(opt_StgStats,                False, Bool)
GLOBAL_VAR(opt_UsageSPInf,  	     	False, Bool)  -- Off by default
GLOBAL_VAR(opt_Strictness,  		True,  Bool)
GLOBAL_VAR(opt_CPR,         		True,  Bool)

hsc_minusO2_flags = hsc_minusO_flags	-- for now

hsc_minusNoO_flags = do
  iter        <- readIORef opt_MaxSimplifierIterations
  return [ 
 	"-fignore-interface-pragmas",
	"-fomit-interface-pragmas"
	]

hsc_minusO_flags = do
  stgstats   <- readIORef opt_StgStats

  return [ 
	"-ffoldr-build-on",
        "-fdo-eta-reduction",
	"-fdo-lambda-eta-expansion",
	"-fcase-of-case",
 	"-fcase-merge",
	"-flet-to-case"
   ]

build_CoreToDo
   :: Int 	-- opt level
   -> Int	-- max iterations
   -> Bool 	-- do usageSP
   -> Bool	-- do strictness
   -> Bool	-- do CPR
   -> Bool	-- do CSE
   -> [CoreToDo]

build_CoreToDo level max_iter usageSP strictness cpr cse
  | level == 0 = [
	CoreDoSimplify (isAmongSimpl [
	    MaxSimplifierIterations max_iter
	])
      ]

  | level >= 1 = [ 

	-- initial simplify: mk specialiser happy: minimum effort please
	CoreDoSimplify (isAmongSimpl [
	    SimplInlinePhase 0,
			-- Don't inline anything till full laziness has bitten
			-- In particular, inlining wrappers inhibits floating
			-- e.g. ...(case f x of ...)...
			--  ==> ...(case (case x of I# x# -> fw x#) of ...)...
			--  ==> ...(case x of I# x# -> case fw x# of ...)...
			-- and now the redex (f x) isn't floatable any more
	    DontApplyRules,
			-- Similarly, don't apply any rules until after full 
			-- laziness.  Notably, list fusion can prevent floating.
            NoCaseOfCase,
			-- Don't do case-of-case transformations.
			-- This makes full laziness work better
	    MaxSimplifierIterations max_iter
	]),

	-- Specialisation is best done before full laziness
	-- so that overloaded functions have all their dictionary lambdas manifest
	CoreDoSpecialising,

	CoreDoFloatOutwards False{-not full-},
	CoreDoFloatInwards,

	CoreDoSimplify (isAmongSimpl [
	   SimplInlinePhase 1,
		-- Want to run with inline phase 1 after the specialiser to give
		-- maximum chance for fusion to work before we inline build/augment
		-- in phase 2.  This made a difference in 'ansi' where an 
		-- overloaded function wasn't inlined till too late.
	   MaxSimplifierIterations max_iter
	]),

	-- infer usage information here in case we need it later.
        -- (add more of these where you need them --KSW 1999-04)
        if usageSP then CoreDoUSPInf else CoreDoNothing,

	CoreDoSimplify (isAmongSimpl [
		-- Need inline-phase2 here so that build/augment get 
		-- inlined.  I found that spectral/hartel/genfft lost some useful
		-- strictness in the function sumcode' if augment is not inlined
		-- before strictness analysis runs
	   SimplInlinePhase 2,
	   MaxSimplifierIterations max_iter
	]),

	CoreDoSimplify (isAmongSimpl [
	   MaxSimplifierIterations 2
		-- No -finline-phase: allow all Ids to be inlined now
		-- This gets foldr inlined before strictness analysis
	]),

	if strictness then CoreDoStrictness else CoreDoNothing,
	if cpr        then CoreDoCPResult   else CoreDoNothing,
	CoreDoWorkerWrapper,
	CoreDoGlomBinds,

	CoreDoSimplify (isAmongSimpl [
	   MaxSimplifierIterations max_iter
		-- No -finline-phase: allow all Ids to be inlined now
	]),

	CoreDoFloatOutwards False{-not full-},
		-- nofib/spectral/hartel/wang doubles in speed if you
		-- do full laziness late in the day.  It only happens
		-- after fusion and other stuff, so the early pass doesn't
		-- catch it.  For the record, the redex is 
		--	  f_el22 (f_el21 r_midblock)

-- Leave out lambda lifting for now
--	  "-fsimplify",	-- Tidy up results of full laziness
--	    "[", 
--		  "-fmax-simplifier-iterations2",
--	    "]",
--	  "-ffloat-outwards-full",	

	-- We want CSE to follow the final full-laziness pass, because it may
	-- succeed in commoning up things floated out by full laziness.
	--
	-- CSE must immediately follow a simplification pass, because it relies
	-- on the no-shadowing invariant.  See comments at the top of CSE.lhs
	-- So it must NOT follow float-inwards, which can give rise to shadowing,
	-- even if its input doesn't have shadows.  Hence putting it between
	-- the two passes.
	if cse then CoreCSE else CoreDoNothing,

	CoreDoFloatInwards,

-- Case-liberation for -O2.  This should be after
-- strictness analysis and the simplification which follows it.

--	  ( ($OptLevel != 2)
--	  ? ""
--	  : "-fliberate-case -fsimplify [ $Oopt_FB_Support -ffloat-lets-exposing-whnf -ffloat-primops-ok -fcase-of-case -fdo-case-elim -fcase-merge -fdo-lambda-eta-expansion -freuse-con -flet-to-case $Oopt_PedanticBottoms $Oopt_MaxSimplifierIterations $Oopt_ShowSimplifierProgress ]" ),
--
--	  "-fliberate-case",

	-- Final clean-up simplification:
	CoreDoSimplify (isAmongSimpl [
	  MaxSimplifierIterations max_iter
		-- No -finline-phase: allow all Ids to be inlined now
	])
   ]

-----------------------------------------------------------------------------
-- Paths & Libraries

split_marker = ':'   -- not configurable (ToDo)

import_paths, include_paths, library_paths :: IORef [String]
GLOBAL_VAR(import_paths,  ["."], [String])
GLOBAL_VAR(include_paths, ["."], [String])
GLOBAL_VAR(library_paths, [],	 [String])

GLOBAL_VAR(cmdline_libraries,   [], [String])

addToDirList :: IORef [String] -> String -> IO ()
addToDirList ref path
  = do paths <- readIORef ref
       writeIORef ref (paths ++ split split_marker path)

-----------------------------------------------------------------------------
-- Packages

GLOBAL_VAR(path_package_config, error "path_package_config", String)

-- package list is maintained in dependency order
packages = global ["std", "rts", "gmp"] :: IORef [String]
-- comma in value, so can't use macro, grrr
{-# NOINLINE packages #-}

addPackage :: String -> IO ()
addPackage package
  = do pkg_details <- readIORef package_details
       case lookupPkg package pkg_details of
	  Nothing -> throwDyn (OtherError ("unknown package name: " ++ package))
	  Just details -> do
	    ps <- readIORef packages
	    unless (package `elem` ps) $ do
		mapM_ addPackage (package_deps details)
		ps <- readIORef packages
		writeIORef packages (package:ps)

getPackageImportPath   :: IO [String]
getPackageImportPath = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (nub (concat (map import_dirs ps')))

getPackageIncludePath   :: IO [String]
getPackageIncludePath = do
  ps <- readIORef packages 
  ps' <- getPackageDetails ps
  return (nub (filter (not.null) (concatMap include_dirs ps')))

	-- includes are in reverse dependency order (i.e. rts first)
getPackageCIncludes   :: IO [String]
getPackageCIncludes = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (reverse (nub (filter (not.null) (concatMap c_includes ps'))))

getPackageLibraryPath  :: IO [String]
getPackageLibraryPath = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (nub (concat (map library_dirs ps')))

getPackageLibraries    :: IO [String]
getPackageLibraries = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  tag <- readIORef build_tag
  let suffix = if null tag then "" else '_':tag
  return (concat (
	map (\p -> map (++suffix) (hs_libraries p) ++ extra_libraries p) ps'
     ))

getPackageExtraGhcOpts :: IO [String]
getPackageExtraGhcOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (concatMap extra_ghc_opts ps')

getPackageExtraCcOpts  :: IO [String]
getPackageExtraCcOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (concatMap extra_cc_opts ps')

getPackageExtraLdOpts  :: IO [String]
getPackageExtraLdOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (concatMap extra_ld_opts ps')

getPackageDetails :: [String] -> IO [Package]
getPackageDetails ps = do
  pkg_details <- readIORef package_details
  return [ pkg | p <- ps, Just pkg <- [ lookupPkg p pkg_details ] ]

GLOBAL_VAR(package_details, (error "package_details"), [Package])

lookupPkg :: String -> [Package] -> Maybe Package
lookupPkg nm ps
   = case [p | p <- ps, name p == nm] of
        []    -> Nothing
        (p:_) -> Just p
-----------------------------------------------------------------------------
-- Ways

-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way".  Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.

-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+ticky-ticky.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

GLOBAL_VAR(build_tag, "", String)

data WayName
  = WayProf
  | WayUnreg
  | WayDll
  | WayTicky
  | WayPar
  | WayGran
  | WaySMP
  | WayDebug
  | WayUser_a
  | WayUser_b
  | WayUser_c
  | WayUser_d
  | WayUser_e
  | WayUser_f
  | WayUser_g
  | WayUser_h
  | WayUser_i
  | WayUser_j
  | WayUser_k
  | WayUser_l
  | WayUser_m
  | WayUser_n
  | WayUser_o
  | WayUser_A
  | WayUser_B
  deriving (Eq,Ord)

GLOBAL_VAR(ways, [] ,[WayName])

-- ToDo: allow WayDll with any other allowed combination

allowed_combinations = 
   [  [WayProf,WayUnreg],
      [WayProf,WaySMP]	   -- works???
   ]

findBuildTag :: IO [String]  -- new options
findBuildTag = do
  way_names <- readIORef ways
  case sort way_names of
     []  -> do  writeIORef build_tag ""
	        return []

     [w] -> do let details = lkupWay w
	       writeIORef build_tag (wayTag details)
	       return (wayOpts details)

     ws  -> if  ws `notElem` allowed_combinations
		then throwDyn (OtherError $
				"combination not supported: "  ++
   				foldr1 (\a b -> a ++ '/':b) 
				(map (wayName . lkupWay) ws))
		else let stuff = map lkupWay ws
			 tag   = concat (map wayTag stuff)
			 flags = map wayOpts stuff
		     in do
		     writeIORef build_tag tag
		     return (concat flags)

lkupWay w = 
   case lookup w way_details of
	Nothing -> error "findBuildTag"
	Just details -> details

data Way = Way {
  wayTag   :: String,
  wayName  :: String,
  wayOpts  :: [String]
  }

way_details :: [ (WayName, Way) ]
way_details =
  [ (WayProf, Way  "p" "Profiling"  
	[ "-fscc-profiling"
	, "-DPROFILING"
	, "-optc-DPROFILING"
	, "-fvia-C" ]),

    (WayTicky, Way  "t" "Ticky-ticky Profiling"  
	[ "-fticky-ticky"
	, "-DTICKY_TICKY"
	, "-optc-DTICKY_TICKY"
	, "-fvia-C" ]),

    (WayUnreg, Way  "u" "Unregisterised" 
	[ "-optc-DNO_REGS"
	, "-optc-DUSE_MINIINTERPRETER"
	, "-fno-asm-mangling"
	, "-funregisterised"
	, "-fvia-C" ]),

    (WayDll, Way  "dll" "DLLized"
        [ ]),

    (WayPar, Way  "mp" "Parallel" 
	[ "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
	, "-fvia-C" ]),

    (WayGran, Way  "mg" "Gransim" 
	[ "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent"
	, "-fvia-C" ]),

    (WaySMP, Way  "s" "SMP"
	[ "-fsmp"
	, "-optc-pthread"
	, "-optl-pthread"
	, "-optc-DSMP"
	, "-fvia-C" ]),

    (WayUser_a,  Way  "a"  "User way 'a'"  ["$WAY_a_REAL_OPTS"]),	
    (WayUser_b,  Way  "b"  "User way 'b'"  ["$WAY_b_REAL_OPTS"]),	
    (WayUser_c,  Way  "c"  "User way 'c'"  ["$WAY_c_REAL_OPTS"]),	
    (WayUser_d,  Way  "d"  "User way 'd'"  ["$WAY_d_REAL_OPTS"]),	
    (WayUser_e,  Way  "e"  "User way 'e'"  ["$WAY_e_REAL_OPTS"]),	
    (WayUser_f,  Way  "f"  "User way 'f'"  ["$WAY_f_REAL_OPTS"]),	
    (WayUser_g,  Way  "g"  "User way 'g'"  ["$WAY_g_REAL_OPTS"]),	
    (WayUser_h,  Way  "h"  "User way 'h'"  ["$WAY_h_REAL_OPTS"]),	
    (WayUser_i,  Way  "i"  "User way 'i'"  ["$WAY_i_REAL_OPTS"]),	
    (WayUser_j,  Way  "j"  "User way 'j'"  ["$WAY_j_REAL_OPTS"]),	
    (WayUser_k,  Way  "k"  "User way 'k'"  ["$WAY_k_REAL_OPTS"]),	
    (WayUser_l,  Way  "l"  "User way 'l'"  ["$WAY_l_REAL_OPTS"]),	
    (WayUser_m,  Way  "m"  "User way 'm'"  ["$WAY_m_REAL_OPTS"]),	
    (WayUser_n,  Way  "n"  "User way 'n'"  ["$WAY_n_REAL_OPTS"]),	
    (WayUser_o,  Way  "o"  "User way 'o'"  ["$WAY_o_REAL_OPTS"]),	
    (WayUser_A,  Way  "A"  "User way 'A'"  ["$WAY_A_REAL_OPTS"]),	
    (WayUser_B,  Way  "B"  "User way 'B'"  ["$WAY_B_REAL_OPTS"]) 
  ]

-----------------------------------------------------------------------------
-- Programs for particular phases

GLOBAL_VAR(pgm_L,   error "pgm_L", String)
GLOBAL_VAR(pgm_P,   cRAWCPP,       String)
GLOBAL_VAR(pgm_c,   cGCC,          String)
GLOBAL_VAR(pgm_m,   error "pgm_m", String)
GLOBAL_VAR(pgm_s,   error "pgm_s", String)
GLOBAL_VAR(pgm_a,   cGCC,          String)
GLOBAL_VAR(pgm_l,   cGCC,          String)

GLOBAL_VAR(opt_dep,    [], [String])
GLOBAL_VAR(anti_opt_C, [], [String])
GLOBAL_VAR(opt_C,      [], [String])
GLOBAL_VAR(opt_l,      [], [String])
GLOBAL_VAR(opt_dll,    [], [String])

getStaticOpts :: IORef [String] -> IO [String]
getStaticOpts ref = readIORef ref >>= return . reverse

-----------------------------------------------------------------------------
-- Via-C compilation stuff

-- flags returned are: ( all C compilations
--		       , registerised HC compilations
--		       )

machdepCCOpts 
   | prefixMatch "alpha"   cTARGETPLATFORM  
	= return ( ["-static"], [] )

   | prefixMatch "hppa"    cTARGETPLATFORM  
        -- ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        -- (very nice, but too bad the HP /usr/include files don't agree.)
	= return ( ["-static", "-D_HPUX_SOURCE"], [] )

   | prefixMatch "m68k"    cTARGETPLATFORM
      -- -fno-defer-pop : for the .hc files, we want all the pushing/
      --    popping of args to routines to be explicit; if we let things
      --    be deferred 'til after an STGJUMP, imminent death is certain!
      --
      -- -fomit-frame-pointer : *don't*
      --     It's better to have a6 completely tied up being a frame pointer
      --     rather than let GCC pick random things to do with it.
      --     (If we want to steal a6, then we would try to do things
      --     as on iX86, where we *do* steal the frame pointer [%ebp].)
	= return ( [], ["-fno-defer-pop", "-fno-omit-frame-pointer"] )

   | prefixMatch "i386"    cTARGETPLATFORM  
      -- -fno-defer-pop : basically the same game as for m68k
      --
      -- -fomit-frame-pointer : *must* in .hc files; because we're stealing
      --   the fp (%ebp) for our register maps.
	= do n_regs <- readState stolen_x86_regs
	     sta    <- readIORef static
	     return ( [ if sta then "-DDONT_WANT_WIN32_DLL_SUPPORT" else "" ],
		      [ "-fno-defer-pop", "-fomit-frame-pointer",
	                "-DSTOLEN_X86_REGS="++show n_regs ]
		    )

   | prefixMatch "mips"    cTARGETPLATFORM
	= return ( ["static"], [] )

   | prefixMatch "powerpc" cTARGETPLATFORM || prefixMatch "rs6000" cTARGETPLATFORM
	= return ( ["static"], ["-finhibit-size-directive"] )

   | otherwise
	= return ( [], [] )


-----------------------------------------------------------------------------
-- Running an external program

run_something phase_name cmd
 = do
   verb <- readIORef verbose
   when verb $ do
	putStr phase_name
	putStrLn ":"
	putStrLn cmd
	hFlush stdout

   -- test for -n flag
   n <- readIORef dry_run
   unless n $ do 

   -- and run it!
#ifndef mingw32_TARGET_OS
   exit_code <- system cmd `catchAllIO` 
		   (\_ -> throwDyn (PhaseFailed phase_name (ExitFailure 1)))
#else
   tmp <- newTempName "sh"
   h <- openFile tmp WriteMode
   hPutStrLn h cmd
   hClose h
   exit_code <- system ("sh - " ++ tmp) `catchAllIO` 
		   (\e -> throwDyn (PhaseFailed phase_name (ExitFailure 1)))
   removeFile tmp
#endif

   if exit_code /= ExitSuccess
	then throwDyn (PhaseFailed phase_name exit_code)
	else do when verb (putStr "\n")
	        return ()

