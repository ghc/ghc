-----------------------------------------------------------------------------
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module Main (main) where

import Package
import Config

import RegexString
import Concurrent
import Posix
import IOExts
import Exception
import Dynamic

import IO
import Array
import List
import System
import Maybe
import Char

#define GLOBAL_VAR(name,value,ty)  \
name = global (value) :: IORef (ty); \
{-# NOINLINE name #-}

-----------------------------------------------------------------------------
-- ToDo:

-- test:
-- stub files

-- time commands when run with -v
-- split marker
-- mkDLL
-- java generation
-- user ways
-- Win32 support
-- make sure OPTIONS in .hs file propogate to .hc file if -C or -keep-hc-file-too

-----------------------------------------------------------------------------
-- Differences vs. old driver:

-- consistency checking removed (may do this properly later)
-- removed -noC
-- no hi diffs (could be added later)
-- no -Ofile

-----------------------------------------------------------------------------
-- non-configured things

_Haskell1Version = "5" -- i.e., Haskell 98

-----------------------------------------------------------------------------
-- Usage Message

short_usage = do
  hPutStr stderr "\nUsage: For basic information, try the `-help' option.\n"
  exitWith ExitSuccess
   
long_usage = do
  let usage_dir = findFile "ghc-usage.txt" (_GHC_DRIVER_DIR++"/ghc-usage.txt")
  usage <- readFile (usage_dir++"/ghc-usage.txt")
  dump usage
  exitWith ExitSuccess
  where
     dump "" = return ()
     dump ('$':'$':s) = hPutStr stderr get_prog_name >> dump s
     dump (c:s) = hPutChar stderr c >> dump s

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
	| Hsc
	| Cc
	| HCc		-- Haskellised C (as opposed to vanilla C) compilation
	| Mangle	-- assembly mangling, now done by a separate script.
	| SplitMangle	-- after mangler if splitting
	| As
	| Ln 
  deriving (Eq,Ord,Enum,Ix,Show,Bounded)

initial_phase = Unlit

-----------------------------------------------------------------------------
-- Errors

data BarfKind
  = UnknownFileType String
  | UnknownFlag String
  | AmbiguousPhase
  | MultipleSrcsOneOutput
  | UnknownPackage String
  | WayCombinationNotSupported [WayName]
  | PhaseFailed String ExitCode
  | Interrupted
  deriving Eq

GLOBAL_VAR(prog_name, "ghc", String)

get_prog_name = unsafePerformIO (readIORef prog_name) -- urk!

instance Show BarfKind where
  showsPrec _ e 
	= showString get_prog_name . showString ": " . showBarf e

showBarf AmbiguousPhase
   = showString "only one of the flags -M, -E, -C, -S, -c is allowed"
showBarf (UnknownFileType s)
   = showString "unknown file type, and linking not done: " . showString s
showBarf (UnknownFlag s)
   = showString "unrecognised flag: " . showString s
showBarf MultipleSrcsOneOutput
   = showString "can't apply -o option to multiple source files"
showBarf (UnknownPackage s)
   = showString "unknown package name: " . showString s
showBarf (WayCombinationNotSupported ws)
   = showString "combination not supported: " 
   . foldr1 (\a b -> a . showChar '/' . b) 
	(map (showString . wayName . lkupWay) ws)

barfKindTc = mkTyCon "BarfKind"

instance Typeable BarfKind where
  typeOf _ = mkAppTy barfKindTc []

-----------------------------------------------------------------------------
-- Temporary files

GLOBAL_VAR(files_to_clean, [], [String])

cleanTempFiles :: IO ()
cleanTempFiles = do
  fs <- readIORef files_to_clean
  verb <- readIORef verbose

  let blowAway f =
	   (do  on verb (hPutStrLn stderr ("removing: " ++ f))
		if '*' `elem` f then system ("rm -f " ++ f) >> return ()
			        else removeLink f)
	    `catchAllIO`
	   (\e -> on verb (hPutStrLn stderr 
				("warning: can't remove tmp file" ++ f)))
  mapM_ blowAway fs

-----------------------------------------------------------------------------
-- Which phase to stop at

GLOBAL_VAR(stop_after, Ln, Phase)

end_phase_flag :: String -> Maybe Phase
end_phase_flag "-M" = Just MkDependHS
end_phase_flag "-E" = Just Cpp
end_phase_flag "-C" = Just Hsc
end_phase_flag "-S" = Just Mangle
end_phase_flag "-c" = Just As
end_phase_flag _    = Nothing

getStopAfter :: [String]
	 -> IO ( [String]   -- rest of command line
	       , Phase	    -- stop after phase
	       , Bool	    -- do linking?
	       )
getStopAfter flags 
  = case my_partition end_phase_flag flags of
	([]   , rest) -> return (rest, As,  True)
	([one], rest) -> return (rest, one, False)
	(_    , rest) -> throwDyn AmbiguousPhase

-----------------------------------------------------------------------------
-- Global compilation flags

	-- Cpp-related flags
GLOBAL_VAR(cpp_flag, False, Bool)
hs_source_cpp_opts = global
	[ "-D__HASKELL1__="++_Haskell1Version
	, "-D__GLASGOW_HASKELL__="++_ProjectVersionInt				
	, "-D__HASKELL98__"
	, "-D__CONCURRENT_HASKELL__"
	]

	-- Keep output from intermediate phases
GLOBAL_VAR(keep_hi_diffs, 	False, 		Bool)
GLOBAL_VAR(keep_hc_files,	False,		Bool)
GLOBAL_VAR(keep_s_files,	False,		Bool)
GLOBAL_VAR(keep_raw_s_files,	False,		Bool)

	-- Compiler RTS options
GLOBAL_VAR(specific_heap_size,  6 * 1000 * 1000, Integer)
GLOBAL_VAR(specific_stack_size, 1000 * 1000,     Integer)
GLOBAL_VAR(scale_sizes_by,      1.0,		 Double)

	-- Verbose
GLOBAL_VAR(verbose, False, Bool)
is_verbose = do v <- readIORef verbose; if v then return "-v" else return ""

	-- Misc
GLOBAL_VAR(dry_run, 		False,		Bool)
GLOBAL_VAR(recomp,  		True,		Bool)
GLOBAL_VAR(tmp_prefix, 		_TMPDIR,	String)
GLOBAL_VAR(stolen_x86_regs, 	4, 		Int)
GLOBAL_VAR(static, 		True,		Bool)  -- ToDo: not for mingw32
GLOBAL_VAR(collect_ghc_timing, 	False,		Bool)
GLOBAL_VAR(do_asm_mangling,	True,		Bool)

-----------------------------------------------------------------------------
-- Splitting object files (for libraries)

GLOBAL_VAR(split_object_files,	False,		Bool)
GLOBAL_VAR(split_prefix,	"",		String)
GLOBAL_VAR(n_split_files,	0,		Int)
	
can_split :: Bool
can_split =  prefixMatch "i386" _TARGETPLATFORM
	  || prefixMatch "alpha" _TARGETPLATFORM
	  || prefixMatch "hppa" _TARGETPLATFORM
	  || prefixMatch "m68k" _TARGETPLATFORM
	  || prefixMatch "mips" _TARGETPLATFORM
	  || prefixMatch "powerpc" _TARGETPLATFORM
	  || prefixMatch "rs6000" _TARGETPLATFORM
	  || prefixMatch "sparc" _TARGETPLATFORM

-----------------------------------------------------------------------------
-- Compiler output options

data HscLang
  = HscC
  | HscAsm
  | HscJava

GLOBAL_VAR(hsc_lang, if _GhcWithNativeCodeGen == "YES" && 
			 prefixMatch "i386" _TARGETPLATFORM
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
	Just d  -> return (newdir f d)

osuf_ify :: String -> IO String
osuf_ify f = do
  osuf_opt <- readIORef output_suf
  case osuf_opt of
	Nothing -> return f
	Just s  -> return (newsuf f s)

-----------------------------------------------------------------------------
-- Hi Files

GLOBAL_VAR(produceHi,    	True,	Bool)
GLOBAL_VAR(hi_on_stdout, 	False,	Bool)
GLOBAL_VAR(hi_with,      	"",	String)
GLOBAL_VAR(hi_suf,          	"hi",	String)

data HiDiffFlag = NormalHiDiffs | UsageHiDiffs | NoHiDiffs
GLOBAL_VAR(hi_diffs, NoHiDiffs, HiDiffFlag)

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
   on (level >= 1) go_via_C
setOptLevel s = throwDyn (UnknownFlag ("-O"++s))

go_via_C = do
   l <- readIORef hsc_lang
   case l of { HscAsm -> writeIORef hsc_lang HscC; 
	       _other -> return () }

GLOBAL_VAR(opt_minus_o2_for_C, False, Bool)

GLOBAL_VAR(opt_MaxSimplifierIterations, 4, Int)
GLOBAL_VAR(opt_StgStats,    False, Bool)
GLOBAL_VAR(opt_UsageSPInf,  False, Bool)  -- Off by default

hsc_minusO2_flags = hsc_minusO_flags	-- for now

hsc_minusNoO_flags = do
  iter        <- readIORef opt_MaxSimplifierIterations
  return [ 
 	"-fignore-interface-pragmas",
	"-fomit-interface-pragmas",
	"-fsimplify",
	    "[",
	        "-fmax-simplifier-iterations" ++ show iter,
	    "]"
	]

hsc_minusO_flags = do
  iter       <- readIORef opt_MaxSimplifierIterations
  usageSP    <- readIORef opt_UsageSPInf
  stgstats   <- readIORef opt_StgStats

  return [ 
	"-ffoldr-build-on",

        "-fdo-eta-reduction",
	"-fdo-lambda-eta-expansion",
	"-fcase-of-case",
 	"-fcase-merge",
	"-flet-to-case",

	-- initial simplify: mk specialiser happy: minimum effort please

	"-fsimplify",
	  "[", 
		"-finline-phase0",
			-- Don't inline anything till full laziness has bitten
			-- In particular, inlining wrappers inhibits floating
			-- e.g. ...(case f x of ...)...
			--  ==> ...(case (case x of I# x# -> fw x#) of ...)...
			--  ==> ...(case x of I# x# -> case fw x# of ...)...
			-- and now the redex (f x) isn't floatable any more

		"-fno-rules",
			-- Similarly, don't apply any rules until after full 
			-- laziness.  Notably, list fusion can prevent floating.

		"-fno-case-of-case",
			-- Don't do case-of-case transformations.
			-- This makes full laziness work better

		"-fmax-simplifier-iterations2",
	  "]",

	-- Specialisation is best done before full laziness
	-- so that overloaded functions have all their dictionary lambdas manifest
	"-fspecialise",

	"-ffloat-outwards",
	"-ffloat-inwards",

	"-fsimplify",
	  "[", 
	  	"-finline-phase1",
		-- Want to run with inline phase 1 after the specialiser to give
		-- maximum chance for fusion to work before we inline build/augment
		-- in phase 2.  This made a difference in 'ansi' where an 
		-- overloaded function wasn't inlined till too late.
	        "-fmax-simplifier-iterations" ++ show iter,
	  "]",

	-- infer usage information here in case we need it later.
        -- (add more of these where you need them --KSW 1999-04)
        if usageSP then "-fusagesp" else "",

	"-fsimplify",
	  "[", 
		-- Need inline-phase2 here so that build/augment get 
		-- inlined.  I found that spectral/hartel/genfft lost some useful
		-- strictness in the function sumcode' if augment is not inlined
		-- before strictness analysis runs

		"-finline-phase2",
		"-fmax-simplifier-iterations2",
	  "]",


	"-fsimplify",
	  "[", 
		"-fmax-simplifier-iterations2",
		-- No -finline-phase: allow all Ids to be inlined now
		-- This gets foldr inlined before strictness analysis
	  "]",

	"-fstrictness",
	"-fcpr-analyse",
	"-fworker-wrapper",

	"-fsimplify",
	  "[", 
	        "-fmax-simplifier-iterations" ++ show iter,
		-- No -finline-phase: allow all Ids to be inlined now
	  "]",

	"-ffloat-outwards",
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
	"-fcse",	
			

	"-ffloat-inwards",

-- Case-liberation for -O2.  This should be after
-- strictness analysis and the simplification which follows it.

--	  ( ($OptLevel != 2)
--	  ? ""
--	  : "-fliberate-case -fsimplify [ $Oopt_FB_Support -ffloat-lets-exposing-whnf -ffloat-primops-ok -fcase-of-case -fdo-case-elim -fcase-merge -fdo-lambda-eta-expansion -freuse-con -flet-to-case $Oopt_PedanticBottoms $Oopt_MaxSimplifierIterations $Oopt_ShowSimplifierProgress ]" ),
--
--	  "-fliberate-case",

	-- Final clean-up simplification:
	"-fsimplify",
	  "[", 
	        "-fmax-simplifier-iterations" ++ show iter,
		-- No -finline-phase: allow all Ids to be inlined now
	  "]"

	]

-----------------------------------------------------------------------------
-- Paths & Libraries

split_marker = ':'   -- not configurable

import_paths, include_paths, library_paths :: IORef [String]
GLOBAL_VAR(import_paths,  ["."], [String])
GLOBAL_VAR(include_paths, ["."], [String])
GLOBAL_VAR(library_paths, [],	 [String])

GLOBAL_VAR(cmdline_libraries,   [], [String])
GLOBAL_VAR(cmdline_hc_includes,	[], [String])

augment_import_paths :: String -> IO ()
augment_import_paths "" = writeIORef import_paths []
augment_import_paths path
  = do paths <- readIORef import_paths
       writeIORef import_paths (paths ++ dirs)
  where dirs = split split_marker path

augment_include_paths :: String -> IO ()
augment_include_paths path
  = do paths <- readIORef include_paths
       writeIORef include_paths (paths ++ split split_marker path)

augment_library_paths :: String -> IO ()
augment_library_paths path
  = do paths <- readIORef library_paths
       writeIORef library_paths (paths ++ split split_marker path)

-----------------------------------------------------------------------------
-- Packages

-- package list is maintained in dependency order
packages = global ["std", "rts", "gmp"] :: IORef [String]
-- comma in value, so can't use macro, grrr
{-# NOINLINE packages #-}

addPackage :: String -> IO ()
addPackage package
  = do pkg_details <- readIORef package_details
       case lookup package pkg_details of
	  Nothing -> throwDyn (UnknownPackage package)
	  Just details -> do
	    ps <- readIORef packages
	    if package `elem` ps 
		then return ()
		else do mapM_ addPackage (package_deps details)
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
  return (nub (filter (not.null) (map include_dir ps')))

	-- includes are in reverse dependency order (i.e. rts first)
getPackageCIncludes   :: IO [String]
getPackageCIncludes = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (reverse (nub (filter (not.null) (map c_include ps'))))

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
  return (concat (map libraries ps'))

getPackageExtraGhcOpts :: IO [String]
getPackageExtraGhcOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (map extra_ghc_opts ps')

getPackageExtraCcOpts  :: IO [String]
getPackageExtraCcOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (map extra_cc_opts ps')

getPackageExtraLdOpts  :: IO [String]
getPackageExtraLdOpts = do
  ps <- readIORef packages
  ps' <- getPackageDetails ps
  return (map extra_ld_opts ps')

getPackageDetails ps = do
  pkg_details <- readIORef package_details
  let getDetails p =  case lookup p pkg_details of
			Just details -> return details
			Nothing -> error "getPackageDetails"
  mapM getDetails ps

GLOBAL_VAR(package_details, (error "package_details"), [(String,Package)])

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
		then throwDyn (WayCombinationNotSupported ws)
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
	, "-optc-DPROFILING" ]),

    (WayTicky, Way  "t" "Ticky-ticky Profiling"  
	[ "-fticky-ticky"
	, "-DTICKY_TICKY"
	, "-optc-DTICKY_TICKY" ]),

    (WayUnreg, Way  "u" "Unregisterised" 
	[ "-optc-DNO_REGS"
	, "-optc-DUSE_MINIINTERPRETER"
	, "-fno-asm-mangling"
	, "-funregisterised" ]),

    (WayPar, Way  "mp" "Parallel" 
	[ "-fstack-check"
	, "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent" ]),

    (WayGran, Way  "mg" "Gransim" 
	[ "-fstack-check"
	, "-fgransim"
	, "-D__GRANSIM__"
	, "-optc-DGRAN"
	, "-package concurrent" ]),

    (WaySMP, Way  "s" "SMP"  
	[ "-fsmp"
	, "-optc-pthread"
	, "-optl-pthread"
	, "-optc-DSMP" ]),

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

GLOBAL_VAR(pgm_dep, findFile "mkdependHS" _GHC_MKDEPENDHS, String)
GLOBAL_VAR(pgm_L,   findFile "unlit"      _GHC_UNLIT,      String)
GLOBAL_VAR(pgm_P,   findFile "hscpp"      _GHC_HSCPP,      String)
GLOBAL_VAR(pgm_C,   findFile "hsc"        _GHC_HSC,        String)
GLOBAL_VAR(pgm_c,   _GCC,	      	     	      	   String)
GLOBAL_VAR(pgm_m,   findFile "ghc-asm"    _GHC_MANGLER,    String)
GLOBAL_VAR(pgm_s,   findFile "ghc-split"  _GHC_SPLIT,      String)
GLOBAL_VAR(pgm_a,   _GCC,	      	     	           String)
GLOBAL_VAR(pgm_l,   _GCC,       	     	           String)

-----------------------------------------------------------------------------
-- Options for particular phases

GLOBAL_VAR(opt_dep, [], [String])
GLOBAL_VAR(opt_L, [], [String])
GLOBAL_VAR(opt_P, [], [String])
GLOBAL_VAR(opt_C, [], [String])
GLOBAL_VAR(opt_Crts, [], [String])
GLOBAL_VAR(opt_c, [], [String])
GLOBAL_VAR(opt_a, [], [String])
GLOBAL_VAR(opt_m, [], [String])
GLOBAL_VAR(opt_l, [], [String])
GLOBAL_VAR(opt_dll, [], [String])

	-- we add to the options from the front, so we need to reverse the list
getOpts :: IORef [String] -> IO [String]
getOpts opts = readIORef opts >>= return . reverse

GLOBAL_VAR(anti_opt_C, [], [String])

-----------------------------------------------------------------------------
-- Via-C compilation stuff

-- flags returned are: ( all C compilations
--		       , registerised HC compilations
--		       )

machdepCCOpts 
   | prefixMatch "alpha"   _TARGETPLATFORM  
	= return ( ["-static"], [] )

   | prefixMatch "hppa"    _TARGETPLATFORM  
        -- ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        -- (very nice, but too bad the HP /usr/include files don't agree.)
	= return ( ["-static", "-D_HPUX_SOURCE"], [] )

   | prefixMatch "m68k"    _TARGETPLATFORM
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

   | prefixMatch "i386"    _TARGETPLATFORM  
      -- -fno-defer-pop : basically the same game as for m68k
      --
      -- -fomit-frame-pointer : *must* in .hc files; because we're stealing
      --   the fp (%ebp) for our register maps.
	= do n_regs <- readIORef stolen_x86_regs
	     sta    <- readIORef static
	     return ( [ if sta then "-DDONT_WANT_WIN32_DLL_SUPPORT" else "" ],
		      [ "-fno-defer-pop", "-fomit-frame-pointer",
	                "-DSTOLEN_X86_REGS="++show n_regs ]
		    )

   | prefixMatch "mips"    _TARGETPLATFORM
	= return ( ["static"], [] )

   | prefixMatch "powerpc" _TARGETPLATFORM || prefixMatch "rs6000" _TARGETPLATFORM
	= return ( ["static"], ["-finhibit-size-directive"] )

   | otherwise
	= return ( [], [] )

-----------------------------------------------------------------------------
-- Build the Hsc command line

build_hsc_opts :: IO [String]
build_hsc_opts = do
  opt_C_ <- getOpts opt_C		-- misc hsc opts

	-- warnings
  warn_level <- readIORef warning_opt
  let warn_opts =  case warn_level of
		  	W_default -> standardWarnings
		  	W_        -> minusWOpts
		  	W_all	  -> minusWallOpts
		  	W_not     -> []

	-- optimisation
  minus_o <- readIORef opt_level
  optimisation_opts <-
        case minus_o of
	    0 -> hsc_minusNoO_flags
	    1 -> hsc_minusO_flags
	    2 -> hsc_minusO2_flags
	    -- ToDo: -Ofile
 
	-- STG passes
  ways_ <- readIORef ways
  let stg_massage | WayProf `elem` ways_ =  "-fmassage-stg-for-profiling"
	          | otherwise            = ""

  stg_stats <- readIORef opt_StgStats
  let stg_stats_flag | stg_stats = "-dstg-stats"
		     | otherwise = ""

  let stg_opts = [ stg_massage, stg_stats_flag, "-flet-no-escape" ]
	-- let-no-escape always on for now

  verb <- is_verbose
  let hi_vers = "-fhi-version="++_ProjectVersionInt
  static <- (do s <- readIORef static; if s then return "-static" else return "")

  l <- readIORef hsc_lang
  let lang = case l of
		HscC    -> "-olang=C"
		HscAsm  -> "-olang=asm"
		HscJava -> "-olang=java"

  -- get hi-file suffix
  hisuf <- readIORef hi_suf

  -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  import_dirs <- readIORef import_paths
  package_import_dirs <- getPackageImportPath
  
  let hi_map = "-himap=" ++
		makeHiMap import_dirs hisuf 
			 package_import_dirs package_hisuf
		   	 split_marker

      hi_map_sep = "-himap-sep=" ++ [split_marker]

  scale <- readIORef scale_sizes_by
  heap  <- readIORef specific_heap_size
  stack <- readIORef specific_stack_size
  cmdline_rts_opts <- getOpts opt_Crts
  let heap'  = truncate (fromIntegral heap  * scale) :: Integer
      stack' = truncate (fromIntegral stack * scale) :: Integer
      rts_opts = [ "+RTS", "-H"++show heap', "-K"++show stack' ]
		 ++ cmdline_rts_opts ++ [ "-RTS" ]

  -- take into account -fno-* flags by removing the equivalent -f*
  -- flag from our list.
  anti_flags <- getOpts anti_opt_C
  let basic_opts = opt_C_ ++ warn_opts ++ optimisation_opts ++ stg_opts
      filtered_opts = filter (`notElem` anti_flags) basic_opts
  
  return 
	(  
	filtered_opts
	-- ToDo: C stub files
	++ [ hi_vers, static, verb, lang, hi_map, hi_map_sep ]
	++ rts_opts
	)

makeHiMap 
  (import_dirs         :: [String])
  (hi_suffix           :: String)
  (package_import_dirs :: [String])
  (package_hi_suffix   :: String)   
  (split_marker        :: Char)
  = foldr (add_dir hi_suffix) 
	(foldr (add_dir package_hi_suffix) "" package_import_dirs)
	import_dirs
  where
     add_dir hisuf dir str = dir ++ "%." ++ hisuf ++ split_marker : str


getOptionsFromSource 
	:: String		-- input file
	-> IO [String]		-- options, if any
getOptionsFromSource file
  = do h <- openFile file ReadMode
       look h
  where
	look h = do
	    l <- hGetLine h
	    case () of
		() | null l -> look h
		   | prefixMatch "{-# LINE" l -> look h
		   | Just (opts:_) <- matchRegex optionRegex l
			-> return (words opts)
		   | otherwise -> return []

optionRegex = mkRegex "{-#[ \t]+OPTIONS[ \t]+(.*)#-}"

-----------------------------------------------------------------------------
-- Main loop

get_source_files :: [String] -> ([String],[String])
get_source_files = partition (('-' /=) . head)

suffixes :: [(String,Phase)]
suffixes =
  [ ("lhs",   Unlit)
  , ("hs",    Cpp)
  , ("hc",    HCc)
  , ("c",     Cc)
  , ("raw_s", Mangle)
  , ("s",     As)
  , ("S",     As)
  , ("o",     Ln)
  ]

phase_input_ext Unlit       = "lhs"
phase_input_ext	Cpp         = "lpp"
phase_input_ext	Hsc         = "cpp"
phase_input_ext	HCc         = "hc"
phase_input_ext Cc          = "c"
phase_input_ext	Mangle      = "raw_s"
phase_input_ext	SplitMangle = "split_s"	-- not really generated
phase_input_ext	As          = "s"
phase_input_ext	Ln          = "o"

find_phase :: String -> ([(Phase,String)], [String])
   -> ([(Phase,String)], [String])
find_phase f (phase_srcs, unknown_srcs)
  = case lookup ext suffixes of
	Just the_phase -> ((the_phase,f):phase_srcs, unknown_srcs)
	Nothing        -> (phase_srcs, f:unknown_srcs)
  where (basename,ext) = split_filename f


find_phases srcs = (phase_srcs, unknown_srcs)
  where (phase_srcs, unknown_srcs) = foldr find_phase ([],[]) srcs

main =
  -- all error messages are propagated as exceptions
  my_catchDyn (\dyn -> case dyn of
			  PhaseFailed phase code -> exitWith code
			  Interrupted -> exitWith (ExitFailure 1)
			  _ -> do hPutStrLn stderr (show (dyn :: BarfKind))
			          exitWith (ExitFailure 1)) $

  later cleanTempFiles $
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

  do
	-- install signal handlers
   main_thread <- myThreadId
   let sig_handler = Catch (raiseInThread main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing

   pgm    <- getProgName
   writeIORef prog_name pgm

   argv   <- getArgs

   -- grab any -B options from the command line first
   argv'  <- setTopDir argv

   -- read the package configuration
   let conf = findFile "package.conf" (_GHC_DRIVER_DIR++"/package.conf.inplace")
   contents <- readFile conf
   writeIORef package_details (read contents)

   -- find the phase to stop after (i.e. -E, -C, -c, -S flags)
   (flags2, stop_phase, do_linking) <- getStopAfter argv'

   -- process all the other arguments, and get the source files
   srcs   <- processArgs flags2 []

   -- find the build tag, and re-process the build-specific options
   more_opts <- findBuildTag
   _ <- processArgs more_opts []

   if stop_phase == MkDependHS		-- mkdependHS is special
	then do_mkdependHS flags2 srcs
	else do

   -- for each source file, find which phase to start at
   let (phase_srcs, unknown_srcs) = find_phases srcs

   o_file <- readIORef output_file
   if isJust o_file && not do_linking && length phase_srcs > 1
	then throwDyn MultipleSrcsOneOutput
	else do

   -- if we have unknown files, and we're not doing linking, complain
   -- (otherwise pass them through to the linker).
   if not (null unknown_srcs) && not do_linking
	then throwDyn (UnknownFileType (head unknown_srcs))
	else do


   let  compileFile :: (Phase, String) -> IO String
	compileFile (phase, src) = do
	  let (orig_base, _) = split_filename src
	  if phase < Ln	-- anything to do?
	      	then run_pipeline stop_phase do_linking orig_base (phase,src)
		else return src

   o_files <- mapM compileFile phase_srcs

   if do_linking
	then do_link o_files unknown_srcs
	else return ()


-- The following compilation pipeline algorithm is fairly hacky.  A
-- better way to do this would be to express the whole comilation as a
-- data flow DAG, where the nodes are the intermediate files and the
-- edges are the compilation phases.  This framework would also work
-- nicely if a haskell dependency generator was included in the
-- driver.

-- It would also deal much more cleanly with compilation phases that
-- generate multiple intermediates, (eg. hsc generates .hc, .hi, and
-- possibly stub files), where some of the output files need to be
-- processed further (eg. the stub files need to be compiled by the C
-- compiler).

-- A cool thing to do would then be to execute the data flow graph
-- concurrently, automatically taking advantage of extra processors on
-- the host machine.  For example, when compiling two Haskell files
-- where one depends on the other, the data flow graph would determine
-- that the C compiler from the first comilation can be overlapped
-- with the hsc comilation for the second file.

run_pipeline
  :: Phase		-- phase to end on (never Linker)
  -> Bool		-- doing linking afterward?
  -> String		-- original basename (eg. Main)
  -> (Phase, String)    -- phase to run, input file
  -> IO String		-- return final filename

run_pipeline last_phase do_linking orig_basename (phase, input_fn) = do

     let (basename,ext) = split_filename input_fn

     split <- readIORef split_object_files
     mangle <- readIORef do_asm_mangling
     lang <- readIORef hsc_lang

	-- figure out what the next phase is.  This is
	-- straightforward, apart from the fact that hsc can generate
	-- either C or assembler direct, and assembly mangling is
	-- optional.
     let next_phase =
	  case phase of
		Hsc -> case lang of
		 	    HscC   -> HCc
			    HscAsm -> As

		HCc  | mangle    -> Mangle
		     | otherwise -> As

		Cc -> As

		Mangle | not split -> As
	
		_  -> succ phase


	-- filename extension for the output
     let new_ext = phase_input_ext next_phase

	-- Figure out what the output from this pass should be called.

	-- If we're keeping the output from this phase, then we just save
	-- it in the current directory, otherwise we generate a new temp file.
     keep_s <- readIORef keep_s_files
     keep_raw_s <- readIORef keep_raw_s_files
     keep_hc <- readIORef keep_hc_files
     let keep_this_output = 
	   case next_phase of
		Ln -> True
		Mangle | keep_raw_s -> True -- first enhancement :)
		As | keep_s  -> True
		Cc | keep_hc -> True
		_other -> False

     output_fn <- 
	(if phase == last_phase && not do_linking
	    then do o_file <- readIORef output_file
		    case o_file of 
		        Just s  -> return s
			Nothing -> do
			    f <- odir_ify (orig_basename ++ '.':new_ext)
			    osuf_ify f

		-- .o files are always kept.  .s files and .hc file may be kept.
		else if keep_this_output
			then odir_ify (orig_basename ++ '.':new_ext)
			else do filename <- newTempName new_ext
				add files_to_clean filename
				return filename
	)

     run_phase phase orig_basename input_fn output_fn

     if (phase == last_phase)
	then return output_fn
	else run_pipeline last_phase do_linking 
		orig_basename (next_phase, output_fn)


-- find a temporary name that doesn't already exist.
newTempName :: String -> IO String
newTempName extn = do
  x <- getProcessID
  tmp_dir <- readIORef tmp_prefix 
  findTempName tmp_dir x
  where findTempName tmp_dir x = do
  	   let filename = tmp_dir ++ "/ghc" ++ show x ++ '.':extn
  	   b  <- fileExist filename
	   if b then findTempName tmp_dir (x+1)
		else return filename

-------------------------------------------------------------------------------
-- mkdependHS phase 

do_mkdependHS :: [String] -> [String] -> IO ()
do_mkdependHS cmd_opts srcs = do
	-- ToDo: push (@MkDependHS_flags, "-o$Osuffix") if $Osuffix;
    -- 	# They're not (currently) needed, but we need to quote any -#include options
    -- foreach (@Cmd_opts) {
    -- 	   s/-#include.*$/'$&'/g;
    -- };  

   mkdependHS      <- readIORef pgm_dep
   mkdependHS_opts <- getOpts opt_dep
   hs_src_cpp_opts <- readIORef hs_source_cpp_opts

   run_something "Dependency generation"
	(unwords (mkdependHS : 
		      mkdependHS_opts
		   ++ hs_src_cpp_opts
		   ++ ("--" : cmd_opts )
		   ++ ("--" : srcs)
	))

-------------------------------------------------------------------------------
-- Unlit phase 

run_phase Unlit basename input_fn output_fn
  = do unlit <- readIORef pgm_L
       unlit_flags <- getOpts opt_L
       run_something "Literate pre-processor"
	  ("echo '{-# LINE 1 \"" ++input_fn++"\" -}' > "++output_fn++" && "
	   ++ unlit ++ ' ':input_fn ++ " - >> " ++ output_fn)

-------------------------------------------------------------------------------
-- HsCpp phase 

run_phase Cpp basename input_fn output_fn
  = do src_opts <- getOptionsFromSource input_fn
       processArgs src_opts []

       do_cpp <- readIORef cpp_flag
       if do_cpp
          then do
       	    cpp <- readIORef pgm_P
	    hscpp_opts <- getOpts opt_P
       	    hs_src_cpp_opts <- readIORef hs_source_cpp_opts

	    cmdline_include_paths <- readIORef include_paths
	    pkg_include_dirs <- getPackageIncludePath
	    let include_paths = map (\p -> "-I"++p) (cmdline_include_paths
							++ pkg_include_dirs)

	    verb <- is_verbose
	    run_something "C pre-processor" 
		(unwords
       	    	   (["echo '{-# LINE 1 \"" ++ input_fn ++ "\" -}'", ">", output_fn, "&&",
		     cpp, verb] 
		    ++ include_paths
		    ++ hs_src_cpp_opts
		    ++ hscpp_opts
		    ++ [ input_fn, ">>", output_fn ]
		   ))
	  else do
	    run_something "Inefective C pre-processor"
	           ("echo '{-# LINE 1 \""  ++ input_fn ++ "\" -}' > " 
		    ++ output_fn ++ " && cat " ++ input_fn
		    ++ " >> " ++ output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

run_phase Hsc	basename input_fn output_fn
  = do  hsc <- readIORef pgm_C
	
  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let (root,dir) = break (=='/') (reverse basename)
	    current_dir = if null dir then "." else reverse dir
	
	paths <- readIORef include_paths
	writeIORef include_paths (current_dir : paths)
	
  -- build the hsc command line
	hsc_opts <- build_hsc_opts
	
	doing_hi <- readIORef produceHi
	tmp_hi_file <- if doing_hi 	
			  then do fn <- newTempName "hi"
				  add files_to_clean fn
				  return fn
			  else return ""
	
	let hi_flag = if doing_hi then "-hifile=" ++ tmp_hi_file
				  else ""
	
  -- deal with -Rghc-timing
	timing <- readIORef collect_ghc_timing
        stat_file <- newTempName "stat"
        add files_to_clean stat_file
	let stat_opts | timing    = [ "+RTS", "-S"++stat_file, "-RTS" ]
		      | otherwise = []

  -- tmp files for foreign export stub code
	tmp_stub_h <- newTempName "stub_h"
	tmp_stub_c <- newTempName "stub_c"
	add files_to_clean tmp_stub_h
	add files_to_clean tmp_stub_c
	
	run_something "Haskell Compiler" 
		 (unwords (hsc : input_fn : (
		    hsc_opts
		    ++ [ hi_flag, " -ofile="++output_fn ]
		    ++ [ "-F="++tmp_stub_c, "-FH="++tmp_stub_h ]
		    ++ stat_opts
		 )))

  -- Copy the .hi file into the current dir if it changed
	on doing_hi 
		  (do ohi <- readIORef output_hi
		      hisuf <- readIORef hi_suf
		      let hi_target = case ohi of
					Nothing -> basename ++ '.':hisuf
					Just fn -> fn
		      new_hi_file <- fileExist tmp_hi_file
		      on new_hi_file
			     (run_something "Copy hi file"
				(unwords ["mv", tmp_hi_file, hi_target]))
		  )	
	
  -- Generate -Rghc-timing info
	on (timing) (
  	    run_something "Generate timing stats"
		(findFile "ghc-stats" _GHC_STATS ++ ' ':stat_file)
	 )

  -- Deal with stubs
	let stub_h = basename ++ "_stub.h"
	let stub_c = basename ++ "_stub.c"
	
		-- copy .h_stub file into current dir if present
	b <- fileExist tmp_stub_h
	on b (do
	      	run_something "Copy stub .h file"
				("cp " ++ tmp_stub_h ++ ' ':stub_h)
	
			-- #include <..._stub.h> in .hc file
		add cmdline_hc_includes tmp_stub_h	-- hack

			-- copy the _stub.c file into the current dir
		run_something "Copy stub .c file" 
		    (unwords [ 
			"rm -f", stub_c, "&&",
			"echo \'#include \""++stub_h++"\"\' >"++stub_c, " &&",
			"cat", tmp_stub_c, ">> ", stub_c
			])

			-- compile the _stub.c file w/ gcc
		run_pipeline As False (basename++"_stub") (Cc, stub_c)
		add ld_inputs (basename++"_stub.o")
	 )

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

run_phase cc_phase basename input_fn output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc <- readIORef pgm_c
       	cc_opts <- getOpts opt_c
       	cmdline_include_dirs <- readIORef include_paths
       -- ToDo: $c_flags .= " -mno-cygwin" if ( $TargetPlatform =~ /-mingw32$/ );

        let hcc = cc_phase == HCc

		-- add package include paths even if we're just compiling
		-- .c files; this is the Value Add(TM) that using
		-- ghc instead of gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath
	let include_paths = map (\p -> "-I"++p) (cmdline_include_dirs 
							++ pkg_include_dirs)

	c_includes <- getPackageCIncludes
	cmdline_includes <- readIORef cmdline_hc_includes -- -#include options

	let cc_injects | hcc = unlines (map mk_include 
					(c_includes ++ reverse cmdline_includes))
		       | otherwise = ""
	    mk_include h_file = 
		case h_file of 
		   '"':_{-"-} -> "#include "++h_file
		   '<':_      -> "#include "++h_file
		   _          -> "#include \""++h_file++"\""

	cc_help <- newTempName "c"
	add files_to_clean cc_help
	h <- openFile cc_help WriteMode
	hPutStr h cc_injects
	hPutStrLn h ("#include \"" ++ input_fn ++ "\"\n")
	hClose h

	ccout <- newTempName "ccout"
	add files_to_clean ccout

	mangle <- readIORef do_asm_mangling
	(md_c_flags, md_regd_c_flags) <- machdepCCOpts

        verb <- is_verbose

	o2 <- readIORef opt_minus_o2_for_C
	let opt_flag | o2        = "-O2"
		     | otherwise = "-O"

	pkg_extra_cc_opts <- getPackageExtraCcOpts

	run_something "C Compiler"
	 (unwords ([ cc, "-x", "c", cc_help, "-o", output_fn ]
		   ++ md_c_flags
		   ++ (if cc_phase == HCc && mangle
			 then md_regd_c_flags
			 else [])
		   ++ [ verb, "-S", "-Wimplicit", opt_flag ]
		   ++ [ "-D__GLASGOW_HASKELL__="++_ProjectVersionInt ]
		   ++ cc_opts
		   ++ include_paths
		   ++ pkg_extra_cc_opts
--		   ++ [">", ccout]
		   ))

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

run_phase Mangle basename input_fn output_fn
  = do mangler <- readIORef pgm_m
       mangler_opts <- getOpts opt_m
       machdep_opts <-
	 if (prefixMatch "i386" _TARGETPLATFORM)
	    then do n_regs <- readIORef stolen_x86_regs
		    return [ show n_regs ]
	    else return []
       run_something "Assembly Mangler"
	(unwords (mangler : 
		     mangler_opts
		  ++ [ input_fn, output_fn ]
		  ++ machdep_opts
		))

-----------------------------------------------------------------------------
-- Splitting phase

run_phase SplitMangle basename input_fn outputfn
  = do  splitter <- readIORef pgm_s

	-- this is the prefix used for the split .s files
	tmp_pfx <- readIORef tmp_prefix
	x <- getProcessID
	let split_s_prefix = tmp_pfx ++ "/ghc" ++ show x
	writeIORef split_prefix split_s_prefix
	add files_to_clean (split_s_prefix ++ "__*") -- d:-)

	-- allocate a tmp file to put the no. of split .s files in (sigh)
	n_files <- newTempName "n_files"
	add files_to_clean n_files

	run_something "Split Assembly File"
	 (unwords [ splitter
		  , input_fn
		  , split_s_prefix
		  , n_files ]
	 )

	-- save the number of split files for future references
	s <- readFile n_files
	let n = read s :: Int
	writeIORef n_split_files n

-----------------------------------------------------------------------------
-- As phase

run_phase As basename input_fn output_fn
  = do 	split <- readIORef split_object_files
	as <- readIORef pgm_a
        as_opts <- getOpts opt_a

        if not split then do
            cmdline_include_paths <- readIORef include_paths
            let cmdline_include_flags = map (\p -> "-I"++p) cmdline_include_paths
            run_something "Assembler"
	     (unwords (as : as_opts
	  	       ++ cmdline_include_flags
	  	       ++ [ "-c", input_fn, "-o",  output_fn ]
	  	    ))

	 else do
	    odir_opt <- readIORef output_dir
    	    let odir | Just s <- odir_opt = s
	    	     | otherwise          = basename
	    
	    split_s_prefix <- readIORef split_prefix
	    n <- readIORef n_split_files

	    odir <- readIORef output_dir
	    let real_odir = case odir of
				Nothing -> basename
				Just d  -> d
	    
	    let assemble_file n = do
		    let input_s  = split_s_prefix ++ "__" ++ show n ++ ".s"
	    	    let output_o = newdir real_odir 
					(basename ++ "__" ++ show n ++ ".o")
	    	    run_something "Assembler" 
	    		    (unwords (as : as_opts
	    			      ++ [ "-c", "-o ", output_o, input_s ]
	    		    ))
	    
	    mapM_ assemble_file [1..n]

-----------------------------------------------------------------------------
-- Linking

do_link :: [String] -> [String] -> IO ()
do_link o_files unknown_srcs = do
    ln <- readIORef pgm_l
    verb <- is_verbose
    o_file <- readIORef output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pkg_lib_paths <- getPackageLibraryPath
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_libs <- getPackageLibraries
    let pkg_lib_opts = map ("-l"++) pkg_libs

    libs <- readIORef cmdline_libraries
    let lib_opts = map ("-l"++) (reverse libs)
	 -- reverse because they're added in reverse order from the cmd line

    pkg_extra_ld_opts <- getPackageExtraLdOpts

	-- probably _stub.o files
    extra_ld_inputs <- readIORef ld_inputs

    run_something "Linker"
       (unwords 
	 ([ ln, verb, "-o", output_fn ]
	     -- ToDo: -u <blah> options
	 ++ o_files
	 ++ unknown_srcs
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ lib_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_lib_opts
	 ++ pkg_extra_ld_opts
	)
       )

-----------------------------------------------------------------------------
-- Running an external program

run_something phase_name cmd
 = do
   verb <- readIORef verbose
   if verb then do
	putStr phase_name
	putStrLn ":"
	putStrLn cmd
     else
	return ()

   -- test for -n flag
   n <- readIORef dry_run
   if n then return () else do 

   -- and run it!
   exit_code <- system cmd  `catchAllIO` 
		   (\e -> throwDyn (PhaseFailed phase_name (ExitFailure 1)))

   if exit_code /= ExitSuccess
	then throwDyn (PhaseFailed phase_name exit_code)
	else do on verb (putStr "\n")
	        return ()

-----------------------------------------------------------------------------
-- Flags

data OptKind 
	= NoArg (IO ()) 		-- flag with no argument
	| HasArg (String -> IO ())	-- flag has an argument (maybe prefix)
	| SepArg (String -> IO ())	-- flag has a separate argument
	| Prefix (String -> IO ())	-- flag is a prefix only
	| OptPrefix (String -> IO ())   -- flag may be a prefix
	| AnySuffix (String -> IO ())   -- flag is a prefix, pass whole arg to fn
	| PassFlag  (String -> IO ())   -- flag with no arg, pass flag to fn

opts = 
  [  ------- help -------------------------------------------------------
     ( "?"    		, NoArg long_usage)
  ,  ( "-help"		, NoArg long_usage)
  

      ------- version ----------------------------------------------------
  ,  ( "-version"	, NoArg (do hPutStrLn stderr (_ProjectName
				      ++ ", version " ++ _ProjectVersion
				      ++ ", patchlevel " ++ _ProjectPatchLevel)
				    exitWith ExitSuccess))

      ------- verbosity ----------------------------------------------------
  ,  ( "v"		, NoArg (writeIORef verbose True) )
  ,  ( "n"              , NoArg (writeIORef dry_run True) )

	------- recompilation checker --------------------------------------
  ,  ( "recomp"		, NoArg (writeIORef recomp True) )
  ,  ( "no-recomp"  	, NoArg (writeIORef recomp False) )

	------- ways --------------------------------------------------------
  ,  ( "prof"		, NoArg (add ways WayProf) )
  ,  ( "unreg"		, NoArg (add ways WayUnreg) )
  ,  ( "ticky"		, NoArg (add ways WayTicky) )
  ,  ( "parallel"	, NoArg (add ways WayPar) )
  ,  ( "gransim"	, NoArg (add ways WayGran) )
  ,  ( "smp"		, NoArg (add ways WaySMP) )
  ,  ( "debug"		, NoArg (add ways WayDebug) )
 	-- ToDo: user ways

	------- Interface files ---------------------------------------------
  ,  ( "hi"		, NoArg (writeIORef produceHi True) )
  ,  ( "nohi"		, NoArg (writeIORef produceHi False) )
  ,  ( "hi-diffs"	, NoArg (writeIORef hi_diffs  NormalHiDiffs) )
  ,  ( "no-hi-diffs"	, NoArg (writeIORef hi_diffs  NoHiDiffs) )
  ,  ( "hi-diffs-with-usages" , NoArg (writeIORef hi_diffs UsageHiDiffs) )
  ,  ( "keep-hi-diffs"	, NoArg (writeIORef keep_hi_diffs True) )
	--"hi-with-*"    -> hiw <- readIORef hi_with  (ToDo)

	--------- Profiling --------------------------------------------------
  ,  ( "auto-dicts"	, NoArg (add opt_C "-fauto-sccs-on-dicts") )
  ,  ( "auto-all"	, NoArg (add opt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "auto"		, NoArg (add opt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "caf-all"	, NoArg (add opt_C "-fauto-sccs-on-individual-cafs") )
         -- "ignore-sccs"  doesn't work  (ToDo)

	------- Miscellaneous -----------------------------------------------
  ,  ( "cpp"		, NoArg (writeIORef cpp_flag True) )
  ,  ( "#include"	, SepArg (add cmdline_hc_includes) )

	------- Output Redirection ------------------------------------------
  ,  ( "odir"		, HasArg (writeIORef output_dir  . Just) )
  ,  ( "o"		, SepArg (writeIORef output_file . Just) )
  ,  ( "osuf"		, HasArg (writeIORef output_suf  . Just) )
  ,  ( "hisuf"		, HasArg (writeIORef hi_suf) )
  ,  ( "tmpdir"		, HasArg (writeIORef tmp_prefix  . (++ "/")) )
  ,  ( "ohi"		, HasArg (\s -> case s of 
					  "-" -> writeIORef hi_on_stdout True
					  _   -> writeIORef output_hi (Just s)) )
	-- -odump?

  ,  ( "keep-hc-file"   , AnySuffix (\_ -> writeIORef keep_hc_files True) )
  ,  ( "keep-s-file"    , AnySuffix (\_ -> writeIORef keep_s_files  True) )
  ,  ( "keep-raw-s-file", AnySuffix (\_ -> writeIORef keep_raw_s_files  True) )

  ,  ( "split-objs"	, NoArg (if can_split
				    then do writeIORef split_object_files True
				            writeIORef hsc_lang HscC
					    add opt_C "-fglobalise-toplev-names"
					    add opt_c "-DUSE_SPLIT_MARKERS"
				    else hPutStrLn stderr
					    "warning: don't know how to  split \
					    \object files on this architecture"
				) )
  
	------- Include/Import Paths ----------------------------------------
  ,  ( "i"		, OptPrefix augment_import_paths )
  ,  ( "I" 		, Prefix augment_include_paths )

	------- Libraries ---------------------------------------------------
  ,  ( "L"		, Prefix augment_library_paths )
  ,  ( "l"		, Prefix (add cmdline_libraries) )

        ------- Packages ----------------------------------------------------
  ,  ( "package-name"   , HasArg (\s -> add opt_C ("-inpackage="++s)) )

  ,  ( "package"        , HasArg (addPackage) )
  ,  ( "syslib"         , HasArg (addPackage) )	-- for compatibility w/ old vsns

        ------- Specific phases  --------------------------------------------
  ,  ( "pgmdep"         , HasArg (writeIORef pgm_dep) )
  ,  ( "pgmL"           , HasArg (writeIORef pgm_L) )
  ,  ( "pgmP"           , HasArg (writeIORef pgm_P) )
  ,  ( "pgmC"           , HasArg (writeIORef pgm_C) )
  ,  ( "pgmc"           , HasArg (writeIORef pgm_c) )
  ,  ( "pgmm"           , HasArg (writeIORef pgm_m) )
  ,  ( "pgms"           , HasArg (writeIORef pgm_s) )
  ,  ( "pgma"           , HasArg (writeIORef pgm_a) )
  ,  ( "pgml"           , HasArg (writeIORef pgm_l) )

  ,  ( "optdep"		, HasArg (add opt_dep) )
  ,  ( "optL"		, HasArg (add opt_L) )
  ,  ( "optP"		, HasArg (add opt_P) )
  ,  ( "optC"		, HasArg (add opt_C) )
  ,  ( "optCrts"        , HasArg (add opt_Crts) )
  ,  ( "optc"		, HasArg (add opt_c) )
  ,  ( "optm"		, HasArg (add opt_m) )
  ,  ( "opta"		, HasArg (add opt_a) )
  ,  ( "optl"		, HasArg (add opt_l) )
  ,  ( "optdll"		, HasArg (add opt_dll) )

	------ HsCpp opts ---------------------------------------------------
  ,  ( "D"		, Prefix (\s -> add opt_P ("-D'"++s++"'") ) )
  ,  ( "U"		, Prefix (\s -> add opt_P ("-U'"++s++"'") ) )

	------ Warning opts -------------------------------------------------
  ,  ( "W"		, NoArg (writeIORef warning_opt W_))
  ,  ( "Wall"		, NoArg (writeIORef warning_opt W_all))
  ,  ( "Wnot"		, NoArg (writeIORef warning_opt W_not))
  ,  ( "w"		, NoArg (writeIORef warning_opt W_not))

	----- Linker --------------------------------------------------------
  ,  ( "static" 	, NoArg (writeIORef static True) )

        ------ Compiler RTS options -----------------------------------------
  ,  ( "H"                 , HasArg (sizeOpt specific_heap_size) )
  ,  ( "K"                 , HasArg (sizeOpt specific_stack_size) )
  ,  ( "Rscale-sizes"	   , HasArg (floatOpt scale_sizes_by) )
  ,  ( "Rghc-timing" 	   , NoArg (writeIORef collect_ghc_timing True) )

	------ Debugging ----------------------------------------------------
  ,  ( "dstg-stats"	   , NoArg (writeIORef opt_StgStats True) )

  ,  ( "dno-"		   , Prefix (\s -> add anti_opt_C ("-d"++s)) )
  ,  ( "d"		   , AnySuffix (add opt_C) )

	------ Machine dependant (-m<blah>) stuff ---------------------------

  ,  ( "monly-2-regs", 		NoArg (writeIORef stolen_x86_regs 2) )
  ,  ( "monly-3-regs", 		NoArg (writeIORef stolen_x86_regs 3) )
  ,  ( "monly-4-regs", 		NoArg (writeIORef stolen_x86_regs 4) )

        ------ Compiler flags -----------------------------------------------
  ,  ( "O2-for-C"	   , NoArg (writeIORef opt_minus_o2_for_C True) )
  ,  ( "O"		   , OptPrefix (setOptLevel) )

  ,  ( "fglasgow-exts-no-lang", NoArg ( do add opt_C "-fglasgow-exts") )

  ,  ( "fglasgow-exts"     , NoArg (do add opt_C "-fglasgow-exts"
				       addPackage "lang"))

  ,  ( "fasm"		   , OptPrefix (\_ -> writeIORef hsc_lang HscAsm) )

  ,  ( "fvia-C"		   , NoArg (writeIORef hsc_lang HscC) )

  ,  ( "fno-asm-mangling"  , NoArg (writeIORef do_asm_mangling True) )

  ,  ( "fmax-simplifier-iterations", 
		Prefix (writeIORef opt_MaxSimplifierIterations . read) )

  ,  ( "fusagesp",		NoArg (do writeIORef opt_UsageSPInf True
					  add opt_C "-fusagesp-on") )

	-- flags that are "active negatives"
  ,  ( "fno-implicit-prelude"	, PassFlag (add opt_C) )
  ,  ( "fno-prune-tydecls"	, PassFlag (add opt_C) )
  ,  ( "fno-prune-instdecls"	, PassFlag (add opt_C) )
  ,  ( "fno-pre-inlining"	, PassFlag (add opt_C) )

	-- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  ,  ( "fno-",			Prefix (\s -> add anti_opt_C ("-f"++s)) )

	-- Pass all remaining "-f<blah>" options to hsc
  ,  ( "f", 			AnySuffix (add opt_C) )
  ]

-----------------------------------------------------------------------------
-- Process command-line  

processArgs :: [String] -> [String] -> IO [String]  -- returns spare args
processArgs [] spare = return (reverse spare)
processArgs args@(('-':_):_) spare = do
  args' <- processOneArg args
  processArgs args' spare
processArgs (arg:args) spare = 
  processArgs args (arg:spare)

processOneArg :: [String] -> IO [String]
processOneArg (('-':arg):args) = do
  let (rest,action) = findArg arg
      dash_arg = '-':arg
  case action of

	NoArg  io -> 
		if rest == ""
			then io >> return args
			else throwDyn (UnknownFlag dash_arg)

	HasArg fio -> 
		if rest /= "" 
			then fio rest >> return args
			else case args of
				[] -> throwDyn (UnknownFlag dash_arg)
				(arg1:args1) -> fio arg1 >> return args1

	SepArg fio -> 
		case args of
			[] -> throwDyn (UnknownFlag dash_arg)
			(arg1:args1) -> fio arg1 >> return args1

	Prefix fio -> 
		if rest /= ""
			then fio rest >> return args
			else throwDyn (UnknownFlag dash_arg)
	
	OptPrefix fio -> fio rest >> return args

	AnySuffix fio -> fio ('-':arg) >> return args

	PassFlag fio  -> 
		if rest /= ""
			then throwDyn (UnknownFlag dash_arg)
			else fio ('-':arg) >> return args

findArg :: String -> (String,OptKind)
findArg arg
  = case [ (rest,k) | (pat,k) <- opts, 
		      Just rest <- [my_prefix_match pat arg],
		      is_prefix k || null rest ] of
	[] -> throwDyn (UnknownFlag ('-':arg))
	(one:_) -> one

is_prefix (NoArg _) = False
is_prefix (SepArg _) = False
is_prefix (PassFlag _) = False
is_prefix _ = True

-----------------------------------------------------------------------------
-- convert sizes like "3.5M" into integers

sizeOpt :: IORef Integer -> String -> IO ()
sizeOpt ref str
  | c == ""		 = writeSizeOpt	ref (truncate n)
  | c == "K" || c == "k" = writeSizeOpt	ref (truncate (n * 1000))
  | c == "M" || c == "m" = writeSizeOpt	ref (truncate (n * 1000 * 1000))
  | c == "G" || c == "g" = writeSizeOpt	ref (truncate (n * 1000 * 1000 * 1000))
  | otherwise            = throwDyn (UnknownFlag str)
  where (m, c) = span pred str
        n      = read m  :: Double
	pred c = isDigit c || c == '.'

writeSizeOpt :: IORef Integer -> Integer -> IO ()
writeSizeOpt ref new = do
  current <- readIORef ref
  if (new > current) 
	then writeIORef ref new
	else return ()

floatOpt :: IORef Double -> String -> IO ()
floatOpt ref str
  = writeIORef ref (read str :: Double)

-----------------------------------------------------------------------------
-- Finding files in the installation

GLOBAL_VAR(topDir, _libdir, String)

	-- grab the last -B option on the command line, and
	-- set topDir to its value.
setTopDir :: [String] -> IO [String]
setTopDir args = do
  let (minusbs, others) = partition (prefixMatch "-B") args
  (case minusbs of
    []   -> writeIORef topDir _libdir
    some -> writeIORef topDir (drop 2 (last some)))
  return others

findFile name alt_path = unsafePerformIO (do
  top_dir <- readIORef topDir
  let installed_file = top_dir ++ '/':name
  let inplace_file   = top_dir ++ '/':_CURRENT_DIR ++ '/':alt_path
  b <- fileExist inplace_file
  if b  then return inplace_file
	else return installed_file
 )

-----------------------------------------------------------------------------
-- Utils

my_partition :: (a -> Maybe b) -> [a] -> ([b],[a])
my_partition p [] = ([],[])
my_partition p (a:as)
  = let (bs,cs) = my_partition p as in
    case p a of
	Nothing -> (bs,a:cs)
	Just b  -> (b:bs,cs)

my_prefix_match :: String -> String -> Maybe String
my_prefix_match [] rest = Just rest
my_prefix_match (p:pat) [] = Nothing
my_prefix_match (p:pat) (r:rest)
  | p == r    = my_prefix_match pat rest
  | otherwise = Nothing

prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] str = True
prefixMatch pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
			  | otherwise = False

postfixMatch :: String -> String -> Bool
postfixMatch pat str = prefixMatch (reverse pat) (reverse str)

later = flip finally

on b io = if b then io >> return (error "on") else return (error "on")

my_catch = flip catchAllIO
my_catchDyn = flip catchDyn

global :: a -> IORef a
global a = unsafePerformIO (newIORef a)

split_filename :: String -> (String,String)
split_filename f = (reverse rev_basename, reverse rev_ext)
  where (rev_ext, '.':rev_basename) = span ('.' /=) (reverse f)

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

add :: IORef [a] -> a -> IO ()
add var x = do
  xs <- readIORef var
  writeIORef var (x:xs)

remove_suffix :: String -> Char -> String
remove_suffix s c 
  | null pre  = reverse suf
  | otherwise = reverse pre
  where (suf,pre) = break (==c) (reverse s)

drop_longest_prefix :: String -> Char -> String
drop_longest_prefix s c = reverse suf
  where (suf,pre) = break (==c) (reverse s)

take_longest_prefix :: String -> Char -> String
take_longest_prefix s c = reverse pre
  where (suf,pre) = break (==c) (reverse s)

newsuf :: String -> String -> String
newsuf suf s = remove_suffix s '.' ++ suf

newdir :: String -> String -> String
newdir dir s = dir ++ '/':drop_longest_prefix s '/'
