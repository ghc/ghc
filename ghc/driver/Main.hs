{-# OPTIONS -W #-}
-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.49 2000/08/04 09:45:20 simonmar Exp $
--
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

-- with path so that ghc -M can find config.h
#include "../includes/config.h"

module Main (main) where

import GetImports
import Package
import Config

import RegexString
import Concurrent
#ifndef mingw32_TARGET_OS
import Posix
#endif
import Directory
import IOExts
import Exception
import Dynamic

import IO
import Monad
import List
import System
import Maybe
import Char

#ifdef mingw32_TARGET_OS
foreign import "_getpid" getProcessID :: IO Int 
#endif

#define GLOBAL_VAR(name,value,ty)  \
name = global (value) :: IORef (ty); \
{-# NOINLINE name #-}

-----------------------------------------------------------------------------
-- ToDo:

-- certain options in OPTIONS pragmas are persistent through subsequent compilations.
-- new mkdependHS doesn't support all the options that the old one did (-X et al.)
-- time commands when run with -v
-- split marker
-- mkDLL
-- java generation
-- user ways
-- Win32 support: proper signal handling
-- make sure OPTIONS in .hs file propogate to .hc file if -C or -keep-hc-file-too
-- reading the package configuration file is too slow

-----------------------------------------------------------------------------
-- Differences vs. old driver:

-- No more "Enter your Haskell program, end with ^D (on a line of its own):"
-- consistency checking removed (may do this properly later)
-- removed -noC
-- no hi diffs (could be added later)
-- no -Ofile

-----------------------------------------------------------------------------
-- non-configured things

cHaskell1Version = "5" -- i.e., Haskell 98

-----------------------------------------------------------------------------
-- Usage Message

short_usage = "Usage: For basic information, try the `-help' option."
   
long_usage = do
  let usage_file = "ghc-usage.txt"
      usage_path = findFile usage_file (cGHC_DRIVER_DIR ++ '/':usage_file)
  usage <- readFile usage_path
  dump usage
  exitWith ExitSuccess
  where
     dump "" = return ()
     dump ('$':'$':s) = hPutStr stderr get_prog_name >> dump s
     dump (c:s) = hPutChar stderr c >> dump s

version_str = cProjectVersion

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

	-- heap/stack sizes
	specific_heap_size	:: Integer,
	specific_stack_size	:: Integer,
  
	-- misc
	stolen_x86_regs		:: Int,
	excess_precision	:: Bool,
	warning_opt		:: WarningState,
	cmdline_hc_includes	:: [String],

	-- options for a particular phase
	anti_opt_C		:: [String],
	opt_dep			:: [String],
	opt_L			:: [String],
	opt_P			:: [String],
	opt_C			:: [String],
	opt_Crts		:: [String],
	opt_c			:: [String],
	opt_a			:: [String],
	opt_m			:: [String],
	opt_l			:: [String],
	opt_dll			:: [String]
   }

initDriverState = DriverState {
	cpp_flag		= False,
	specific_heap_size	= 6 * 1000 * 1000,
	specific_stack_size	= 1000 * 1000,
	stolen_x86_regs		= 4,
	excess_precision	= False,
	warning_opt		= W_default,
	cmdline_hc_includes	= [],
	anti_opt_C		= [],
	opt_dep			= [],
	opt_L			= [],
	opt_P			= [],
	opt_C			= [],
	opt_Crts		= [],
	opt_c			= [],
	opt_a			= [],
	opt_m			= [],
	opt_l			= [],
	opt_dll			= []
   }
	
GLOBAL_VAR(driver_state, initDriverState, DriverState)

readState :: (DriverState -> a) -> IO a
readState f = readIORef driver_state >>= return . f

updateState :: (DriverState -> DriverState) -> IO ()
updateState f = readIORef driver_state >>= writeIORef driver_state . f

addAntiOpt_C a = updateState (\s -> s{anti_opt_C =  a : anti_opt_C s})
addOpt_dep   a = updateState (\s -> s{opt_dep    =  a : opt_dep    s})
addOpt_L     a = updateState (\s -> s{opt_L      =  a : opt_L      s})
addOpt_P     a = updateState (\s -> s{opt_P      =  a : opt_P      s})
addOpt_C     a = updateState (\s -> s{opt_C      =  a : opt_C      s})
addOpt_Crts  a = updateState (\s -> s{opt_Crts   =  a : opt_Crts   s})
addOpt_c     a = updateState (\s -> s{opt_c      =  a : opt_c      s})
addOpt_a     a = updateState (\s -> s{opt_a      =  a : opt_a      s})
addOpt_m     a = updateState (\s -> s{opt_m      =  a : opt_m      s})
addOpt_l     a = updateState (\s -> s{opt_l      =  a : opt_l      s})
addOpt_dll   a = updateState (\s -> s{opt_dll    =  a : opt_dll    s})

addCmdlineHCInclude a = 
   updateState (\s -> s{cmdline_hc_includes =  a : cmdline_hc_includes s})

	-- we add to the options from the front, so we need to reverse the list
getOpts :: (DriverState -> [a]) -> IO [a]
getOpts opts = readState opts >>= return . reverse

newHeapSize :: Integer -> IO ()
newHeapSize new = updateState 
   (\s -> let current = specific_heap_size s in
	  s{ specific_heap_size = if new > current then new else current })

newStackSize :: Integer -> IO ()
newStackSize new = updateState 
   (\s -> let current = specific_stack_size s in
	  s{ specific_stack_size = if new > current then new else current })

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
	| SplitAs
	| As
	| Ln 
  deriving (Eq)

-----------------------------------------------------------------------------
-- Errors

data BarfKind
  = PhaseFailed String ExitCode
  | Interrupted
  | UsageError String			-- prints the short usage msg after the error
  | OtherError String			-- just prints the error message
  deriving Eq

GLOBAL_VAR(prog_name, "ghc", String)

get_prog_name = unsafePerformIO (readIORef prog_name) -- urk!

instance Show BarfKind where
  showsPrec _ e 
	= showString get_prog_name . showString ": " . showBarf e

showBarf (UsageError str) = showString str . showChar '\n' . showString short_usage
showBarf (OtherError str) = showString str
showBarf (PhaseFailed phase code) = 
	showString phase . showString " failed, code = " . shows code
showBarf (Interrupted) = showString "interrupted"

unknownFlagErr f = throwDyn (UsageError ("unrecognised flag: " ++ f))

barfKindTc = mkTyCon "BarfKind"
instance Typeable BarfKind where
  typeOf _ = mkAppTy barfKindTc []

-----------------------------------------------------------------------------
-- Temporary files

GLOBAL_VAR(files_to_clean, [], [String])
GLOBAL_VAR(keep_tmp_files, False, Bool)

cleanTempFiles :: IO ()
cleanTempFiles = do
  forget_it <- readIORef keep_tmp_files
  unless forget_it $ do

  fs <- readIORef files_to_clean
  verb <- readIORef verbose

  let blowAway f =
	   (do  when verb (hPutStrLn stderr ("removing: " ++ f))
		if '*' `elem` f then system ("rm -f " ++ f) >> return ()
			        else removeFile f)
	    `catchAllIO`
	   (\_ -> when verb (hPutStrLn stderr 
				("warning: can't remove tmp file" ++ f)))
  mapM_ blowAway fs

-----------------------------------------------------------------------------
-- Global compilation flags

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

	-- Keep output from intermediate phases
GLOBAL_VAR(keep_hi_diffs, 	False, 		Bool)
GLOBAL_VAR(keep_hc_files,	False,		Bool)
GLOBAL_VAR(keep_s_files,	False,		Bool)
GLOBAL_VAR(keep_raw_s_files,	False,		Bool)

	-- Misc
GLOBAL_VAR(scale_sizes_by,      1.0,		Double)
GLOBAL_VAR(dry_run, 		False,		Bool)
GLOBAL_VAR(recomp,  		True,		Bool)
GLOBAL_VAR(tmp_prefix, 		cTMPDIR,	String)
#if !defined(HAVE_WIN32_DLL_SUPPORT) || defined(DONT_WANT_WIN32_DLL_SUPPORT)
GLOBAL_VAR(static, 		True,		Bool)
#else
GLOBAL_VAR(static,              False,          Bool)
#endif
GLOBAL_VAR(collect_ghc_timing, 	False,		Bool)
GLOBAL_VAR(do_asm_mangling,	True,		Bool)

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

data HscLang
  = HscC
  | HscAsm
  | HscJava

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

GLOBAL_VAR(package_config, (findFile "package.conf" (cGHC_DRIVER_DIR++"/package.conf.inplace")), String)

listPackages :: IO ()
listPackages = do 
  details <- readIORef package_details
  hPutStr stdout (listPkgs details)
  hPutChar stdout '\n'
  exitWith ExitSuccess

newPackage :: IO ()
newPackage = do
  checkConfigAccess
  details <- readIORef package_details
  hPutStr stdout "Reading package info from stdin... "
  stuff <- getContents
  let new_pkg = read stuff :: (String,Package)
  catchAll new_pkg
  	(\_ -> throwDyn (OtherError "parse error in package info"))
  hPutStrLn stdout "done."
  if (fst new_pkg `elem` map fst details)
	then throwDyn (OtherError ("package `" ++ fst new_pkg ++ 
					"' already installed"))
	else do
  conf_file <- readIORef package_config
  savePackageConfig conf_file
  maybeRestoreOldConfig conf_file $ do
  writeNewConfig conf_file ( ++ [new_pkg])
  exitWith ExitSuccess

deletePackage :: String -> IO ()
deletePackage pkg = do  
  checkConfigAccess
  details <- readIORef package_details
  if (pkg `notElem` map fst details)
	then throwDyn (OtherError ("package `" ++ pkg ++ "' not installed"))
	else do
  conf_file <- readIORef package_config
  savePackageConfig conf_file
  maybeRestoreOldConfig conf_file $ do
  writeNewConfig conf_file (filter ((/= pkg) . fst))
  exitWith ExitSuccess

checkConfigAccess :: IO ()
checkConfigAccess = do
  conf_file <- readIORef package_config
  access <- getPermissions conf_file
  unless (writable access)
	(throwDyn (OtherError "you don't have permission to modify the package configuration file"))

maybeRestoreOldConfig :: String -> IO () -> IO ()
maybeRestoreOldConfig conf_file io
  = catchAllIO io (\e -> do
        hPutStr stdout "\nWARNING: an error was encountered while the new \n\ 
        	       \configuration was being written.  Attempting to \n\ 
        	       \restore the old configuration... "
        system ("cp " ++ conf_file ++ ".old " ++ conf_file)
        hPutStrLn stdout "done."
	throw e
    )

writeNewConfig :: String -> ([(String,Package)] -> [(String,Package)]) -> IO ()
writeNewConfig conf_file fn = do
  hPutStr stdout "Writing new package config file... "
  old_details <- readIORef package_details
  h <- openFile conf_file WriteMode
  hPutStr h (dumpPackages (fn old_details))
  hClose h
  hPutStrLn stdout "done."

savePackageConfig :: String -> IO ()
savePackageConfig conf_file = do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  system ("mv " ++ conf_file ++ " " ++ conf_file ++ ".old")
  hPutStrLn stdout "done."

-- package list is maintained in dependency order
packages = global ["std", "rts", "gmp"] :: IORef [String]
-- comma in value, so can't use macro, grrr
{-# NOINLINE packages #-}

addPackage :: String -> IO ()
addPackage package
  = do pkg_details <- readIORef package_details
       case lookup package pkg_details of
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
  return [ pkg | p <- ps, Just pkg <- [ lookup p pkg_details ] ]

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
	[ "-fstack-check"
	, "-fparallel"
	, "-D__PARALLEL_HASKELL__"
	, "-optc-DPAR"
	, "-package concurrent"
	, "-fvia-C" ]),

    (WayGran, Way  "mg" "Gransim" 
	[ "-fstack-check"
	, "-fgransim"
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

GLOBAL_VAR(pgm_L,   findFile "unlit"      cGHC_UNLIT,      String)
GLOBAL_VAR(pgm_P,   cRAWCPP,				   String)
GLOBAL_VAR(pgm_C,   findFile "hsc"        cGHC_HSC,        String)
GLOBAL_VAR(pgm_c,   cGCC,	      	     	      	   String)
GLOBAL_VAR(pgm_m,   findFile "ghc-asm"    cGHC_MANGLER,    String)
GLOBAL_VAR(pgm_s,   findFile "ghc-split"  cGHC_SPLIT,      String)
GLOBAL_VAR(pgm_a,   cGCC,	      	     	           String)
GLOBAL_VAR(pgm_l,   cGCC,       	     	           String)

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
-- Build the Hsc command line

build_hsc_opts :: IO [String]
build_hsc_opts = do
  opt_C_ <- getOpts opt_C		-- misc hsc opts

	-- warnings
  warn_level <- readState warning_opt
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
	    _ -> error "unknown opt level"
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
  let hi_vers = "-fhi-version="++cProjectVersionInt
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
  heap  <- readState specific_heap_size
  stack <- readState specific_stack_size
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
       catchIO justIoErrors (look h)
	  (\e -> if isEOFError e then return [] else ioError e)
  where
	look h = do
	    l <- hGetLine h
	    case () of
		() | null l -> look h
		   | prefixMatch "#" l -> look h
		   | prefixMatch "{-# LINE" l -> look h
		   | Just (opts:_) <- matchRegex optionRegex l
			-> return (words opts)
		   | otherwise -> return []

optionRegex = mkRegex "{-#[ \t]+OPTIONS[ \t]+(.*)#-}"

-----------------------------------------------------------------------------
-- Main loop

get_source_files :: [String] -> ([String],[String])
get_source_files = partition (('-' /=) . head)

main =
  -- all error messages are propagated as exceptions
  my_catchDyn (\dyn -> case dyn of
			  PhaseFailed _phase code -> exitWith code
			  Interrupted -> exitWith (ExitFailure 1)
			  _ -> do hPutStrLn stderr (show (dyn :: BarfKind))
			          exitWith (ExitFailure 1)
	      ) $

  later cleanTempFiles $
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

  do
	-- install signal handlers
   main_thread <- myThreadId

#ifndef mingw32_TARGET_OS
   let sig_handler = Catch (raiseInThread main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing
#endif

   pgm    <- getProgName
   writeIORef prog_name pgm

   argv   <- getArgs

	-- grab any -B options from the command line first
   argv'  <- setTopDir argv

	-- read the package configuration
   conf_file <- readIORef package_config
   contents <- readFile conf_file
   writeIORef package_details (read contents)

	-- find the phase to stop after (i.e. -E, -C, -c, -S flags)
   (flags2, todo, stop_flag) <- getToDo argv'

	-- process all the other arguments, and get the source files
   srcs <- processArgs driver_opts flags2 []

	-- find the build tag, and re-process the build-specific options
   more_opts <- findBuildTag
   _ <- processArgs driver_opts more_opts []

	-- get the -v flag
   verb <- readIORef verbose

   when verb (hPutStrLn stderr ("Using package config file: " ++ conf_file))

	-- mkdependHS is special
   when (todo == DoMkDependHS) beginMkDependHS

	-- for each source file, find which phases to run
   pipelines <- mapM (genPipeline todo stop_flag) srcs
   let src_pipelines = zip srcs pipelines

   o_file <- readIORef output_file
   if isJust o_file && todo /= DoLink && length srcs > 1
	then throwDyn (UsageError "can't apply -o option to multiple source files")
	else do

   if null srcs then throwDyn (UsageError "no input files") else do

	-- save the flag state, because this could be modified by OPTIONS pragmas
	-- during the compilation, and we'll need to restore it before starting
	-- the next compilation.
   saved_driver_state <- readIORef driver_state

   let compileFile (src, phases) = do
	  r <- run_pipeline phases src (todo==DoLink) True orig_base orig_suff
	  writeIORef driver_state saved_driver_state
	  return r
	  where (orig_base, orig_suff) = splitFilename src

   o_files <- mapM compileFile src_pipelines

   when (todo == DoMkDependHS) endMkDependHS

   when (todo == DoLink) (do_link o_files)


-----------------------------------------------------------------------------
-- Which phase to stop at

data ToDo = DoMkDependHS | DoMkDLL | StopBefore Phase | DoLink
  deriving (Eq)

todoFlag :: String -> Maybe ToDo
todoFlag "-M" = Just $ DoMkDependHS
todoFlag "-E" = Just $ StopBefore Hsc
todoFlag "-C" = Just $ StopBefore HCc
todoFlag "-S" = Just $ StopBefore As
todoFlag "-c" = Just $ StopBefore Ln
todoFlag _    = Nothing

getToDo :: [String]
	 -> IO ( [String]   -- rest of command line
	       , ToDo	    -- phase to stop at
	       , String	    -- "stop at" flag
	       )
getToDo flags 
  = case my_partition todoFlag flags of
	([]   , rest) -> return (rest, DoLink,  "") -- default is to do linking
	([(flag,one)], rest) -> return (rest, one, flag)
	(_    , _   ) -> 
	  throwDyn (OtherError "only one of the flags -M, -E, -C, -S, -c is allowed")

-----------------------------------------------------------------------------
-- genPipeline
--
-- Herein is all the magic about which phases to run in which order, whether
-- the intermediate files should be in /tmp or in the current directory,
-- what the suffix of the intermediate files should be, etc.

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

data IntermediateFileType
  = Temporary
  | Persistent
  deriving (Eq)

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase "lhs"   = Unlit
startPhase "hs"    = Cpp
startPhase "hc"    = HCc
startPhase "c"     = Cc
startPhase "raw_s" = Mangle
startPhase "s"     = As
startPhase "S"     = As
startPhase "o"     = Ln     
startPhase _       = Ln	   -- all unknown file types

genPipeline
   :: ToDo		-- when to stop
   -> String		-- "stop after" flag (for error messages)
   -> String		-- original filename
   -> IO [ 		-- list of phases to run for this file
	     (Phase,
	      IntermediateFileType,  -- keep the output from this phase?
	      String)   	     -- output file suffix
         ]	

genPipeline todo stop_flag filename
 = do
   split      <- readIORef split_object_files
   mangle     <- readIORef do_asm_mangling
   lang       <- readIORef hsc_lang
   keep_hc    <- readIORef keep_hc_files
   keep_raw_s <- readIORef keep_raw_s_files
   keep_s     <- readIORef keep_s_files

   let
   ----------- -----  ----   ---   --   --  -  -  -
    start_phase = startPhase suffix

    (_basename, suffix) = splitFilename filename

    haskell_ish_file = suffix `elem` [ "hs", "lhs", "hc" ]
    c_ish_file       = suffix `elem` [ "c", "s", "S" ]  -- maybe .cc et al.??

	-- hack for .hc files
    real_lang | suffix == "hc" = HscC
	      | otherwise      = lang

    pipeline
      | todo == DoMkDependHS = [ Unlit, Cpp, MkDependHS ]

      | haskell_ish_file = 
       case real_lang of
	HscC    | split && mangle -> [ Unlit, Cpp, Hsc, HCc, Mangle, 
					SplitMangle, SplitAs ]
	        | mangle          -> [ Unlit, Cpp, Hsc, HCc, Mangle, As ]
	        | split	       	  -> not_valid
	        | otherwise       -> [ Unlit, Cpp, Hsc, HCc, As ]

	HscAsm  | split           -> [ Unlit, Cpp, Hsc, SplitMangle, SplitAs ]
	        | otherwise       -> [ Unlit, Cpp, Hsc, As ]

	HscJava	| split	          -> not_valid
		| otherwise       -> error "not implemented: compiling via Java"

      | c_ish_file      = [ Cc, As ]

      | otherwise       = [ ]  -- just pass this file through to the linker

	-- ToDo: this is somewhat cryptic
    not_valid = throwDyn (OtherError ("invalid option combination"))
   ----------- -----  ----   ---   --   --  -  -  -

	-- this shouldn't happen.
   if start_phase /= Ln && start_phase `notElem` pipeline
	then throwDyn (OtherError ("can't find starting phase for "
				    ++ filename))
	else do

	-- if we can't find the phase we're supposed to stop before,
	-- something has gone wrong.
   case todo of
	StopBefore phase -> 
	   when (phase /= Ln 
		 && phase `notElem` pipeline
	   	 && not (phase == As && SplitAs `elem` pipeline)) $
	      throwDyn (OtherError 
		("flag " ++ stop_flag
		 ++ " is incompatible with source file `" ++ filename ++ "'"))
	_ -> return ()

   let
   ----------- -----  ----   ---   --   --  -  -  -
      annotatePipeline
	 :: [Phase]		-- raw pipeline
	 -> Phase		-- phase to stop before
     	 -> [(Phase, IntermediateFileType, String{-file extension-})]
      annotatePipeline []     _    = []
      annotatePipeline (Ln:_) _    = []
      annotatePipeline (phase:next_phase:ps) stop = 
     	  (phase, keep_this_output, phase_input_ext next_phase)
	     : annotatePipeline (next_phase:ps) stop
     	  where
     		keep_this_output
     		     | next_phase == stop = Persistent
     		     | otherwise =
     			case next_phase of
     			     Ln -> Persistent
     			     Mangle | keep_raw_s -> Persistent
     			     As     | keep_s     -> Persistent
     			     HCc    | keep_hc    -> Persistent
     			     _other              -> Temporary

	-- add information about output files to the pipeline
	-- the suffix on an output file is determined by the next phase
	-- in the pipeline, so we add linking to the end of the pipeline
	-- to force the output from the final phase to be a .o file.
      stop_phase = case todo of StopBefore phase -> phase
				DoLink           -> Ln
      annotated_pipeline = annotatePipeline (pipeline ++ [ Ln ]) stop_phase

      phase_ne p (p1,_,_) = (p1 /= p)
   ----------- -----  ----   ---   --   --  -  -  -

   return $
     dropWhile (phase_ne start_phase) . 
	foldr (\p ps -> if phase_ne stop_phase p then p:ps else [])  []
		$ annotated_pipeline



-- the output suffix for a given phase is uniquely determined by
-- the input requirements of the next phase.
phase_input_ext Unlit       = "lhs"
phase_input_ext	Cpp         = "lpp"
phase_input_ext	Hsc         = "cpp"
phase_input_ext	HCc         = "hc"
phase_input_ext Cc          = "c"
phase_input_ext	Mangle      = "raw_s"
phase_input_ext	SplitMangle = "split_s"	-- not really generated
phase_input_ext	As          = "s"
phase_input_ext	SplitAs     = "split_s" -- not really generated
phase_input_ext	Ln          = "o"
phase_input_ext MkDependHS  = "dep"

run_pipeline
  :: [ (Phase, IntermediateFileType, String) ] -- phases to run
  -> String			-- input file
  -> Bool			-- doing linking afterward?
  -> Bool			-- take into account -o when generating output?
  -> String			-- original basename (eg. Main)
  -> String			-- original suffix   (eg. hs)
  -> IO String			-- return final filename

run_pipeline [] input_fn _ _ _ _ = return input_fn
run_pipeline ((phase, keep, o_suffix):phases) 
	input_fn do_linking use_ofile orig_basename orig_suffix
  = do

     output_fn <- 
	(if null phases && not do_linking && use_ofile
	    then do o_file <- readIORef output_file
		    case o_file of 
		        Just s  -> return s
			Nothing -> do
			    f <- odir_ify (orig_basename ++ '.':o_suffix)
			    osuf_ify f

	    else if keep == Persistent
			then odir_ify (orig_basename ++ '.':o_suffix)
			else do filename <- newTempName o_suffix
				add files_to_clean filename
				return filename
	)

     run_phase phase orig_basename orig_suffix input_fn output_fn

	-- sadly, ghc -E is supposed to write the file to stdout.  We
	-- generate <file>.cpp, so we also have to cat the file here.
     when (null phases && phase == Cpp) $
	run_something "Dump pre-processed file to stdout"
		      ("cat " ++ output_fn)

     run_pipeline phases output_fn do_linking use_ofile orig_basename orig_suffix


-- find a temporary name that doesn't already exist.
newTempName :: String -> IO String
newTempName extn = do
  x <- getProcessID
  tmp_dir <- readIORef tmp_prefix 
  findTempName tmp_dir x
  where findTempName tmp_dir x = do
  	   let filename = tmp_dir ++ "/ghc" ++ show x ++ '.':extn
  	   b  <- doesFileExist filename
	   if b then findTempName tmp_dir (x+1)
		else return filename

-------------------------------------------------------------------------------
-- mkdependHS

	-- flags
GLOBAL_VAR(dep_makefile, 	"Makefile", String);
GLOBAL_VAR(dep_include_prelude, False, Bool);
GLOBAL_VAR(dep_ignore_dirs,	[], [String]);
GLOBAL_VAR(dep_suffixes,	[], [String]);
GLOBAL_VAR(dep_warnings,	True, Bool);

	-- global vars
GLOBAL_VAR(dep_makefile_hdl,   	error "dep_makefile_hdl", Maybe Handle);
GLOBAL_VAR(dep_tmp_file,       	error "dep_tmp_file", String);
GLOBAL_VAR(dep_tmp_hdl,        	error "dep_tmp_hdl", Handle);
GLOBAL_VAR(dep_dir_contents,   	error "dep_dir_contents", [(String,[String])]);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = [
   (  "s", 			SepArg (add dep_suffixes) ),
   (  "f", 			SepArg (writeIORef dep_makefile) ),
   (  "w", 			NoArg (writeIORef dep_warnings False) ),
   (  "-include-prelude",  	NoArg (writeIORef dep_include_prelude True) ),
   (  "X", 			Prefix (addToDirList dep_ignore_dirs) ),
   (  "-exclude-directory=",	Prefix (addToDirList dep_ignore_dirs) )
 ]

beginMkDependHS :: IO ()
beginMkDependHS = do

  	-- slurp in the mkdependHS-style options
  flags <- getOpts opt_dep
  _ <- processArgs dep_opts flags []

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  dep_file <- newTempName "dep"
  add files_to_clean dep_file
  writeIORef dep_tmp_file dep_file
  tmp_hdl <- openFile dep_file WriteMode
  writeIORef dep_tmp_hdl tmp_hdl

  	-- open the makefile
  makefile <- readIORef dep_makefile
  exists <- doesFileExist makefile
  if not exists
	then do 
	   writeIORef dep_makefile_hdl Nothing
	   return ()

	else do
  	   makefile_hdl <- openFile makefile ReadMode
  	   writeIORef dep_makefile_hdl (Just makefile_hdl)

		-- slurp through until we get the magic start string,
		-- copying the contents into dep_makefile
  	   let slurp = do
		l <- hGetLine makefile_hdl
		if (l == depStartMarker)
			then return ()
			else do hPutStrLn tmp_hdl l; slurp
	 
		-- slurp through until we get the magic end marker,
		-- throwing away the contents
  	   let chuck = do
		l <- hGetLine makefile_hdl
		if (l == depEndMarker)
			then return ()
			else chuck
	 
	   catchIO justIoErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)
	   catchIO justIoErrors chuck
		(\e -> if isEOFError e then return () else ioError e)


	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  	-- cache the contents of all the import directories, for future
	-- reference.
  import_dirs <- readIORef import_paths
  pkg_import_dirs <- getPackageImportPath
  import_dir_contents <- mapM getDirectoryContents import_dirs
  pkg_import_dir_contents <- mapM getDirectoryContents pkg_import_dirs
  writeIORef dep_dir_contents 
	(zip import_dirs import_dir_contents ++
  	 zip pkg_import_dirs pkg_import_dir_contents)

	-- ignore packages unless --include-prelude is on
  include_prelude <- readIORef dep_include_prelude
  when (not include_prelude) $
    mapM_ (add dep_ignore_dirs) pkg_import_dirs

  return ()


endMkDependHS :: IO ()
endMkDependHS = do
  makefile     <- readIORef dep_makefile
  makefile_hdl <- readIORef dep_makefile_hdl
  tmp_file     <- readIORef dep_tmp_file
  tmp_hdl      <- readIORef dep_tmp_hdl

	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

	  -- slurp the rest of the orignal makefile and copy it into the output
  	let slurp = do
		l <- hGetLine hdl
		hPutStrLn tmp_hdl l
		slurp
	 
  	catchIO justIoErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)

	hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

	-- create a backup of the original makefile
  when (isJust makefile_hdl) $
     run_something ("Backing up " ++ makefile)
	(unwords [ "cp", makefile, makefile++".bak" ])

  	-- copy the new makefile in place
  run_something "Installing new makefile"
	(unwords [ "cp", tmp_file, makefile ])


findDependency :: String -> Import -> IO (Maybe (String, Bool))
findDependency mod imp = do
   dir_contents <- readIORef dep_dir_contents
   ignore_dirs  <- readIORef dep_ignore_dirs
   hisuf <- readIORef hi_suf

   let
     (imp_mod, is_source) = 
	case imp of
	   Normal str -> (str, False)
	   Source str -> (str, True )	

     imp_hi = imp_mod ++ '.':hisuf
     imp_hiboot = imp_mod ++ ".hi-boot"
     imp_hiboot_v = imp_mod ++ ".hi-boot-" ++ cHscIfaceFileVersion
     imp_hs = imp_mod ++ ".hs"
     imp_lhs = imp_mod ++ ".lhs"

     deps | is_source = [ imp_hiboot_v, imp_hiboot, imp_hs, imp_lhs ]
     	  | otherwise = [ imp_hi, imp_hs, imp_lhs ]

     search [] = throwDyn (OtherError ("can't find one of the following: " ++
				      unwords (map (\d -> '`': d ++ "'") deps) ++
				      " (imported from `" ++ mod ++ "')"))
     search ((dir, contents) : dirs)
	   | null present = search dirs
	   | otherwise = 
		if dir `elem` ignore_dirs 
			then return Nothing
			else if is_source
				then if dep /= imp_hiboot_v 
					then return (Just (dir++'/':imp_hiboot, False))	
					else return (Just (dir++'/':dep, False))	
				else return (Just (dir++'/':imp_hi, not is_source))
	   where
		present = filter (`elem` contents) deps
		dep     = head present
 
   -- in
   search dir_contents


-------------------------------------------------------------------------------
-- Unlit phase 

run_phase Unlit _basename _suff input_fn output_fn
  = do unlit <- readIORef pgm_L
       unlit_flags <- getOpts opt_L
       run_something "Literate pre-processor"
	  ("echo '# 1 \"" ++input_fn++"\"' > "++output_fn++" && "
	   ++ unlit ++ ' ':input_fn ++ " - >> " ++ output_fn)

-------------------------------------------------------------------------------
-- Cpp phase 

run_phase Cpp _basename _suff input_fn output_fn
  = do src_opts <- getOptionsFromSource input_fn
	-- ToDo: this is *wrong* if we're processing more than one file:
	-- the OPTIONS will persist through the subsequent compilations.
       _ <- processArgs driver_opts src_opts []

       do_cpp <- readState cpp_flag
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
		    ++ [ "-x", "c", input_fn, ">>", output_fn ]
		   ))
	  else do
	    run_something "Inefective C pre-processor"
	           ("echo '{-# LINE 1 \""  ++ input_fn ++ "\" -}' > " 
		    ++ output_fn ++ " && cat " ++ input_fn
		    ++ " >> " ++ output_fn)

-----------------------------------------------------------------------------
-- MkDependHS phase

run_phase MkDependHS basename suff input_fn _output_fn = do 
   src <- readFile input_fn
   let imports = getImports src

   deps <- mapM (findDependency basename) imports

   osuf_opt <- readIORef output_suf
   let osuf = case osuf_opt of
			Nothing -> "o"
			Just s  -> s

   extra_suffixes <- readIORef dep_suffixes
   let suffixes = osuf : map (++ ('_':osuf)) extra_suffixes
       ofiles = map (\suf -> basename ++ '.':suf) suffixes
   	   
   objs <- mapM odir_ify ofiles
   
   hdl <- readIORef dep_tmp_hdl

	-- std dependeny of the object(s) on the source file
   hPutStrLn hdl (unwords objs ++ " : " ++ basename ++ '.':suff)

   let genDep (dep, False {- not an hi file -}) = 
	  hPutStrLn hdl (unwords objs ++ " : " ++ dep)
       genDep (dep, True  {- is an hi file -}) = do
	  hisuf <- readIORef hi_suf
	  let dep_base = remove_suffix '.' dep
	      deps = (dep_base ++ hisuf)
		     : map (\suf -> dep_base ++ suf ++ '_':hisuf) extra_suffixes
		  -- length objs should be == length deps
	  sequence_ (zipWith (\o d -> hPutStrLn hdl (o ++ " : " ++ d)) objs deps)

   mapM genDep [ d | Just d <- deps ]

   return ()

-- add the lines to dep_makefile:
	   -- always:
		   -- this.o : this.hs

  	   -- if the dependency is on something other than a .hi file:
   		   -- this.o this.p_o ... : dep
   	   -- otherwise
   		   -- if the import is {-# SOURCE #-}
   			   -- this.o this.p_o ... : dep.hi-boot[-$vers]
   			   
   		   -- else
   			   -- this.o ...   : dep.hi
   			   -- this.p_o ... : dep.p_hi
   			   -- ...
   
   	   -- (where .o is $osuf, and the other suffixes come from
   	   -- the cmdline -s options).
   
-----------------------------------------------------------------------------
-- Hsc phase

run_phase Hsc	basename _suff input_fn output_fn
  = do  hsc <- readIORef pgm_C
	
  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the import path, since this is
  -- what gcc does, and it's probably what you want.
	let current_dir = getdir basename
	
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
	
  -- figure out where to put the .hi file
	ohi    <- readIORef output_hi
	hisuf  <- readIORef hi_suf
	let hi_flags = case ohi of
			   Nothing -> [ "-hidir="++current_dir, "-hisuf="++hisuf ]
			   Just fn -> [ "-hifile="++fn ]

  -- run the compiler!
	run_something "Haskell Compiler" 
		 (unwords (hsc : input_fn : (
		    hsc_opts
		    ++ hi_flags
		    ++ [ 
			  "-ofile="++output_fn, 
			  "-F="++tmp_stub_c, 
			  "-FH="++tmp_stub_h 
		       ]
		    ++ stat_opts
		 )))

  -- Generate -Rghc-timing info
	when (timing) (
  	    run_something "Generate timing stats"
		(findFile "ghc-stats" cGHC_STATS ++ ' ':stat_file)
	 )

  -- Deal with stubs
	let stub_h = basename ++ "_stub.h"
	let stub_c = basename ++ "_stub.c"
	
		-- copy .h_stub file into current dir if present
	b <- doesFileExist tmp_stub_h
	when b (do
	      	run_something "Copy stub .h file"
				("cp " ++ tmp_stub_h ++ ' ':stub_h)
	
			-- #include <..._stub.h> in .hc file
		addCmdlineHCInclude tmp_stub_h	-- hack

			-- copy the _stub.c file into the current dir
		run_something "Copy stub .c file" 
		    (unwords [ 
			"rm -f", stub_c, "&&",
			"echo \'#include \""++stub_h++"\"\' >"++stub_c, " &&",
			"cat", tmp_stub_c, ">> ", stub_c
			])

			-- compile the _stub.c file w/ gcc
		pipeline <- genPipeline (StopBefore Ln) "" stub_c
		run_pipeline pipeline stub_c False{-no linking-} 
				False{-no -o option-}
				(basename++"_stub") "c"

		add ld_inputs (basename++"_stub.o")
	 )

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

run_phase cc_phase _basename _suff input_fn output_fn
   | cc_phase == Cc || cc_phase == HCc
   = do	cc <- readIORef pgm_c
       	cc_opts <- (getOpts opt_c)
       	cmdline_include_dirs <- readIORef include_paths

        let hcc = cc_phase == HCc

		-- add package include paths even if we're just compiling
		-- .c files; this is the Value Add(TM) that using
		-- ghc instead of gcc gives you :)
        pkg_include_dirs <- getPackageIncludePath
	let include_paths = map (\p -> "-I"++p) (cmdline_include_dirs 
							++ pkg_include_dirs)

	c_includes <- getPackageCIncludes
	cmdline_includes <- readState cmdline_hc_includes -- -#include options

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

	excessPrecision <- readState excess_precision

	run_something "C Compiler"
	 (unwords ([ cc, "-x", "c", cc_help, "-o", output_fn ]
		   ++ md_c_flags
		   ++ (if cc_phase == HCc && mangle
			 then md_regd_c_flags
			 else [])
		   ++ [ verb, "-S", "-Wimplicit", opt_flag ]
		   ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
		   ++ cc_opts
#ifdef mingw32_TARGET_OS
                   ++ [" -mno-cygwin"]
#endif
		   ++ (if excessPrecision then [] else [ "-ffloat-store" ])
		   ++ include_paths
		   ++ pkg_extra_cc_opts
--		   ++ [">", ccout]
		   ))

	-- ToDo: postprocess the output from gcc

-----------------------------------------------------------------------------
-- Mangle phase

run_phase Mangle _basename _suff input_fn output_fn
  = do mangler <- readIORef pgm_m
       mangler_opts <- getOpts opt_m
       machdep_opts <-
	 if (prefixMatch "i386" cTARGETPLATFORM)
	    then do n_regs <- readState stolen_x86_regs
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

run_phase SplitMangle _basename _suff input_fn _output_fn
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

run_phase As _basename _suff input_fn output_fn
  = do 	as <- readIORef pgm_a
        as_opts <- getOpts opt_a

        cmdline_include_paths <- readIORef include_paths
        let cmdline_include_flags = map (\p -> "-I"++p) cmdline_include_paths
        run_something "Assembler"
	   (unwords (as : as_opts
		       ++ cmdline_include_flags
		       ++ [ "-c", input_fn, "-o",  output_fn ]
		    ))

run_phase SplitAs basename _suff _input_fn _output_fn
  = do  as <- readIORef pgm_a
        as_opts <- getOpts opt_a

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
		    real_o <- osuf_ify output_o
		    run_something "Assembler" 
			    (unwords (as : as_opts
				      ++ [ "-c", "-o", real_o, input_s ]
			    ))
	
	mapM_ assemble_file [1..n]

-----------------------------------------------------------------------------
-- Linking

do_link :: [String] -> IO ()
do_link o_files = do
    ln <- readIORef pgm_l
    verb <- is_verbose
    o_file <- readIORef output_file
    let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

    pkg_lib_paths <- getPackageLibraryPath
    let pkg_lib_path_opts = map ("-L"++) pkg_lib_paths

    lib_paths <- readIORef library_paths
    let lib_path_opts = map ("-L"++) lib_paths

    pkg_libs <- getPackageLibraries
    let pkg_lib_opts = map (\lib -> "-l"++lib) pkg_libs

    libs <- readIORef cmdline_libraries
    let lib_opts = map ("-l"++) (reverse libs)
	 -- reverse because they're added in reverse order from the cmd line

    pkg_extra_ld_opts <- getPackageExtraLdOpts

	-- probably _stub.o files
    extra_ld_inputs <- readIORef ld_inputs

	-- opts from -optl-<blah>
    extra_ld_opts <- getOpts opt_l

    run_something "Linker"
       (unwords 
	 ([ ln, verb, "-o", output_fn ]
	 ++ o_files
	 ++ extra_ld_inputs
	 ++ lib_path_opts
	 ++ lib_opts
	 ++ pkg_lib_path_opts
	 ++ pkg_lib_opts
	 ++ pkg_extra_ld_opts
	 ++ extra_ld_opts
	)
       )

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

-- note that ordering is important in the following list: any flag which
-- is a prefix flag (i.e. HasArg, Prefix, OptPrefix, AnySuffix) will override
-- flags further down the list with the same prefix.

driver_opts = 
  [  ------- help -------------------------------------------------------
     ( "?"    		, NoArg long_usage)
  ,  ( "-help"		, NoArg long_usage)
  

      ------- version ----------------------------------------------------
  ,  ( "-version"	 , NoArg (do hPutStrLn stdout (cProjectName
				      ++ ", version " ++ version_str)
				     exitWith ExitSuccess))
  ,  ( "-numeric-version", NoArg (do hPutStrLn stdout version_str
				     exitWith ExitSuccess))

      ------- verbosity ----------------------------------------------------
  ,  ( "v"		, NoArg (writeIORef verbose True) )
  ,  ( "n"              , NoArg (writeIORef dry_run True) )

	------- recompilation checker --------------------------------------
  ,  ( "recomp"		, NoArg (writeIORef recomp True) )
  ,  ( "no-recomp"  	, NoArg (writeIORef recomp False) )

	------- ways --------------------------------------------------------
  ,  ( "prof"		, NoArg (addNoDups ways	WayProf) )
  ,  ( "unreg"		, NoArg (addNoDups ways	WayUnreg) )
  ,  ( "dll"            , NoArg (addNoDups ways WayDll) )
  ,  ( "ticky"		, NoArg (addNoDups ways	WayTicky) )
  ,  ( "parallel"	, NoArg (addNoDups ways	WayPar) )
  ,  ( "gransim"	, NoArg (addNoDups ways	WayGran) )
  ,  ( "smp"		, NoArg (addNoDups ways	WaySMP) )
  ,  ( "debug"		, NoArg (addNoDups ways	WayDebug) )
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
  ,  ( "auto-dicts"	, NoArg (addOpt_C "-fauto-sccs-on-dicts") )
  ,  ( "auto-all"	, NoArg (addOpt_C "-fauto-sccs-on-all-toplevs") )
  ,  ( "auto"		, NoArg (addOpt_C "-fauto-sccs-on-exported-toplevs") )
  ,  ( "caf-all"	, NoArg (addOpt_C "-fauto-sccs-on-individual-cafs") )
         -- "ignore-sccs"  doesn't work  (ToDo)

	------- Miscellaneous -----------------------------------------------
  ,  ( "cpp"		, NoArg (updateState (\s -> s{ cpp_flag = True })) )
  ,  ( "#include"	, HasArg (addCmdlineHCInclude) )
  ,  ( "no-link-chk"    , NoArg (return ()) ) -- ignored for backwards compat

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
  ,  ( "keep-tmp-files" , AnySuffix (\_ -> writeIORef keep_tmp_files True) )

  ,  ( "split-objs"	, NoArg (if can_split
				    then do writeIORef split_object_files True
					    addOpt_C "-fglobalise-toplev-names"
					    addOpt_c "-DUSE_SPLIT_MARKERS"
				    else hPutStrLn stderr
					    "warning: don't know how to  split \
					    \object files on this architecture"
				) )
  
	------- Include/Import Paths ----------------------------------------
  ,  ( "i"		, OptPrefix (addToDirList import_paths) )
  ,  ( "I" 		, Prefix    (addToDirList include_paths) )

	------- Libraries ---------------------------------------------------
  ,  ( "L"		, Prefix (addToDirList library_paths) )
  ,  ( "l"		, Prefix (add cmdline_libraries) )

        ------- Packages ----------------------------------------------------
  ,  ( "package-name"   , HasArg (\s -> addOpt_C ("-inpackage="++s)) )

  ,  ( "package"        , HasArg (addPackage) )
  ,  ( "syslib"         , HasArg (addPackage) )	-- for compatibility w/ old vsns

  ,  ( "-list-packages"  , NoArg (listPackages) )
  ,  ( "-add-package"    , NoArg (newPackage) )
  ,  ( "-delete-package" , SepArg (deletePackage) )

        ------- Specific phases  --------------------------------------------
  ,  ( "pgmL"           , HasArg (writeIORef pgm_L) )
  ,  ( "pgmP"           , HasArg (writeIORef pgm_P) )
  ,  ( "pgmC"           , HasArg (writeIORef pgm_C) )
  ,  ( "pgmc"           , HasArg (writeIORef pgm_c) )
  ,  ( "pgmm"           , HasArg (writeIORef pgm_m) )
  ,  ( "pgms"           , HasArg (writeIORef pgm_s) )
  ,  ( "pgma"           , HasArg (writeIORef pgm_a) )
  ,  ( "pgml"           , HasArg (writeIORef pgm_l) )

  ,  ( "optdep"		, HasArg (addOpt_dep) )
  ,  ( "optL"		, HasArg (addOpt_L) )
  ,  ( "optP"		, HasArg (addOpt_P) )
  ,  ( "optCrts"        , HasArg (addOpt_Crts) )
  ,  ( "optC"		, HasArg (addOpt_C) )
  ,  ( "optc"		, HasArg (addOpt_c) )
  ,  ( "optm"		, HasArg (addOpt_m) )
  ,  ( "opta"		, HasArg (addOpt_a) )
  ,  ( "optl"		, HasArg (addOpt_l) )
  ,  ( "optdll"		, HasArg (addOpt_dll) )

	------ HsCpp opts ---------------------------------------------------
  ,  ( "D"		, Prefix (\s -> addOpt_P ("-D'"++s++"'") ) )
  ,  ( "U"		, Prefix (\s -> addOpt_P ("-U'"++s++"'") ) )

	------ Warning opts -------------------------------------------------
  ,  ( "W"		, NoArg (updateState (\s -> s{ warning_opt = W_ })))
  ,  ( "Wall"		, NoArg (updateState (\s -> s{ warning_opt = W_all })))
  ,  ( "Wnot"		, NoArg (updateState (\s -> s{ warning_opt = W_not })))
  ,  ( "w"		, NoArg (updateState (\s -> s{ warning_opt = W_not })))

	----- Linker --------------------------------------------------------
  ,  ( "static" 	, NoArg (writeIORef static True) )

        ------ Compiler RTS options -----------------------------------------
  ,  ( "H"                 , HasArg (newHeapSize  . decodeSize) )
  ,  ( "K"                 , HasArg (newStackSize . decodeSize) )
  ,  ( "Rscale-sizes"	   , HasArg (floatOpt scale_sizes_by) )
  ,  ( "Rghc-timing" 	   , NoArg  (writeIORef collect_ghc_timing True) )

	------ Debugging ----------------------------------------------------
  ,  ( "dstg-stats"	   , NoArg (writeIORef opt_StgStats True) )

  ,  ( "dno-"		   , Prefix (\s -> addAntiOpt_C ("-d"++s)) )
  ,  ( "d"		   , AnySuffix (addOpt_C) )

	------ Machine dependant (-m<blah>) stuff ---------------------------

  ,  ( "monly-2-regs", 		NoArg (updateState (\s -> s{stolen_x86_regs = 2}) ))
  ,  ( "monly-3-regs", 		NoArg (updateState (\s -> s{stolen_x86_regs = 3}) ))
  ,  ( "monly-4-regs", 		NoArg (updateState (\s -> s{stolen_x86_regs = 4}) ))

        ------ Compiler flags -----------------------------------------------
  ,  ( "O2-for-C"	   , NoArg (writeIORef opt_minus_o2_for_C True) )
  ,  ( "O"		   , OptPrefix (setOptLevel) )

  ,  ( "fglasgow-exts-no-lang", NoArg ( do addOpt_C "-fglasgow-exts") )

  ,  ( "fglasgow-exts"     , NoArg (do addOpt_C "-fglasgow-exts"
				       addPackage "lang"))

  ,  ( "fasm"		   , OptPrefix (\_ -> writeIORef hsc_lang HscAsm) )

  ,  ( "fvia-c"		   , NoArg (writeIORef hsc_lang HscC) )
  ,  ( "fvia-C"		   , NoArg (writeIORef hsc_lang HscC) )

  ,  ( "fno-asm-mangling"  , NoArg (writeIORef do_asm_mangling False) )

  ,  ( "fmax-simplifier-iterations", 
		Prefix (writeIORef opt_MaxSimplifierIterations . read) )

  ,  ( "fusagesp"	   , NoArg (do writeIORef opt_UsageSPInf True
				       addOpt_C "-fusagesp-on") )

  ,  ( "fexcess-precision" , NoArg (do updateState 
					   (\s -> s{ excess_precision = True })
				       addOpt_C "-fexcess-precision"))

	-- flags that are "active negatives"
  ,  ( "fno-implicit-prelude"	, PassFlag (addOpt_C) )
  ,  ( "fno-prune-tydecls"	, PassFlag (addOpt_C) )
  ,  ( "fno-prune-instdecls"	, PassFlag (addOpt_C) )
  ,  ( "fno-pre-inlining"	, PassFlag (addOpt_C) )

	-- All other "-fno-<blah>" options cancel out "-f<blah>" on the hsc cmdline
  ,  ( "fno-",			Prefix (\s -> addAntiOpt_C ("-f"++s)) )

	-- Pass all remaining "-f<blah>" options to hsc
  ,  ( "f", 			AnySuffix (addOpt_C) )
  ]

-----------------------------------------------------------------------------
-- Process command-line  

processArgs :: [(String,OptKind)] -> [String] -> [String]
   -> IO [String]  -- returns spare args
processArgs _spec [] spare = return (reverse spare)
processArgs spec args@(('-':_):_) spare = do
  args' <- processOneArg spec args
  processArgs spec args' spare
processArgs spec (arg:args) spare = 
  processArgs spec args (arg:spare)

processOneArg :: [(String,OptKind)] -> [String] -> IO [String]
processOneArg spec (('-':arg):args) = do
  let (rest,action) = findArg spec arg
      dash_arg = '-':arg
  case action of

	NoArg  io -> 
		if rest == ""
			then io >> return args
			else unknownFlagErr dash_arg

	HasArg fio -> 
		if rest /= "" 
			then fio rest >> return args
			else case args of
				[] -> unknownFlagErr dash_arg
				(arg1:args1) -> fio arg1 >> return args1

	SepArg fio -> 
		case args of
			[] -> unknownFlagErr dash_arg
			(arg1:args1) -> fio arg1 >> return args1

	Prefix fio -> 
		if rest /= ""
			then fio rest >> return args
			else unknownFlagErr dash_arg
	
	OptPrefix fio -> fio rest >> return args

	AnySuffix fio -> fio ('-':arg) >> return args

	PassFlag fio  -> 
		if rest /= ""
			then unknownFlagErr dash_arg
			else fio ('-':arg) >> return args

findArg :: [(String,OptKind)] -> String -> (String,OptKind)
findArg spec arg
  = case [ (remove_spaces rest, k) | (pat,k) <- spec,
				     Just rest <- [my_prefix_match pat arg],
				     is_prefix k || null rest ] of
	[] -> unknownFlagErr ('-':arg)
	(one:_) -> one

is_prefix (NoArg _) = False
is_prefix (SepArg _) = False
is_prefix (PassFlag _) = False
is_prefix _ = True

-----------------------------------------------------------------------------
-- convert sizes like "3.5M" into integers

decodeSize :: String -> Integer
decodeSize str
  | c == ""		 = truncate n
  | c == "K" || c == "k" = truncate (n * 1000)
  | c == "M" || c == "m" = truncate (n * 1000 * 1000)
  | c == "G" || c == "g" = truncate (n * 1000 * 1000 * 1000)
  | otherwise            = throwDyn (OtherError ("can't decode size: " ++ str))
  where (m, c) = span pred str
        n      = read m  :: Double
	pred c = isDigit c || c == '.'

floatOpt :: IORef Double -> String -> IO ()
floatOpt ref str
  = writeIORef ref (read str :: Double)

-----------------------------------------------------------------------------
-- Finding files in the installation

GLOBAL_VAR(topDir, clibdir, String)

	-- grab the last -B option on the command line, and
	-- set topDir to its value.
setTopDir :: [String] -> IO [String]
setTopDir args = do
  let (minusbs, others) = partition (prefixMatch "-B") args
  (case minusbs of
    []   -> writeIORef topDir clibdir
    some -> writeIORef topDir (drop 2 (last some)))
  return others

findFile name alt_path = unsafePerformIO (do
  top_dir <- readIORef topDir
  let installed_file = top_dir ++ '/':name
  let inplace_file   = top_dir ++ '/':cCURRENT_DIR ++ '/':alt_path
  b <- doesFileExist inplace_file
  if b  then return inplace_file
	else return installed_file
 )

-----------------------------------------------------------------------------
-- Utils

my_partition :: (a -> Maybe b) -> [a] -> ([(a,b)],[a])
my_partition _ [] = ([],[])
my_partition p (a:as)
  = let (bs,cs) = my_partition p as in
    case p a of
	Nothing -> (bs,a:cs)
	Just b  -> ((a,b):bs,cs)

my_prefix_match :: String -> String -> Maybe String
my_prefix_match [] rest = Just rest
my_prefix_match (_:_) [] = Nothing
my_prefix_match (p:pat) (r:rest)
  | p == r    = my_prefix_match pat rest
  | otherwise = Nothing

prefixMatch :: Eq a => [a] -> [a] -> Bool
prefixMatch [] _str = True
prefixMatch _pat [] = False
prefixMatch (p:ps) (s:ss) | p == s    = prefixMatch ps ss
			  | otherwise = False

postfixMatch :: String -> String -> Bool
postfixMatch pat str = prefixMatch (reverse pat) (reverse str)

later = flip finally

my_catchDyn = flip catchDyn

global :: a -> IORef a
global a = unsafePerformIO (newIORef a)

splitFilename :: String -> (String,String)
splitFilename f = (reverse (stripDot rev_basename), reverse rev_ext)
  where (rev_ext, rev_basename) = span ('.' /=) (reverse f)
        stripDot ('.':xs) = xs
        stripDot xs       = xs

suffixOf :: String -> String
suffixOf s = drop_longest_prefix s '.'

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

add :: IORef [a] -> a -> IO ()
add var x = do
  xs <- readIORef var
  writeIORef var (x:xs)

addNoDups :: Eq a => IORef [a] -> a -> IO ()
addNoDups var x = do
  xs <- readIORef var
  unless (x `elem` xs) $ writeIORef var (x:xs)

remove_suffix :: Char -> String -> String
remove_suffix c s
  | null pre  = reverse suf
  | otherwise = reverse pre
  where (suf,pre) = break (==c) (reverse s)

drop_longest_prefix :: String -> Char -> String
drop_longest_prefix s c = reverse suf
  where (suf,_pre) = break (==c) (reverse s)

take_longest_prefix :: String -> Char -> String
take_longest_prefix s c = reverse pre
  where (_suf,pre) = break (==c) (reverse s)

newsuf :: String -> String -> String
newsuf suf s = remove_suffix '.' s ++ suf

-- getdir strips the filename off the input string, returning the directory.
getdir :: String -> String
getdir s = if null dir then "." else init dir
  where dir = take_longest_prefix s '/'

newdir :: String -> String -> String
newdir dir s = dir ++ '/':drop_longest_prefix s '/'

remove_spaces :: String -> String
remove_spaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
