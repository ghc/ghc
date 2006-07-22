-----------------------------------------------------------------------------
--
-- Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from
-- compilation to compilation using OPTIONS_GHC pragmas, and in a
-- multi-session GHC each session can be using different dynamic
-- flags.  Dynamic flags can also be set at the prompt in GHCi.
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module DynFlags (
	-- Dynamic flags
	DynFlag(..),
	DynFlags(..),
	HscTarget(..),
	GhcMode(..), isOneShot,
	GhcLink(..), isNoLink,
	PackageFlag(..),
	Option(..),

	-- Configuration of the core-to-core and stg-to-stg phases
	CoreToDo(..),
	StgToDo(..),
	SimplifierSwitch(..), 
	SimplifierMode(..), FloatOutSwitches(..),
	getCoreToDo, getStgToDo,
	
	-- Manipulating DynFlags
	defaultDynFlags,		-- DynFlags
	initDynFlags,			-- DynFlags -> IO DynFlags

	dopt,				-- DynFlag -> DynFlags -> Bool
	dopt_set, dopt_unset,		-- DynFlags -> DynFlag -> DynFlags
	getOpts,			-- (DynFlags -> [a]) -> IO [a]
	getVerbFlag,
	updOptLevel,
	setTmpDir,
	
	-- parsing DynFlags
	parseDynamicFlags,
        allFlags,

	-- misc stuff
	machdepCCOpts, picCCOpts,
  ) where

#include "HsVersions.h"

import Module		( Module, mkModule )
import PrelNames	( mAIN )
import StaticFlags	( opt_Static, opt_PIC, 
			  WayName(..), v_Ways, v_Build_tag, v_RTS_Build_tag )
import {-# SOURCE #-} Packages (PackageState)
import DriverPhases	( Phase(..), phaseInputExt )
import Config
import CmdLineParser
import Panic		( panic, GhcException(..) )
import Util		( notNull, splitLongestPrefix, split, normalisePath )
import SrcLoc           ( SrcSpan )

import DATA_IOREF	( readIORef )
import EXCEPTION	( throwDyn )
import Monad		( when )
#ifdef mingw32_TARGET_OS
import Data.List	( isPrefixOf )
#endif
import Maybe		( fromJust )
import Char		( isDigit, isUpper )
import Outputable
import System.IO        ( hPutStrLn, stderr )
import {-# SOURCE #-} ErrUtils ( Severity(..), Message, mkLocMessage )

-- -----------------------------------------------------------------------------
-- DynFlags

data DynFlag

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_asm
   | Opt_D_dump_cpranal
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_flatC
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_rn
   | Opt_D_dump_simpl
   | Opt_D_dump_simpl_iterations
   | Opt_D_dump_spec
   | Opt_D_dump_prep
   | Opt_D_dump_stg
   | Opt_D_dump_stranal
   | Opt_D_dump_tc
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_cse
   | Opt_D_dump_worker_wrapper
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_opt_cmm
   | Opt_D_dump_simpl_stats
   | Opt_D_dump_tc_trace
   | Opt_D_dump_if_trace
   | Opt_D_dump_splices
   | Opt_D_dump_BCOs
   | Opt_D_dump_vect
   | Opt_D_source_stats
   | Opt_D_verbose_core2core
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_minimal_imports
   | Opt_D_faststring_stats
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting

   | Opt_WarnIsError		-- -Werror; makes warnings fatal
   | Opt_WarnDuplicateExports
   | Opt_WarnHiShadows
   | Opt_WarnIncompletePatterns
   | Opt_WarnIncompletePatternsRecUpd
   | Opt_WarnMissingFields
   | Opt_WarnMissingMethods
   | Opt_WarnMissingSigs
   | Opt_WarnNameShadowing
   | Opt_WarnOverlappingPatterns
   | Opt_WarnSimplePatterns
   | Opt_WarnTypeDefaults
   | Opt_WarnUnusedBinds
   | Opt_WarnUnusedImports
   | Opt_WarnUnusedMatches
   | Opt_WarnDeprecations
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans

   -- language opts
   | Opt_AllowOverlappingInstances
   | Opt_AllowUndecidableInstances
   | Opt_AllowIncoherentInstances
   | Opt_MonomorphismRestriction
   | Opt_MonoPatBinds
   | Opt_GlasgowExts
   | Opt_FFI
   | Opt_PArr			       -- syntactic support for parallel arrays
   | Opt_Arrows			       -- Arrow-notation syntax
   | Opt_TH
   | Opt_ImplicitParams
   | Opt_Generics
   | Opt_ImplicitPrelude 
   | Opt_ScopedTypeVariables
   | Opt_BangPatterns

   -- optimisation opts
   | Opt_Strictness
   | Opt_FullLaziness
   | Opt_CSE
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_IgnoreBreakpoints
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_UnboxStrictFields
   | Opt_DictsCheap

   -- misc opts
   | Opt_Cpp
   | Opt_Pp
   | Opt_RecompChecking
   | Opt_DryRun
   | Opt_DoAsmMangling
   | Opt_ExcessPrecision
   | Opt_ReadUserPackageConf
   | Opt_NoHsMain
   | Opt_SplitObjs
   | Opt_StgStats
   | Opt_HideAllPackages

   -- keeping stuff
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepRawSFiles
   | Opt_KeepTmpFiles

   deriving (Eq)
 
data DynFlags = DynFlags {
  ghcMode		:: GhcMode,
  ghcLink		:: GhcLink,
  coreToDo   		:: Maybe [CoreToDo], -- reserved for -Ofile
  stgToDo    		:: Maybe [StgToDo],  -- similarly
  hscTarget    		:: HscTarget,
  hscOutName 		:: String,  	-- name of the output file
  extCoreName		:: String,	-- name of the .core output file
  verbosity  		:: Int,	 	-- verbosity level
  optLevel		:: Int,		-- optimisation level
  maxSimplIterations    :: Int,		-- max simplifier iterations
  ruleCheck		:: Maybe String,
  stolen_x86_regs	:: Int,		
  cmdlineHcIncludes	:: [String],	-- -#includes
  importPaths		:: [FilePath],
  mainModIs		:: Module,
  mainFunIs		:: Maybe String,

  -- ways
  wayNames		:: [WayName],	-- way flags from the cmd line
  buildTag		:: String,	-- the global "way" (eg. "p" for prof)
  rtsBuildTag		:: String,	-- the RTS "way"
  
  -- paths etc.
  objectDir		:: Maybe String,
  hiDir			:: Maybe String,
  stubDir		:: Maybe String,

  objectSuf		:: String,
  hcSuf			:: String,
  hiSuf			:: String,

  outputFile		:: Maybe String,
  outputHi		:: Maybe String,

  includePaths		:: [String],
  libraryPaths		:: [String],
  frameworkPaths	:: [String],  	-- used on darwin only
  cmdlineFrameworks	:: [String],	-- ditto
  tmpDir		:: String,	-- no trailing '/'
  
  -- options for particular phases
  opt_L			:: [String],
  opt_P			:: [String],
  opt_F			:: [String],
  opt_c			:: [String],
  opt_m			:: [String],
  opt_a			:: [String],
  opt_l			:: [String],
  opt_dll		:: [String],
  opt_dep		:: [String],

  -- commands for particular phases
  pgm_L			:: String,
  pgm_P			:: (String,[Option]),
  pgm_F			:: String,
  pgm_c			:: (String,[Option]),
  pgm_m			:: (String,[Option]),
  pgm_s			:: (String,[Option]),
  pgm_a			:: (String,[Option]),
  pgm_l			:: (String,[Option]),
  pgm_dll		:: (String,[Option]),

  --  ** Package flags
  extraPkgConfs		:: [FilePath],
	-- The -package-conf flags given on the command line, in the order
	-- they appeared.

  packageFlags		:: [PackageFlag],
	-- The -package and -hide-package flags from the command-line

  --  ** Package state
  pkgState		:: PackageState,

  -- hsc dynamic flags
  flags      		:: [DynFlag],
  
  -- message output
  log_action            :: Severity -> SrcSpan -> PprStyle -> Message -> IO ()
 }

data HscTarget
  = HscC
  | HscAsm
  | HscJava
  | HscILX
  | HscInterpreted
  | HscNothing
  deriving (Eq, Show)

data GhcMode
  = BatchCompile	-- | @ghc --make Main@
  | Interactive		-- | @ghc --interactive@
  | OneShot		-- | @ghc -c Foo.hs@
  | JustTypecheck	-- | Development environemnts, refactorer, etc.
  | MkDepend
  deriving Eq

isOneShot :: GhcMode -> Bool
isOneShot OneShot = True
isOneShot _other  = False

data GhcLink	-- What to do in the link step, if there is one
  =		-- Only relevant for modes
		-- 	DoMake and StopBefore StopLn
    NoLink		-- Don't link at all
  | StaticLink		-- Ordinary linker [the default]
  | MkDLL		-- Make a DLL

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink other  = False

data PackageFlag
  = ExposePackage  String
  | HidePackage    String
  | IgnorePackage  String

defaultHscTarget
  | cGhcWithNativeCodeGen == "YES" 	=  HscAsm
  | otherwise				=  HscC

initDynFlags dflags = do
 -- someday these will be dynamic flags
 ways <- readIORef v_Ways
 build_tag <- readIORef v_Build_tag
 rts_build_tag <- readIORef v_RTS_Build_tag
 return dflags{
	wayNames	= ways,
	buildTag	= build_tag,
	rtsBuildTag	= rts_build_tag
	}

defaultDynFlags =
     DynFlags {
	ghcMode			= OneShot,
	ghcLink			= StaticLink,
	coreToDo 		= Nothing,
	stgToDo			= Nothing, 
	hscTarget		= defaultHscTarget, 
	hscOutName		= "", 
	extCoreName		= "",
	verbosity		= 0, 
	optLevel		= 0,
	maxSimplIterations	= 4,
	ruleCheck		= Nothing,
	stolen_x86_regs		= 4,
	cmdlineHcIncludes	= [],
	importPaths		= ["."],
	mainModIs		= mAIN,
	mainFunIs		= Nothing,
	
	wayNames		= panic "ways",
	buildTag		= panic "buildTag",
	rtsBuildTag		= panic "rtsBuildTag",

	objectDir		= Nothing,
	hiDir			= Nothing,
	stubDir			= Nothing,

	objectSuf		= phaseInputExt StopLn,
	hcSuf			= phaseInputExt HCc,
	hiSuf			= "hi",

	outputFile		= Nothing,
	outputHi		= Nothing,
	includePaths		= [],
	libraryPaths		= [],
	frameworkPaths		= [],
	cmdlineFrameworks	= [],
	tmpDir			= cDEFAULT_TMPDIR,
	
	opt_L			= [],
	opt_P			= [],
	opt_F			= [],
	opt_c			= [],
	opt_a			= [],
	opt_m			= [],
	opt_l			= [],
	opt_dll			= [],
	opt_dep			= [],
	
	pgm_L			= panic "pgm_L",
	pgm_P			= panic "pgm_P",
	pgm_F			= panic "pgm_F",
	pgm_c			= panic "pgm_c",
	pgm_m			= panic "pgm_m",
	pgm_s			= panic "pgm_s",
	pgm_a			= panic "pgm_a",
	pgm_l			= panic "pgm_l",
	pgm_dll			= panic "pgm_mkdll",
	
	extraPkgConfs		= [],
	packageFlags		= [],
	pkgState		= panic "pkgState",
	
	flags = [ 
    	    Opt_RecompChecking,
    	    Opt_ReadUserPackageConf,
    
	    Opt_MonoPatBinds, 	-- Experimentally, I'm making this non-standard
				-- behaviour the default, to see if anyone notices
				-- SLPJ July 06

    	    Opt_ImplicitPrelude,
    	    Opt_MonomorphismRestriction,
    	    Opt_Strictness,
    			-- strictness is on by default, but this only
    			-- applies to -O.
    	    Opt_CSE,		-- similarly for CSE.
    	    Opt_FullLaziness,	-- ...and for full laziness
    
    	    Opt_DoLambdaEtaExpansion,
    			-- This one is important for a tiresome reason:
    			-- we want to make sure that the bindings for data 
    			-- constructors are eta-expanded.  This is probably
    			-- a good thing anyway, but it seems fragile.
    
    	    Opt_DoAsmMangling,
    
    	    -- and the default no-optimisation options:
    	    Opt_IgnoreInterfacePragmas,
    	    Opt_OmitInterfacePragmas
    
               ] ++ standardWarnings,
               
        log_action = \severity srcSpan style msg -> 
                        case severity of
                          SevInfo  -> hPutStrLn stderr (show (msg style))
                          SevFatal -> hPutStrLn stderr (show (msg style))
                          _        -> hPutStrLn stderr ('\n':show ((mkLocMessage srcSpan msg) style))
      }

{- 
    Verbosity levels:
	
    0	|   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"
-}

dopt :: DynFlag -> DynFlags -> Bool
dopt f dflags  = f `elem` (flags dflags)

dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs{ flags = f : flags dfs }

dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

getOpts :: DynFlags -> (DynFlags -> [a]) -> [a]
getOpts dflags opts = reverse (opts dflags)
	-- We add to the options from the front, so we need to reverse the list

getVerbFlag :: DynFlags -> String
getVerbFlag dflags 
  | verbosity dflags >= 3  = "-v" 
  | otherwise =  ""

setObjectDir  f d = d{ objectDir  = f}
setHiDir      f d = d{ hiDir      = f}
setStubDir    f d = d{ stubDir    = f}

setObjectSuf  f d = d{ objectSuf  = f}
setHiSuf      f d = d{ hiSuf      = f}
setHcSuf      f d = d{ hcSuf      = f}

setOutputFile f d = d{ outputFile = f}
setOutputHi   f d = d{ outputHi   = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f d = let (pgm:args) = words f in d{ pgm_P   = (pgm, map Option args)}

setPgmL   f d = d{ pgm_L   = f}
setPgmF   f d = d{ pgm_F   = f}
setPgmc   f d = d{ pgm_c   = (f,[])}
setPgmm   f d = d{ pgm_m   = (f,[])}
setPgms   f d = d{ pgm_s   = (f,[])}
setPgma   f d = d{ pgm_a   = (f,[])}
setPgml   f d = d{ pgm_l   = (f,[])}
setPgmdll f d = d{ pgm_dll = (f,[])}

addOptL   f d = d{ opt_L   = f : opt_L d}
addOptP   f d = d{ opt_P   = f : opt_P d}
addOptF   f d = d{ opt_F   = f : opt_F d}
addOptc   f d = d{ opt_c   = f : opt_c d}
addOptm   f d = d{ opt_m   = f : opt_m d}
addOpta   f d = d{ opt_a   = f : opt_a d}
addOptl   f d = d{ opt_l   = f : opt_l d}
addOptdll f d = d{ opt_dll = f : opt_dll d}
addOptdep f d = d{ opt_dep = f : opt_dep d}

addCmdlineFramework f d = d{ cmdlineFrameworks = f : cmdlineFrameworks d}

-- -----------------------------------------------------------------------------
-- Command-line options

-- When invoking external tools as part of the compilation pipeline, we
-- pass these a sequence of options on the command-line. Rather than
-- just using a list of Strings, we use a type that allows us to distinguish
-- between filepaths and 'other stuff'. [The reason being, of course, that
-- this type gives us a handle on transforming filenames, and filenames only,
-- to whatever format they're expected to be on a particular platform.]

data Option
 = FileOption -- an entry that _contains_ filename(s) / filepaths.
              String  -- a non-filepath prefix that shouldn't be 
		      -- transformed (e.g., "/out=")
 	      String  -- the filepath/filename portion
 | Option     String
 
-----------------------------------------------------------------------------
-- Setting the optimisation level

updOptLevel :: Int -> DynFlags -> DynFlags
-- Set dynflags appropriate to the optimisation level
updOptLevel n dfs
  = if (n >= 1)
     then dfs2{ hscTarget = HscC, optLevel = n } -- turn on -fvia-C with -O
     else dfs2{ optLevel = n }
  where
   dfs1 = foldr (flip dopt_unset) dfs  remove_dopts
   dfs2 = foldr (flip dopt_set)   dfs1 extra_dopts

   extra_dopts
	| n == 0    = opt_0_dopts
	| otherwise = opt_1_dopts

   remove_dopts
	| n == 0    = opt_1_dopts
	| otherwise = opt_0_dopts
	
opt_0_dopts =  [ 
	Opt_IgnoreInterfacePragmas,
	Opt_OmitInterfacePragmas
    ]

opt_1_dopts = [
	Opt_IgnoreAsserts,
	Opt_DoEtaReduction,
	Opt_CaseMerge
     ]

-- -----------------------------------------------------------------------------
-- Standard sets of warning options

standardWarnings
    = [ Opt_WarnDeprecations,
	Opt_WarnOverlappingPatterns,
	Opt_WarnMissingFields,
	Opt_WarnMissingMethods,
	Opt_WarnDuplicateExports
      ]

minusWOpts
    = standardWarnings ++ 
      [	Opt_WarnUnusedBinds,
	Opt_WarnUnusedMatches,
	Opt_WarnUnusedImports,
	Opt_WarnIncompletePatterns,
	Opt_WarnDodgyImports
      ]

minusWallOpts
    = minusWOpts ++
      [	Opt_WarnTypeDefaults,
	Opt_WarnNameShadowing,
	Opt_WarnMissingSigs,
	Opt_WarnHiShadows,
	Opt_WarnOrphans
      ]

-- -----------------------------------------------------------------------------
-- CoreToDo:  abstraction of core-to-core passes to run.

data CoreToDo		-- These are diff core-to-core passes,
			-- which may be invoked in any order,
  			-- as many times as you like.

  = CoreDoSimplify	-- The core-to-core simplifier.
	SimplifierMode
	[SimplifierSwitch]
			-- Each run of the simplifier can take a different
			-- set of simplifier-specific flags.
  | CoreDoFloatInwards
  | CoreDoFloatOutwards FloatOutSwitches
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoSpecConstr
  | CoreDoOldStrictness
  | CoreDoGlomBinds
  | CoreCSE
  | CoreDoRuleCheck Int{-CompilerPhase-} String	-- Check for non-application of rules 
						-- matching this string

  | CoreDoNothing 	 -- useful when building up lists of these things

data SimplifierMode 		-- See comments in SimplMonad
  = SimplGently
  | SimplPhase Int

data SimplifierSwitch
  = MaxSimplifierIterations Int
  | NoCaseOfCase

data FloatOutSwitches
  = FloatOutSw  Bool 	-- True <=> float lambdas to top level
		Bool	-- True <=> float constants to top level,
			-- 	    even if they do not escape a lambda


-- The core-to-core pass ordering is derived from the DynFlags:

getCoreToDo :: DynFlags -> [CoreToDo]
getCoreToDo dflags
  | Just todo <- coreToDo dflags = todo -- set explicitly by user
  | otherwise = core_todo
  where
    opt_level  	  = optLevel dflags
    max_iter   	  = maxSimplIterations dflags
    strictness    = dopt Opt_Strictness dflags
    full_laziness = dopt Opt_FullLaziness dflags
    cse           = dopt Opt_CSE dflags
    rule_check    = ruleCheck dflags

    core_todo = 
     if opt_level == 0 then
      [
	CoreDoSimplify (SimplPhase 0) [
	    MaxSimplifierIterations max_iter
	]
      ]
     else {- opt_level >= 1 -} [ 

	-- initial simplify: mk specialiser happy: minimum effort please
	CoreDoSimplify SimplGently [
			-- 	Simplify "gently"
			-- Don't inline anything till full laziness has bitten
			-- In particular, inlining wrappers inhibits floating
			-- e.g. ...(case f x of ...)...
			--  ==> ...(case (case x of I# x# -> fw x#) of ...)...
			--  ==> ...(case x of I# x# -> case fw x# of ...)...
			-- and now the redex (f x) isn't floatable any more
			-- Similarly, don't apply any rules until after full 
			-- laziness.  Notably, list fusion can prevent floating.

            NoCaseOfCase,	-- Don't do case-of-case transformations.
				-- This makes full laziness work better
	    MaxSimplifierIterations max_iter
	],

	-- Specialisation is best done before full laziness
	-- so that overloaded functions have all their dictionary lambdas manifest
	CoreDoSpecialising,

	if full_laziness then CoreDoFloatOutwards (FloatOutSw False False)
			 else CoreDoNothing,

	CoreDoFloatInwards,

	CoreDoSimplify (SimplPhase 2) [
		-- Want to run with inline phase 2 after the specialiser to give
		-- maximum chance for fusion to work before we inline build/augment
		-- in phase 1.  This made a difference in 'ansi' where an 
		-- overloaded function wasn't inlined till too late.
	   MaxSimplifierIterations max_iter
	],
	case rule_check of { Just pat -> CoreDoRuleCheck 2 pat; Nothing -> CoreDoNothing },

	CoreDoSimplify (SimplPhase 1) [
		-- Need inline-phase2 here so that build/augment get 
		-- inlined.  I found that spectral/hartel/genfft lost some useful
		-- strictness in the function sumcode' if augment is not inlined
		-- before strictness analysis runs
	   MaxSimplifierIterations max_iter
	],
	case rule_check of { Just pat -> CoreDoRuleCheck 1 pat; Nothing -> CoreDoNothing },

	CoreDoSimplify (SimplPhase 0) [
		-- Phase 0: allow all Ids to be inlined now
		-- This gets foldr inlined before strictness analysis

	   MaxSimplifierIterations 3
		-- At least 3 iterations because otherwise we land up with
		-- huge dead expressions because of an infelicity in the 
		-- simpifier.   
		--	let k = BIG in foldr k z xs
		-- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
		-- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
		-- Don't stop now!

	],
	case rule_check of { Just pat -> CoreDoRuleCheck 0 pat; Nothing -> CoreDoNothing },

#ifdef OLD_STRICTNESS
	CoreDoOldStrictness
#endif
	if strictness then CoreDoStrictness else CoreDoNothing,
	CoreDoWorkerWrapper,
	CoreDoGlomBinds,

	CoreDoSimplify (SimplPhase 0) [
	   MaxSimplifierIterations max_iter
	],

	if full_laziness then
	  CoreDoFloatOutwards (FloatOutSw False	  -- Not lambdas
					  True)   -- Float constants
	else CoreDoNothing,
		-- nofib/spectral/hartel/wang doubles in speed if you
		-- do full laziness late in the day.  It only happens
		-- after fusion and other stuff, so the early pass doesn't
		-- catch it.  For the record, the redex is 
		--	  f_el22 (f_el21 r_midblock)


	-- We want CSE to follow the final full-laziness pass, because it may
	-- succeed in commoning up things floated out by full laziness.
	-- CSE used to rely on the no-shadowing invariant, but it doesn't any more

	if cse then CoreCSE else CoreDoNothing,

	CoreDoFloatInwards,

-- Case-liberation for -O2.  This should be after
-- strictness analysis and the simplification which follows it.

	case rule_check of { Just pat -> CoreDoRuleCheck 0 pat; Nothing -> CoreDoNothing }
     ]

	++ 

     (if opt_level >= 2 then
	   [  CoreLiberateCase,
	      CoreDoSimplify (SimplPhase 0) [
		  MaxSimplifierIterations max_iter
	      ],	-- Run the simplifier after LiberateCase to vastly 
			-- reduce the possiblility of shadowing
			-- Reason: see Note [Shadowing] in SpecConstr.lhs
	     CoreDoSpecConstr
	   ]
      else
	   [])

	++

	-- Final clean-up simplification:
     [	CoreDoSimplify (SimplPhase 0) [
	  MaxSimplifierIterations max_iter
	]
     ]

-- -----------------------------------------------------------------------------
-- StgToDo:  abstraction of stg-to-stg passes to run.

data StgToDo
  = StgDoMassageForProfiling  -- should be (next to) last
  -- There's also setStgVarInfo, but its absolute "lastness"
  -- is so critical that it is hardwired in (no flag).
  | D_stg_stats

getStgToDo :: DynFlags -> [StgToDo]
getStgToDo dflags
  | Just todo <- stgToDo dflags = todo -- set explicitly by user
  | otherwise = todo2
  where
	stg_stats = dopt Opt_StgStats dflags

	todo1 = if stg_stats then [D_stg_stats] else []

	todo2 | WayProf `elem` wayNames dflags
	      = StgDoMassageForProfiling : todo1
	      | otherwise
	      = todo1

-- -----------------------------------------------------------------------------
-- DynFlags parser

allFlags :: [String]
allFlags = map ('-':) $
           [ name | (name, optkind) <- dynamic_flags, ok optkind ] ++
           map ("fno-"++) flags ++
           map ("f"++) flags
    where ok (PrefixPred _ _) = False
          ok _ = True
          flags = map fst fFlags

dynamic_flags :: [(String, OptKind DynP)]
dynamic_flags = [
     ( "n"              , NoArg  (setDynFlag Opt_DryRun) )
  ,  ( "cpp"		, NoArg  (setDynFlag Opt_Cpp))
  ,  ( "F"		, NoArg  (setDynFlag Opt_Pp))
  ,  ( "#include"	, HasArg (addCmdlineHCInclude) )
  ,  ( "v"		, OptPrefix (setVerbosity) )

        ------- Specific phases  --------------------------------------------
  ,  ( "pgmL"           , HasArg (upd . setPgmL) )  
  ,  ( "pgmP"           , HasArg (upd . setPgmP) )  
  ,  ( "pgmF"           , HasArg (upd . setPgmF) )  
  ,  ( "pgmc"           , HasArg (upd . setPgmc) )  
  ,  ( "pgmm"           , HasArg (upd . setPgmm) )  
  ,  ( "pgms"           , HasArg (upd . setPgms) )  
  ,  ( "pgma"           , HasArg (upd . setPgma) )  
  ,  ( "pgml"           , HasArg (upd . setPgml) )  
  ,  ( "pgmdll"		, HasArg (upd . setPgmdll) )

  ,  ( "optL"		, HasArg (upd . addOptL) )  
  ,  ( "optP"		, HasArg (upd . addOptP) )  
  ,  ( "optF"           , HasArg (upd . addOptF) )  
  ,  ( "optc"		, HasArg (upd . addOptc) )  
  ,  ( "optm"		, HasArg (upd . addOptm) )  
  ,  ( "opta"		, HasArg (upd . addOpta) )  
  ,  ( "optl"		, HasArg (upd . addOptl) )  
  ,  ( "optdll"		, HasArg (upd . addOptdll) )  
  ,  ( "optdep"		, HasArg (upd . addOptdep) )

  ,  ( "split-objs"	, NoArg (if can_split
				    then setDynFlag Opt_SplitObjs
				    else return ()) )

	-------- Linking ----------------------------------------------------
  ,  ( "c"		, NoArg (upd $ \d -> d{ ghcLink=NoLink } ))
  ,  ( "no-link"	, NoArg (upd $ \d -> d{ ghcLink=NoLink } )) -- Dep.
  ,  ( "-mk-dll"	, NoArg (upd $ \d -> d{ ghcLink=MkDLL } ))

	------- Libraries ---------------------------------------------------
  ,  ( "L"		, Prefix addLibraryPath )
  ,  ( "l"		, AnySuffix (\s -> do upd (addOptl s)
					      upd (addOptdll s)))

	------- Frameworks --------------------------------------------------
        -- -framework-path should really be -F ...
  ,  ( "framework-path" , HasArg addFrameworkPath )
  ,  ( "framework"	, HasArg (upd . addCmdlineFramework) )

	------- Output Redirection ------------------------------------------
  ,  ( "odir"		, HasArg (upd . setObjectDir  . Just))
  ,  ( "o"		, SepArg (upd . setOutputFile . Just))
  ,  ( "ohi"		, HasArg (upd . setOutputHi   . Just ))
  ,  ( "osuf"		, HasArg (upd . setObjectSuf))
  ,  ( "hcsuf"		, HasArg (upd . setHcSuf))
  ,  ( "hisuf"		, HasArg (upd . setHiSuf))
  ,  ( "hidir"		, HasArg (upd . setHiDir . Just))
  ,  ( "tmpdir"		, HasArg (upd . setTmpDir))
  ,  ( "stubdir"	, HasArg (upd . setStubDir . Just))

	------- Keeping temporary files -------------------------------------
  ,  ( "keep-hc-file"   , AnySuffix (\_ -> setDynFlag Opt_KeepHcFiles))
  ,  ( "keep-s-file"    , AnySuffix (\_ -> setDynFlag Opt_KeepSFiles))
  ,  ( "keep-raw-s-file", AnySuffix (\_ -> setDynFlag Opt_KeepRawSFiles))
  ,  ( "keep-tmp-files" , AnySuffix (\_ -> setDynFlag Opt_KeepTmpFiles))

	------- Miscellaneous ----------------------------------------------
  ,  ( "no-hs-main"     , NoArg (setDynFlag Opt_NoHsMain))
  ,  ( "main-is"   	, SepArg setMainIs )

	------- recompilation checker --------------------------------------
  ,  ( "recomp"		, NoArg (setDynFlag   Opt_RecompChecking) )
  ,  ( "no-recomp"  	, NoArg (unSetDynFlag Opt_RecompChecking) )

        ------- Packages ----------------------------------------------------
  ,  ( "package-conf"   , HasArg extraPkgConf_ )
  ,  ( "no-user-package-conf", NoArg (unSetDynFlag Opt_ReadUserPackageConf) )
  ,  ( "package-name"   , HasArg ignorePackage ) -- for compatibility
  ,  ( "package"        , HasArg exposePackage )
  ,  ( "hide-package"   , HasArg hidePackage )
  ,  ( "hide-all-packages", NoArg (setDynFlag Opt_HideAllPackages) )
  ,  ( "ignore-package" , HasArg ignorePackage )
  ,  ( "syslib"         , HasArg exposePackage )  -- for compatibility

	------ HsCpp opts ---------------------------------------------------
  ,  ( "D",		AnySuffix (upd . addOptP) )
  ,  ( "U",		AnySuffix (upd . addOptP) )

	------- Include/Import Paths ----------------------------------------
  ,  ( "I" 		, Prefix    addIncludePath)
  ,  ( "i"		, OptPrefix addImportPath )

	------ Debugging ----------------------------------------------------
  ,  ( "dstg-stats",	NoArg (setDynFlag Opt_StgStats))

  ,  ( "ddump-cmm",         	 setDumpFlag Opt_D_dump_cmm)
  ,  ( "ddump-asm",          	 setDumpFlag Opt_D_dump_asm)
  ,  ( "ddump-cpranal",      	 setDumpFlag Opt_D_dump_cpranal)
  ,  ( "ddump-deriv",        	 setDumpFlag Opt_D_dump_deriv)
  ,  ( "ddump-ds",           	 setDumpFlag Opt_D_dump_ds)
  ,  ( "ddump-flatC",        	 setDumpFlag Opt_D_dump_flatC)
  ,  ( "ddump-foreign",      	 setDumpFlag Opt_D_dump_foreign)
  ,  ( "ddump-inlinings",    	 setDumpFlag Opt_D_dump_inlinings)
  ,  ( "ddump-occur-anal",   	 setDumpFlag Opt_D_dump_occur_anal)
  ,  ( "ddump-parsed",       	 setDumpFlag Opt_D_dump_parsed)
  ,  ( "ddump-rn",           	 setDumpFlag Opt_D_dump_rn)
  ,  ( "ddump-simpl",        	 setDumpFlag Opt_D_dump_simpl)
  ,  ( "ddump-simpl-iterations", setDumpFlag Opt_D_dump_simpl_iterations)
  ,  ( "ddump-spec",         	 setDumpFlag Opt_D_dump_spec)
  ,  ( "ddump-prep",          	 setDumpFlag Opt_D_dump_prep)
  ,  ( "ddump-stg",          	 setDumpFlag Opt_D_dump_stg)
  ,  ( "ddump-stranal",      	 setDumpFlag Opt_D_dump_stranal)
  ,  ( "ddump-tc",           	 setDumpFlag Opt_D_dump_tc)
  ,  ( "ddump-types",        	 setDumpFlag Opt_D_dump_types)
  ,  ( "ddump-rules",        	 setDumpFlag Opt_D_dump_rules)
  ,  ( "ddump-cse",          	 setDumpFlag Opt_D_dump_cse)
  ,  ( "ddump-worker-wrapper",   setDumpFlag Opt_D_dump_worker_wrapper)
  ,  ( "ddump-rn-trace",         NoArg (setDynFlag Opt_D_dump_rn_trace))
  ,  ( "ddump-if-trace",         NoArg (setDynFlag Opt_D_dump_if_trace))
  ,  ( "ddump-tc-trace",         setDumpFlag Opt_D_dump_tc_trace)
  ,  ( "ddump-splices",          setDumpFlag Opt_D_dump_splices)
  ,  ( "ddump-rn-stats",         NoArg (setDynFlag Opt_D_dump_rn_stats))
  ,  ( "ddump-opt-cmm",          setDumpFlag Opt_D_dump_opt_cmm)
  ,  ( "ddump-simpl-stats",      setDumpFlag Opt_D_dump_simpl_stats)
  ,  ( "ddump-bcos",             setDumpFlag Opt_D_dump_BCOs)
  ,  ( "dsource-stats",          setDumpFlag Opt_D_source_stats)
  ,  ( "dverbose-core2core",     setDumpFlag Opt_D_verbose_core2core)
  ,  ( "dverbose-stg2stg",       setDumpFlag Opt_D_verbose_stg2stg)
  ,  ( "ddump-hi-diffs",         NoArg (setDynFlag Opt_D_dump_hi_diffs))
  ,  ( "ddump-hi",               setDumpFlag Opt_D_dump_hi)
  ,  ( "ddump-minimal-imports",  NoArg (setDynFlag Opt_D_dump_minimal_imports))
  ,  ( "ddump-vect",         	 setDumpFlag Opt_D_dump_vect)
  ,  ( "dcore-lint",       	 NoArg (setDynFlag Opt_DoCoreLinting))
  ,  ( "dstg-lint",        	 NoArg (setDynFlag Opt_DoStgLinting))
  ,  ( "dcmm-lint",		 NoArg (setDynFlag Opt_DoCmmLinting))
  ,  ( "dshow-passes",           NoArg (do unSetDynFlag Opt_RecompChecking
				           setVerbosity "2") )
  ,  ( "dfaststring-stats",	 NoArg (setDynFlag Opt_D_faststring_stats))

	------ Machine dependant (-m<blah>) stuff ---------------------------

  ,  ( "monly-2-regs", 	NoArg (upd (\s -> s{stolen_x86_regs = 2}) ))
  ,  ( "monly-3-regs", 	NoArg (upd (\s -> s{stolen_x86_regs = 3}) ))
  ,  ( "monly-4-regs", 	NoArg (upd (\s -> s{stolen_x86_regs = 4}) ))

	------ Warning opts -------------------------------------------------
  ,  ( "W"		, NoArg (mapM_ setDynFlag   minusWOpts)    )
  ,  ( "Werror"		, NoArg (setDynFlag   	    Opt_WarnIsError) )
  ,  ( "Wall"		, NoArg (mapM_ setDynFlag   minusWallOpts) )
  ,  ( "Wnot"		, NoArg (mapM_ unSetDynFlag minusWallOpts) ) /* DEPREC */
  ,  ( "w"		, NoArg (mapM_ unSetDynFlag minusWallOpts) )

	------ Optimisation flags ------------------------------------------
  ,  ( "O"		   , NoArg (upd (setOptLevel 1)))
  ,  ( "Onot"		   , NoArg (upd (setOptLevel 0)))
  ,  ( "O"		   , PrefixPred (all isDigit) 
				(\f -> upd (setOptLevel (read f))))

  ,  ( "fmax-simplifier-iterations", 
		PrefixPred (all isDigit) 
		  (\n -> upd (\dfs -> 
			dfs{ maxSimplIterations = read n })) )

  ,  ( "frule-check", 
		SepArg (\s -> upd (\dfs -> dfs{ ruleCheck = Just s })))

        ------ Compiler flags -----------------------------------------------

  ,  ( "fno-code",	NoArg (setTarget HscNothing))
  ,  ( "fasm",		AnySuffix (\_ -> setTarget HscAsm) )
  ,  ( "fvia-c",	NoArg (setTarget HscC) )
  ,  ( "fvia-C",	NoArg (setTarget HscC) )
  ,  ( "filx",		NoArg (setTarget HscILX) )

  ,  ( "fglasgow-exts",    NoArg (mapM_ setDynFlag   glasgowExtsFlags) )
  ,  ( "fno-glasgow-exts", NoArg (mapM_ unSetDynFlag glasgowExtsFlags) )

	-- the rest of the -f* and -fno-* flags
  ,  ( "fno-", 		PrefixPred (\f -> isFFlag f) (\f -> unSetDynFlag (getFFlag f)) )
  ,  ( "f",		PrefixPred (\f -> isFFlag f) (\f -> setDynFlag (getFFlag f)) )
 ]

-- these -f<blah> flags can all be reversed with -fno-<blah>

fFlags = [
  ( "warn-duplicate-exports",    	Opt_WarnDuplicateExports ),
  ( "warn-hi-shadowing",         	Opt_WarnHiShadows ),
  ( "warn-incomplete-patterns",  	Opt_WarnIncompletePatterns ),
  ( "warn-incomplete-record-updates",  	Opt_WarnIncompletePatternsRecUpd ),
  ( "warn-missing-fields",       	Opt_WarnMissingFields ),
  ( "warn-missing-methods",      	Opt_WarnMissingMethods ),
  ( "warn-missing-signatures",   	Opt_WarnMissingSigs ),
  ( "warn-name-shadowing",       	Opt_WarnNameShadowing ),
  ( "warn-overlapping-patterns", 	Opt_WarnOverlappingPatterns ),
  ( "warn-simple-patterns",      	Opt_WarnSimplePatterns ),
  ( "warn-type-defaults",        	Opt_WarnTypeDefaults ),
  ( "warn-unused-binds",         	Opt_WarnUnusedBinds ),
  ( "warn-unused-imports",       	Opt_WarnUnusedImports ),
  ( "warn-unused-matches",       	Opt_WarnUnusedMatches ),
  ( "warn-deprecations",         	Opt_WarnDeprecations ),
  ( "warn-orphans",	         	Opt_WarnOrphans ),
  ( "fi",				Opt_FFI ),  -- support `-ffi'...
  ( "ffi",				Opt_FFI ),  -- ...and also `-fffi'
  ( "arrows",				Opt_Arrows ), -- arrow syntax
  ( "parr",				Opt_PArr ),
  ( "th",				Opt_TH ),
  ( "implicit-prelude",  		Opt_ImplicitPrelude ),
  ( "scoped-type-variables",  		Opt_ScopedTypeVariables ),
  ( "bang-patterns",	  		Opt_BangPatterns ),
  ( "monomorphism-restriction",		Opt_MonomorphismRestriction ),
  ( "mono-pat-binds",			Opt_MonoPatBinds ),
  ( "implicit-params",			Opt_ImplicitParams ),
  ( "allow-overlapping-instances", 	Opt_AllowOverlappingInstances ),
  ( "allow-undecidable-instances", 	Opt_AllowUndecidableInstances ),
  ( "allow-incoherent-instances", 	Opt_AllowIncoherentInstances ),
  ( "generics",  			Opt_Generics ),
  ( "strictness",			Opt_Strictness ),
  ( "full-laziness",			Opt_FullLaziness ),
  ( "cse",				Opt_CSE ),
  ( "ignore-interface-pragmas",		Opt_IgnoreInterfacePragmas ),
  ( "omit-interface-pragmas",		Opt_OmitInterfacePragmas ),
  ( "do-lambda-eta-expansion",		Opt_DoLambdaEtaExpansion ),
  ( "ignore-asserts",			Opt_IgnoreAsserts ),
  ( "ignore-breakpoints",               Opt_IgnoreBreakpoints),
  ( "do-eta-reduction",			Opt_DoEtaReduction ),
  ( "case-merge",			Opt_CaseMerge ),
  ( "unbox-strict-fields",		Opt_UnboxStrictFields ),
  ( "dicts-cheap",			Opt_DictsCheap ),
  ( "excess-precision",			Opt_ExcessPrecision ),
  ( "asm-mangling",			Opt_DoAsmMangling )
  ]

glasgowExtsFlags = [ 
  Opt_GlasgowExts, 
  Opt_FFI, 
  Opt_ImplicitParams, 
  Opt_ScopedTypeVariables,
  Opt_BangPatterns ]

isFFlag f = f `elem` (map fst fFlags)
getFFlag f = fromJust (lookup f fFlags)

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.

parseDynamicFlags :: DynFlags -> [String] -> IO (DynFlags,[String])
parseDynamicFlags dflags args = do
  let ((leftover,errs),dflags') 
	  = runCmdLine (processArgs dynamic_flags args) dflags
  when (not (null errs)) $ do
    throwDyn (UsageError (unlines errs))
  return (dflags', leftover)


type DynP = CmdLineP DynFlags

upd :: (DynFlags -> DynFlags) -> DynP ()
upd f = do 
   dfs <- getCmdLineState
   putCmdLineState $! (f dfs)

setDynFlag, unSetDynFlag :: DynFlag -> DynP ()
setDynFlag f   = upd (\dfs -> dopt_set dfs f)
unSetDynFlag f = upd (\dfs -> dopt_unset dfs f)

setDumpFlag :: DynFlag -> OptKind DynP
setDumpFlag dump_flag 
  = NoArg (unSetDynFlag Opt_RecompChecking >> setDynFlag dump_flag)
	-- Whenver we -ddump, switch off the recompilation checker,
	-- else you don't see the dump!

setVerbosity "" = upd (\dfs -> dfs{ verbosity = 3 })
setVerbosity n 
  | all isDigit n = upd (\dfs -> dfs{ verbosity = read n })
  | otherwise     = throwDyn (UsageError "can't parse verbosity flag (-v<n>)")

addCmdlineHCInclude a = upd (\s -> s{cmdlineHcIncludes =  a : cmdlineHcIncludes s})

extraPkgConf_  p = upd (\s -> s{ extraPkgConfs = p : extraPkgConfs s })

exposePackage p = 
  upd (\s -> s{ packageFlags = ExposePackage p : packageFlags s })
hidePackage p = 
  upd (\s -> s{ packageFlags = HidePackage p : packageFlags s })
ignorePackage p = 
  upd (\s -> s{ packageFlags = IgnorePackage p : packageFlags s })

-- we can only switch between HscC, HscAsmm, and HscILX with dynamic flags 
-- (-fvia-C, -fasm, -filx respectively).
setTarget l = upd (\dfs -> case hscTarget dfs of
					HscC   -> dfs{ hscTarget = l }
					HscAsm -> dfs{ hscTarget = l }
					HscILX -> dfs{ hscTarget = l }
					_      -> dfs)

setOptLevel :: Int -> DynFlags -> DynFlags
setOptLevel n dflags
   | hscTarget dflags == HscInterpreted && n > 0
	= dflags
	    -- not in IO any more, oh well:
	    -- putStr "warning: -O conflicts with --interactive; -O ignored.\n"
   | otherwise
	= updOptLevel n dflags


setMainIs :: String -> DynP ()
setMainIs arg
  | not (null main_fn)		-- The arg looked like "Foo.baz"
  = upd $ \d -> d{ mainFunIs = Just main_fn,
	  	   mainModIs = mkModule main_mod }

  | isUpper (head main_mod)	-- The arg looked like "Foo"
  = upd $ \d -> d{ mainModIs = mkModule main_mod }
  
  | otherwise			-- The arg looked like "baz"
  = upd $ \d -> d{ mainFunIs = Just main_mod }
  where
    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

-----------------------------------------------------------------------------
-- Paths & Libraries

-- -i on its own deletes the import paths
addImportPath "" = upd (\s -> s{importPaths = []})
addImportPath p  = upd (\s -> s{importPaths = importPaths s ++ splitPathList p})


addLibraryPath p = 
  upd (\s -> s{libraryPaths = libraryPaths s ++ splitPathList p})

addIncludePath p = 
  upd (\s -> s{includePaths = includePaths s ++ splitPathList p})

addFrameworkPath p = 
  upd (\s -> s{frameworkPaths = frameworkPaths s ++ splitPathList p})

split_marker = ':'   -- not configurable (ToDo)

splitPathList :: String -> [String]
splitPathList s = filter notNull (splitUp s)
		-- empty paths are ignored: there might be a trailing
		-- ':' in the initial list, for example.  Empty paths can
		-- cause confusion when they are translated into -I options
		-- for passing to gcc.
  where
#ifndef mingw32_TARGET_OS
    splitUp xs = split split_marker xs
#else 
     -- Windows: 'hybrid' support for DOS-style paths in directory lists.
     -- 
     -- That is, if "foo:bar:baz" is used, this interpreted as
     -- consisting of three entries, 'foo', 'bar', 'baz'.
     -- However, with "c:/foo:c:\\foo;x:/bar", this is interpreted
     -- as 3 elts, "c:/foo", "c:\\foo", "x:/bar"
     --
     -- Notice that no attempt is made to fully replace the 'standard'
     -- split marker ':' with the Windows / DOS one, ';'. The reason being
     -- that this will cause too much breakage for users & ':' will
     -- work fine even with DOS paths, if you're not insisting on being silly.
     -- So, use either.
    splitUp []             = []
    splitUp (x:':':div:xs) | div `elem` dir_markers
			   = ((x:':':div:p): splitUp rs)
			   where
			      (p,rs) = findNextPath xs
	  -- we used to check for existence of the path here, but that
	  -- required the IO monad to be threaded through the command-line
   	  -- parser which is quite inconvenient.  The 
    splitUp xs = cons p (splitUp rs)
	       where
		 (p,rs) = findNextPath xs
    
		 cons "" xs = xs
		 cons x  xs = x:xs

    -- will be called either when we've consumed nought or the
    -- "<Drive>:/" part of a DOS path, so splitting is just a Q of
    -- finding the next split marker.
    findNextPath xs = 
        case break (`elem` split_markers) xs of
	   (p, d:ds) -> (p, ds)
	   (p, xs)   -> (p, xs)

    split_markers :: [Char]
    split_markers = [':', ';']

    dir_markers :: [Char]
    dir_markers = ['/', '\\']
#endif

-- -----------------------------------------------------------------------------
-- tmpDir, where we store temporary files.

setTmpDir :: FilePath -> DynFlags -> DynFlags
setTmpDir dir dflags = dflags{ tmpDir = canonicalise dir }
  where
#if !defined(mingw32_HOST_OS)
     canonicalise p = normalisePath p
#else
	-- Canonicalisation of temp path under win32 is a bit more
	-- involved: (a) strip trailing slash, 
	-- 	     (b) normalise slashes
	--	     (c) just in case, if there is a prefix /cygdrive/x/, change to x:
	-- 
     canonicalise path = normalisePath (xltCygdrive (removeTrailingSlash path))

        -- if we're operating under cygwin, and TMP/TEMP is of
	-- the form "/cygdrive/drive/path", translate this to
	-- "drive:/path" (as GHC isn't a cygwin app and doesn't
	-- understand /cygdrive paths.)
     xltCygdrive path
      | "/cygdrive/" `isPrefixOf` path = 
	  case drop (length "/cygdrive/") path of
	    drive:xs@('/':_) -> drive:':':xs
	    _ -> path
      | otherwise = path

        -- strip the trailing backslash (awful, but we only do this once).
     removeTrailingSlash path = 
       case last path of
         '/'  -> init path
         '\\' -> init path
         _    -> path
#endif

-----------------------------------------------------------------------------
-- Via-C compilation stuff

machdepCCOpts :: DynFlags -> ([String], -- flags for all C compilations
			      [String]) -- for registerised HC compilations
machdepCCOpts dflags
#if alpha_TARGET_ARCH
	=       ( ["-w", "-mieee"
#ifdef HAVE_THREADED_RTS_SUPPORT
		    , "-D_REENTRANT"
#endif
		   ], [] )
	-- For now, to suppress the gcc warning "call-clobbered
	-- register used for global register variable", we simply
	-- disable all warnings altogether using the -w flag. Oh well.

#elif hppa_TARGET_ARCH
        -- ___HPUX_SOURCE, not _HPUX_SOURCE, is #defined if -ansi!
        -- (very nice, but too bad the HP /usr/include files don't agree.)
	= ( ["-D_HPUX_SOURCE"], [] )

#elif m68k_TARGET_ARCH
      -- -fno-defer-pop : for the .hc files, we want all the pushing/
      --    popping of args to routines to be explicit; if we let things
      --    be deferred 'til after an STGJUMP, imminent death is certain!
      --
      -- -fomit-frame-pointer : *don't*
      --     It's better to have a6 completely tied up being a frame pointer
      --     rather than let GCC pick random things to do with it.
      --     (If we want to steal a6, then we would try to do things
      --     as on iX86, where we *do* steal the frame pointer [%ebp].)
	= ( [], ["-fno-defer-pop", "-fno-omit-frame-pointer"] )

#elif i386_TARGET_ARCH
      -- -fno-defer-pop : basically the same game as for m68k
      --
      -- -fomit-frame-pointer : *must* in .hc files; because we're stealing
      --   the fp (%ebp) for our register maps.
	=  let n_regs = stolen_x86_regs dflags
	       sta = opt_Static
	   in
	            ( [ if sta then "-DDONT_WANT_WIN32_DLL_SUPPORT" else ""
--                    , if suffixMatch "mingw32" cTARGETPLATFORM then "-mno-cygwin" else "" 
		      ],
		      [ "-fno-defer-pop",
#ifdef HAVE_GCC_MNO_OMIT_LFPTR
			-- Some gccs are configured with
			-- -momit-leaf-frame-pointer on by default, and it
			-- apparently takes precedence over 
			-- -fomit-frame-pointer, so we disable it first here.
			"-mno-omit-leaf-frame-pointer",
#endif
#ifdef HAVE_GCC_HAS_NO_UNIT_AT_A_TIME
		 	"-fno-unit-at-a-time",
			-- unit-at-a-time doesn't do us any good, and screws
			-- up -split-objs by moving the split markers around.
			-- It's only turned on with -O2, but put it here just
			-- in case someone uses -optc-O2.
#endif
			"-fomit-frame-pointer",
			-- we want -fno-builtin, because when gcc inlines
			-- built-in functions like memcpy() it tends to
			-- run out of registers, requiring -monly-n-regs
			"-fno-builtin",
	                "-DSTOLEN_X86_REGS="++show n_regs ]
		    )

#elif ia64_TARGET_ARCH
	= ( [], ["-fomit-frame-pointer", "-G0"] )

#elif x86_64_TARGET_ARCH
	= ( [], ["-fomit-frame-pointer",
		 "-fno-asynchronous-unwind-tables",
			-- the unwind tables are unnecessary for HC code,
			-- and get in the way of -split-objs.  Another option
			-- would be to throw them away in the mangler, but this
			-- is easier.
#ifdef HAVE_GCC_HAS_NO_UNIT_AT_A_TIME
		 "-fno-unit-at-a-time",
			-- unit-at-a-time doesn't do us any good, and screws
			-- up -split-objs by moving the split markers around.
			-- It's only turned on with -O2, but put it here just
			-- in case someone uses -optc-O2.
#endif
		 "-fno-builtin"
			-- calling builtins like strlen() using the FFI can
			-- cause gcc to run out of regs, so use the external
			-- version.
		] )

#elif mips_TARGET_ARCH
	= ( ["-static"], [] )

#elif sparc_TARGET_ARCH
	= ( [], ["-w"] )
	-- For now, to suppress the gcc warning "call-clobbered
	-- register used for global register variable", we simply
	-- disable all warnings altogether using the -w flag. Oh well.

#elif powerpc_apple_darwin_TARGET
      -- -no-cpp-precomp:
      --     Disable Apple's precompiling preprocessor. It's a great thing
      --     for "normal" programs, but it doesn't support register variable
      --     declarations.
        = ( [], ["-no-cpp-precomp"] )
#else
	= ( [], [] )
#endif

picCCOpts :: DynFlags -> [String]
picCCOpts dflags
#if darwin_TARGET_OS
      -- Apple prefers to do things the other way round.
      -- PIC is on by default.
      -- -mdynamic-no-pic:
      --     Turn off PIC code generation.
      -- -fno-common:
      --     Don't generate "common" symbols - these are unwanted
      --     in dynamic libraries.

    | opt_PIC
        = ["-fno-common"]
    | otherwise
        = ["-mdynamic-no-pic"]
#elif mingw32_TARGET_OS
      -- no -fPIC for Windows
        = []
#else
    | opt_PIC
        = ["-fPIC"]
    | otherwise
        = []
#endif

-- -----------------------------------------------------------------------------
-- Splitting

can_split :: Bool
can_split =  
#if    defined(i386_TARGET_ARCH)     \
    || defined(x86_64_TARGET_ARCH)   \
    || defined(alpha_TARGET_ARCH)    \
    || defined(hppa_TARGET_ARCH)     \
    || defined(m68k_TARGET_ARCH)     \
    || defined(mips_TARGET_ARCH)     \
    || defined(powerpc_TARGET_ARCH)  \
    || defined(rs6000_TARGET_ARCH)   \
    || defined(sparc_TARGET_ARCH) 
   True
#else
   False
#endif

