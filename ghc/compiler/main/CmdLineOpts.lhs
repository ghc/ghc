
% (c) The University of Glasgow, 1996-2000
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}

module CmdLineOpts (
	CoreToDo(..), buildCoreToDo, StgToDo(..),
	SimplifierSwitch(..), 
	SimplifierMode(..), FloatOutSwitches(..),

	HscLang(..),
	DynFlag(..),	-- needed non-abstractly by DriverFlags
	DynFlags(..),

	v_Static_hsc_opts,

	isStaticHscFlag,

	-- Manipulating DynFlags
	defaultDynFlags,		-- DynFlags
	defaultHscLang,			-- HscLang
	dopt,				-- DynFlag -> DynFlags -> Bool
	dopt_set, dopt_unset,		-- DynFlags -> DynFlag -> DynFlags
	dopt_CoreToDo,			-- DynFlags -> [CoreToDo]
	dopt_StgToDo,			-- DynFlags -> [StgToDo]
	dopt_HscLang,			-- DynFlags -> HscLang
	dopt_OutName,			-- DynFlags -> String
	getOpts,			-- (DynFlags -> [a]) -> IO [a]
	setLang,
	getVerbFlag,
	setOptLevel,

	-- Manipulating the DynFlags state
	getDynFlags,			-- IO DynFlags
	setDynFlags,			-- DynFlags -> IO ()
	updDynFlags, 			-- (DynFlags -> DynFlags) -> IO ()
	dynFlag,			-- (DynFlags -> a) -> IO a
	setDynFlag, unSetDynFlag,	-- DynFlag -> IO ()
	saveDynFlags, 		 	-- IO ()
	restoreDynFlags,		-- IO DynFlags

	-- sets of warning opts
 	standardWarnings,
	minusWOpts,
	minusWallOpts,

	-- Output style options
	opt_PprStyle_NoPrags,
	opt_PprStyle_RawTypes,
	opt_PprUserLength,
	opt_PprStyle_Debug,

	-- profiling opts
	opt_AutoSccsOnAllToplevs,
	opt_AutoSccsOnExportedToplevs,
	opt_AutoSccsOnIndividualCafs,
	opt_AutoSccsOnDicts,
	opt_SccProfilingOn,
	opt_DoTickyProfiling,

	-- language opts
	opt_AllStrict,
	opt_DictsStrict,
        opt_MaxContextReductionDepth,
	opt_IrrefutableTuples,
	opt_NumbersStrict,
	opt_Parallel,
	opt_SMP,
	opt_RuntimeTypes,
	opt_Flatten,

	-- optimisation opts
	opt_NoMethodSharing,
	opt_DoSemiTagging,
	opt_LiberateCaseThreshold,
	opt_CprOff,
	opt_RulesOff,
	opt_SimplNoPreInlining,
	opt_SimplExcessPrecision,
	opt_MaxWorkerArgs,

	-- Unfolding control
	opt_UF_CreationThreshold,
	opt_UF_UseThreshold,
	opt_UF_FunAppDiscount,
	opt_UF_KeenessFactor,
	opt_UF_UpdateInPlace,
	opt_UF_CheapOp,
	opt_UF_DearOp,

	-- misc opts
	opt_InPackage,
	opt_EmitCExternDecls,
	opt_EnsureSplittableC,
	opt_GranMacros,
	opt_HiVersion,
	opt_HistorySize,
        opt_NoHiCheck,
	opt_OmitBlackHoling,
	opt_NoPruneDecls,
	opt_Static,
	opt_Unregisterised,
	opt_EmitExternalCore
    ) where

#include "HsVersions.h"

import Constants	-- Default values for some flags
import Util
import FastString	( FastString, mkFastString )
import Config
import Maybes		( firstJust )

import GLAEXTS
import DATA_IOREF	( IORef, readIORef, writeIORef )
import UNSAFE_IO	( unsafePerformIO )
\end{code}

%************************************************************************
%*									*
\subsection{Command-line options}
%*									*
%************************************************************************

The hsc command-line options are split into two categories:

  - static flags
  - dynamic flags

Static flags are represented by top-level values of type Bool or Int,
for example.  They therefore have the same value throughout the
invocation of hsc.

Dynamic flags are represented by an abstract type, DynFlags, which is
passed into hsc by the compilation manager for every compilation.
Dynamic flags are those that change on a per-compilation basis,
perhaps because they may be present in the OPTIONS pragma at the top
of a module.

Other flag-related blurb:

A list of {\em ToDo}s is things to be done in a particular part of
processing.  A (fictitious) example for the Core-to-Core simplifier
might be: run the simplifier, then run the strictness analyser, then
run the simplifier again (three ``todos'').

There are three ``to-do processing centers'' at the moment.  In the
main loop (\tr{main/Main.lhs}), in the Core-to-Core processing loop
(\tr{simplCore/SimplCore.lhs), and in the STG-to-STG processing loop
(\tr{simplStg/SimplStg.lhs}).

%************************************************************************
%*									*
\subsection{Datatypes associated with command-line options}
%*									*
%************************************************************************

\begin{code}
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
\end{code}

\begin{code}
data StgToDo
  = StgDoMassageForProfiling  -- should be (next to) last
  -- There's also setStgVarInfo, but its absolute "lastness"
  -- is so critical that it is hardwired in (no flag).
  | D_stg_stats
\end{code}

\begin{code}
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
\end{code}

%************************************************************************
%*									*
\subsection{Dynamic command-line options}
%*									*
%************************************************************************

\begin{code}
data DynFlag

   -- debugging flags
   = Opt_D_dump_absC
   | Opt_D_dump_asm
   | Opt_D_dump_cpranal
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_flatC
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_realC
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
   | Opt_D_dump_stix
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
   | Opt_DoCoreLinting
   | Opt_DoStgLinting

   | Opt_WarnIsError		-- -Werror; makes warnings fatal
   | Opt_WarnDuplicateExports
   | Opt_WarnHiShadows
   | Opt_WarnIncompletePatterns
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
   | Opt_WarnMisc

   -- language opts
   | Opt_AllowOverlappingInstances
   | Opt_AllowUndecidableInstances
   | Opt_AllowIncoherentInstances
   | Opt_NoMonomorphismRestriction
   | Opt_GlasgowExts
   | Opt_FFI
   | Opt_PArr			       -- syntactic support for parallel arrays
   | Opt_Arrows			       -- Arrow-notation syntax
   | Opt_TH
   | Opt_ImplicitParams
   | Opt_Generics
   | Opt_NoImplicitPrelude 

   -- optimisation opts
   | Opt_Strictness
   | Opt_CSE
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_UnboxStrictFields

   deriving (Eq)

data DynFlags = DynFlags {
  coreToDo   		:: Maybe [CoreToDo], -- reserved for use with -Ofile
  stgToDo    		:: [StgToDo],
  hscLang    		:: HscLang,
  hscOutName 		:: String,  	-- name of the output file
  hscStubHOutName	:: String,  	-- name of the .stub_h output file
  hscStubCOutName	:: String,  	-- name of the .stub_c output file
  extCoreName		:: String,	-- name of the .core output file
  verbosity  		:: Int,	 	-- verbosity level
  optLevel		:: Int,		-- optimisation level
  maxSimplIterations    :: Int,		-- max simplifier iterations
  ruleCheck		:: Maybe String,
  cppFlag    		:: Bool,	-- preprocess with cpp?
  ppFlag                :: Bool,        -- preprocess with a Haskell Pp?
  stolen_x86_regs	:: Int,		
  cmdlineHcIncludes	:: [String],	-- -#includes

  -- options for particular phases
  opt_L			:: [String],
  opt_P			:: [String],
  opt_F			:: [String],
  opt_c			:: [String],
  opt_a			:: [String],
  opt_m			:: [String],
#ifdef ILX			   
  opt_I			:: [String],
  opt_i			:: [String],
#endif

  -- hsc dynamic flags
  flags      		:: [DynFlag]
 }

data HscLang
  = HscC
  | HscAsm
  | HscJava
  | HscILX
  | HscInterpreted
  | HscNothing
    deriving (Eq, Show)

defaultHscLang
  | cGhcWithNativeCodeGen == "YES" && 
	(prefixMatch "i386" cTARGETPLATFORM ||
	 prefixMatch "sparc" cTARGETPLATFORM ||
	 prefixMatch "powerpc" cTARGETPLATFORM)   =  HscAsm
  | otherwise					=  HscC

defaultDynFlags = DynFlags {
  coreToDo = Nothing, stgToDo = [], 
  hscLang = defaultHscLang, 
  hscOutName = "", 
  hscStubHOutName = "", hscStubCOutName = "",
  extCoreName = "",
  verbosity		= 0, 
  optLevel		= 0,
  maxSimplIterations    = 4,
  ruleCheck		= Nothing,
  cppFlag		= False,
  ppFlag                = False,
  stolen_x86_regs	= 4,
  cmdlineHcIncludes	= [],
  opt_L			= [],
  opt_P			= [],
  opt_F                 = [],
  opt_c			= [],
  opt_a			= [],
  opt_m			= [],
#ifdef ILX
  opt_I                 = [],
  opt_i                 = [],
#endif
  flags = [ 
	    Opt_Generics,
			-- Generating the helper-functions for
			-- generics is now on by default
	    Opt_Strictness,
			-- strictness is on by default, but this only
			-- applies to -O.
	    Opt_CSE,
			-- similarly for CSE.
	    Opt_DoLambdaEtaExpansion,
			-- This one is important for a tiresome reason:
			-- we want to make sure that the bindings for data 
			-- constructors are eta-expanded.  This is probably
			-- a good thing anyway, but it seems fragile.

	    -- and the default no-optimisation options:
	    Opt_IgnoreInterfacePragmas,
	    Opt_OmitInterfacePragmas

           ] ++ standardWarnings,
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

dopt_CoreToDo :: DynFlags -> Maybe [CoreToDo]
dopt_CoreToDo = coreToDo

dopt_StgToDo :: DynFlags -> [StgToDo]
dopt_StgToDo = stgToDo

dopt_OutName :: DynFlags -> String
dopt_OutName = hscOutName

dopt_HscLang :: DynFlags -> HscLang
dopt_HscLang = hscLang

dopt_set :: DynFlags -> DynFlag -> DynFlags
dopt_set dfs f = dfs{ flags = f : flags dfs }

dopt_unset :: DynFlags -> DynFlag -> DynFlags
dopt_unset dfs f = dfs{ flags = filter (/= f) (flags dfs) }

getOpts :: (DynFlags -> [a]) -> IO [a]
	-- We add to the options from the front, so we need to reverse the list
getOpts opts = dynFlag opts >>= return . reverse

-- we can only switch between HscC, HscAsmm, and HscILX with dynamic flags 
-- (-fvia-C, -fasm, -filx respectively).
setLang l = updDynFlags (\ dfs -> case hscLang dfs of
					HscC   -> dfs{ hscLang = l }
					HscAsm -> dfs{ hscLang = l }
					HscILX -> dfs{ hscLang = l }
					_      -> dfs)

getVerbFlag = do
   verb <- dynFlag verbosity
   if verb >= 3  then return  "-v" else return ""

-----------------------------------------------------------------------------
-- Setting the optimisation level

setOptLevel :: Int -> IO ()
setOptLevel n 
  = do dflags <- getDynFlags
       if hscLang dflags == HscInterpreted && n > 0
	  then putStr "warning: -O conflicts with --interactive; -O ignored.\n"
	  else updDynFlags (setOptLevel' n)

setOptLevel' n dfs
  = if (n >= 1)
     then dfs2{ hscLang = HscC, optLevel = n } -- turn on -fvia-C with -O
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

-- Core-to-core phases:

buildCoreToDo :: DynFlags -> [CoreToDo]
buildCoreToDo dflags = core_todo
  where
    opt_level  = optLevel dflags
    max_iter   = maxSimplIterations dflags
    strictness = dopt Opt_Strictness dflags
    cse        = dopt Opt_CSE dflags
    rule_check = ruleCheck dflags

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

            NoCaseOfCase,
			-- Don't do case-of-case transformations.
			-- This makes full laziness work better
	    MaxSimplifierIterations max_iter
	],

	-- Specialisation is best done before full laziness
	-- so that overloaded functions have all their dictionary lambdas manifest
	CoreDoSpecialising,

	CoreDoFloatOutwards (FloatOutSw False False),
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

	CoreDoFloatOutwards (FloatOutSw False	-- Not lambdas
					True),	-- Float constants
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

	case rule_check of { Just pat -> CoreDoRuleCheck 0 pat; Nothing -> CoreDoNothing },

	if opt_level >= 2 then
	   CoreLiberateCase
	else
	   CoreDoNothing,
	if opt_level >= 2 then
	   CoreDoSpecConstr
	else
	   CoreDoNothing,

	-- Final clean-up simplification:
	CoreDoSimplify (SimplPhase 0) [
	  MaxSimplifierIterations max_iter
	]
     ]

-- --------------------------------------------------------------------------
-- Mess about with the mutable variables holding the dynamic arguments

-- v_InitDynFlags 
--	is the "baseline" dynamic flags, initialised from
-- 	the defaults and command line options, and updated by the
--	':s' command in GHCi.
--
-- v_DynFlags
--	is the dynamic flags for the current compilation.  It is reset
--	to the value of v_InitDynFlags before each compilation, then
--	updated by reading any OPTIONS pragma in the current module.

GLOBAL_VAR(v_InitDynFlags, defaultDynFlags, DynFlags)
GLOBAL_VAR(v_DynFlags,     defaultDynFlags, DynFlags)

setDynFlags :: DynFlags -> IO ()
setDynFlags dfs = writeIORef v_DynFlags dfs

saveDynFlags :: IO ()
saveDynFlags = do dfs <- readIORef v_DynFlags
		  writeIORef v_InitDynFlags dfs

restoreDynFlags :: IO DynFlags
restoreDynFlags = do dfs <- readIORef v_InitDynFlags
		     writeIORef v_DynFlags dfs
		     return dfs

getDynFlags :: IO DynFlags
getDynFlags = readIORef v_DynFlags

updDynFlags :: (DynFlags -> DynFlags) -> IO ()
updDynFlags f = do dfs <- readIORef v_DynFlags
		   writeIORef v_DynFlags (f dfs)

dynFlag :: (DynFlags -> a) -> IO a
dynFlag f = do dflags <- readIORef v_DynFlags; return (f dflags)

setDynFlag, unSetDynFlag :: DynFlag -> IO ()
setDynFlag f   = updDynFlags (\dfs -> dopt_set dfs f)
unSetDynFlag f = updDynFlags (\dfs -> dopt_unset dfs f)
\end{code}


%************************************************************************
%*									*
\subsection{Warnings}
%*									*
%************************************************************************

\begin{code}
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
	Opt_WarnMisc
      ]

minusWallOpts
    = minusWOpts ++
      [	Opt_WarnTypeDefaults,
	Opt_WarnNameShadowing,
	Opt_WarnMissingSigs,
	Opt_WarnHiShadows
      ]
\end{code}

%************************************************************************
%*									*
\subsection{Classifying command-line options}
%*									*
%************************************************************************

\begin{code}
-- v_Statis_hsc_opts is here to avoid a circular dependency with
-- main/DriverState.
GLOBAL_VAR(v_Static_hsc_opts, [], [String])

lookUp	       	 :: FastString -> Bool
lookup_int     	 :: String -> Maybe Int
lookup_def_int   :: String -> Int -> Int
lookup_def_float :: String -> Float -> Float
lookup_str       :: String -> Maybe String

unpacked_static_opts = unsafePerformIO (readIORef v_Static_hsc_opts)
packed_static_opts   = map mkFastString unpacked_static_opts

lookUp     sw = sw `elem` packed_static_opts
	
lookup_str sw = firstJust (map (startsWith sw) unpacked_static_opts)

lookup_int sw = case (lookup_str sw) of
		  Nothing -> Nothing
		  Just xx -> Just (read xx)

lookup_def_int sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> read xx

lookup_def_float sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> read xx


{-
 Putting the compiler options into temporary at-files
 may turn out to be necessary later on if we turn hsc into
 a pure Win32 application where I think there's a command-line
 length limit of 255. unpacked_opts understands the @ option.

unpacked_opts :: [String]
unpacked_opts =
  concat $
  map (expandAts) $
  map unpackFS argv  -- NOT ARGV any more: v_Static_hsc_opts
  where
   expandAts ('@':fname) = words (unsafePerformIO (readFile fname))
   expandAts l = [l]
-}
\end{code}

%************************************************************************
%*									*
\subsection{Static options}
%*									*
%************************************************************************

\begin{code}
-- debugging opts
opt_PprStyle_NoPrags		= lookUp  FSLIT("-dppr-noprags")
opt_PprStyle_Debug		= lookUp  FSLIT("-dppr-debug")
opt_PprStyle_RawTypes		= lookUp  FSLIT("-dppr-rawtypes")
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name

-- profiling opts
opt_AutoSccsOnAllToplevs	= lookUp  FSLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookUp  FSLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookUp  FSLIT("-fauto-sccs-on-individual-cafs")
opt_AutoSccsOnDicts		= lookUp  FSLIT("-fauto-sccs-on-dicts")
opt_SccProfilingOn		= lookUp  FSLIT("-fscc-profiling")
opt_DoTickyProfiling		= lookUp  FSLIT("-fticky-ticky")

-- language opts
opt_AllStrict			= lookUp  FSLIT("-fall-strict")
opt_DictsStrict			= lookUp  FSLIT("-fdicts-strict")
opt_IrrefutableTuples		= lookUp  FSLIT("-firrefutable-tuples")
opt_MaxContextReductionDepth	= lookup_def_int "-fcontext-stack" mAX_CONTEXT_REDUCTION_DEPTH
opt_NumbersStrict		= lookUp  FSLIT("-fnumbers-strict")
opt_Parallel			= lookUp  FSLIT("-fparallel")
opt_SMP				= lookUp  FSLIT("-fsmp")
opt_Flatten			= lookUp  FSLIT("-fflatten")

-- optimisation opts
opt_NoMethodSharing		= lookUp  FSLIT("-fno-method-sharing")
opt_DoSemiTagging		= lookUp  FSLIT("-fsemi-tagging")
opt_CprOff			= lookUp  FSLIT("-fcpr-off")
opt_RulesOff			= lookUp  FSLIT("-frules-off")
	-- Switch off CPR analysis in the new demand analyser
opt_LiberateCaseThreshold	= lookup_def_int "-fliberate-case-threshold" (10::Int)
opt_MaxWorkerArgs		= lookup_def_int "-fmax-worker-args" (10::Int)

{-
   The optional '-inpackage=P' flag tells what package
   we are compiling this module for.
   The Prelude, for example is compiled with '-inpackage std'
-}
opt_InPackage			= case lookup_str "-inpackage=" of
				    Just p  -> mkFastString p
				    Nothing -> FSLIT("Main")	-- The package name if none is specified

opt_EmitCExternDecls	        = lookUp  FSLIT("-femit-extern-decls")
opt_EnsureSplittableC		= lookUp  FSLIT("-fglobalise-toplev-names")
opt_GranMacros			= lookUp  FSLIT("-fgransim")
opt_HiVersion			= read (cProjectVersionInt ++ cProjectPatchLevel) :: Int
opt_HistorySize			= lookup_def_int "-fhistory-size" 20
opt_NoHiCheck                   = lookUp  FSLIT("-fno-hi-version-check")
opt_OmitBlackHoling		= lookUp  FSLIT("-dno-black-holing")
opt_RuntimeTypes		= lookUp  FSLIT("-fruntime-types")

-- Simplifier switches
opt_SimplNoPreInlining		= lookUp  FSLIT("-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplExcessPrecision	= lookUp  FSLIT("-fexcess-precision")

-- Unfolding control
opt_UF_CreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  (45::Int)
opt_UF_UseThreshold		= lookup_def_int "-funfolding-use-threshold"	   (8::Int)	-- Discounts can be big
opt_UF_FunAppDiscount		= lookup_def_int "-funfolding-fun-discount"	   (6::Int)	-- It's great to inline a fn
opt_UF_KeenessFactor		= lookup_def_float "-funfolding-keeness-factor"	   (1.5::Float)
opt_UF_UpdateInPlace		= lookUp  FSLIT("-funfolding-update-in-place")

opt_UF_CheapOp  = ( 1 :: Int)	-- Only one instruction; and the args are charged for
opt_UF_DearOp   = ( 4 :: Int)
			
opt_NoPruneDecls		= lookUp  FSLIT("-fno-prune-decls")
opt_Static			= lookUp  FSLIT("-static")
opt_Unregisterised		= lookUp  FSLIT("-funregisterised")
opt_EmitExternalCore		= lookUp  FSLIT("-fext-core")
\end{code}

%************************************************************************
%*									*
\subsection{List of static hsc flags}
%*									*
%************************************************************************

\begin{code}
isStaticHscFlag f =
  f `elem` [
	"fauto-sccs-on-all-toplevs",
	"fauto-sccs-on-exported-toplevs",
	"fauto-sccs-on-individual-cafs",
	"fauto-sccs-on-dicts",
	"fscc-profiling",
	"fticky-ticky",
	"fall-strict",
	"fdicts-strict",
	"firrefutable-tuples",
	"fnumbers-strict",
	"fparallel",
	"fsmp",
	"fflatten",
	"fsemi-tagging",
	"flet-no-escape",
	"femit-extern-decls",
	"fglobalise-toplev-names",
	"fgransim",
	"fno-hi-version-check",
	"dno-black-holing",
	"fno-method-sharing",
	"fruntime-types",
	"fno-pre-inlining",
	"fexcess-precision",
	"funfolding-update-in-place",
	"fno-prune-decls",
	"static",
	"funregisterised",
	"fext-core",
	"frule-check",
	"frules-off",
	"fcpr-off"
	]
  || any (flip prefixMatch f) [
	"fcontext-stack",
	"fliberate-case-threshold",
	"fmax-worker-args",
	"fhistory-size",
	"funfolding-creation-threshold",
	"funfolding-use-threshold",
	"funfolding-fun-discount",
	"funfolding-keeness-factor"
     ]
\end{code}

%************************************************************************
%*									*
\subsection{Misc functions for command-line options}
%*									*
%************************************************************************



\begin{code}
startsWith :: String -> String -> Maybe String
-- startsWith pfx (pfx++rest) = Just rest

startsWith []     str = Just str
startsWith (c:cs) (s:ss)
  = if c /= s then Nothing else startsWith cs ss
startsWith  _	  []  = Nothing

endsWith  :: String -> String -> Maybe String
endsWith cs ss
  = case (startsWith (reverse cs) (reverse ss)) of
      Nothing -> Nothing
      Just rs -> Just (reverse rs)
\end{code}
