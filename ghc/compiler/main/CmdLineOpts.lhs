%
% (c) The University of Glasgow, 1996-2000
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}

module CmdLineOpts (
	CoreToDo(..),
	SimplifierSwitch(..), isAmongSimpl,
	StgToDo(..),
	SwitchResult(..),
	HscLang(..),
	DynFlag(..),	-- needed non-abstractly by DriverFlags
	DynFlags(..),

	intSwitchSet,
	switchIsOn,
	isStaticHscFlag,

	-- debugging opts
	dopt_D_dump_absC,
	dopt_D_dump_asm,
	dopt_D_dump_cpranal,
	dopt_D_dump_cse,
	dopt_D_dump_deriv,
	dopt_D_dump_ds,
	dopt_D_dump_flatC,
	dopt_D_dump_foreign,
	dopt_D_dump_hi_diffs,
	dopt_D_dump_inlinings,
	dopt_D_dump_occur_anal,
	dopt_D_dump_parsed,
	dopt_D_dump_realC,
	dopt_D_dump_rn,
	dopt_D_dump_rules,
	dopt_D_dump_simpl,
	dopt_D_dump_simpl_iterations,
	dopt_D_dump_simpl_stats,
	dopt_D_dump_spec,
	dopt_D_dump_stg,
	dopt_D_dump_stranal,
	dopt_D_dump_tc,
	dopt_D_dump_types,
        dopt_D_dump_usagesp,
	dopt_D_dump_worker_wrapper,
	dopt_D_show_passes,
	dopt_D_dump_rn_trace,
	dopt_D_dump_rn_stats,
        dopt_D_dump_stix,
	dopt_D_dump_minimal_imports,
	dopt_D_source_stats,
	dopt_D_verbose_core2core,
	dopt_D_verbose_stg2stg,
	dopt_DoCoreLinting,
	dopt_DoStgLinting,
        dopt_DoUSPLinting,

	opt_PprStyle_NoPrags,
	opt_PprUserLength,
	opt_PprStyle_Debug,

	-- other dynamic flags
	dopt_CoreToDo,
	dopt_StgToDo,

	-- warning opts
	opt_WarnDuplicateExports,
	opt_WarnHiShadows,
	opt_WarnIncompletePatterns,
	opt_WarnMissingFields,
	opt_WarnMissingMethods,
	opt_WarnMissingSigs,
	opt_WarnNameShadowing,
	opt_WarnOverlappingPatterns,
	opt_WarnSimplePatterns,
	opt_WarnTypeDefaults,
	opt_WarnUnusedBinds,
	opt_WarnUnusedImports,
	opt_WarnUnusedMatches,
	opt_WarnDeprecations,

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
        dopt_AllowOverlappingInstances,
 	dopt_AllowUndecidableInstances,
	dopt_GlasgowExts,
	opt_Generics,
	opt_IrrefutableTuples,
	opt_NumbersStrict,
	opt_Parallel,
	opt_SMP,

	-- optimisation opts
	opt_DoSemiTagging,
	opt_FoldrBuildOn,
	opt_LiberateCaseThreshold,
	opt_StgDoLetNoEscapes,
	opt_UnfoldCasms,
        opt_UsageSPOn,
	opt_UnboxStrictFields,
	opt_SimplNoPreInlining,
	opt_SimplDoEtaReduction,
	opt_SimplDoLambdaEtaExpansion,
	opt_SimplCaseOfCase,
	opt_SimplCaseMerge,
	opt_SimplPedanticBottoms,
	opt_SimplExcessPrecision,

	-- Unfolding control
	opt_UF_HiFileThreshold,
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
	opt_IgnoreAsserts,
	opt_IgnoreIfacePragmas,
        opt_NoHiCheck,
	opt_NoImplicitPrelude,
	opt_OmitBlackHoling,
	opt_OmitInterfacePragmas,
	opt_NoPruneTyDecls,
	opt_NoPruneDecls,
	opt_ReportCompile,
	opt_Static,
	opt_Unregisterised,
	opt_Verbose,

	-- Code generation
	opt_UseVanillaRegs,
	opt_UseFloatRegs,
	opt_UseDoubleRegs,
	opt_UseLongRegs
    ) where

#include "HsVersions.h"

import Array	( array, (//) )
import GlaExts
import Argv
import Constants	-- Default values for some flags
import Util
import FastTypes

import Maybes		( firstJust )
import Panic		( panic )

#if __GLASGOW_HASKELL__ < 301
import ArrBase	( Array(..) )
#else
import PrelArr  ( Array(..) )
#endif
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
data SwitchResult
  = SwBool	Bool		-- on/off
  | SwString	FAST_STRING	-- nothing or a String
  | SwInt	Int		-- nothing or an Int
\end{code}

\begin{code}
data CoreToDo		-- These are diff core-to-core passes,
			-- which may be invoked in any order,
  			-- as many times as you like.

  = CoreDoSimplify	-- The core-to-core simplifier.
	(SimplifierSwitch -> SwitchResult)
			-- Each run of the simplifier can take a different
			-- set of simplifier-specific flags.
  | CoreDoFloatInwards
  | CoreDoFloatOutwards Bool	-- True <=> float lambdas to top level
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoUSPInf
  | CoreDoCPResult 
  | CoreDoGlomBinds
  | CoreCSE

  | CoreDoNothing 	 -- useful when building up lists of these things
\end{code}

\begin{code}
data StgToDo
  = StgDoStaticArgs
  | StgDoLambdaLift
  | StgDoMassageForProfiling  -- should be (next to) last
  -- There's also setStgVarInfo, but its absolute "lastness"
  -- is so critical that it is hardwired in (no flag).
  | D_stg_stats
\end{code}

\begin{code}
data SimplifierSwitch
  = MaxSimplifierIterations Int
  | SimplInlinePhase Int
  | DontApplyRules
  | NoCaseOfCase
  | SimplLetToCase
\end{code}

%************************************************************************
%*									*
\subsection{Dynamic command-line options}
%*									*
%************************************************************************

\begin{code}
data DynFlag

   -- debugging flags
   = Opt_D_dump_all
   | Opt_D_dump_most
   | Opt_D_dump_absC
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
   | Opt_D_dump_stg
   | Opt_D_dump_stranal
   | Opt_D_dump_tc
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_usagesp
   | Opt_D_dump_cse
   | Opt_D_dump_worker_wrapper
   | Opt_D_show_passes
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_stix
   | Opt_D_dump_simpl_stats
   | Opt_D_source_stats
   | Opt_D_verbose_core2core
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_minimal_imports
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoUSPLinting

   -- language opts
   | Opt_AllowOverlappingInstances
   | Opt_AllowUndecidableInstances
   | Opt_GlasgowExts
   deriving (Eq)

data DynFlags = DynFlags {
  coreToDo :: CoreToDo,
  stgToDo  :: StgToDo,
  hscLang  :: HscLang,
  flags    :: [DynFlag]
 }

boolOpt :: DynFlag -> DynFlags -> Bool
boolOpt f dflags  = f `elem` (flags dflags)

dopt_D_dump_all              = boolOpt Opt_D_dump_all
dopt_D_dump_most             = boolOpt Opt_D_dump_most
dopt_D_dump_absC             = boolOpt Opt_D_dump_absC
dopt_D_dump_asm              = boolOpt Opt_D_dump_asm
dopt_D_dump_cpranal          = boolOpt Opt_D_dump_cpranal
dopt_D_dump_deriv            = boolOpt Opt_D_dump_deriv
dopt_D_dump_ds               = boolOpt Opt_D_dump_ds
dopt_D_dump_flatC            = boolOpt Opt_D_dump_flatC
dopt_D_dump_foreign          = boolOpt Opt_D_dump_foreign
dopt_D_dump_inlinings        = boolOpt Opt_D_dump_inlinings
dopt_D_dump_occur_anal       = boolOpt Opt_D_dump_occur_anal
dopt_D_dump_parsed           = boolOpt Opt_D_dump_parsed
dopt_D_dump_realC            = boolOpt Opt_D_dump_realC
dopt_D_dump_rn               = boolOpt Opt_D_dump_rn
dopt_D_dump_simpl            = boolOpt Opt_D_dump_simpl
dopt_D_dump_simpl_iterations = boolOpt Opt_D_dump_simpl_iterations
dopt_D_dump_spec             = boolOpt Opt_D_dump_spec
dopt_D_dump_stg              = boolOpt Opt_D_dump_stg
dopt_D_dump_stranal          = boolOpt Opt_D_dump_stranal
dopt_D_dump_tc               = boolOpt Opt_D_dump_tc
dopt_D_dump_types            = boolOpt Opt_D_dump_types
dopt_D_dump_rules            = boolOpt Opt_D_dump_rules
dopt_D_dump_usagesp          = boolOpt Opt_D_dump_usagesp
dopt_D_dump_cse              = boolOpt Opt_D_dump_cse
dopt_D_dump_worker_wrapper   = boolOpt Opt_D_dump_worker_wrapper
dopt_D_show_passes           = boolOpt Opt_D_show_passes
dopt_D_dump_rn_trace         = boolOpt Opt_D_dump_rn_trace
dopt_D_dump_rn_stats         = boolOpt Opt_D_dump_rn_stats
dopt_D_dump_stix             = boolOpt Opt_D_dump_stix
dopt_D_dump_simpl_stats      = boolOpt Opt_D_dump_simpl_stats
dopt_D_source_stats          = boolOpt Opt_D_source_stats
dopt_D_verbose_core2core     = boolOpt Opt_D_verbose_core2core
dopt_D_verbose_stg2stg       = boolOpt Opt_D_verbose_stg2stg
dopt_D_dump_hi_diffs         = boolOpt Opt_D_dump_hi_diffs
dopt_D_dump_minimal_imports  = boolOpt Opt_D_dump_minimal_imports
dopt_DoCoreLinting           = boolOpt Opt_DoCoreLinting
dopt_DoStgLinting            = boolOpt Opt_DoStgLinting
dopt_DoUSPLinting            = boolOpt Opt_DoUSPLinting

dopt_AllowOverlappingInstances = boolOpt Opt_AllowOverlappingInstances
dopt_AllowUndecidableInstances = boolOpt Opt_AllowUndecidableInstances
dopt_GlasgowExts               = boolOpt Opt_GlasgowExts

dopt_CoreToDo :: DynFlags -> CoreToDo
dopt_CoreToDo = coreToDo

dopt_StgToDo :: DynFlags -> StgToDo
dopt_StgToDo = stgToDo

data HscLang
  = HscC
  | HscAsm
  | HscJava
  deriving Eq

dopt_HscLang :: DynFlags -> HscLang
dopt_HscLang = hscLang
\end{code}

%************************************************************************
%*									*
\subsection{Classifying command-line options}
%*									*
%************************************************************************

\begin{code}
lookUp	       	 :: FAST_STRING -> Bool
lookup_int     	 :: String -> Maybe Int
lookup_def_int   :: String -> Int -> Int
lookup_def_float :: String -> Float -> Float
lookup_str       :: String -> Maybe String

lookUp     sw = sw `elem` argv
	
lookup_str sw = firstJust (map (startsWith sw) unpacked_opts)

lookup_int sw = case (lookup_str sw) of
		  Nothing -> Nothing
		  Just xx -> Just (read xx)

lookup_def_int sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> read xx

lookup_def_char sw def = case (lookup_str sw) of
		  	    Just (xx:_) -> xx
			    _           -> def		-- Use default

lookup_def_float sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> read xx

unpacked_opts = map _UNPK_ argv

{-
 Putting the compiler options into temporary at-files
 may turn out to be necessary later on if we turn hsc into
 a pure Win32 application where I think there's a command-line
 length limit of 255. unpacked_opts understands the @ option.

unpacked_opts :: [String]
unpacked_opts =
  concat $
  map (expandAts) $
  map _UNPK_ argv
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
opt_PprStyle_NoPrags		= lookUp  SLIT("-dppr-noprags")
opt_PprStyle_Debug		= lookUp  SLIT("-dppr-debug")
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name

-- warning opts
opt_WarnDuplicateExports	= lookUp  SLIT("-fwarn-duplicate-exports")
opt_WarnHiShadows		= lookUp  SLIT("-fwarn-hi-shadowing")
opt_WarnIncompletePatterns	= lookUp  SLIT("-fwarn-incomplete-patterns")
opt_WarnMissingFields		= lookUp  SLIT("-fwarn-missing-fields")
opt_WarnMissingMethods		= lookUp  SLIT("-fwarn-missing-methods")
opt_WarnMissingSigs		= lookUp  SLIT("-fwarn-missing-signatures")
opt_WarnNameShadowing		= lookUp  SLIT("-fwarn-name-shadowing")
opt_WarnOverlappingPatterns	= lookUp  SLIT("-fwarn-overlapping-patterns")
opt_WarnSimplePatterns	     	= lookUp  SLIT("-fwarn-simple-patterns")
opt_WarnTypeDefaults		= lookUp  SLIT("-fwarn-type-defaults")
opt_WarnUnusedBinds		= lookUp  SLIT("-fwarn-unused-binds")
opt_WarnUnusedImports		= lookUp  SLIT("-fwarn-unused-imports")
opt_WarnUnusedMatches		= lookUp  SLIT("-fwarn-unused-matches")
opt_WarnDeprecations		= lookUp  SLIT("-fwarn-deprecations")

-- profiling opts
opt_AutoSccsOnAllToplevs	= lookUp  SLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookUp  SLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookUp  SLIT("-fauto-sccs-on-individual-cafs")
opt_AutoSccsOnDicts		= lookUp  SLIT("-fauto-sccs-on-dicts")
opt_SccProfilingOn		= lookUp  SLIT("-fscc-profiling")
opt_DoTickyProfiling		= lookUp  SLIT("-fticky-ticky")

-- language opts
opt_AllStrict			= lookUp  SLIT("-fall-strict")
opt_DictsStrict			= lookUp  SLIT("-fdicts-strict")
opt_Generics			= lookUp  SLIT("-fgenerics")
opt_IrrefutableTuples		= lookUp  SLIT("-firrefutable-tuples")
opt_MaxContextReductionDepth	= lookup_def_int "-fcontext-stack" mAX_CONTEXT_REDUCTION_DEPTH
opt_NumbersStrict		= lookUp  SLIT("-fnumbers-strict")
opt_Parallel			= lookUp  SLIT("-fparallel")
opt_SMP				= lookUp  SLIT("-fsmp")

-- optimisation opts
opt_DoSemiTagging		= lookUp  SLIT("-fsemi-tagging")
opt_FoldrBuildOn		= lookUp  SLIT("-ffoldr-build-on")
opt_LiberateCaseThreshold	= lookup_def_int "-fliberate-case-threshold" (10::Int)
opt_StgDoLetNoEscapes		= lookUp  SLIT("-flet-no-escape")
opt_UnfoldCasms		        = lookUp SLIT("-funfold-casms-in-hi-file")
opt_UsageSPOn           	= lookUp  SLIT("-fusagesp-on")
opt_UnboxStrictFields		= lookUp  SLIT("-funbox-strict-fields")

{-
   The optional '-inpackage=P' flag tells what package 
   we are compiling this module for.
   The Prelude, for example is compiled with '-package prelude'
-}
opt_InPackage			= case lookup_str "-inpackage=" of
				    Just p  -> _PK_ p
				    Nothing -> SLIT("Main")	-- The package name if none is specified

opt_EmitCExternDecls	        = lookUp  SLIT("-femit-extern-decls")
opt_EnsureSplittableC		= lookUp  SLIT("-fglobalise-toplev-names")
opt_GranMacros			= lookUp  SLIT("-fgransim")
opt_HiVersion			= lookup_def_int "-fhi-version=" 0 -- what version we're compiling.
opt_HistorySize			= lookup_def_int "-fhistory-size" 20
opt_IgnoreAsserts               = lookUp  SLIT("-fignore-asserts")
opt_IgnoreIfacePragmas		= lookUp  SLIT("-fignore-interface-pragmas")
opt_NoHiCheck                   = lookUp  SLIT("-fno-hi-version-check")
opt_NoImplicitPrelude		= lookUp  SLIT("-fno-implicit-prelude")
opt_OmitBlackHoling		= lookUp  SLIT("-dno-black-holing")
opt_OmitInterfacePragmas	= lookUp  SLIT("-fomit-interface-pragmas")

-- Simplifier switches
opt_SimplNoPreInlining		= lookUp SLIT("-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplDoEtaReduction		= lookUp SLIT("-fdo-eta-reduction")
opt_SimplDoLambdaEtaExpansion	= lookUp SLIT("-fdo-lambda-eta-expansion")
opt_SimplCaseOfCase		= lookUp SLIT("-fcase-of-case")
opt_SimplCaseMerge		= lookUp SLIT("-fcase-merge")
opt_SimplPedanticBottoms	= lookUp SLIT("-fpedantic-bottoms")
opt_SimplExcessPrecision	= lookUp SLIT("-fexcess-precision")

-- Unfolding control
opt_UF_HiFileThreshold		= lookup_def_int "-funfolding-interface-threshold" (45::Int)
opt_UF_CreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  (45::Int)
opt_UF_UseThreshold		= lookup_def_int "-funfolding-use-threshold"	   (8::Int)	-- Discounts can be big
opt_UF_FunAppDiscount		= lookup_def_int "-funfolding-fun-discount"	   (6::Int)	-- It's great to inline a fn
opt_UF_KeenessFactor		= lookup_def_float "-funfolding-keeness-factor"	   (1.5::Float)
opt_UF_UpdateInPlace		= lookUp  SLIT("-funfolding-update-in-place")

opt_UF_CheapOp  = ( 1 :: Int)	-- Only one instruction; and the args are charged for
opt_UF_DearOp   = ( 4 :: Int)
			
opt_ReportCompile               = lookUp SLIT("-freport-compile")
opt_NoPruneDecls		= lookUp SLIT("-fno-prune-decls")
opt_NoPruneTyDecls		= lookUp SLIT("-fno-prune-tydecls")
opt_Static			= lookUp SLIT("-static")
opt_Unregisterised		= lookUp SLIT("-funregisterised")
opt_Verbose			= lookUp SLIT("-v")

opt_UseVanillaRegs | opt_Unregisterised = 0
		   | otherwise          = mAX_Real_Vanilla_REG
opt_UseFloatRegs   | opt_Unregisterised = 0
		   | otherwise          = mAX_Real_Float_REG
opt_UseDoubleRegs  | opt_Unregisterised = 0
		   | otherwise          = mAX_Real_Double_REG
opt_UseLongRegs    | opt_Unregisterised = 0
		   | otherwise          = mAX_Real_Long_REG
\end{code}

%************************************************************************
%*									*
\subsection{List of static hsc flags}
%*									*
%************************************************************************

\begin{code}
isStaticHscFlag f = 
  f `elem` [
	"-fwarn-duplicate-exports",
	"-fwarn-hi-shadowing",
	"-fwarn-incomplete-patterns",
	"-fwarn-missing-fields",
	"-fwarn-missing-methods",
	"-fwarn-missing-signatures",
	"-fwarn-name-shadowing",
	"-fwarn-overlapping-patterns",
	"-fwarn-simple-patterns",
	"-fwarn-type-defaults",
	"-fwarn-unused-binds",
	"-fwarn-unused-imports",
	"-fwarn-unused-matches",
	"-fwarn-deprecations",
	"-fauto-sccs-on-all-toplevs",
	"-fauto-sccs-on-exported-toplevs",
	"-fauto-sccs-on-individual-cafs",
	"-fauto-sccs-on-dicts",
	"-fscc-profiling",
	"-fticky-ticky",
	"-fall-strict",
	"-fdicts-strict",
	"-fgenerics",
	"-firrefutable-tuples",
	"-fnumbers-strict",
	"-fparallel",
	"-fsmp",
	"-fsemi-tagging",
	"-ffoldr-build-on",
	"-flet-no-escape",
	"-funfold-casms-in-hi-file",
	"-fusagesp-on",
	"-funbox-strict-fields",
	"-femit-extern-decls",
	"-fglobalise-toplev-names",
	"-fgransim",
	"-fignore-asserts",
	"-fignore-interface-pragmas",
	"-fno-hi-version-check",
	"-fno-implicit-prelude",
	"-dno-black-holing",
	"-fomit-interface-pragmas",
	"-fno-pre-inlining",
	"-fdo-eta-reduction",
	"-fdo-lambda-eta-expansion",
	"-fcase-of-case",
	"-fcase-merge",
	"-fpedantic-bottoms",
	"-fexcess-precision",
	"-funfolding-update-in-place",
	"-freport-compile",
	"-fno-prune-decls",
	"-fno-prune-tydecls",
	"-static",
	"-funregisterised",
	"-v" ]
  || any (flip prefixMatch f) [
	"-fcontext-stack",
	"-fliberate-case-threshold",
	"-fhi-version=",
	"-fhistory-size",
	"-funfolding-interface-threshold",
	"-funfolding-creation-threshold",
	"-funfolding-use-threshold",
	"-funfolding-fun-discount",
	"-funfolding-keeness-factor"
     ]
\end{code}

%************************************************************************
%*									*
\subsection{Switch ordering}
%*									*
%************************************************************************

These things behave just like enumeration types.

\begin{code}
instance Eq SimplifierSwitch where
    a == b = tagOf_SimplSwitch a ==# tagOf_SimplSwitch b

instance Ord SimplifierSwitch where
    a <  b  = tagOf_SimplSwitch a <# tagOf_SimplSwitch b
    a <= b  = tagOf_SimplSwitch a <=# tagOf_SimplSwitch b


tagOf_SimplSwitch (SimplInlinePhase _)		= _ILIT(1)
tagOf_SimplSwitch (MaxSimplifierIterations _)	= _ILIT(2)
tagOf_SimplSwitch DontApplyRules		= _ILIT(3)
tagOf_SimplSwitch SimplLetToCase		= _ILIT(4)
tagOf_SimplSwitch NoCaseOfCase			= _ILIT(5)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

lAST_SIMPL_SWITCH_TAG = 5
\end{code}

%************************************************************************
%*									*
\subsection{Switch lookup}
%*									*
%************************************************************************

\begin{code}
isAmongSimpl :: [SimplifierSwitch] -> SimplifierSwitch -> SwitchResult
isAmongSimpl on_switches		-- Switches mentioned later occur *earlier*
					-- in the list; defaults right at the end.
  = let
	tidied_on_switches = foldl rm_dups [] on_switches
		-- The fold*l* ensures that we keep the latest switches;
		-- ie the ones that occur earliest in the list.

	sw_tbl :: Array Int SwitchResult
	sw_tbl = (array	(0, lAST_SIMPL_SWITCH_TAG) -- bounds...
			all_undefined)
		 // defined_elems

	all_undefined = [ (i, SwBool False) | i <- [0 .. lAST_SIMPL_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
    -- (avoid some unboxing, bounds checking, and other horrible things:)
#if __GLASGOW_HASKELL__ < 405
    case sw_tbl of { Array bounds_who_needs_'em stuff ->
#else
    case sw_tbl of { Array _ _ stuff ->
#endif
    \ switch ->
	case (indexArray# stuff (tagOf_SimplSwitch switch)) of
#if __GLASGOW_HASKELL__ < 400
	  Lift v -> v
#elif __GLASGOW_HASKELL__ < 403
	  (# _, v #) -> v
#else
	  (# v #) -> v
#endif
    }
  where
    mk_assoc_elem k@(MaxSimplifierIterations lvl) = (_IBOX(tagOf_SimplSwitch k), SwInt lvl)
    mk_assoc_elem k@(SimplInlinePhase n)          = (_IBOX(tagOf_SimplSwitch k), SwInt n)
    mk_assoc_elem k 				  = (_IBOX(tagOf_SimplSwitch k), SwBool True) -- I'm here, Mom!

    -- cannot have duplicates if we are going to use the array thing
    rm_dups switches_so_far switch
      = if switch `is_elem` switches_so_far
    	then switches_so_far
	else switch : switches_so_far
      where
	sw `is_elem` []     = False
	sw `is_elem` (s:ss) = (tagOf_SimplSwitch sw) ==# (tagOf_SimplSwitch s)
			    || sw `is_elem` ss
\end{code}

Default settings for simplifier switches

\begin{code}
defaultSimplSwitches = [MaxSimplifierIterations	1]
\end{code}

%************************************************************************
%*									*
\subsection{Misc functions for command-line options}
%*									*
%************************************************************************


\begin{code}
switchIsOn :: (switch -> SwitchResult) -> switch -> Bool

switchIsOn lookup_fn switch
  = case (lookup_fn switch) of
      SwBool False -> False
      _	    	   -> True

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}

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
