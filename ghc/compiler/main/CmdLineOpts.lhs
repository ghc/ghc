%
% (c) The AQUA Project, Glasgow University, 1996-98
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}
module CmdLineOpts (
	CoreToDo(..),
	SimplifierSwitch(..),
	StgToDo(..),
	SwitchResult(..),
	classifyOpts,

	intSwitchSet,
	switchIsOn,

	src_filename,

	-- debugging opts
	opt_D_dump_absC,
	opt_D_dump_asm,
	opt_D_dump_cpranal,
	opt_D_dump_cse,
	opt_D_dump_deriv,
	opt_D_dump_ds,
	opt_D_dump_flatC,
	opt_D_dump_foreign,
	opt_D_dump_inlinings,
	opt_D_dump_occur_anal,
	opt_D_dump_parsed,
	opt_D_dump_realC,
	opt_D_dump_rn,
	opt_D_dump_rules,
	opt_D_dump_simpl,
	opt_D_dump_simpl_iterations,
	opt_D_dump_simpl_stats,
	opt_D_dump_spec,
	opt_D_dump_stg,
	opt_D_dump_stranal,
	opt_D_dump_tc,
        opt_D_dump_usagesp,
	opt_D_dump_worker_wrapper,
	opt_D_show_passes,
	opt_D_dump_rn_trace,
	opt_D_dump_rn_stats,
	opt_D_source_stats,
	opt_D_verbose_core2core,
	opt_D_verbose_stg2stg,
	opt_DoCoreLinting,
	opt_DoStgLinting,
        opt_DoUSPLinting,
	opt_PprStyle_Debug,
	opt_PprStyle_NoPrags,
	opt_PprUserLength,

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

	-- profiling opts
	opt_AutoSccsOnAllToplevs,
	opt_AutoSccsOnExportedToplevs,
	opt_AutoSccsOnIndividualCafs,
	opt_AutoSccsOnDicts,
	opt_SccGroup,
	opt_SccProfilingOn,
	opt_DoTickyProfiling,

	-- language opts
	opt_AllStrict,
	opt_DictsStrict,
        opt_MaxContextReductionDepth,
        opt_AllowOverlappingInstances,
 	opt_AllowUndecidableInstances,
	opt_GlasgowExts,
	opt_IrrefutableTuples,
	opt_NumbersStrict,
	opt_Parallel,

	-- optimisation opts
	opt_DoEtaReduction,
	opt_DoSemiTagging,
	opt_FoldrBuildOn,
	opt_LiberateCaseThreshold,
	opt_NoPreInlining,
	opt_StgDoLetNoEscapes,
	opt_UnfoldCasms,
        opt_UsageSPOn,
	opt_UnboxStrictFields,
	opt_SimplNoPreInlining,
	opt_SimplDoEtaReduction,
	opt_SimplDoLambdaEtaExpansion,
	opt_SimplCaseOfCase,
	opt_SimplCaseMerge,
	opt_SimplLetToCase,
	opt_SimplPedanticBottoms,

	-- Unfolding control
	opt_UF_HiFileThreshold,
	opt_UF_CreationThreshold,
	opt_UF_UseThreshold,
	opt_UF_ScrutConDiscount,
	opt_UF_FunAppDiscount,
	opt_UF_PrimArgDiscount,
	opt_UF_KeenessFactor,
	opt_UF_CheapOp,
	opt_UF_DearOp,
	opt_UF_NoRepLit,

	-- misc opts
	opt_CompilingPrelude,
	opt_EmitCExternDecls,
	opt_EnsureSplittableC,
	opt_GranMacros,
	opt_HiMap,
	opt_HiMapSep,
	opt_HiVersion,
	opt_HistorySize,
	opt_IgnoreAsserts,
	opt_IgnoreIfacePragmas,
        opt_NoHiCheck,
	opt_NoImplicitPrelude,
	opt_OmitBlackHoling,
	opt_OmitInterfacePragmas,
	opt_ProduceC,
	opt_ProduceExportCStubs,
	opt_ProduceExportHStubs,
	opt_ProduceHi,
	opt_ProduceS,
	opt_NoPruneDecls,
	opt_ReportCompile,
	opt_SourceUnchanged,
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

import FastString	( headFS )
import Maybes		( assocMaybe, firstJust, maybeToBool )
import Panic		( panic, panic# )

#if __GLASGOW_HASKELL__ < 301
import ArrBase	( Array(..) )
#else
import PrelArr  ( Array(..) )
#endif
\end{code}

A command-line {\em switch} is (generally) either on or off; e.g., the
``verbose'' (-v) switch is either on or off.  (The \tr{-G<group>}
switch is an exception; it's set to a string, or nothing.)

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
  | CoreDoFullLaziness
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoWorkerWrapper
  | CoreDoSpecialising
  | CoreDoUSPInf
  | CoreDoCPResult 
  | CoreCSE
\end{code}

\begin{code}
data StgToDo
  = StgDoStaticArgs
  | StgDoUpdateAnalysis
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

lookUp     sw = maybeToBool (assoc_opts sw)
	
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

assoc_opts    = assocMaybe [ (a, True) | a <- argv ]
unpacked_opts = map _UNPK_ argv

{-
 Putting the compiler options into temporary at-files
 may turn out to be necessary later on if we turn hsc into
 a pure Win32 application where I think there's a command-line
 length limit of 255. unpacked_opts understands the @ option.

assoc_opts    = assocMaybe [ (_PK_ a, True) | a <- unpacked_opts ]

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

\begin{code}
src_filename :: FAST_STRING
src_filename = case argv of
		  filename : rest | headFS filename /= '-' -> filename
		  otherwise -> panic "no filename"
\end{code}

\begin{code}
-- debugging opts
opt_D_dump_all   {- do not -}   = lookUp  SLIT("-ddump-all")
opt_D_dump_most  {- export -}   = opt_D_dump_all  || lookUp  SLIT("-ddump-most")

opt_D_dump_absC			= opt_D_dump_all  || lookUp  SLIT("-ddump-absC")
opt_D_dump_asm			= opt_D_dump_all  || lookUp  SLIT("-ddump-asm")
opt_D_dump_cpranal	        = opt_D_dump_most || lookUp  SLIT("-ddump-cpranal")
opt_D_dump_deriv		= opt_D_dump_most || lookUp  SLIT("-ddump-deriv")
opt_D_dump_ds			= opt_D_dump_most || lookUp  SLIT("-ddump-ds")
opt_D_dump_flatC		= opt_D_dump_all  || lookUp  SLIT("-ddump-flatC")
opt_D_dump_foreign		= opt_D_dump_most || lookUp  SLIT("-ddump-foreign-stubs")
opt_D_dump_inlinings		= opt_D_dump_all  || lookUp  SLIT("-ddump-inlinings")
opt_D_dump_occur_anal		= opt_D_dump_most || lookUp  SLIT("-ddump-occur-anal")
opt_D_dump_parsed		= opt_D_dump_most || lookUp  SLIT("-ddump-parsed")
opt_D_dump_realC		= opt_D_dump_all  || lookUp  SLIT("-ddump-realC")
opt_D_dump_rn			= opt_D_dump_most || lookUp  SLIT("-ddump-rn")
opt_D_dump_simpl		= opt_D_dump_most || lookUp  SLIT("-ddump-simpl")
opt_D_dump_simpl_iterations	= opt_D_dump_all  || lookUp  SLIT("-ddump-simpl-iterations")
opt_D_dump_spec			= opt_D_dump_most || lookUp  SLIT("-ddump-spec")
opt_D_dump_stg			= opt_D_dump_most || lookUp  SLIT("-ddump-stg")
opt_D_dump_stranal		= opt_D_dump_most || lookUp  SLIT("-ddump-stranal")
opt_D_dump_tc			= opt_D_dump_most || lookUp  SLIT("-ddump-tc")
opt_D_dump_rules		= opt_D_dump_most || lookUp  SLIT("-ddump-rules")
opt_D_dump_usagesp              = opt_D_dump_most || lookUp  SLIT("-ddump-usagesp")
opt_D_dump_cse 	                = opt_D_dump_most || lookUp  SLIT("-ddump-cse")
opt_D_dump_worker_wrapper	= opt_D_dump_most || lookUp  SLIT("-ddump-workwrap")
opt_D_show_passes		= opt_D_dump_most || lookUp  SLIT("-dshow-passes")
opt_D_dump_rn_trace		= opt_D_dump_all  || lookUp  SLIT("-ddump-rn-trace")
opt_D_dump_rn_stats		= opt_D_dump_most || lookUp  SLIT("-ddump-rn-stats")
opt_D_dump_simpl_stats		= opt_D_dump_most || lookUp  SLIT("-ddump-simpl-stats")
opt_D_source_stats		= opt_D_dump_most || lookUp  SLIT("-dsource-stats")
opt_D_verbose_core2core		= opt_D_dump_all  || lookUp  SLIT("-dverbose-simpl")
opt_D_verbose_stg2stg		= opt_D_dump_all  || lookUp  SLIT("-dverbose-stg")

opt_DoCoreLinting		= lookUp  SLIT("-dcore-lint")
opt_DoStgLinting		= lookUp  SLIT("-dstg-lint")
opt_DoUSPLinting		= lookUp  SLIT("-dusagesp-lint")
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

-- profiling opts
opt_AutoSccsOnAllToplevs	= lookUp  SLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookUp  SLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookUp  SLIT("-fauto-sccs-on-individual-cafs")
opt_AutoSccsOnDicts		= lookUp  SLIT("-fauto-sccs-on-dicts")
opt_SccGroup  			= lookup_str "-G="
opt_SccProfilingOn		= lookUp  SLIT("-fscc-profiling")
opt_DoTickyProfiling		= lookUp  SLIT("-fticky-ticky")

-- language opts
opt_AllStrict			= lookUp  SLIT("-fall-strict")
opt_DictsStrict			= lookUp  SLIT("-fdicts-strict")
opt_AllowOverlappingInstances   = lookUp  SLIT("-fallow-overlapping-instances")
opt_AllowUndecidableInstances 	= lookUp  SLIT("-fallow-undecidable-instances")
opt_GlasgowExts			= lookUp  SLIT("-fglasgow-exts")
opt_IrrefutableTuples		= lookUp  SLIT("-firrefutable-tuples")
opt_MaxContextReductionDepth	= lookup_def_int "-fcontext-stack" mAX_CONTEXT_REDUCTION_DEPTH
opt_NumbersStrict		= lookUp  SLIT("-fnumbers-strict")
opt_Parallel			= lookUp  SLIT("-fparallel")

-- optimisation opts
opt_DoEtaReduction		= lookUp  SLIT("-fdo-eta-reduction")
opt_DoSemiTagging		= lookUp  SLIT("-fsemi-tagging")
opt_FoldrBuildOn		= lookUp  SLIT("-ffoldr-build-on")
opt_LiberateCaseThreshold	= lookup_def_int "-fliberate-case-threshold" (10::Int)
opt_NoPreInlining		= lookUp  SLIT("-fno-pre-inlining")
opt_StgDoLetNoEscapes		= lookUp  SLIT("-flet-no-escape")
opt_UnfoldCasms		        = lookUp SLIT("-funfold-casms-in-hi-file")
opt_UsageSPOn           	= lookUp  SLIT("-fusagesp-on")
opt_UnboxStrictFields		= lookUp  SLIT("-funbox-strict-fields")

  {-
   It's a bit unfortunate to have to re-introduce this chap, but on Win32
   platforms we do need a way of distinguishing between the case when we're
   compiling a static version of the Prelude and one that's going to be
   put into a DLL. Why? Because the compiler's wired in modules need to
   be attributed as either coming from a DLL or not.
  -}
opt_CompilingPrelude		= lookUp  SLIT("-fcompiling-prelude")
opt_EmitCExternDecls	        = lookUp  SLIT("-femit-extern-decls")
opt_EnsureSplittableC		= lookUp  SLIT("-fglobalise-toplev-names")
opt_GranMacros			= lookUp  SLIT("-fgransim")
opt_HiMap 			= lookup_str "-himap="       -- file saying where to look for .hi files
opt_HiMapSep                    = lookup_def_char "-himap-sep=" ':'
opt_HiVersion			= lookup_def_int "-fhi-version=" 0 -- what version we're compiling.
opt_HistorySize			= lookup_def_int "-fhistory-size" 20
opt_IgnoreAsserts               = lookUp  SLIT("-fignore-asserts")
opt_IgnoreIfacePragmas		= lookUp  SLIT("-fignore-interface-pragmas")
opt_NoHiCheck                   = lookUp  SLIT("-fno-hi-version-check")
opt_NoImplicitPrelude		= lookUp  SLIT("-fno-implicit-prelude")
opt_OmitBlackHoling		= lookUp  SLIT("-dno-black-holing")
opt_OmitInterfacePragmas	= lookUp  SLIT("-fomit-interface-pragmas")
opt_ProduceC  			= lookup_str "-C="
opt_ProduceExportCStubs		= lookup_str "-F="
opt_ProduceExportHStubs		= lookup_str "-FH="
opt_ProduceHi 			= lookup_str "-hifile=" -- the one to produce this time 

-- Simplifier switches
opt_SimplNoPreInlining		= lookUp SLIT("-fno-pre-inlining")
	-- NoPreInlining is there just to see how bad things
	-- get if you don't do it!
opt_SimplDoEtaReduction		= lookUp SLIT("-fdo-eta-reduction")
opt_SimplDoLambdaEtaExpansion	= lookUp SLIT("-fdo-lambda-eta-expansion")
opt_SimplCaseOfCase		= lookUp SLIT("-fcase-of-case")
opt_SimplCaseMerge		= lookUp SLIT("-fcase-merge")
opt_SimplLetToCase		= lookUp SLIT("-flet-to-case")
opt_SimplPedanticBottoms	= lookUp SLIT("-fpedantic-bottoms")

-- Unfolding control
opt_UF_HiFileThreshold		= lookup_def_int "-funfolding-interface-threshold" (30::Int)
opt_UF_CreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  (30::Int)
opt_UF_UseThreshold		= lookup_def_int "-funfolding-use-threshold"	   (8::Int)	-- Discounts can be big
opt_UF_ScrutConDiscount		= lookup_def_int "-funfolding-con-discount"	   (2::Int)
opt_UF_FunAppDiscount		= lookup_def_int "-funfolding-fun-discount"	   (6::Int)	-- It's great to inline a fn
opt_UF_PrimArgDiscount		= lookup_def_int "-funfolding-prim-discount"	   (1::Int)
opt_UF_KeenessFactor		= lookup_def_float "-funfolding-keeness-factor"	   (2.0::Float)

opt_UF_CheapOp  = ( 0 :: Int)	-- Only one instruction; and the args are charged for
opt_UF_DearOp   = ( 4 :: Int)
opt_UF_NoRepLit = ( 20 :: Int)	-- Strings can be pretty big
			
opt_ProduceS  			= lookup_str "-S="
opt_ReportCompile               = lookUp SLIT("-freport-compile")
opt_NoPruneDecls		= lookUp SLIT("-fno-prune-decls")
opt_SourceUnchanged		= lookUp SLIT("-fsource-unchanged")
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

\begin{code}
classifyOpts :: ([CoreToDo],	-- Core-to-Core processing spec
		 [StgToDo])	-- STG-to-STG   processing spec

classifyOpts = sep argv [] [] -- accumulators...
  where
    sep :: [FAST_STRING]	         -- cmd-line opts (input)
	-> [CoreToDo] -> [StgToDo]	 -- to_do accumulators
	-> ([CoreToDo], [StgToDo])	 -- result

    sep [] core_td stg_td -- all done!
      = (reverse core_td, reverse stg_td)

#	define CORE_TD(to_do) sep opts (to_do:core_td) stg_td
#	define STG_TD(to_do)  sep opts core_td (to_do:stg_td)

    sep (opt1:opts) core_td stg_td
      = case (_UNPK_ opt1) of -- the non-"just match a string" options are at the end...
	  ',' : _	-> sep opts core_td stg_td -- it is for the parser

	  "-fsimplify"  -> -- gather up SimplifierSwitches specially...
			   simpl_sep opts defaultSimplSwitches core_td stg_td

	  "-ffloat-inwards"  -> CORE_TD(CoreDoFloatInwards)
	  "-ffull-laziness"  -> CORE_TD(CoreDoFullLaziness)
	  "-fliberate-case"  -> CORE_TD(CoreLiberateCase)
	  "-fcse"  	     -> CORE_TD(CoreCSE)
	  "-fprint-core"     -> CORE_TD(CoreDoPrintCore)
	  "-fstatic-args"    -> CORE_TD(CoreDoStaticArgs)
	  "-fstrictness"     -> CORE_TD(CoreDoStrictness)
	  "-fworker-wrapper" -> CORE_TD(CoreDoWorkerWrapper)
	  "-fspecialise"     -> CORE_TD(CoreDoSpecialising)
	  "-fusagesp"        -> CORE_TD(CoreDoUSPInf)
	  "-fcpr-analyse"    -> CORE_TD(CoreDoCPResult)

	  "-fstg-static-args" -> STG_TD(StgDoStaticArgs)
	  "-fupdate-analysis" -> STG_TD(StgDoUpdateAnalysis)
	  "-dstg-stats"	      -> STG_TD(D_stg_stats)
	  "-flambda-lift"     -> STG_TD(StgDoLambdaLift)
	  "-fmassage-stg-for-profiling" -> STG_TD(StgDoMassageForProfiling)

	  _ -> -- NB: the driver is really supposed to handle bad options
	       sep opts core_td stg_td

    ----------------

    simpl_sep :: [FAST_STRING]            -- cmd-line opts (input)
	      -> [SimplifierSwitch]	  -- simplifier-switch accumulator
	      -> [CoreToDo] -> [StgToDo]  -- to_do accumulators
	      -> ([CoreToDo], [StgToDo])  -- result

	-- "simpl_sep" tailcalls "sep" once it's seen one set
	-- of SimplifierSwitches for a CoreDoSimplify.

#ifdef DEBUG
    simpl_sep input@[] simpl_sw core_td stg_td
      = panic "simpl_sep []"
#endif

	-- The SimplifierSwitches should be delimited by "[" and "]".

    simpl_sep (opt1:opts) simpl_sw core_td stg_td
      = case (_UNPK_ opt1) of
	  "[" -> simpl_sep opts simpl_sw core_td stg_td
	  "]" -> let
		    this_simpl = CoreDoSimplify (isAmongSimpl simpl_sw)
		 in
		 sep opts (this_simpl : core_td) stg_td

	  opt -> case matchSimplSw opt of
			Just sw -> simpl_sep opts (sw:simpl_sw) core_td stg_td
			Nothing -> simpl_sep opts simpl_sw      core_td stg_td

matchSimplSw opt
  = firstJust	[ matchSwInt  opt "-fmax-simplifier-iterations"		MaxSimplifierIterations
		, matchSwInt  opt "-finline-phase"			SimplInlinePhase
		]

matchSwBool :: String -> String -> a -> Maybe a
matchSwBool opt str sw | opt == str = Just sw
		       | otherwise  = Nothing

matchSwInt :: String -> String -> (Int -> a) -> Maybe a
matchSwInt opt str sw = case startsWith str opt of
			    Just opt_left -> Just (sw (read opt_left))
			    Nothing	  -> Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Switch ordering}
%*									*
%************************************************************************

In spite of the @Produce*@ and @SccGroup@ constructors, these things
behave just like enumeration types.

\begin{code}
instance Eq SimplifierSwitch where
    a == b = tagOf_SimplSwitch a _EQ_ tagOf_SimplSwitch b

instance Ord SimplifierSwitch where
    a <  b  = tagOf_SimplSwitch a _LT_ tagOf_SimplSwitch b
    a <= b  = tagOf_SimplSwitch a _LE_ tagOf_SimplSwitch b


tagOf_SimplSwitch (SimplInlinePhase _)		= ILIT(1)
tagOf_SimplSwitch (MaxSimplifierIterations _)	= ILIT(2)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

lAST_SIMPL_SWITCH_TAG = 2
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
    case sw_tbl of { Array bounds_who_needs_'em stuff ->
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
    mk_assoc_elem k@(MaxSimplifierIterations lvl) = (IBOX(tagOf_SimplSwitch k), SwInt lvl)
    mk_assoc_elem k@(SimplInlinePhase n)          = (IBOX(tagOf_SimplSwitch k), SwInt n)
    mk_assoc_elem k 				  = (IBOX(tagOf_SimplSwitch k), SwBool True) -- I'm here, Mom!

    -- cannot have duplicates if we are going to use the array thing
    rm_dups switches_so_far switch
      = if switch `is_elem` switches_so_far
    	then switches_so_far
	else switch : switches_so_far
      where
	sw `is_elem` []     = False
	sw `is_elem` (s:ss) = (tagOf_SimplSwitch sw) _EQ_ (tagOf_SimplSwitch s)
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
