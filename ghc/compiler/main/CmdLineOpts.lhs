%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}
#include "HsVersions.h"

module CmdLineOpts (
	CoreToDo(..),
	SimplifierSwitch(..),
	StgToDo(..),
	SwitchResult(..),
	classifyOpts,

	intSwitchSet,
	switchIsOn,

	maybe_CompilingGhcInternals,
	opt_AllDemanded,
	opt_AllStrict,
	opt_AutoSccsOnAllToplevs,
	opt_AutoSccsOnExportedToplevs,
	opt_AutoSccsOnIndividualCafs,
	opt_CompilingGhcInternals,
	opt_D_dump_absC,
	opt_D_dump_asm,
	opt_D_dump_deriv,
	opt_D_dump_ds,
	opt_D_dump_flatC,
	opt_D_dump_occur_anal,
	opt_D_dump_rdr,
	opt_D_dump_realC,
	opt_D_dump_rn,
	opt_D_dump_simpl,
	opt_D_dump_simpl_iterations,
	opt_D_dump_spec,
	opt_D_dump_stg,
	opt_D_dump_stranal,
	opt_D_dump_tc,
	opt_D_show_passes,
	opt_D_show_rn_trace,
	opt_D_simplifier_stats,
	opt_D_source_stats,
	opt_D_verbose_core2core,
	opt_D_verbose_stg2stg,
	opt_DoCoreLinting,
	opt_DoStgLinting,
	opt_DoSemiTagging,
	opt_DoEtaReduction,
	opt_DoTickyProfiling,
	opt_EnsureSplittableC,
	opt_FoldrBuildOn,
	opt_FoldrBuildTrace,
	opt_ForConcurrent,
	opt_GlasgowExts,
	opt_GranMacros,
	opt_HiMap,
	opt_IgnoreIfacePragmas,
	opt_IgnoreStrictnessPragmas,
	opt_IrrefutableEverything,
	opt_IrrefutableTuples,
	opt_LiberateCaseThreshold,
	opt_NoImplicitPrelude,
	opt_NumbersStrict,
	opt_OmitBlackHoling,
	opt_OmitDefaultInstanceMethods,
	opt_OmitInterfacePragmas,
	opt_PprStyle_All,
	opt_PprStyle_Debug,
	opt_PprStyle_User,		-- ToDo: rm
	opt_PprUserLength,
	opt_ProduceC,
	opt_ProduceHi,
	opt_ProduceS,
	opt_ReportWhyUnfoldingsDisallowed,
	opt_ReturnInRegsThreshold,
	opt_SccGroup,
	opt_SccProfilingOn,
	opt_ShowImportSpecs,
	opt_ShowPragmaNameErrs,
	opt_SigsRequired,
	opt_SourceUnchanged,
	opt_SpecialiseAll,
	opt_SpecialiseImports,
	opt_SpecialiseOverloaded,
	opt_SpecialiseTrace,
	opt_SpecialiseUnboxed,
	opt_StgDoLetNoEscapes,

	opt_InterfaceUnfoldThreshold,
	opt_UnfoldingCreationThreshold,
	opt_UnfoldingConDiscount,
	opt_UnfoldingUseThreshold,

	opt_Verbose,
	opt_WarnNameShadowing,
	opt_WarnUnusedNames,
	opt_WarnIncompletePatterns, opt_WarnOverlappedPatterns,
	opt_PruneTyDecls, opt_PruneInstDecls,
	opt_D_show_unused_imports,
	opt_D_show_rn_stats,
	
	all_toplev_ids_visible
    ) where

IMPORT_1_3(Array(array, (//)))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
import PreludeGlaST	-- bad bad bad boy, Will (_Array internals)
#else
import GlaExts
import ArrBase
-- 2.04 and later exports Lift from GlaExts
#if __GLASGOW_HASKELL__ < 204
import PrelBase (Lift(..))
#endif
#endif

CHK_Ubiq() -- debugging consistency check

import Argv
import Constants	-- Default values for some flags
import Maybes		( assocMaybe, firstJust, maybeToBool )
import Util		( startsWith, panic, panic#, assertPanic )
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
  | CoreDoCalcInlinings1
  | CoreDoCalcInlinings2
  | CoreDoFloatInwards
  | CoreDoFullLaziness
  | CoreLiberateCase
  | CoreDoPrintCore
  | CoreDoStaticArgs
  | CoreDoStrictness
  | CoreDoSpecialising
  | CoreDoFoldrBuildWorkerWrapper
  | CoreDoFoldrBuildWWAnal
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
  = SimplOkToDupCode
  | SimplFloatLetsExposingWHNF
  | SimplOkToFloatPrimOps
  | SimplAlwaysFloatLetsFromLets
  | SimplDoCaseElim
  | SimplReuseCon
  | SimplCaseOfCase
  | SimplLetToCase
  | SimplMayDeleteConjurableIds
  | SimplPedanticBottoms -- see Simplifier for an explanation
  | SimplDoArityExpand	 -- expand arity of bindings
  | SimplDoFoldrBuild	 -- This is the per-simplification flag;
			 -- see also FoldrBuildOn, used elsewhere
			 -- in the compiler.
  | SimplDoInlineFoldrBuild
			 -- inline foldr/build (*after* f/b rule is used)

  | IgnoreINLINEPragma
  | SimplDoLambdaEtaExpansion

  | EssentialUnfoldingsOnly -- never mind the thresholds, only
			    -- do unfoldings that *must* be done
			    -- (to saturate constructors and primitives)

  | ShowSimplifierProgress  -- report counts on every interation

  | MaxSimplifierIterations Int

  | KeepSpecPragmaIds	    -- We normally *toss* Ids we can do without
  | KeepUnusedBindings

  | SimplNoLetFromCase	    -- used when turning off floating entirely
  | SimplNoLetFromApp	    -- (for experimentation only) WDP 95/10
  | SimplNoLetFromStrictLet

  | SimplDontFoldBackAppend
		 	-- we fold `foldr (:)' back into flip (++),
			-- but we *don't* want to do it when compiling
			-- List.hs, otherwise
			-- xs ++ ys = foldr (:) ys xs
			-- {- via our loopback -}
			-- xs ++ ys = xs ++ ys
			-- Oops!
			-- So only use this flag inside List.hs
			-- (Sigh, what a HACK, Andy.  WDP 96/01)

  | SimplCaseMerge
  | SimplCaseScrutinee	-- This flag tells that the expression being simplified is
			-- the scrutinee of a case expression, so we should
			-- apply the scrutinee discount when considering inlinings.
			-- See SimplVar.lhs
\end{code}

%************************************************************************
%*									*
\subsection{Classifying command-line options}
%*									*
%************************************************************************

\begin{code}
lookUp	       :: FAST_STRING -> Bool
lookup_int     :: String -> Maybe Int
lookup_def_int :: String -> Int -> Int
lookup_str     :: String -> Maybe String

lookUp     sw = maybeToBool (assoc_opts sw)
	
lookup_str sw = firstJust (map (startsWith sw) unpacked_opts)

lookup_int sw = case (lookup_str sw) of
		  Nothing -> Nothing
		  Just xx -> Just (read xx)

lookup_def_int sw def = case (lookup_str sw) of
			    Nothing -> def		-- Use default
		  	    Just xx -> read xx

assoc_opts    = assocMaybe [ (a, True) | a <- argv ]
unpacked_opts = map _UNPK_ argv
\end{code}

\begin{code}
opt_AllDemanded			= lookUp  SLIT("-fall-demanded")
opt_AllStrict			= lookUp  SLIT("-fall-strict")
opt_AutoSccsOnAllToplevs	= lookUp  SLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookUp  SLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookUp  SLIT("-fauto-sccs-on-individual-cafs")
opt_CompilingGhcInternals	= maybeToBool maybe_CompilingGhcInternals
maybe_CompilingGhcInternals	= lookup_str "-fcompiling-ghc-internals="
opt_D_dump_absC			= lookUp  SLIT("-ddump-absC")
opt_D_dump_asm			= lookUp  SLIT("-ddump-asm")
opt_D_dump_deriv		= lookUp  SLIT("-ddump-deriv")
opt_D_dump_ds			= lookUp  SLIT("-ddump-ds")
opt_D_dump_flatC		= lookUp  SLIT("-ddump-flatC")
opt_D_dump_occur_anal		= lookUp  SLIT("-ddump-occur-anal")
opt_D_dump_rdr			= lookUp  SLIT("-ddump-rdr")
opt_D_dump_realC		= lookUp  SLIT("-ddump-realC")
opt_D_dump_rn			= lookUp  SLIT("-ddump-rn")
opt_D_dump_simpl		= lookUp  SLIT("-ddump-simpl")
opt_D_dump_simpl_iterations	= lookUp  SLIT("-ddump-simpl-iterations")
opt_D_dump_spec			= lookUp  SLIT("-ddump-spec")
opt_D_dump_stg			= lookUp  SLIT("-ddump-stg")
opt_D_dump_stranal		= lookUp  SLIT("-ddump-stranal")
opt_D_dump_tc			= lookUp  SLIT("-ddump-tc")
opt_D_show_passes		= lookUp  SLIT("-dshow-passes")
opt_D_show_rn_trace		= lookUp  SLIT("-dshow-rn-trace")
opt_D_simplifier_stats		= lookUp  SLIT("-dsimplifier-stats")
opt_D_source_stats		= lookUp  SLIT("-dsource-stats")
opt_D_verbose_core2core		= lookUp  SLIT("-dverbose-simpl")
opt_D_verbose_stg2stg		= lookUp  SLIT("-dverbose-stg")
opt_DoCoreLinting		= lookUp  SLIT("-dcore-lint")
opt_DoStgLinting		= lookUp  SLIT("-dstg-lint")
opt_DoSemiTagging		= lookUp  SLIT("-fsemi-tagging")
opt_DoTickyProfiling		= lookUp  SLIT("-fticky-ticky")
opt_DoEtaReduction		= lookUp  SLIT("-fdo-eta-reduction")
opt_EnsureSplittableC		= lookUp  SLIT("-fglobalise-toplev-names")
opt_FoldrBuildOn		= lookUp  SLIT("-ffoldr-build-on")
opt_FoldrBuildTrace		= lookUp  SLIT("-ffoldr-build-trace")
opt_ForConcurrent		= lookUp  SLIT("-fconcurrent")
opt_GranMacros			= lookUp  SLIT("-fgransim")
opt_GlasgowExts			= lookUp  SLIT("-fglasgow-exts")
--UNUSED:opt_Haskell_1_3	= lookUp  SLIT("-fhaskell-1.3")
opt_HiMap 			= lookup_str "-himap="  -- file saying where to look for .hi files
opt_IgnoreIfacePragmas		= lookUp  SLIT("-fignore-interface-pragmas")
opt_IgnoreStrictnessPragmas	= lookUp  SLIT("-fignore-strictness-pragmas")
opt_IrrefutableEverything	= lookUp  SLIT("-firrefutable-everything")
opt_IrrefutableTuples		= lookUp  SLIT("-firrefutable-tuples")
opt_NoImplicitPrelude		= lookUp  SLIT("-fno-implicit-prelude")
opt_NumbersStrict		= lookUp  SLIT("-fnumbers-strict")
opt_OmitBlackHoling		= lookUp  SLIT("-dno-black-holing")
opt_OmitDefaultInstanceMethods	= lookUp  SLIT("-fomit-default-instance-methods")
opt_OmitInterfacePragmas	= lookUp  SLIT("-fomit-interface-pragmas")
opt_PprStyle_All		= lookUp  SLIT("-dppr-all")
opt_PprStyle_Debug		= lookUp  SLIT("-dppr-debug")
opt_PprStyle_User		= lookUp  SLIT("-dppr-user")
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name
opt_ProduceC  			= lookup_str "-C="
opt_ProduceS  			= lookup_str "-S="
opt_ProduceHi 			= lookup_str "-hifile=" -- the one to produce this time 
opt_ReportWhyUnfoldingsDisallowed= lookUp SLIT("-freport-disallowed-unfoldings")
opt_SccProfilingOn		= lookUp  SLIT("-fscc-profiling")
opt_ShowImportSpecs		= lookUp  SLIT("-fshow-import-specs")
opt_ShowPragmaNameErrs		= lookUp  SLIT("-fshow-pragma-name-errs")
opt_SigsRequired		= lookUp  SLIT("-fsignatures-required")
opt_SourceUnchanged		= lookUp  SLIT("-fsource-unchanged")
opt_SpecialiseAll		= lookUp  SLIT("-fspecialise-all")
opt_SpecialiseImports		= lookUp  SLIT("-fspecialise-imports")
opt_SpecialiseOverloaded	= lookUp  SLIT("-fspecialise-overloaded")
opt_SpecialiseTrace		= lookUp  SLIT("-ftrace-specialisation")
opt_SpecialiseUnboxed		= lookUp  SLIT("-fspecialise-unboxed")
opt_StgDoLetNoEscapes		= lookUp  SLIT("-flet-no-escape")
opt_ReturnInRegsThreshold	= lookup_int "-freturn-in-regs-threshold"
opt_SccGroup  			= lookup_str "-G="
opt_Verbose			= lookUp  SLIT("-v")

opt_InterfaceUnfoldThreshold	= lookup_def_int "-funfolding-interface-threshold" iNTERFACE_UNFOLD_THRESHOLD
opt_UnfoldingCreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  uNFOLDING_CREATION_THRESHOLD
opt_UnfoldingUseThreshold	= lookup_def_int "-funfolding-use-threshold"	   uNFOLDING_USE_THRESHOLD
opt_UnfoldingConDiscount	= lookup_def_int "-funfolding-con-discount"	   uNFOLDING_CON_DISCOUNT_WEIGHT
			
opt_LiberateCaseThreshold	= lookup_def_int "-fliberate-case-threshold"	   lIBERATE_CASE_THRESHOLD
opt_WarnNameShadowing		= lookUp  SLIT("-fwarn-name-shadowing")
opt_WarnIncompletePatterns	= not (lookUp  SLIT("-fno-warn-incomplete-patterns"))
opt_WarnOverlappedPatterns	= not (lookUp  SLIT("-fno-warn-overlapped-patterns"))
opt_WarnUnusedNames		= lookUp  SLIT("-fwarn-unused-names")
opt_PruneTyDecls		= not (lookUp SLIT("-fno-prune-tydecls"))
opt_PruneInstDecls		= not (lookUp SLIT("-fno-prune-instdecls"))
opt_D_show_unused_imports	= lookUp SLIT("-dshow-unused-imports")
opt_D_show_rn_stats		= lookUp SLIT("-dshow-rn-stats")

-- opt_UnfoldingOverrideThreshold	= lookup_int "-funfolding-override-threshold"
\end{code}


\begin{code}
all_toplev_ids_visible :: Bool
all_toplev_ids_visible = 
  not opt_OmitInterfacePragmas ||  -- Pragmas can make them visible
  opt_EnsureSplittableC        ||  -- Splitting requires visiblilty
  opt_AutoSccsOnAllToplevs	   -- ditto for profiling 
				   -- (ToDo: fix up the auto-annotation
				   -- pass in the desugarer to avoid having
				   -- to do this)

\end{code}



\begin{code}
classifyOpts :: ([CoreToDo],	-- Core-to-Core processing spec
		 [StgToDo])	-- STG-to-STG   processing spec

classifyOpts = sep argv [] [] -- accumulators...
  where
    sep :: [FAST_STRING]			 -- cmd-line opts (input)
	-> [CoreToDo] -> [StgToDo]	 -- to_do accumulators
	-> ([CoreToDo], [StgToDo])	 -- result

    sep [] core_td stg_td -- all done!
      = (reverse core_td, reverse stg_td)

#	define CORE_TD(to_do) sep opts (to_do:core_td) stg_td
#	define STG_TD(to_do)  sep opts core_td (to_do:stg_td)
#	define IGNORE_ARG()   sep opts core_td stg_td

    sep (opt1:opts) core_td stg_td
      =
	case (_UNPK_ opt1) of -- the non-"just match a string" options are at the end...

	  ',' : _	-> IGNORE_ARG() -- it is for the parser

	  "-fsimplify"  -> -- gather up SimplifierSwitches specially...
			   simpl_sep opts defaultSimplSwitches core_td stg_td

	  "-fcalc-inlinings1"-> CORE_TD(CoreDoCalcInlinings1)
	  "-fcalc-inlinings2"-> CORE_TD(CoreDoCalcInlinings2)
	  "-ffloat-inwards"  -> CORE_TD(CoreDoFloatInwards)
	  "-ffull-laziness"  -> CORE_TD(CoreDoFullLaziness)
	  "-fliberate-case"  -> CORE_TD(CoreLiberateCase)
	  "-fprint-core"     -> CORE_TD(CoreDoPrintCore)
	  "-fstatic-args"    -> CORE_TD(CoreDoStaticArgs)
	  "-fstrictness"     -> CORE_TD(CoreDoStrictness)
	  "-fspecialise"     -> CORE_TD(CoreDoSpecialising)
	  "-ffoldr-build-worker-wrapper"  -> CORE_TD(CoreDoFoldrBuildWorkerWrapper)
	  "-ffoldr-build-ww-anal"  -> CORE_TD(CoreDoFoldrBuildWWAnal)

	  "-fstg-static-args" -> STG_TD(StgDoStaticArgs)
	  "-fupdate-analysis" -> STG_TD(StgDoUpdateAnalysis)
	  "-dstg-stats"	      -> STG_TD(D_stg_stats)
	  "-flambda-lift"     -> STG_TD(StgDoLambdaLift)
	  "-fmassage-stg-for-profiling" -> STG_TD(StgDoMassageForProfiling)

	  _ -> -- NB: the driver is really supposed to handle bad options
	       IGNORE_ARG()

    ----------------

    simpl_sep :: [FAST_STRING]	    -- cmd-line opts (input)
	-> [SimplifierSwitch]	    -- simplifier-switch accumulator
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

#	  define SIMPL_SW(sw) simpl_sep opts (sw:simpl_sw) core_td stg_td

	  -- the non-"just match a string" options are at the end...
	  "-fshow-simplifier-progress"	    -> SIMPL_SW(ShowSimplifierProgress)
	  "-fcode-duplication-ok"	    -> SIMPL_SW(SimplOkToDupCode)
	  "-ffloat-lets-exposing-whnf"	    -> SIMPL_SW(SimplFloatLetsExposingWHNF)
	  "-ffloat-primops-ok"		    -> SIMPL_SW(SimplOkToFloatPrimOps)
	  "-falways-float-lets-from-lets"   -> SIMPL_SW(SimplAlwaysFloatLetsFromLets)
	  "-fdo-case-elim"		    -> SIMPL_SW(SimplDoCaseElim)
	  "-fdo-lambda-eta-expansion"	    -> SIMPL_SW(SimplDoLambdaEtaExpansion)
	  "-fdo-foldr-build"		    -> SIMPL_SW(SimplDoFoldrBuild)
	  "-fdo-not-fold-back-append"	    -> SIMPL_SW(SimplDontFoldBackAppend)
	  "-fdo-arity-expand"		    -> SIMPL_SW(SimplDoArityExpand)
	  "-fdo-inline-foldr-build"	    -> SIMPL_SW(SimplDoInlineFoldrBuild)
	  "-freuse-con"			    -> SIMPL_SW(SimplReuseCon)
	  "-fcase-of-case"		    -> SIMPL_SW(SimplCaseOfCase)
	  "-fcase-merge"		    -> SIMPL_SW(SimplCaseMerge)
	  "-flet-to-case"		    -> SIMPL_SW(SimplLetToCase)
	  "-fpedantic-bottoms"		    -> SIMPL_SW(SimplPedanticBottoms)
	  "-fkeep-spec-pragma-ids"	    -> SIMPL_SW(KeepSpecPragmaIds)
	  "-fkeep-unused-bindings"	    -> SIMPL_SW(KeepUnusedBindings)
	  "-fmay-delete-conjurable-ids"     -> SIMPL_SW(SimplMayDeleteConjurableIds)
	  "-fessential-unfoldings-only"     -> SIMPL_SW(EssentialUnfoldingsOnly)
	  "-fignore-inline-pragma"  	    -> SIMPL_SW(IgnoreINLINEPragma)
	  "-fno-let-from-case"		    -> SIMPL_SW(SimplNoLetFromCase)
	  "-fno-let-from-app"		    -> SIMPL_SW(SimplNoLetFromApp)
	  "-fno-let-from-strict-let"	    -> SIMPL_SW(SimplNoLetFromStrictLet)

	  o | starts_with_msi  -> SIMPL_SW(MaxSimplifierIterations (read after_msi))
	   where
	    maybe_msi		= startsWith "-fmax-simplifier-iterations"   o
	    starts_with_msi	= maybeToBool maybe_msi
	    (Just after_msi)	= maybe_msi

	  _ -> -- NB: the driver is really supposed to handle bad options
	       simpl_sep opts simpl_sw core_td stg_td
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

tagOf_SimplSwitch SimplOkToDupCode		=(ILIT(0) :: FAST_INT)
tagOf_SimplSwitch SimplFloatLetsExposingWHNF	= ILIT(1)
tagOf_SimplSwitch SimplOkToFloatPrimOps		= ILIT(2)
tagOf_SimplSwitch SimplAlwaysFloatLetsFromLets	= ILIT(3)
tagOf_SimplSwitch SimplDoCaseElim		= ILIT(4)
tagOf_SimplSwitch SimplReuseCon			= ILIT(5)
tagOf_SimplSwitch SimplCaseOfCase		= ILIT(6)
tagOf_SimplSwitch SimplLetToCase		= ILIT(7)
tagOf_SimplSwitch SimplMayDeleteConjurableIds	= ILIT(9)
tagOf_SimplSwitch SimplPedanticBottoms		= ILIT(10)
tagOf_SimplSwitch SimplDoArityExpand		= ILIT(11)
tagOf_SimplSwitch SimplDoFoldrBuild		= ILIT(12)
tagOf_SimplSwitch SimplDoInlineFoldrBuild	= ILIT(14)
tagOf_SimplSwitch IgnoreINLINEPragma 		= ILIT(15)
tagOf_SimplSwitch SimplDoLambdaEtaExpansion	= ILIT(16)
tagOf_SimplSwitch EssentialUnfoldingsOnly	= ILIT(19)
tagOf_SimplSwitch ShowSimplifierProgress	= ILIT(20)
tagOf_SimplSwitch (MaxSimplifierIterations _)	= ILIT(21)
tagOf_SimplSwitch KeepSpecPragmaIds		= ILIT(25)
tagOf_SimplSwitch KeepUnusedBindings		= ILIT(26)
tagOf_SimplSwitch SimplNoLetFromCase		= ILIT(27)
tagOf_SimplSwitch SimplNoLetFromApp		= ILIT(28)
tagOf_SimplSwitch SimplNoLetFromStrictLet	= ILIT(29)
tagOf_SimplSwitch SimplDontFoldBackAppend       = ILIT(30)
tagOf_SimplSwitch SimplCaseMerge		= ILIT(31)
tagOf_SimplSwitch SimplCaseScrutinee		= ILIT(32)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

tagOf_SimplSwitch _ = panic# "tagOf_SimplSwitch"

lAST_SIMPL_SWITCH_TAG = IBOX(tagOf_SimplSwitch SimplCaseScrutinee)
\end{code}

%************************************************************************
%*									*
\subsection{Switch lookup}
%*									*
%************************************************************************

\begin{code}
#if __GLASGOW_HASKELL__ == 201
# define ARRAY	    Array
# define LIFT	    GHCbase.Lift
# define SET_TO	    =:
(=:) a b = (a,b)
#elif __GLASGOW_HASKELL__ >= 202
# define ARRAY	    Array
# define LIFT	    Lift
# define SET_TO	    =:
(=:) a b = (a,b)
#else
# define ARRAY	    _Array
# define LIFT	    _Lift
# define SET_TO	    :=
#endif

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

	all_undefined = [ i SET_TO SwBool False | i <- [0 .. lAST_SIMPL_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    case sw_tbl of { ARRAY bounds_who_needs_'em stuff ->
    \ switch ->
	case (indexArray# stuff (tagOf_SimplSwitch switch)) of
	  LIFT v -> v
    }
  where
    mk_assoc_elem k@(MaxSimplifierIterations lvl)       = IBOX(tagOf_SimplSwitch k) SET_TO SwInt lvl

    mk_assoc_elem k = IBOX(tagOf_SimplSwitch k) SET_TO SwBool   True -- I'm here, Mom!

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
defaultSimplSwitches = [MaxSimplifierIterations		1
		       ]
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

stringSwitchSet :: (switch -> SwitchResult)
		-> (FAST_STRING -> switch)
		-> Maybe FAST_STRING

stringSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "stringSwitchSet"))) of
      SwString str -> Just str
      _	    	   -> Nothing

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}
