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

	opt_AllStrict,
        opt_AllowOverlappingInstances,
 	opt_AllowUndecidableInstances,
	opt_AutoSccsOnAllToplevs,
	opt_AutoSccsOnExportedToplevs,
	opt_AutoSccsOnIndividualCafs,
	opt_AutoSccsOnDicts,
	opt_CompilingPrelude,
	opt_D_dump_absC,
	opt_D_dump_asm,
	opt_D_dump_deriv,
	opt_D_dump_ds,
	opt_D_dump_flatC,
	opt_D_dump_inlinings,
	opt_D_dump_foreign,
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
	opt_D_show_rn_imports,
	opt_D_simplifier_stats,
	opt_D_source_stats,
	opt_D_verbose_core2core,
	opt_D_verbose_stg2stg,
	opt_DictsStrict,
	opt_DoCoreLinting,
	opt_DoStgLinting,
	opt_DoSemiTagging,
	opt_DoEtaReduction,
	opt_DoTickyProfiling,
	opt_EmitCExternDecls,
	opt_EnsureSplittableC,
	opt_FoldrBuildOn,
	opt_GlasgowExts,
	opt_GranMacros,
	opt_HiMap,
	opt_HiVersion,
	opt_IgnoreIfacePragmas,
	opt_IrrefutableTuples,
	opt_LiberateCaseThreshold,
        opt_MaxContextReductionDepth,
	opt_MultiParamClasses,
        opt_NoHiCheck,
	opt_NoImplicitPrelude,
	opt_NoPreInlining,
	opt_NumbersStrict,
	opt_OmitBlackHoling,
	opt_OmitInterfacePragmas,
	opt_PprStyle_NoPrags,
	opt_PprStyle_Debug,
	opt_PprUserLength,
	opt_ProduceC,
	opt_ProduceHi,
	opt_ProduceS,
	opt_ProduceExportCStubs,
	opt_ProduceExportHStubs,
	opt_ReportCompile,
	opt_SccGroup,
	opt_SccProfilingOn,
	opt_SourceUnchanged,
	opt_Static,
	opt_StgDoLetNoEscapes,
	opt_Parallel,

	opt_InterfaceUnfoldThreshold,
	opt_UnfoldCasms,
	opt_UnfoldingCreationThreshold,
	opt_UnfoldingConDiscount,
	opt_UnfoldingUseThreshold,
	opt_UnfoldingKeenessFactor,

	opt_Verbose,

	opt_WarnNameShadowing,
	opt_WarnUnusedMatches,
	opt_WarnUnusedBinds,
	opt_WarnUnusedImports,
	opt_WarnIncompletePatterns,
	opt_WarnOverlappingPatterns,
	opt_WarnSimplePatterns,
	opt_WarnTypeDefaults,
	opt_WarnMissingMethods,
	opt_WarnDuplicateExports,
	opt_WarnHiShadows,
	opt_WarnMissingSigs,
	opt_PruneTyDecls, opt_PruneInstDecls,
	opt_D_show_rn_stats
    ) where

#include "HsVersions.h"

import Array	( array, (//) )
import GlaExts
import Argv
import Constants	-- Default values for some flags

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

  | MaxSimplifierIterations Int

  | SimplNoLetFromCase	    -- used when turning off floating entirely
  | SimplNoLetFromApp	    -- (for experimentation only) WDP 95/10
  | SimplNoLetFromStrictLet

  | SimplCaseMerge
  | SimplPleaseClone
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
opt_AllStrict			= lookUp  SLIT("-fall-strict")
opt_AllowOverlappingInstances   = lookUp  SLIT("-fallow-overlapping-instances")
opt_AllowUndecidableInstances 	= lookUp  SLIT("-fallow-undecidable-instances")
opt_AutoSccsOnAllToplevs	= lookUp  SLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookUp  SLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookUp  SLIT("-fauto-sccs-on-individual-cafs")
opt_AutoSccsOnDicts		= lookUp  SLIT("-fauto-sccs-on-dicts")
  {-
   It's a bit unfortunate to have to re-introduce this chap, but on Win32
   platforms we do need a way of distinguishing between the case when we're
   compiling a static version of the Prelude and one that's going to be
   put into a DLL. Why? Because the compiler's wired in modules need to
   be attributed as either coming from a DLL or not.
  -}
opt_CompilingPrelude		= lookUp  SLIT("-fcompiling-prelude")
opt_D_dump_absC			= lookUp  SLIT("-ddump-absC")
opt_D_dump_asm			= lookUp  SLIT("-ddump-asm")
opt_D_dump_deriv		= lookUp  SLIT("-ddump-deriv")
opt_D_dump_ds			= lookUp  SLIT("-ddump-ds")
opt_D_dump_flatC		= lookUp  SLIT("-ddump-flatC")
opt_D_dump_inlinings		= lookUp  SLIT("-ddump-inlinings")
opt_D_dump_foreign		= lookUp  SLIT("-ddump-foreign-stubs")
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
opt_D_show_rn_imports		= lookUp  SLIT("-dshow-rn-imports")
opt_D_simplifier_stats		= lookUp  SLIT("-dsimplifier-stats")
opt_D_source_stats		= lookUp  SLIT("-dsource-stats")
opt_D_verbose_core2core		= lookUp  SLIT("-dverbose-simpl")
opt_D_verbose_stg2stg		= lookUp  SLIT("-dverbose-stg")
opt_DictsStrict			= lookUp  SLIT("-fdicts-strict")
opt_DoCoreLinting		= lookUp  SLIT("-dcore-lint")
opt_DoStgLinting		= lookUp  SLIT("-dstg-lint")
opt_DoEtaReduction		= lookUp  SLIT("-fdo-eta-reduction")
opt_DoSemiTagging		= lookUp  SLIT("-fsemi-tagging")
opt_DoTickyProfiling		= lookUp  SLIT("-fticky-ticky")
opt_EmitCExternDecls	        = lookUp  SLIT("-femit-extern-decls")
opt_EnsureSplittableC		= lookUp  SLIT("-fglobalise-toplev-names")
opt_FoldrBuildOn		= lookUp  SLIT("-ffoldr-build-on")
opt_GranMacros			= lookUp  SLIT("-fgransim")
opt_GlasgowExts			= lookUp  SLIT("-fglasgow-exts")
opt_HiMap 			= lookup_str "-himap="       -- file saying where to look for .hi files
opt_HiVersion			= lookup_def_int "-fhi-version=" 0 -- what version we're compiling.
opt_IgnoreIfacePragmas		= lookUp  SLIT("-fignore-interface-pragmas")
opt_IrrefutableTuples		= lookUp  SLIT("-firrefutable-tuples")
opt_MaxContextReductionDepth	= lookup_def_int "-fcontext-stack" mAX_CONTEXT_REDUCTION_DEPTH
opt_MultiParamClasses		= opt_GlasgowExts
opt_NoHiCheck                   = lookUp  SLIT("-fno-hi-version-check")
opt_NoImplicitPrelude		= lookUp  SLIT("-fno-implicit-prelude")
opt_NoPreInlining		= lookUp  SLIT("-fno-pre-inlining")
opt_NumbersStrict		= lookUp  SLIT("-fnumbers-strict")
opt_OmitBlackHoling		= lookUp  SLIT("-dno-black-holing")
opt_OmitInterfacePragmas	= lookUp  SLIT("-fomit-interface-pragmas")
opt_PprStyle_NoPrags		= lookUp  SLIT("-dppr-noprags")
opt_PprStyle_Debug		= lookUp  SLIT("-dppr-debug")
opt_PprUserLength	        = lookup_def_int "-dppr-user-length" 5 --ToDo: give this a name
opt_ProduceC  			= lookup_str "-C="
opt_ProduceS  			= lookup_str "-S="
opt_ProduceExportCStubs		= lookup_str "-F="
opt_ProduceExportHStubs		= lookup_str "-FH="
opt_ProduceHi 			= lookup_str "-hifile=" -- the one to produce this time 
opt_ReportCompile                = lookUp SLIT("-freport-compile")
opt_SccProfilingOn		= lookUp  SLIT("-fscc-profiling")
opt_SourceUnchanged		= lookUp  SLIT("-fsource-unchanged")
opt_StgDoLetNoEscapes		= lookUp  SLIT("-flet-no-escape")
opt_Parallel			= lookUp  SLIT("-fparallel")
opt_Static			= lookUp  SLIT("-static")
opt_SccGroup  			= lookup_str "-G="
opt_Verbose			= lookUp  SLIT("-v")

opt_UnfoldCasms		        = lookUp SLIT("-funfold-casms-in-hi-file")
opt_InterfaceUnfoldThreshold	= lookup_def_int "-funfolding-interface-threshold" iNTERFACE_UNFOLD_THRESHOLD
opt_UnfoldingCreationThreshold	= lookup_def_int "-funfolding-creation-threshold"  uNFOLDING_CREATION_THRESHOLD
opt_UnfoldingUseThreshold	= lookup_def_int "-funfolding-use-threshold"	   uNFOLDING_USE_THRESHOLD
opt_UnfoldingConDiscount	= lookup_def_int "-funfolding-con-discount"	   uNFOLDING_CON_DISCOUNT_WEIGHT
			
opt_LiberateCaseThreshold	= lookup_def_int "-fliberate-case-threshold"	   lIBERATE_CASE_THRESHOLD
opt_UnfoldingKeenessFactor	= lookup_def_float "-funfolding-keeness-factor"	   uNFOLDING_KEENESS_FACTOR
opt_WarnNameShadowing		= lookUp  SLIT("-fwarn-name-shadowing")
opt_WarnHiShadows		= lookUp  SLIT("-fwarn-hi-shadowing")
opt_WarnIncompletePatterns	= lookUp  SLIT("-fwarn-incomplete-patterns")
opt_WarnOverlappingPatterns	= lookUp  SLIT("-fwarn-overlapping-patterns")
opt_WarnSimplePatterns	     	= lookUp  SLIT("-fwarn-simple-patterns")
opt_WarnTypeDefaults		= lookUp  SLIT("-fwarn-type-defaults")
opt_WarnUnusedMatches		= lookUp  SLIT("-fwarn-unused-matches")
opt_WarnUnusedBinds		= lookUp  SLIT("-fwarn-unused-binds")
opt_WarnUnusedImports		= lookUp  SLIT("-fwarn-unused-imports")
opt_WarnMissingMethods		= lookUp  SLIT("-fwarn-missing-methods")
opt_WarnDuplicateExports	= lookUp  SLIT("-fwarn-duplicate-exports")
opt_WarnMissingSigs		= lookUp  SLIT("-fwarn-missing-signatures")
opt_PruneTyDecls		= not (lookUp SLIT("-fno-prune-tydecls"))
opt_PruneInstDecls		= not (lookUp SLIT("-fno-prune-instdecls"))
opt_D_show_rn_stats		= lookUp SLIT("-dshow-rn-stats")

-- opt_UnfoldingOverrideThreshold	= lookup_int "-funfolding-override-threshold"
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

#	  define SIMPL_SW(sw) simpl_sep opts (sw:simpl_sw) core_td stg_td

	  -- the non-"just match a string" options are at the end...
	  "-fcode-duplication-ok"	    -> SIMPL_SW(SimplOkToDupCode)
	  "-ffloat-lets-exposing-whnf"	    -> SIMPL_SW(SimplFloatLetsExposingWHNF)
	  "-ffloat-primops-ok"		    -> SIMPL_SW(SimplOkToFloatPrimOps)
	  "-falways-float-lets-from-lets"   -> SIMPL_SW(SimplAlwaysFloatLetsFromLets)
	  "-fdo-case-elim"		    -> SIMPL_SW(SimplDoCaseElim)
	  "-fdo-lambda-eta-expansion"	    -> SIMPL_SW(SimplDoLambdaEtaExpansion)
	  "-fdo-foldr-build"		    -> SIMPL_SW(SimplDoFoldrBuild)
	  "-fdo-arity-expand"		    -> SIMPL_SW(SimplDoArityExpand)
	  "-fdo-inline-foldr-build"	    -> SIMPL_SW(SimplDoInlineFoldrBuild)
	  "-fcase-of-case"		    -> SIMPL_SW(SimplCaseOfCase)
	  "-fcase-merge"		    -> SIMPL_SW(SimplCaseMerge)
	  "-flet-to-case"		    -> SIMPL_SW(SimplLetToCase)
	  "-fpedantic-bottoms"		    -> SIMPL_SW(SimplPedanticBottoms)
	  "-fmay-delete-conjurable-ids"     -> SIMPL_SW(SimplMayDeleteConjurableIds)
	  "-fessential-unfoldings-only"     -> SIMPL_SW(EssentialUnfoldingsOnly)
	  "-fignore-inline-pragma"  	    -> SIMPL_SW(IgnoreINLINEPragma)
	  "-fno-let-from-case"		    -> SIMPL_SW(SimplNoLetFromCase)
	  "-fno-let-from-app"		    -> SIMPL_SW(SimplNoLetFromApp)
	  "-fno-let-from-strict-let"	    -> SIMPL_SW(SimplNoLetFromStrictLet)
	  "-fclone-binds"		    -> SIMPL_SW(SimplPleaseClone)

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
tagOf_SimplSwitch (MaxSimplifierIterations _)	= ILIT(21)
tagOf_SimplSwitch SimplNoLetFromCase		= ILIT(27)
tagOf_SimplSwitch SimplNoLetFromApp		= ILIT(28)
tagOf_SimplSwitch SimplNoLetFromStrictLet	= ILIT(29)
tagOf_SimplSwitch SimplCaseMerge		= ILIT(31)
tagOf_SimplSwitch SimplPleaseClone		= ILIT(32)

-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

tagOf_SimplSwitch _ = panic# "tagOf_SimplSwitch"

lAST_SIMPL_SWITCH_TAG = IBOX(tagOf_SimplSwitch SimplPleaseClone)
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
#else
	  (# _, v #) -> v
#endif
    }
  where
    mk_assoc_elem k@(MaxSimplifierIterations lvl)       = (IBOX(tagOf_SimplSwitch k), SwInt lvl)

    mk_assoc_elem k = (IBOX(tagOf_SimplSwitch k), SwBool True) -- I'm here, Mom!

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

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}

\begin{code}
startsWith, endsWith :: String -> String -> Maybe String

startsWith []     str = Just str
startsWith (c:cs) (s:ss)
  = if c /= s then Nothing else startsWith cs ss
startsWith  _	  []  = Nothing

endsWith cs ss
  = case (startsWith (reverse cs) (reverse ss)) of
      Nothing -> Nothing
      Just rs -> Just (reverse rs)
\end{code}
