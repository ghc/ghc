%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}
#include "HsVersions.h"

module CmdLineOpts where

import PreludeGlaST	-- bad bad bad boy, Will (_Array internals)
import Argv

CHK_Ubiq() -- debugging consistency check

import Maybes		( assocMaybe, firstJust, maybeToBool, Maybe(..) )
import Util		( panic, panic#, assertPanic )
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
  | CoreDoDeforest
  | CoreDoAutoCostCentres
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
  | SimplDoEtaReduction

  | EssentialUnfoldingsOnly -- never mind the thresholds, only
			    -- do unfoldings that *must* be done
			    -- (to saturate constructors and primitives)

  | ShowSimplifierProgress  -- report counts on every interation

  | MaxSimplifierIterations Int

  | SimplUnfoldingUseThreshold      Int -- per-simplification variants
  | SimplUnfoldingCreationThreshold Int

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
\end{code}

%************************************************************************
%*									*
\subsection{Classifying command-line options}
%*									*
%************************************************************************

\begin{code}
lookup	   :: FAST_STRING -> Bool
lookup_int :: FAST_STRING -> Maybe Int
lookup_str :: FAST_STRING -> Maybe FAST_STRING 

lookup     sw = maybeToBool (assoc_opts sw)
	
lookup_str sw = let
		    unpk_sw = _UNPK_ sw
		in
		case (firstJust (map (starts_with unpk_sw) unpacked_opts)) of
		  Nothing -> Nothing
		  Just xx -> Just (_PK_ xx)

lookup_int sw = case (lookup_str sw) of
		  Nothing -> Nothing
		  Just xx -> Just (read (_UNPK_ xx))

assoc_opts    = assocMaybe [ (a, True) | a <- argv ]
unpacked_opts = map _UNPK_ argv

starts_with :: String -> String -> Maybe String

starts_with []     str = Just str
starts_with (c:cs) (s:ss)
  = if c /= s then Nothing else starts_with cs ss
\end{code}

\begin{code}
opt_AllDemanded			= lookup  SLIT("-fall-demanded")
opt_AllStrict			= lookup  SLIT("-fall-strict")
opt_AutoSccsOnAllToplevs	= lookup  SLIT("-fauto-sccs-on-all-toplevs")
opt_AutoSccsOnExportedToplevs	= lookup  SLIT("-fauto-sccs-on-exported-toplevs")
opt_AutoSccsOnIndividualCafs	= lookup  SLIT("-fauto-sccs-on-individual-cafs")
opt_CompilingPrelude		= lookup  SLIT("-prelude")
opt_D_dump_absC			= lookup  SLIT("-ddump-absC")
opt_D_dump_asm			= lookup  SLIT("-ddump-asm")
opt_D_dump_deforest		= lookup  SLIT("-ddump-deforest")
opt_D_dump_deriv		= lookup  SLIT("-ddump-deriv")
opt_D_dump_ds			= lookup  SLIT("-ddump-ds")
opt_D_dump_flatC		= lookup  SLIT("-ddump-flatC")
opt_D_dump_occur_anal		= lookup  SLIT("-ddump-occur-anal")
opt_D_dump_rdr			= lookup  SLIT("-ddump-rdr")
opt_D_dump_realC		= lookup  SLIT("-ddump-realC")
opt_D_dump_rn			= lookup  SLIT("-ddump-rn")
opt_D_dump_simpl		= lookup  SLIT("-ddump-simpl")
opt_D_dump_spec			= lookup  SLIT("-ddump-spec")
opt_D_dump_stg			= lookup  SLIT("-ddump-stg")
opt_D_dump_stranal		= lookup  SLIT("-ddump-stranal")
opt_D_dump_tc			= lookup  SLIT("-ddump-tc")
opt_D_show_passes		= lookup  SLIT("-dshow-passes")
opt_D_simplifier_stats		= lookup  SLIT("-dsimplifier-stats")
opt_D_source_stats		= lookup  SLIT("-dsource-stats")
opt_D_verbose_core2core		= lookup  SLIT("-dverbose-simpl")
opt_D_verbose_stg2stg		= lookup  SLIT("-dverbose-stg")
opt_DoCoreLinting		= lookup  SLIT("-dcore-lint")
opt_DoSemiTagging		= lookup  SLIT("-fsemi-tagging")
opt_DoTickyProfiling		= lookup  SLIT("-fticky-ticky")
opt_EmitArityChecks		= lookup  SLIT("-darity-checks")
opt_FoldrBuildOn		= lookup  SLIT("-ffoldr-build-on")
opt_FoldrBuildTrace		= lookup  SLIT("-ffoldr-build-trace")
opt_ForConcurrent		= lookup  SLIT("-fconcurrent")
opt_GlasgowExts			= lookup  SLIT("-fglasgow-exts")
opt_Haskell_1_3			= lookup  SLIT("-fhaskell-1.3")
opt_HideBuiltinNames		= lookup  SLIT("-fhide-builtin-names")
opt_HideMostBuiltinNames	= lookup  SLIT("-fmin-builtin-names")
opt_IgnoreStrictnessPragmas	= lookup  SLIT("-fignore-strictness-pragmas")
opt_IrrefutableEverything	= lookup  SLIT("-firrefutable-everything")
opt_IrrefutableTuples		= lookup  SLIT("-firrefutable-tuples")
opt_NameShadowingNotOK		= lookup  SLIT("-fname-shadowing-not-ok")
opt_NumbersStrict		= lookup  SLIT("-fnumbers-strict")
opt_OmitBlackHoling		= lookup  SLIT("-dno-black-holing")
opt_OmitDefaultInstanceMethods	= lookup  SLIT("-fomit-default-instance-methods")
opt_OmitInterfacePragmas	= lookup  SLIT("-fomit-interface-pragmas")
opt_OmitReexportedInstances	= lookup  SLIT("-fomit-reexported-instances")
opt_PprStyle_All		= lookup  SLIT("-dppr-all")
opt_PprStyle_Debug		= lookup  SLIT("-dppr-debug")
opt_PprStyle_User		= lookup  SLIT("-dppr-user")
opt_ReportWhyUnfoldingsDisallowed= lookup SLIT("-freport-disallowed-unfoldings")
opt_SccProfilingOn		= lookup  SLIT("-fscc-profiling")
opt_ShowImportSpecs		= lookup  SLIT("-fshow-import-specs")
opt_ShowPragmaNameErrs		= lookup  SLIT("-fshow-pragma-name-errs")
opt_SigsRequired		= lookup  SLIT("-fsignatures-required")
opt_SpecialiseAll		= lookup  SLIT("-fspecialise-all")
opt_SpecialiseImports		= lookup  SLIT("-fspecialise-imports")
opt_SpecialiseOverloaded	= lookup  SLIT("-fspecialise-overloaded")
opt_SpecialiseTrace		= lookup  SLIT("-ftrace-specialisation")
opt_SpecialiseUnboxed		= lookup  SLIT("-fspecialise-unboxed")
opt_StgDoLetNoEscapes		= lookup  SLIT("-flet-no-escape")
opt_UseGetMentionedVars		= lookup  SLIT("-fuse-get-mentioned-vars")
opt_Verbose			= lookup  SLIT("-v")
opt_AsmTarget 			= lookup_str SLIT("-fasm-")
opt_SccGroup  			= lookup_str SLIT("-G")
opt_ProduceC  			= lookup_str SLIT("-C")
opt_ProduceS  			= lookup_str SLIT("-S")
opt_ProduceHi 			= lookup_str SLIT("-hi")
opt_EnsureSplittableC		= lookup_str SLIT("-fglobalise-toplev-names")
opt_UnfoldingUseThreshold	= lookup_int SLIT("-funfolding-use-threshold")
opt_UnfoldingCreationThreshold	= lookup_int SLIT("-funfolding-creation-threshold")
opt_UnfoldingOverrideThreshold	= lookup_int SLIT("-funfolding-override-threshold")
opt_ReturnInRegsThreshold	= lookup_int SLIT("-freturn-in-regs-threshold")
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
			   simpl_sep opts [] core_td stg_td

	  "-fcalc-inlinings1"-> CORE_TD(CoreDoCalcInlinings1)
	  "-fcalc-inlinings2"-> CORE_TD(CoreDoCalcInlinings2)
	  "-ffloat-inwards"  -> CORE_TD(CoreDoFloatInwards)
	  "-ffull-laziness"  -> CORE_TD(CoreDoFullLaziness)
	  "-fliberate-case"  -> CORE_TD(CoreLiberateCase)
	  "-fprint-core"     -> CORE_TD(CoreDoPrintCore)
	  "-fstatic-args"    -> CORE_TD(CoreDoStaticArgs)
	  "-fstrictness"     -> CORE_TD(CoreDoStrictness)
	  "-fspecialise"     -> CORE_TD(CoreDoSpecialising)
	  "-fdeforest"	     -> CORE_TD(CoreDoDeforest)
	  "-fadd-auto-sccs"  -> CORE_TD(CoreDoAutoCostCentres)
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

	-- The SimplifierSwitches should be delimited by "(" and ")".

    simpl_sep (opt1:opts) simpl_sw core_td stg_td
      = case (_UNPK_ opt1) of
	  "(" -> ASSERT (null simpl_sw)
		 simpl_sep opts [] core_td stg_td
	  ")" -> let
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
	  "-fdo-eta-reduction"		    -> SIMPL_SW(SimplDoEtaReduction)
	  "-fdo-lambda-eta-expansion"	    -> SIMPL_SW(SimplDoLambdaEtaExpansion)
	  "-fdo-foldr-build"		    -> SIMPL_SW(SimplDoFoldrBuild)
	  "-fdo-not-fold-back-append"	    -> SIMPL_SW(SimplDontFoldBackAppend)
	  "-fdo-arity-expand"		    -> SIMPL_SW(SimplDoArityExpand)
	  "-fdo-inline-foldr-build"	    -> SIMPL_SW(SimplDoInlineFoldrBuild)
	  "-freuse-con"			    -> SIMPL_SW(SimplReuseCon)
	  "-fcase-of-case"		    -> SIMPL_SW(SimplCaseOfCase)
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
	    | starts_with_suut -> SIMPL_SW(SimplUnfoldingUseThreshold (read after_suut))
	    | starts_with_suct -> SIMPL_SW(SimplUnfoldingCreationThreshold (read after_suct))
	   where
	    maybe_suut		= starts_with "-fsimpl-uf-use-threshold"      o
	    maybe_suct		= starts_with "-fsimpl-uf-creation-threshold" o
	    maybe_msi		= starts_with "-fmax-simplifier-iterations"   o
	    starts_with_suut	= maybeToBool maybe_suut
	    starts_with_suct	= maybeToBool maybe_suct
	    starts_with_msi	= maybeToBool maybe_msi
	    (Just after_suut)	= maybe_suut
	    (Just after_suct)	= maybe_suct
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
tagOf_SimplSwitch SimplDoEtaReduction		= ILIT(18)
tagOf_SimplSwitch EssentialUnfoldingsOnly	= ILIT(19)
tagOf_SimplSwitch ShowSimplifierProgress	= ILIT(20)
tagOf_SimplSwitch (MaxSimplifierIterations _)	= ILIT(21)
tagOf_SimplSwitch (SimplUnfoldingUseThreshold _)      = ILIT(22)
tagOf_SimplSwitch (SimplUnfoldingCreationThreshold _) = ILIT(23)
tagOf_SimplSwitch KeepSpecPragmaIds		= ILIT(24)
tagOf_SimplSwitch KeepUnusedBindings		= ILIT(25)
tagOf_SimplSwitch SimplNoLetFromCase		= ILIT(26)
tagOf_SimplSwitch SimplNoLetFromApp		= ILIT(27)
tagOf_SimplSwitch SimplNoLetFromStrictLet	= ILIT(28)
tagOf_SimplSwitch SimplDontFoldBackAppend       = ILIT(29)
-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

tagOf_SimplSwitch _ = panic# "tagOf_SimplSwitch"

lAST_SIMPL_SWITCH_TAG = IBOX(tagOf_SimplSwitch SimplDontFoldBackAppend)
\end{code}

%************************************************************************
%*									*
\subsection{Switch lookup}
%*									*
%************************************************************************

\begin{code}
isAmongSimpl :: [SimplifierSwitch] -> SimplifierSwitch -> SwitchResult

isAmongSimpl on_switches
  = let
	tidied_on_switches = foldl rm_dups [] on_switches

	sw_tbl :: Array Int SwitchResult

	sw_tbl = (array	(0, lAST_SIMPL_SWITCH_TAG) -- bounds...
			all_undefined)
		 // defined_elems

	all_undefined = [ i := SwBool False | i <- [0 .. lAST_SIMPL_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    case sw_tbl of { _Array bounds_who_needs_'em stuff ->
    \ switch ->
	case (indexArray# stuff (tagOf_SimplSwitch switch)) of
	  _Lift v -> v
    }
  where
    mk_assoc_elem k@(MaxSimplifierIterations lvl) = IBOX(tagOf_SimplSwitch k) := SwInt lvl
    mk_assoc_elem k@(SimplUnfoldingUseThreshold      i) = IBOX(tagOf_SimplSwitch k) := SwInt i
    mk_assoc_elem k@(SimplUnfoldingCreationThreshold i) = IBOX(tagOf_SimplSwitch k) := SwInt i

    mk_assoc_elem k = IBOX(tagOf_SimplSwitch k) := SwBool   True -- I'm here, Mom!

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
