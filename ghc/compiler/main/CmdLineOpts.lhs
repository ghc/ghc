%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CmdLineOpts]{Things to do with command-line options}

\begin{code}
#include "HsVersions.h"

module CmdLineOpts (
	CmdLineInfo(..), SwitchResult(..),
	GlobalSwitch(..), SimplifierSwitch(..),
	CoreToDo(..),
	StgToDo(..),
#ifdef DPH
	PodizeToDo(..),
#endif {- Data Parallel Haskell -}
	
	classifyOpts,
	switchIsOn, stringSwitchSet, intSwitchSet,
	
	-- to make the interface self-sufficient
	Maybe, MainIO(..)
    ) where

import MainMonad
import Maybes		( maybeToBool, Maybe(..) )
import Outputable
import Util
#ifdef __GLASGOW_HASKELL__
import PreludeGlaST	-- bad bad bad boy, Will
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

We use function @classifyOpts@ to take raw command-line arguments from
@GetArgs@ and get back the @CmdLineInfo@, which is what we really
want.

%************************************************************************
%*									*
\subsection[CmdLineOpts-datatype]{Datatypes associated with command-line options}
%*									*
%************************************************************************

\begin{code}
type CmdLineInfo 
  = (GlobalSwitch -> SwitchResult,	-- Switch lookup function
     [CoreToDo],			-- Core-to-core spec
#ifdef DPH 
     [PodizeToDo],			-- Podizer spec
     [CoreToDo],			-- post podized Core-to-core spec 
#endif
     [StgToDo]				-- Stg-to-stg spec
    )

data SwitchResult
  = SwBool	Bool	-- on/off
  | SwString	String	-- nothing or a String
  | SwInt	Int	-- nothing or an Int
\end{code}

\begin{code}
data CoreToDo		-- These are diff core-to-core passes,
			-- which may be invoked in any order,
  			-- as many times as you like.

  = CoreDoSimplify	-- The core-to-core simplifier.
	(SimplifierSwitch -> SwitchResult)
			-- Each run of the simplifier can take a different
			-- set of simplifier-specific flags.

  | Core_Unused_Flag_1
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
-- ANDY:
--| CoreDoHaskPrint
--| CoreDoHaskLetlessPrint
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
#ifdef DPH
data PodizeToDo
  = PodizeNeeded Int		-- Which dimensioned PODs need vectorizing
#endif {- Data Parallel Haskell -}
\end{code}

@GlobalSwitches@ may be visible everywhere in the compiler.
@SimplifierSwitches@ (which follow) are visible only in the main
Core-to-Core simplifier.

\begin{code}
data GlobalSwitch
  = ProduceC	String	-- generate C output into this file
  | ProduceS	String	-- generate native-code assembler into this file
  | ProduceHi	String	-- generate .hi interface  into this file

  | AsmTarget	String  -- architecture we are generating code for
  | ForConcurrent

  | Haskell_1_3		-- if set => Haskell 1.3; else 1.2
  | GlasgowExts		-- Glasgow Haskell extensions allowed
  | CompilingPrelude	-- Compiling prelude source

  | HideBuiltinNames 	-- fiddle builtin namespace; used for compiling Prelude
  | HideMostBuiltinNames
  | EnsureSplittableC String -- (by globalising all top-level Ids w/ this String)

  | Verbose
  | PprStyle_User	-- printing "level" (mostly for debugging)
  | PprStyle_Debug
  | PprStyle_All

  | DoCoreLinting	-- paranoia flags
  | EmitArityChecks

  | OmitInterfacePragmas
  | OmitDerivedRead
  | OmitReexportedInstances

  | UnfoldingUseThreshold      Int  -- global one; see also SimplUnf...
  | UnfoldingCreationThreshold Int  -- ditto
  | UnfoldingOverrideThreshold Int

  | ReportWhyUnfoldingsDisallowed
  | UseGetMentionedVars
  | ShowPragmaNameErrs
  | NameShadowingNotOK
  | SigsRequired

  | SccProfilingOn
  | AutoSccsOnExportedToplevs
  | AutoSccsOnAllToplevs
  | AutoSccsOnIndividualCafs
  | SccGroup String	-- name of "group" for this cost centres in this module

  | DoTickyProfiling

  | DoSemiTagging

  -- ToDo: turn these into SimplifierSwitches?
  | FoldrBuildOn	-- If foldr/build-style transformations are on.
			-- See also SimplDoFoldrBuild, which is used
			-- inside the simplifier.
  | FoldrBuildTrace	-- show all foldr/build optimisations.

  | SpecialiseImports	   -- Treat non-essential spec requests as errors
  | ShowImportSpecs	   -- Output spec requests for non-essential specs
  | OmitDefaultInstanceMethods
  | SpecialiseOverloaded
  | SpecialiseUnboxed
  | SpecialiseAll
  | SpecialiseTrace

  -- this batch of flags is for particular experiments;
  -- v unlikely to be used in any other circumstance
  | OmitBlackHoling
  | StgDoLetNoEscapes
  | IgnoreStrictnessPragmas -- ToDo: still useful?
  | IrrefutableTuples	    -- We inject extra "LazyPat"s in the typechecker
  | IrrefutableEverything   -- (TcPat); doing it any earlier would mean that
			    -- deriving-generated code wouldn't be irrefutablified.
  | AllStrict
  | NumbersStrict
  | AllDemanded

  | ReturnInRegsThreshold   Int
  | VectoredReturnThreshold Int -- very likely UNUSED

  | D_dump_rif2hs	-- debugging: print out various things
  | D_dump_rn4
  | D_dump_tc
  | D_dump_deriv
  | D_dump_ds
  | D_dump_occur_anal
  | D_dump_simpl
  | D_dump_spec
  | D_dump_stranal
  | D_dump_deforest
  | D_dump_stg
  | D_dump_absC
  | D_dump_flatC
  | D_dump_realC
  | D_dump_asm
  | D_show_passes
--ANDY:  | D_dump_core_passes_info	-- A Gill-ism

  | D_verbose_core2core
  | D_verbose_stg2stg
  | D_simplifier_stats
  | D_source_stats

#ifdef DPH
  | PodizeIntelligent
  | PodizeAggresive
  | PodizeVeryAggresive
  | PodizeExtremelyAggresive
  | D_dump_pod
  | D_dump_psimpl
  | D_dump_nextC
#endif {- Data Parallel Haskell -}
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
  | SimplDoNewOccurAnal	 --  use the *new*, all singing, Occurance analysis
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
{-
  | Extra__SimplFlag1
  | Extra__SimplFlag2
  | Extra__SimplFlag3
  | Extra__SimplFlag4
  | Extra__SimplFlag5
  | Extra__SimplFlag6
  | Extra__SimplFlag7
  | Extra__SimplFlag8
-}
\end{code}

%************************************************************************
%*									*
\subsection[CmdLineOpts-classify]{Classifying command-line options}
%*									*
%************************************************************************

\begin{code}
classifyOpts :: [String]	    -- cmd-line args, straight from GetArgs
	     -> MainIO CmdLineInfo
-- The MainIO bit is because we might find an unknown flag
-- in which case we print an error message

#ifndef DPH
classifyOpts opts
  = sep opts [] [] [] -- accumulators...
  where
    sep :: [String]				 -- cmd-line opts (input)
	-> [GlobalSwitch]			 -- switch accumulator
	-> [CoreToDo] -> [StgToDo]		 -- to_do accumulators
	-> MainIO CmdLineInfo			 -- result

    sep [] glob_sw core_td stg_td
      = returnMn (
	  isAmong glob_sw,
	  reverse core_td,
	  reverse stg_td
	)

    sep (opt1:opts) glob_sw core_td stg_td

#else {- Data Parallel Haskell -}
classifyOpts opts
  = sep opts [] [] [] [] [] -- accumulators...
  where
    sep :: [String]				 -- cmd-line opts (input)
	-> [GlobalSwitch]			 -- switch accumulator
	-> [CoreToDo] -> [PodizeToDo]		 -- to_do accumulators
	-> [CoreToDo] -> [StgToDo]
	-> MainIO CmdLineInfo			 -- result

    -- see also the related "simpl_sep" function, used
    -- to collect up the SimplifierSwitches for a "-fsimplify".

    sep [] glob_sw core_td pod_td pcore_td stg_td
      = returnMn (
	  isAmong glob_sw,
	  reverse core_td,
	  reverse pod_td,
	  reverse pcore_td,
	  reverse stg_td
	)

    sep (opt1:opts) glob_sw core_td pod_td pcore_td stg_td
#endif {- Data Parallel Haskell -}

#ifndef DPH
#define GLOBAL_SW(switch)   sep opts (switch:glob_sw) core_td stg_td
#define CORE_TD(to_do)	    sep opts glob_sw (to_do:core_td) stg_td
#define POD_TD(to_do)       sep opts glob_sw core_td stg_td
#define PAR_CORE_TD(to_do)  sep opts glob_sw core_td stg_td
#define BOTH_CORE_TD(to_do) sep opts glob_sw (to_do:core_td) stg_td
#define STG_TD(to_do)	    sep opts glob_sw core_td (to_do:stg_td)
#define IGNORE_ARG()	    sep opts glob_sw core_td stg_td

#else

#define GLOBAL_SW(switch) sep opts (switch:glob_sw) core_td pod_td pcore_td stg_td
#define CORE_TD(to_do)	  sep opts glob_sw (to_do:core_td) pod_td pcore_td stg_td
#define POD_TD(to_do)	  sep opts glob_sw core_td (to_do:pod_td) pcore_td stg_td
#define PAR_CORE_TD(do)	  sep opts glob_sw core_td pod_td (do:pcore_td) stg_td
#define BOTH_CORE_TD(do)  sep opts glob_sw (do:core_td) pod_td (do:pcore_td) stg_td
#define STG_TD(to_do)	  sep opts glob_sw core_td pod_td pcore_td (to_do:stg_td)
#define IGNORE_ARG()	  sep opts glob_sw core_td pod_td pcore_td stg_td

#endif {- Data Parallel Haskell -}

-- ToDo: DPH-ify
#define GLOBAL_SIMPL_SW(switch) simpl_sep opts (switch:simpl_sw) glob_sw core_td stg_td

      = let
	    maybe_fasm		= starts_with "-fasm-"  opt1
	    maybe_G		= starts_with "-G"	opt1
	    maybe_C		= starts_with "-C"	opt1
	    maybe_S		= starts_with "-S"	opt1
	    maybe_hi		= starts_with "-hi"	opt1
	    maybe_hu		= starts_with "-hu"	opt1
	    maybe_uut		= starts_with "-funfolding-use-threshold"      opt1
	    maybe_uct		= starts_with "-funfolding-creation-threshold" opt1
	    maybe_uot		= starts_with "-funfolding-override-threshold" opt1
	    maybe_rirt		= starts_with "-freturn-in-regs-threshold"     opt1
	    maybe_gtn		= starts_with "-fglobalise-toplev-names"       opt1
	    starts_with_fasm	= maybeToBool maybe_fasm
	    starts_with_G	= maybeToBool maybe_G
	    starts_with_C	= maybeToBool maybe_C
	    starts_with_S	= maybeToBool maybe_S
	    starts_with_hi	= maybeToBool maybe_hi
	    starts_with_hu	= maybeToBool maybe_hu
	    starts_with_uut	= maybeToBool maybe_uut
	    starts_with_uct	= maybeToBool maybe_uct
	    starts_with_uot	= maybeToBool maybe_uot
	    starts_with_rirt	= maybeToBool maybe_rirt
	    starts_with_gtn	= maybeToBool maybe_gtn
	    (Just after_fasm)	= maybe_fasm
	    (Just after_G)	= maybe_G
	    (Just after_C)	= maybe_C
	    (Just after_S)	= maybe_S
	    (Just after_hi)	= maybe_hi
	    (Just after_hu)	= maybe_hu
	    (Just after_uut)	= maybe_uut
	    (Just after_uct)	= maybe_uct
	    (Just after_uot)	= maybe_uot
	    (Just after_rirt)	= maybe_rirt
	    (Just after_gtn)	= maybe_gtn
	in
	case opt1 of -- the non-"just match a string" options are at the end...
	  ',' : _	   -> IGNORE_ARG() -- it is for the parser
	  "-ddump-rif2hs"  -> GLOBAL_SW(D_dump_rif2hs)
	  "-ddump-rn4"	   -> GLOBAL_SW(D_dump_rn4)
	  "-ddump-tc"	   -> GLOBAL_SW(D_dump_tc)
	  "-ddump-deriv"   -> GLOBAL_SW(D_dump_deriv)
	  "-ddump-ds"	   -> GLOBAL_SW(D_dump_ds)
	  "-ddump-stranal" -> GLOBAL_SW(D_dump_stranal)
	  "-ddump-deforest"-> GLOBAL_SW(D_dump_deforest)
	  "-ddump-spec"    -> GLOBAL_SW(D_dump_spec)
	  "-ddump-simpl"   -> GLOBAL_SW(D_dump_simpl)
	  "-ddump-occur-anal" -> GLOBAL_SW(D_dump_occur_anal)
#ifdef DPH
	  "-ddump-pod"    ->  GLOBAL_SW(D_dump_pod)
	  "-ddump-psimpl" ->  GLOBAL_SW(D_dump_psimpl)
	  "-ddump-nextC"  ->  GLOBAL_SW(D_dump_nextC)
#endif {- Data Parallel Haskell -}

	  "-ddump-stg"	  ->  GLOBAL_SW(D_dump_stg)
	  "-ddump-absC"	  ->  GLOBAL_SW(D_dump_absC)
	  "-ddump-flatC"  ->  GLOBAL_SW(D_dump_flatC)
	  "-ddump-realC"  ->  GLOBAL_SW(D_dump_realC)
          "-ddump-asm"    ->  GLOBAL_SW(D_dump_asm)
          "-dshow-passes" ->  GLOBAL_SW(D_show_passes)

-- ANDY:  "-ddump-haskell"	    -> GLOBAL_SW(D_dump_core_passes_info)
	  "-dsimplifier-stats"	    -> GLOBAL_SW(D_simplifier_stats)
	  "-dsource-stats"	    -> GLOBAL_SW(D_source_stats)

	  "-dverbose-simpl" ->GLOBAL_SW(D_verbose_core2core)
	  "-dverbose-stg" ->  GLOBAL_SW(D_verbose_stg2stg)

	  "-fuse-get-mentioned-vars" -> GLOBAL_SW(UseGetMentionedVars)

	  "-fhaskell-1.3"		-> GLOBAL_SW(Haskell_1_3)
	  "-dcore-lint"			-> GLOBAL_SW(DoCoreLinting)
	  "-fomit-interface-pragmas"	-> GLOBAL_SW(OmitInterfacePragmas)
	  "-fignore-strictness-pragmas" -> GLOBAL_SW(IgnoreStrictnessPragmas)
	  "-firrefutable-tuples"	-> GLOBAL_SW(IrrefutableTuples)
	  "-firrefutable-everything"	-> GLOBAL_SW(IrrefutableEverything)
	  "-fall-strict"		-> GLOBAL_SW(AllStrict)
	  "-fnumbers-strict"		-> GLOBAL_SW(NumbersStrict)
	  "-fall-demanded"		-> GLOBAL_SW(AllDemanded)

	  "-fsemi-tagging"   -> GLOBAL_SW(DoSemiTagging)

	  "-fsimplify"       -> -- gather up SimplifierSwitches specially...
				simpl_sep opts [] glob_sw core_td stg_td

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
--ANDY:   "-fprint-haskell-core" -> CORE_TD(CoreDoHaskPrint)
--        "-fprint-haskell-letless-core" -> CORE_TD(CoreDoHaskLetlessPrint)
	  "-fomit-default-instance-methods" -> GLOBAL_SW(OmitDefaultInstanceMethods)
	  "-fspecialise-overloaded" -> GLOBAL_SW(SpecialiseOverloaded)
	  "-fspecialise-unboxed"    -> GLOBAL_SW(SpecialiseUnboxed)
	  "-fspecialise-all" 	    -> GLOBAL_SW(SpecialiseAll)
	  "-fspecialise-imports"    -> GLOBAL_SW(SpecialiseImports)
	  "-fshow-import-specs"     -> GLOBAL_SW(ShowImportSpecs)
	  "-ftrace-specialisation"  -> GLOBAL_SW(SpecialiseTrace)

	  "-freport-disallowed-unfoldings"
			     -> GLOBAL_SW(ReportWhyUnfoldingsDisallowed)

	  "-fomit-derived-read" -> GLOBAL_SW(OmitDerivedRead)

          "-ffoldr-build-on"	    -> GLOBAL_SW(FoldrBuildOn)
          "-ffoldr-build-trace"	    -> GLOBAL_SW(FoldrBuildTrace)

	  "-fstg-static-args" -> STG_TD(StgDoStaticArgs)
	  "-fupdate-analysis" -> STG_TD(StgDoUpdateAnalysis)
	  "-dstg-stats"	      -> STG_TD(D_stg_stats)
	  "-flambda-lift"     -> STG_TD(StgDoLambdaLift)
	  "-fmassage-stg-for-profiling" -> STG_TD(StgDoMassageForProfiling)

	  "-flet-no-escape"   -> GLOBAL_SW(StgDoLetNoEscapes)

#ifdef DPH
	  "-fpodize-vector"              -> POD_TD(PodizeNeeded 1)
	  "-fpodize-matrix"              -> POD_TD(PodizeNeeded 2)
	  "-fpodize-cube"                -> POD_TD(PodizeNeeded 3)
	  "-fpodize-intelligent"         -> GLOBAL_SW(PodizeIntelligent)
	  "-fpodize-aggresive"           -> GLOBAL_SW(PodizeAggresive)
	  "-fpodize-very-aggresive"      -> GLOBAL_SW(PodizeVeryAggresive)
	  "-fpodize-extremely-aggresive" -> GLOBAL_SW(PodizeExtremelyAggresive)
#endif {- Data Parallel Haskell -}

	  "-v"		->	    GLOBAL_SW(Verbose)

	  "-fglasgow-exts" ->	    GLOBAL_SW(GlasgowExts)
	  "-prelude"	->	    GLOBAL_SW(CompilingPrelude)

	  "-fscc-profiling" 	    	    -> GLOBAL_SW(SccProfilingOn)
	  "-fauto-sccs-on-exported-toplevs" -> GLOBAL_SW(AutoSccsOnExportedToplevs)
	  "-fauto-sccs-on-all-toplevs"	    -> GLOBAL_SW(AutoSccsOnAllToplevs)
	  "-fauto-sccs-on-individual-cafs"  -> GLOBAL_SW(AutoSccsOnIndividualCafs)

	  "-fticky-ticky"  -> GLOBAL_SW(DoTickyProfiling)

	  "-dppr-user"	->  	    GLOBAL_SW(PprStyle_User)
	  "-dppr-debug"	->  	    GLOBAL_SW(PprStyle_Debug)
	  "-dppr-all"	->  	    GLOBAL_SW(PprStyle_All)

	  "-fhide-builtin-names"-> 	GLOBAL_SW(HideBuiltinNames)
	  "-fmin-builtin-names"	-> 	GLOBAL_SW(HideMostBuiltinNames)

	  "-fconcurrent"	    -> GLOBAL_SW(ForConcurrent)

	  "-fshow-pragma-name-errs" -> GLOBAL_SW(ShowPragmaNameErrs)
	  "-fname-shadowing-not-ok" -> GLOBAL_SW(NameShadowingNotOK)
	  "-fsignatures-required"   -> GLOBAL_SW(SigsRequired)
	  "-fomit-reexported-instances" -> GLOBAL_SW(OmitReexportedInstances)
	  "-darity-checks"  -> GLOBAL_SW(EmitArityChecks)
	  "-dno-black-holing"-> GLOBAL_SW(OmitBlackHoling)

	  _ | starts_with_fasm -> GLOBAL_SW(AsmTarget after_fasm)
	    | starts_with_G    -> GLOBAL_SW(SccGroup  after_G)  -- profiling "group"
	    | starts_with_C    -> GLOBAL_SW(ProduceC  after_C)  -- main C output 
	    | starts_with_S    -> GLOBAL_SW(ProduceS  after_S)  -- main .s output 
	    | starts_with_hi   -> GLOBAL_SW(ProduceHi after_hi) -- interface 
--UNUSED:   | starts_with_hu   -> GLOBAL_SW(ProduceHu after_hu)	-- usage info

	    | starts_with_uut  -> GLOBAL_SW(UnfoldingUseThreshold      (read after_uut))
	    | starts_with_uct  -> GLOBAL_SW(UnfoldingCreationThreshold (read after_uct))
	    | starts_with_uot  -> GLOBAL_SW(UnfoldingOverrideThreshold (read after_uot))

	    | starts_with_rirt -> -- trace ("rirt:"++after_rirt) $
				  GLOBAL_SW(ReturnInRegsThreshold (read after_rirt))

	    | starts_with_gtn  -> GLOBAL_SW(EnsureSplittableC after_gtn)


	  _ -> writeMn stderr ("*** WARNING: bad option: "++opt1++"\n") `thenMn` ( \ _ ->
		-- NB: the driver is really supposed to handle bad options
	       IGNORE_ARG() )

    ----------------

    starts_with :: String -> String -> Maybe String

    starts_with []     str = Just str
    starts_with (c:cs) (s:ss)
      = if c /= s then Nothing else starts_with cs ss

    ----------------

    -- ToDo: DPH-ify "simpl_sep"!

    simpl_sep :: [String]			-- cmd-line opts (input)
	-> [SimplifierSwitch]			-- simplifier-switch accumulator
	-> [GlobalSwitch]			-- switch accumulator
	-> [CoreToDo] -> [StgToDo]		-- to_do accumulators
	-> MainIO CmdLineInfo			-- result

	-- "simpl_sep" tailcalls "sep" once it's seen one set
	-- of SimplifierSwitches for a CoreDoSimplify.

#ifdef DEBUG
    simpl_sep input@[] simpl_sw glob_sw core_td stg_td
      = panic "simpl_sep []"
#endif

	-- The SimplifierSwitches should be delimited by "(" and ")".

    simpl_sep ("(":opts) [{-better be empty-}] glob_sw core_td stg_td
      = simpl_sep opts [] glob_sw core_td stg_td

    simpl_sep (")":opts) simpl_sw glob_sw core_td stg_td
      = let
	    this_CoreDoSimplify = CoreDoSimplify (isAmongSimpl simpl_sw)
	in
	sep opts glob_sw (this_CoreDoSimplify : core_td) stg_td

    simpl_sep (opt1:opts) simpl_sw glob_sw core_td stg_td
      = let
	    maybe_suut		= starts_with "-fsimpl-uf-use-threshold"      opt1
	    maybe_suct		= starts_with "-fsimpl-uf-creation-threshold" opt1
	    maybe_msi		= starts_with "-fmax-simplifier-iterations"   opt1
	    starts_with_suut	= maybeToBool maybe_suut
	    starts_with_suct	= maybeToBool maybe_suct
	    starts_with_msi	= maybeToBool maybe_msi
	    (Just after_suut)	= maybe_suut
	    (Just after_suct)	= maybe_suct
	    (Just after_msi)	= maybe_msi
	in
	case opt1 of -- the non-"just match a string" options are at the end...
	  "-fshow-simplifier-progress" -> GLOBAL_SIMPL_SW(ShowSimplifierProgress)

	  "-fcode-duplication-ok" -> GLOBAL_SIMPL_SW(SimplOkToDupCode)
	  "-ffloat-lets-exposing-whnf"	-> GLOBAL_SIMPL_SW(SimplFloatLetsExposingWHNF)
	  "-ffloat-primops-ok"	-> GLOBAL_SIMPL_SW(SimplOkToFloatPrimOps)
	  "-falways-float-lets-from-lets" -> GLOBAL_SIMPL_SW(SimplAlwaysFloatLetsFromLets)
	  "-fdo-case-elim" -> GLOBAL_SIMPL_SW(SimplDoCaseElim)
	  "-fdo-eta-reduction" -> GLOBAL_SIMPL_SW(SimplDoEtaReduction)
	  "-fdo-lambda-eta-expansion" -> GLOBAL_SIMPL_SW(SimplDoLambdaEtaExpansion)
	  "-fdo-foldr-build"  -> GLOBAL_SIMPL_SW(SimplDoFoldrBuild)
	  "-fdo-new-occur-anal"  -> GLOBAL_SIMPL_SW(SimplDoNewOccurAnal)
	  "-fdo-arity-expand"  -> GLOBAL_SIMPL_SW(SimplDoArityExpand)
	  "-fdo-inline-foldr-build"  -> GLOBAL_SIMPL_SW(SimplDoInlineFoldrBuild)
	  "-freuse-con"       -> GLOBAL_SIMPL_SW(SimplReuseCon)
	  "-fcase-of-case"    ->    GLOBAL_SIMPL_SW(SimplCaseOfCase)
	  "-flet-to-case"     -> GLOBAL_SIMPL_SW(SimplLetToCase)
	  "-fpedantic-bottoms" -> GLOBAL_SIMPL_SW(SimplPedanticBottoms)
	  "-fkeep-spec-pragma-ids" -> GLOBAL_SIMPL_SW(KeepSpecPragmaIds)
	  "-fkeep-unused-bindings" -> GLOBAL_SIMPL_SW(KeepUnusedBindings)
	  "-fmay-delete-conjurable-ids" -> GLOBAL_SIMPL_SW(SimplMayDeleteConjurableIds)
	  "-fessential-unfoldings-only" -> GLOBAL_SIMPL_SW(EssentialUnfoldingsOnly) 
	  "-fignore-inline-pragma"  -> GLOBAL_SIMPL_SW(IgnoreINLINEPragma)
	  "-fno-let-from-case"  -> GLOBAL_SIMPL_SW(SimplNoLetFromCase)
	  "-fno-let-from-app"  -> GLOBAL_SIMPL_SW(SimplNoLetFromApp)
	  "-fno-let-from-strict-let"  -> GLOBAL_SIMPL_SW(SimplNoLetFromStrictLet)

	  _ | starts_with_msi  -> GLOBAL_SIMPL_SW(MaxSimplifierIterations (read after_msi))
	    | starts_with_suut  -> GLOBAL_SIMPL_SW(SimplUnfoldingUseThreshold (read after_suut))
	    | starts_with_suct  -> GLOBAL_SIMPL_SW(SimplUnfoldingCreationThreshold (read after_suct))

	  _ -> writeMn stderr ("*** WARNING: bad simplifier option: "++opt1++"\n") `thenMn` ( \ _ ->
		-- NB: the driver is really supposed to handle bad options
	       simpl_sep opts simpl_sw glob_sw core_td stg_td )
\end{code}

%************************************************************************
%*									*
\subsection[CmdLineOpts-order]{Switch ordering}
%*									*
%************************************************************************

In spite of the @Produce*@ and @SccGroup@ constructors, these things
behave just like enumeration types.

\begin{code}
instance Eq GlobalSwitch where
    a == b = tagOf_Switch a _EQ_ tagOf_Switch b

instance Ord GlobalSwitch where
    a <  b  = tagOf_Switch a _LT_ tagOf_Switch b
    a <= b  = tagOf_Switch a _LE_ tagOf_Switch b

instance Eq SimplifierSwitch where
    a == b = tagOf_SimplSwitch a _EQ_ tagOf_SimplSwitch b

instance Ord SimplifierSwitch where
    a <  b  = tagOf_SimplSwitch a _LT_ tagOf_SimplSwitch b
    a <= b  = tagOf_SimplSwitch a _LE_ tagOf_SimplSwitch b

tagOf_Switch (ProduceC _)		=(ILIT(0) :: FAST_INT)
tagOf_Switch (ProduceS _)		= ILIT(1)
tagOf_Switch (ProduceHi	_)		= ILIT(2)
tagOf_Switch (AsmTarget _)              = ILIT(4)
tagOf_Switch ForConcurrent		= ILIT(6)
tagOf_Switch Haskell_1_3		= ILIT(8)
tagOf_Switch GlasgowExts		= ILIT(9)
tagOf_Switch CompilingPrelude		= ILIT(10)
tagOf_Switch HideBuiltinNames		= ILIT(11)
tagOf_Switch HideMostBuiltinNames	= ILIT(12)
tagOf_Switch (EnsureSplittableC _)	= ILIT(13)
tagOf_Switch Verbose			= ILIT(14)
tagOf_Switch PprStyle_User		= ILIT(15)
tagOf_Switch PprStyle_Debug		= ILIT(16)
tagOf_Switch PprStyle_All		= ILIT(17)
tagOf_Switch DoCoreLinting		= ILIT(18)
tagOf_Switch EmitArityChecks		= ILIT(19)
tagOf_Switch OmitInterfacePragmas	= ILIT(20)
tagOf_Switch OmitDerivedRead		= ILIT(21)
tagOf_Switch OmitReexportedInstances	= ILIT(22)
tagOf_Switch (UnfoldingUseThreshold _)  = ILIT(23)
tagOf_Switch (UnfoldingCreationThreshold _) = ILIT(24)
tagOf_Switch (UnfoldingOverrideThreshold _) = ILIT(25)
tagOf_Switch ReportWhyUnfoldingsDisallowed = ILIT(26)
tagOf_Switch UseGetMentionedVars	= ILIT(27)
tagOf_Switch ShowPragmaNameErrs		= ILIT(28)
tagOf_Switch NameShadowingNotOK		= ILIT(29)
tagOf_Switch SigsRequired		= ILIT(30)
tagOf_Switch SccProfilingOn		= ILIT(31)
tagOf_Switch AutoSccsOnExportedToplevs	= ILIT(32)
tagOf_Switch AutoSccsOnAllToplevs	= ILIT(33)
tagOf_Switch AutoSccsOnIndividualCafs	= ILIT(34)
tagOf_Switch (SccGroup _)		= ILIT(36)
tagOf_Switch DoTickyProfiling		= ILIT(37)
tagOf_Switch DoSemiTagging		= ILIT(38)
tagOf_Switch FoldrBuildOn		= ILIT(39)
tagOf_Switch FoldrBuildTrace		= ILIT(40)
tagOf_Switch SpecialiseImports		= ILIT(41)
tagOf_Switch ShowImportSpecs		= ILIT(42)
tagOf_Switch OmitDefaultInstanceMethods	= ILIT(43)
tagOf_Switch SpecialiseOverloaded	= ILIT(44)
tagOf_Switch SpecialiseUnboxed		= ILIT(45)
tagOf_Switch SpecialiseAll		= ILIT(46)
tagOf_Switch SpecialiseTrace		= ILIT(47)

tagOf_Switch OmitBlackHoling		= ILIT(49)
tagOf_Switch StgDoLetNoEscapes		= ILIT(50)
tagOf_Switch IgnoreStrictnessPragmas	= ILIT(51)
tagOf_Switch IrrefutableTuples		= ILIT(52)
tagOf_Switch IrrefutableEverything	= ILIT(53)
tagOf_Switch AllStrict			= ILIT(54)
tagOf_Switch NumbersStrict		= ILIT(55)
tagOf_Switch AllDemanded		= ILIT(56)

tagOf_Switch (ReturnInRegsThreshold _)	= ILIT(57)
tagOf_Switch (VectoredReturnThreshold _)= ILIT(58)
tagOf_Switch D_dump_rif2hs		= ILIT(59)
tagOf_Switch D_dump_rn4			= ILIT(60)
tagOf_Switch D_dump_tc			= ILIT(61)
tagOf_Switch D_dump_deriv		= ILIT(62)
tagOf_Switch D_dump_ds			= ILIT(63)
tagOf_Switch D_dump_simpl		= ILIT(64)
tagOf_Switch D_dump_spec		= ILIT(65)
tagOf_Switch D_dump_occur_anal		= ILIT(66)
tagOf_Switch D_dump_stranal		= ILIT(67)
tagOf_Switch D_dump_stg			= ILIT(68)
tagOf_Switch D_dump_absC		= ILIT(69)
tagOf_Switch D_dump_flatC		= ILIT(70)
tagOf_Switch D_dump_realC		= ILIT(71)
tagOf_Switch D_dump_asm			= ILIT(72)
tagOf_Switch D_show_passes		= ILIT(73)
--ANDY:tagOf_Switch D_dump_core_passes_info	= ILIT(??)
tagOf_Switch D_verbose_core2core	= ILIT(74)
tagOf_Switch D_verbose_stg2stg		= ILIT(75)
tagOf_Switch D_simplifier_stats		= ILIT(76)
tagOf_Switch D_source_stats		= ILIT(77) {-see note below!-}

#ifndef DPH
tagOf_Switch _ = case (panic "tagOf_Switch") of -- BUG avoidance
		   s -> tagOf_Switch s

lAST_SWITCH_TAG = IBOX(tagOf_Switch D_source_stats)

#else {- Data Parallel Haskell -}

tagOf_Switch PodizeIntelligent		= ILIT(90)
tagOf_Switch PodizeAggresive		= ILIT(91)
tagOf_Switch PodizeVeryAggresive	= ILIT(92)
tagOf_Switch PodizeExtremelyAggresive	= ILIT(93)
tagOf_Switch D_dump_pod			= ILIT(94)
tagOf_Switch D_dump_psimpl		= ILIT(95)
tagOf_Switch D_dump_nextC		= ILIT(96)

tagOf_Switch _ = case (panic "tagOf_Switch") of -- BUG avoidance
		   s -> tagOf_Switch s

lAST_SWITCH_TAG = IBOX(tagOf_Switch D_dump_nextC)

#endif {- Data Parallel Haskell -}
\end{code}

(Note For Will): Could you please leave a little extra room between
your last option and @D_dump_spec@... Thanks... jon...

\begin{code}
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
tagOf_SimplSwitch SimplDoNewOccurAnal		= ILIT(13)
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
-- If you add anything here, be sure to change lAST_SIMPL_SWITCH_TAG, too!

{-
tagOf_SimplSwitch Extra__SimplFlag1		= ILIT(26)
tagOf_SimplSwitch Extra__SimplFlag2		= ILIT(27)
tagOf_SimplSwitch Extra__SimplFlag3		= ILIT(28)
tagOf_SimplSwitch Extra__SimplFlag4		= ILIT(29)
tagOf_SimplSwitch Extra__SimplFlag5		= ILIT(30)
tagOf_SimplSwitch Extra__SimplFlag6		= ILIT(31)
tagOf_SimplSwitch Extra__SimplFlag8		= ILIT(32)
-}

tagOf_SimplSwitch _ = case (panic "tagOf_SimplSwitch") of -- BUG avoidance
			s -> tagOf_SimplSwitch s

lAST_SIMPL_SWITCH_TAG = IBOX(tagOf_SimplSwitch SimplNoLetFromStrictLet)
\end{code}

%************************************************************************
%*									*
\subsection[CmdLineOpts-lookup]{Switch lookup}
%*									*
%************************************************************************

\begin{code}
isAmong	     :: [GlobalSwitch]     -> GlobalSwitch     -> SwitchResult
isAmongSimpl :: [SimplifierSwitch] -> SimplifierSwitch -> SwitchResult

isAmong on_switches
  = let
	tidied_on_switches = foldl rm_dups [] on_switches

	sw_tbl :: Array Int SwitchResult

	sw_tbl = (array	(0, lAST_SWITCH_TAG) -- bounds...
			all_undefined)
		 // defined_elems

	all_undefined = [ i := SwBool False | i <- [0 .. lAST_SWITCH_TAG ] ]

	defined_elems = map mk_assoc_elem tidied_on_switches
    in
#ifndef __GLASGOW_HASKELL__
    \ switch -> sw_tbl ! IBOX((tagOf_Switch switch))	-- but this is fast!
#else
    -- and this is faster!
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    case sw_tbl of { _Array bounds_who_needs_'em stuff ->
    \ switch ->
	case (indexArray# stuff (tagOf_Switch switch)) of
	  _Lift v -> v
    }
#endif
  where
    mk_assoc_elem k@(ProduceC  str) = IBOX(tagOf_Switch k) := SwString str
    mk_assoc_elem k@(ProduceS  str) = IBOX(tagOf_Switch k) := SwString str
    mk_assoc_elem k@(ProduceHi str) = IBOX(tagOf_Switch k) := SwString str
--UNUSED:    mk_assoc_elem k@(ProduceHu str) = IBOX(tagOf_Switch k) := SwString str
    mk_assoc_elem k@(SccGroup  str) = IBOX(tagOf_Switch k) := SwString str
    mk_assoc_elem k@(AsmTarget str) = IBOX(tagOf_Switch k) := SwString str
    mk_assoc_elem k@(EnsureSplittableC str) = IBOX(tagOf_Switch k) := SwString str

    mk_assoc_elem k@(UnfoldingUseThreshold      lvl) = IBOX(tagOf_Switch k) := SwInt lvl
    mk_assoc_elem k@(UnfoldingCreationThreshold lvl) = IBOX(tagOf_Switch k) := SwInt lvl
    mk_assoc_elem k@(UnfoldingOverrideThreshold lvl) = IBOX(tagOf_Switch k) := SwInt lvl

    mk_assoc_elem k@(ReturnInRegsThreshold lvl) = IBOX(tagOf_Switch k) := SwInt lvl

    mk_assoc_elem k = IBOX(tagOf_Switch k) := SwBool True -- I'm here, Mom!

    -- cannot have duplicates if we are going to use the array thing

    rm_dups switches_so_far switch
      = if switch `is_elem` switches_so_far
    	then switches_so_far
	else switch : switches_so_far
      where
	sw `is_elem` []     = False
	sw `is_elem` (s:ss) = (tagOf_Switch sw) _EQ_ (tagOf_Switch s)
			    || sw `is_elem` ss
\end{code}

Same thing for @SimplifierSwitches@; for efficiency reasons, we
probably do {\em not} want something overloaded.
 \begin{code}
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
#ifndef __GLASGOW_HASKELL__
    \ switch -> sw_tbl ! IBOX((tagOf_SimplSwitch switch)) -- but this is fast!
#else
    -- and this is faster!
    -- (avoid some unboxing, bounds checking, and other horrible things:)
    case sw_tbl of { _Array bounds_who_needs_'em stuff ->
    \ switch ->
	case (indexArray# stuff (tagOf_SimplSwitch switch)) of
	  _Lift v -> v
    }
#endif
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
\subsection[CmdLineOpts-misc]{Misc functions for command-line options}
%*									*
%************************************************************************


\begin{code}
switchIsOn :: (switch -> SwitchResult) -> switch -> Bool

switchIsOn lookup_fn switch
  = case (lookup_fn switch) of
      SwBool False -> False
      _	    	   -> True

stringSwitchSet :: (switch -> SwitchResult)
		-> (String -> switch)
		-> Maybe String

stringSwitchSet lookup_fn switch
  = case (lookup_fn (switch (panic "stringSwitchSet"))) of
      SwString str -> Just str
      _	    	   -> Nothing

intSwitchSet :: (switch -> SwitchResult)
	     -> (Int -> switch)
	     -> Maybe Int

intSwitchSet lookup_fn switch
  = -- pprTrace "intSwitchSet:" (ppInt (IBOX (tagOf_Switch (switch (panic "xxx"))))) $
    case (lookup_fn (switch (panic "intSwitchSet"))) of
      SwInt int -> Just int
      _	    	-> Nothing
\end{code}
