{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[GHC.Driver.Config.Core.Opt]{Configuration of the driver for optimizing @Core@ programs}
-}

{-# LANGUAGE CPP #-}

module GHC.Driver.Config.Core.Opt
  ( getCoreToDo

  , initArityOpts
  , initCallerCCOpts
  , initDmdAnalOpts
  , initLiberateCaseOpts
  , initRuleCheckOpts
  , initSimplifyExprOpts
  , initSimplifyOpts
  , initSimplMode
  , initGentleSimplMode
  , initSpecConstrOpts
  , initSpecialiseOpts
  , initWorkWrapOpts
  ) where

import GHC.Prelude

import GHC.Driver.Config ( initOptCoercionOpts, initSimpleOpts )
import GHC.Driver.Config.Core.Lint ( defaultLintFlags, initLintPassResultConfig )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Driver.Config.Diagnostic ( initDiagOpts )
import GHC.Driver.Session
import GHC.Platform.Ways  ( hasWay, Way(WayProf) )

import GHC.Core.Opt.Config ( CoreToDo(..) )
import GHC.Core.Opt.FloatOutSwitches ( FloatOutSwitches(..) )
import GHC.Core.Opt.Arity ( ArityOpts(..) )
import GHC.Core.Opt.CallerCC ( CallerCCOpts(..) )
import GHC.Core.Opt.DmdAnal ( DmdAnalOpts(..) )
import GHC.Core.Opt.LiberateCase ( LibCaseOpts(..) )
import GHC.Core.Opt.RuleCheck ( RuleCheckOpts(..) )
import GHC.Core.Opt.Simplify ( SimplifyExprOpts(..), SimplifyOpts(..) )
import GHC.Core.Opt.Simplify.Env ( FloatEnable(..), SimplMode(..) )
import GHC.Core.Opt.Simplify.Monad ( TopEnvConfig(..) )
import GHC.Core.Opt.SpecConstr ( SpecConstrOpts (..) )
import GHC.Core.Opt.Specialise ( SpecialiseOpts (..) )
import GHC.Core.Opt.WorkWrap ( WwOpts(..) )

import GHC.Runtime.Context ( InteractiveContext(..) )

import GHC.Types.Basic
import GHC.Types.Var ( Var )

import GHC.Utils.Error ( mkMCDiagnostic )
import GHC.Utils.Outputable ( defaultUserStyle, text )

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.Trans.Writer.Strict ( Writer, execWriter, tell )
import Data.Foldable

{-
************************************************************************
*                                                                      *
           Generating the main optimisation pipeline
*                                                                      *
************************************************************************
-}

-- | Construct the main optimisation pipeline from the driver's session state.
-- See Note [The architecture of the Core optimizer].
getCoreToDo :: DynFlags -> [Var] -> [CoreToDo]
getCoreToDo dflags extra_vars = execWriter $ do
  -- We want to do the static argument transform before full laziness as it
  -- may expose extra opportunities to float things outwards. However, to fix
  -- up the output of the transformation we need at do at least one simplify
  -- after this before anything else
  when static_args $ do
    simpl_gently
    enqueue CoreDoStaticArgs

  -- initial simplify: make specialiser happy: minimum effort please
  when do_presimplify $
    simpl_gently

  -- Specialisation is best done before full laziness
  -- so that overloaded functions have all their dictionary lambdas manifest
  when do_specialise $
    enqueue $ coreDoSpecialising dflags

  if full_laziness then
    -- Was: gentleFloatOutSwitches
    --
    -- I have no idea why, but not floating constants to
    -- top level is very bad in some cases.
    --
    -- Notably: p_ident in spectral/rewrite
    --          Changing from "gentle" to "constantsOnly"
    --          improved rewrite's allocation by 19%, and
    --          made 0.0% difference to any other nofib
    --          benchmark
    --
    -- Not doing floatOutOverSatApps yet, we'll do
    -- that later on when we've had a chance to get more
    -- accurate arity information.  In fact it makes no
    -- difference at all to performance if we do it here,
    -- but maybe we save some unnecessary to-and-fro in
    -- the simplifier.
    enqueue $ CoreDoFloatOutwards FloatOutSwitches
      { floatOutLambdas   = Just 0
      , floatOutConstants = True
      , floatOutOverSatApps = False
      , floatToTopLevelOnly = False
      }

  else
    -- Even with full laziness turned off, we still need to float static
    -- forms to the top level. See Note [Grand plan for static forms] in
    -- GHC.Iface.Tidy.StaticPtrTable.
    --
    when static_ptrs $ do
      -- Float Out can't handle type lets (sometimes created
      -- by simpleOptPgm via mkParallelBindings)
      simpl_gently
      -- Static forms are moved to the top level with the FloatOut pass.
      -- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
      enqueue $ CoreDoFloatOutwards FloatOutSwitches
        { floatOutLambdas   = Just 0
        , floatOutConstants = True
        , floatOutOverSatApps = False
        , floatToTopLevelOnly = True
        }

  -- Run the simplier phases 2,1,0 to allow rewrite rules to fire
  when do_simpl3 $ do
    for_ [phases, phases-1 .. 1] $ \phase ->
      simpl_phase (Phase phase) "main" max_iter

    -- Phase 0: allow all Ids to be inlined now
    -- This gets foldr inlined before strictness analysis

    -- At least 3 iterations because otherwise we land up with
    -- huge dead expressions because of an infelicity in the
    -- simplifier.
    --      let k = BIG in foldr k z xs
    -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
    -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
    -- Don't stop now!
    simpl_phase (Phase 0) "main" (max max_iter 3)

  -- Run float-inwards immediately before the strictness analyser
  -- Doing so pushes bindings nearer their use site and hence makes
  -- them more likely to be strict. These bindings might only show
  -- up after the inlining from simplification.  Example in fulsom,
  -- Csg.calc, where an arg of timesDouble thereby becomes strict.
  when do_float_in $
    enqueue $ CoreDoFloatInwards platform

  when call_arity $ do
    enqueue CoreDoCallArity
    simplify "post-call-arity"

  -- Strictness analysis
  when strictness $ do
    dmd_cpr_ww
    simplify "post-worker-wrapper"

  -- See Note [Placement of the exitification pass]
  when exitification $
    enqueue CoreDoExitify

  when full_laziness $
    enqueue $ CoreDoFloatOutwards FloatOutSwitches
      { floatOutLambdas     = floatLamArgs dflags
      , floatOutConstants   = True
      , floatOutOverSatApps = True
      , floatToTopLevelOnly = False
      }
      -- nofib/spectral/hartel/wang doubles in speed if you
      -- do full laziness late in the day.  It only happens
      -- after fusion and other stuff, so the early pass doesn't
      -- catch it.  For the record, the redex is
      --        f_el22 (f_el21 r_midblock)

  -- We want CSE to follow the final full-laziness pass, because it may
  -- succeed in commoning up things floated out by full laziness.
  -- CSE used to rely on the no-shadowing invariant, but it doesn't any more
  when cse $
    enqueue CoreCSE

  when do_float_in $
    enqueue $ CoreDoFloatInwards platform

  -- Final tidy-up
  simplify "final"

  maybe_rule_check FinalPhase

  --------  After this we have -O2 passes -----------------
  -- None of them run with -O

  -- Case-liberation for -O2.  This should be after
  -- strictness analysis and the simplification which follows it.
  when liberate_case $ do
    enqueue $ CoreLiberateCase (initLiberateCaseOpts dflags)
    -- Run the simplifier after LiberateCase to vastly
    -- reduce the possibility of shadowing
    -- Reason: see Note [Shadowing] in GHC.Core.Opt.SpecConstr
    simplify "post-liberate-case"

  when spec_constr $ do
    enqueue $ CoreDoSpecConstr (initSpecConstrOpts dflags)
    -- See Note [Simplify after SpecConstr]
    simplify "post-spec-constr"

  maybe_rule_check FinalPhase

  when late_specialise $ do
    enqueue $ coreDoSpecialising dflags
    simplify "post-late-spec"

  -- LiberateCase can yield new CSE opportunities because it peels
  -- off one layer of a recursive function (concretely, I saw this
  -- in wheel-sieve1), and I'm guessing that SpecConstr can too
  -- And CSE is a very cheap pass. So it seems worth doing here.
  when ((liberate_case || spec_constr) && cse) $ do
    enqueue CoreCSE
    simplify "post-final-cse"

  ---------  End of -O2 passes --------------

  when late_dmd_anal $ do
    dmd_cpr_ww
    simplify "post-late-ww"

  -- Final run of the demand_analyser, ensures that one-shot thunks are
  -- really really one-shot thunks. Only needed if the demand analyser
  -- has run at all. See Note [Final Demand Analyser run] in GHC.Core.Opt.DmdAnal
  -- It is EXTREMELY IMPORTANT to run this pass, otherwise execution
  -- can become /exponentially/ more expensive. See #11731, #12996.
  when (strictness || late_dmd_anal) $
    enqueue $ coreDoDemand dflags

  maybe_rule_check FinalPhase

  when profiling $ do
    when (not (null $ callerCcFilters dflags)) $
      enqueue $ CoreAddCallerCcs (initCallerCCOpts dflags)
    when (gopt Opt_ProfLateInlineCcs dflags) $
      enqueue $ CoreAddLateCcs (gopt Opt_ProfCountEntries dflags)
  where
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
    platform      = targetPlatform     dflags
    rule_check    = ruleCheck          dflags
    const_fold    = gopt Opt_CoreConstantFolding          dflags
    call_arity    = gopt Opt_CallArity                    dflags
    exitification = gopt Opt_Exitification                dflags
    strictness    = gopt Opt_Strictness                   dflags
    full_laziness = gopt Opt_FullLaziness                 dflags
    do_specialise = gopt Opt_Specialise                   dflags
    do_float_in   = gopt Opt_FloatIn                      dflags
    cse           = gopt Opt_CSE                          dflags
    spec_constr   = gopt Opt_SpecConstr                   dflags
    liberate_case = gopt Opt_LiberateCase                 dflags
    late_dmd_anal = gopt Opt_LateDmdAnal                  dflags
    late_specialise = gopt Opt_LateSpecialise             dflags
    static_args   = gopt Opt_StaticArgumentTransformation dflags
    rules_on      = gopt Opt_EnableRewriteRules           dflags
    ww_on         = gopt Opt_WorkerWrapper                dflags
    static_ptrs   = xopt LangExt.StaticPointers           dflags
    profiling     = ways dflags `hasWay` WayProf

    do_presimplify = do_specialise -- TODO: any other optimizations benefit from pre-simplification?
    do_simpl3      = const_fold || rules_on -- TODO: any other optimizations benefit from three-phase simplification?

    maybe_rule_check phase = for_ rule_check $
      enqueue . CoreDoRuleCheck . initRuleCheckOpts dflags phase

    maybe_strictness_before (Phase phase)
      | phase `elem` strictnessBefore dflags = enqueue $ coreDoDemand dflags
    maybe_strictness_before _ = return ()

    simpl_phase phase name iter = do
      maybe_strictness_before phase
      enqueue $ CoreDoSimplify $ initSimplifyOpts dflags extra_vars iter
                                 (initSimplMode dflags phase name)
      maybe_rule_check phase

    -- Run GHC's internal simplification phase, after all rules have run.
    -- See Note [Compiler phases] in GHC.Types.Basic
    simplify name = simpl_phase FinalPhase name max_iter

    -- initial simplify: make specialiser happy: minimum effort please
    -- See Note [Inline in InitialPhase]
    -- See Note [RULEs enabled in InitialPhase]
    simpl_gently = enqueue $ CoreDoSimplify $
      initSimplifyOpts dflags extra_vars max_iter (initGentleSimplMode dflags)

    dmd_cpr_ww = do
      enqueue $ coreDoDemand dflags
      enqueue CoreDoCpr
      when ww_on $
        enqueue $ CoreDoWorkerWrapper (initWorkWrapOpts dflags)



enqueue :: CoreToDo -> Writer [CoreToDo] ()
enqueue pass = tell [pass]

coreDoDemand :: DynFlags -> CoreToDo
coreDoDemand dflags = CoreDoDemand $ initDmdAnalOpts dflags

coreDoSpecialising :: DynFlags -> CoreToDo
coreDoSpecialising dflags = CoreDoSpecialising (initSpecialiseOpts dflags simplMask)

-- TODO: Deduplication
simplMask :: Char
simplMask = 's'

{- Note [Inline in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 8 and earlier we did not inline anything in the InitialPhase. But that is
confusing for users because when they say INLINE they expect the function to inline
right away.

So now we do inlining immediately, even in the InitialPhase, assuming that the
Id's Activation allows it.

This is a surprisingly big deal. Compiler performance improved a lot
when I made this change:

   perf/compiler/T5837.run            T5837 [stat too good] (normal)
   perf/compiler/parsing001.run       parsing001 [stat too good] (normal)
   perf/compiler/T12234.run           T12234 [stat too good] (optasm)
   perf/compiler/T9020.run            T9020 [stat too good] (optasm)
   perf/compiler/T3064.run            T3064 [stat too good] (normal)
   perf/compiler/T9961.run            T9961 [stat too good] (normal)
   perf/compiler/T13056.run           T13056 [stat too good] (optasm)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)
   perf/compiler/T783.run             T783 [stat too good] (normal)
   perf/compiler/T12227.run           T12227 [stat too good] (normal)
   perf/should_run/lazy-bs-alloc.run  lazy-bs-alloc [stat too good] (normal)
   perf/compiler/T1969.run            T1969 [stat too good] (normal)
   perf/compiler/T9872a.run           T9872a [stat too good] (normal)
   perf/compiler/T9872c.run           T9872c [stat too good] (normal)
   perf/compiler/T9872b.run           T9872b [stat too good] (normal)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)

Note [RULEs enabled in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RULES are enabled when doing "gentle" simplification in InitialPhase,
or with -O0.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.

Note [Simplify after SpecConstr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to run the simplifier after SpecConstr, and before late-Specialise,
for two reasons, both shown up in test perf/compiler/T16473,
with -O2 -flate-specialise

1.  I found that running late-Specialise after SpecConstr, with no
    simplification in between meant that the carefullly constructed
    SpecConstr rule never got to fire.  (It was something like
          lvl = f a   -- Arity 1
          ....g lvl....
    SpecConstr specialised g for argument lvl; but Specialise then
    specialised lvl = f a to lvl = $sf, and inlined. Or something like
    that.)

2.  Specialise relies on unfoldings being available for top-level dictionary
    bindings; but SpecConstr kills them all!  The Simplifer restores them.

This extra run of the simplifier has a cost, but this is only with -O2.

-}

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the Arity pass
*                                                                      *
************************************************************************
-}

initArityOpts :: DynFlags -> ArityOpts
initArityOpts dflags = ArityOpts
  { ao_ped_bot = gopt Opt_PedanticBottoms dflags
  , ao_dicts_cheap = gopt Opt_DictsCheap dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the CallerCC pass
*                                                                      *
************************************************************************
-}

initCallerCCOpts :: DynFlags -> CallerCCOpts
initCallerCCOpts dflags = CallerCCOpts
  { cc_countEntries = gopt Opt_ProfCountEntries dflags
  , cc_filters = callerCcFilters dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the DmdAnal pass
*                                                                      *
************************************************************************
-}

initDmdAnalOpts :: DynFlags -> DmdAnalOpts
initDmdAnalOpts dflags = DmdAnalOpts
  { dmd_strict_dicts    = gopt Opt_DictsStrict dflags
  , dmd_unbox_width     = dmdUnboxWidth dflags
  , dmd_max_worker_args = maxWorkerArgs dflags
  , dmd_ppr_debug       = hasPprDebug dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the LiberateCase pass
*                                                                      *
************************************************************************
-}

-- | Initialize configuration for the liberate case Core optomization
-- pass.
initLiberateCaseOpts :: DynFlags -> LibCaseOpts
initLiberateCaseOpts dflags = LibCaseOpts
  { lco_threshold = liberateCaseThreshold dflags
  , lco_unfolding_opts = unfoldingOpts dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the RuleCheck pass
*                                                                      *
************************************************************************
-}

initRuleCheckOpts :: DynFlags -> CompilerPhase -> String -> RuleCheckOpts
initRuleCheckOpts dflags phase pat = RuleCheckOpts
  { rc_phase = phase
  , rc_pattern = pat
  , rc_rule_opts = initRuleOpts dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the Simplify pass
*                                                                      *
************************************************************************
-}

initSimplifyExprOpts :: DynFlags -> InteractiveContext -> SimplifyExprOpts
initSimplifyExprOpts dflags ic = SimplifyExprOpts
  { se_fam_inst = snd $ ic_instances ic
  , se_mode = (initSimplMode dflags InitialPhase "GHCi")
    { sm_inline = False
      -- Do not do any inlining, in case we expose some
      -- unboxed tuple stuff that confuses the bytecode
      -- interpreter
    }
  , se_top_env_cfg = TopEnvConfig
    { te_history_size = historySize dflags
    , te_tick_factor = simplTickFactor dflags
    }
  }

initSimplifyOpts :: DynFlags -> [Var] -> Int -> SimplMode -> SimplifyOpts
initSimplifyOpts dflags extra_vars iterations mode = SimplifyOpts
  { so_dump_core_sizes = not $ gopt Opt_SuppressCoreSizes dflags
  , so_iterations = iterations
  , so_mode = mode
  , so_pass_result_cfg = if gopt Opt_DoCoreLinting dflags
    then Just $ initLintPassResultConfig dflags extra_vars
      (defaultLintFlags dflags)
      (text "Simplifier")
      -- Disable Lint warnings on the first simplifier pass, because
      -- there may be some INLINE knots still tied, which is tiresomely noisy
      (sm_phase mode /= InitialPhase)
    else Nothing
  , so_top_env_cfg = TopEnvConfig
      { te_history_size = historySize dflags
      , te_tick_factor = simplTickFactor dflags
      }
  }

initSimplMode :: DynFlags -> CompilerPhase -> String -> SimplMode
initSimplMode dflags phase name = SimplMode
  { sm_names = [name]
  , sm_phase = phase
  , sm_rules = gopt Opt_EnableRewriteRules dflags
  , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dflags
  , sm_cast_swizzle = True
  , sm_inline = True
  , sm_uf_opts = unfoldingOpts dflags
  , sm_case_case = True
  , sm_pre_inline = gopt Opt_SimplPreInlining dflags
  , sm_float_enable = floatEnable dflags
  , sm_do_eta_reduction = gopt Opt_DoEtaReduction dflags
  , sm_arity_opts = initArityOpts dflags
  , sm_rule_opts = initRuleOpts dflags
  , sm_case_folding = gopt Opt_CaseFolding dflags
  , sm_case_merge = gopt Opt_CaseMerge dflags
  , sm_co_opt_opts = initOptCoercionOpts dflags
  }

initGentleSimplMode :: DynFlags -> SimplMode
initGentleSimplMode dflags = (initSimplMode dflags InitialPhase "Gentle")
  { -- Don't do case-of-case transformations.
    -- This makes full laziness work better
    sm_case_case = False
  }

floatEnable :: DynFlags -> FloatEnable
floatEnable dflags =
  case (gopt Opt_LocalFloatOut dflags, gopt Opt_LocalFloatOutTopLevel dflags) of
    (True, True) -> FloatEnabled
    (True, False)-> FloatNestedOnly
    (False, _)   -> FloatDisabled

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the SpecConstr pass
*                                                                      *
************************************************************************
-}

initSpecConstrOpts :: DynFlags -> SpecConstrOpts
initSpecConstrOpts dflags = SpecConstrOpts
  { sc_max_args    = maxWorkerArgs dflags
  , sc_debug       = hasPprDebug dflags
  , sc_uf_opts     = unfoldingOpts dflags
  , sc_size        = specConstrThreshold dflags
  , sc_count       = specConstrCount     dflags
  , sc_recursive   = specConstrRecursive dflags
  , sc_keen        = gopt Opt_SpecConstrKeen dflags
  }

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the Specialise pass
*                                                                      *
************************************************************************
-}

initSpecialiseOpts :: DynFlags -> Char -> SpecialiseOpts
initSpecialiseOpts dflags mask = SpecialiseOpts
  { so_uniq_mask = mask
  , so_cross_module_specialise = gopt Opt_CrossModuleSpecialise dflags
  , so_specialise_aggressively = gopt Opt_SpecialiseAggressively dflags
  , so_warn_missed_specs = warn_missed_specs
  , so_warn_all_missed_specs = warn_all_missed_specs
  , so_sdoc_context = initSDocContext dflags defaultUserStyle
  , so_simpl_opts = initSimpleOpts dflags
  , so_rule_opts = initRuleOpts dflags
  }
  where
    diag_opts = initDiagOpts dflags
    warn_missed_specs
      | wopt Opt_WarnMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnMissedSpecs)
      | otherwise = Nothing
    warn_all_missed_specs
      | wopt Opt_WarnAllMissedSpecs dflags = Just $ mkMCDiagnostic diag_opts (WarningWithFlag Opt_WarnAllMissedSpecs)
      | otherwise = Nothing

{-
************************************************************************
*                                                                      *
           Initialization of the configuration of the WorkWrap pass
*                                                                      *
************************************************************************
-}

initWorkWrapOpts :: DynFlags -> WwOpts
initWorkWrapOpts dflags = MkWwOpts
  { wo_simple_opts       = initSimpleOpts dflags
  , wo_cpr_anal          = gopt Opt_CprAnal dflags
  , wo_unlift_strict     = gopt Opt_WorkerWrapperUnlift dflags
  }
