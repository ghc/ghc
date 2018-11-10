{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}

module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import GhcPrelude

import DynFlags
import CoreSyn
import HscTypes
import CSE              ( cseProgram )
import Rules            ( mkRuleBase, unionRuleBase,
                          extendRuleBaseList, ruleCheckProgram, addRuleInfo,
                          getRules )
import PprCore          ( pprCoreBindings, pprCoreExpr )
import OccurAnal        ( occurAnalysePgm, occurAnalyseExpr )
import IdInfo
import CoreStats        ( coreBindsSize, coreBindsStats, exprSize )
import CoreUtils        ( mkTicks, stripTicksTop )
import CoreLint         ( endPass, lintPassResult, dumpPassResult,
                          lintAnnots )
import Simplify         ( simplTopBinds, simplExpr, simplRules )
import SimplUtils       ( simplEnvForGHCi, activeRule, activeUnfolding )
import SimplEnv
import SimplMonad
import CoreMonad
import qualified ErrUtils as Err
import FloatIn          ( floatInwards )
import FloatOut         ( floatOutwards )
import FamInstEnv
import Id
import ErrUtils         ( withTiming )
import BasicTypes       ( CompilerPhase(..), isDefaultInlinePragma, defaultInlinePragma )
import VarSet
import VarEnv
import LiberateCase     ( liberateCase )
import SAT              ( doStaticArgs )
import Specialise       ( specProgram)
import SpecConstr       ( specConstrProgram)
import DmdAnal          ( dmdAnalProgram )
import CallArity        ( callArityAnalProgram )
import Exitify          ( exitifyProgram )
import WorkWrap         ( wwTopBinds )
import SrcLoc
import Util
import Module
import Plugins          ( withPlugins, installCoreToDos )
import DynamicLoading  -- ( initializePlugins )

import UniqSupply       ( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import UniqFM
import Outputable
import Control.Monad
import qualified GHC.LanguageExtensions as LangExt
{-
************************************************************************
*                                                                      *
\subsection{The driver for the simplifier}
*                                                                      *
************************************************************************
-}

core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core hsc_env guts@(ModGuts { mg_module  = mod
                                , mg_loc     = loc
                                , mg_deps    = deps
                                , mg_rdr_env = rdr_env })
  = do { us <- mkSplitUniqSupply 's'
       -- make sure all plugins are loaded

       ; let builtin_passes = getCoreToDo dflags
             orph_mods = mkModuleSet (mod : dep_orphs deps)
       ;
       ; (guts2, stats) <- runCoreM hsc_env hpt_rule_base us mod
                                    orph_mods print_unqual loc $
                           do { hsc_env' <- getHscEnv
                              ; dflags' <- liftIO $ initializePlugins hsc_env'
                                                      (hsc_dflags hsc_env')
                              ; all_passes <- withPlugins dflags'
                                                installCoreToDos
                                                builtin_passes
                              ; runCorePasses all_passes guts }

       ; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags         = hsc_dflags hsc_env
    home_pkg_rules = hptRules hsc_env (dep_mods deps)
    hpt_rule_base  = mkRuleBase home_pkg_rules
    print_unqual   = mkPrintUnqualified dflags rdr_env
    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.

{-
************************************************************************
*                                                                      *
           Generating the main optimisation pipeline
*                                                                      *
************************************************************************
-}

getCoreToDo :: DynFlags -> [CoreToDo]
getCoreToDo dflags
  = flatten_todos core_todo
  where
    opt_level     = optLevel           dflags
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
    rule_check    = ruleCheck          dflags
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
    eta_expand_on = gopt Opt_DoLambdaEtaExpansion         dflags
    ww_on         = gopt Opt_WorkerWrapper                dflags
    static_ptrs   = xopt LangExt.StaticPointers           dflags

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    maybe_strictness_before phase
      = runWhen (phase `elem` strictnessBefore dflags) CoreDoStrictness

    base_mode = SimplMode { sm_phase      = panic "base_mode"
                          , sm_names      = []
                          , sm_dflags     = dflags
                          , sm_rules      = rules_on
                          , sm_eta_expand = eta_expand_on
                          , sm_inline     = True
                          , sm_case_case  = True }

    simpl_phase phase names iter
      = CoreDoPasses
      $   [ maybe_strictness_before phase
          , CoreDoSimplify iter
                (base_mode { sm_phase = Phase phase
                           , sm_names = names })

          , maybe_rule_check (Phase phase) ]

    simpl_phases = CoreDoPasses [ simpl_phase phase ["main"] max_iter
                                | phase <- [phases, phases-1 .. 1] ]


        -- initial simplify: mk specialiser happy: minimum effort please
    simpl_gently = CoreDoSimplify max_iter
                       (base_mode { sm_phase = InitialPhase
                                  , sm_names = ["Gentle"]
                                  , sm_rules = rules_on   -- Note [RULEs enabled in SimplGently]
                                  , sm_inline = True
                                              -- See Note [Inline in InitialPhase]
                                  , sm_case_case = False })
                          -- Don't do case-of-case transformations.
                          -- This makes full laziness work better

    strictness_pass = if ww_on
                       then [CoreDoStrictness,CoreDoWorkerWrapper]
                       else [CoreDoStrictness]


    -- New demand analyser
    demand_analyser = (CoreDoPasses (
                           strictness_pass ++
                           [simpl_phase 0 ["post-worker-wrapper"] max_iter]
                           ))

    -- Static forms are moved to the top level with the FloatOut pass.
    -- See Note [Grand plan for static forms] in StaticPtrTable.
    static_ptrs_float_outwards =
      runWhen static_ptrs $ CoreDoPasses
        [ simpl_gently -- Float Out can't handle type lets (sometimes created
                       -- by simpleOptPgm via mkParallelBindings)
        , CoreDoFloatOutwards FloatOutSwitches
          { floatOutLambdas   = Just 0
          , floatOutConstants = True
          , floatOutOverSatApps = False
          , floatToTopLevelOnly = True
          }
        ]

    core_todo =
     if opt_level == 0 then
       [ static_ptrs_float_outwards,
         CoreDoSimplify max_iter
             (base_mode { sm_phase = Phase 0
                        , sm_names = ["Non-opt simplification"] })
       ]

     else {- opt_level >= 1 -} [

    -- We want to do the static argument transform before full laziness as it
    -- may expose extra opportunities to float things outwards. However, to fix
    -- up the output of the transformation we need at do at least one simplify
    -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- initial simplify: mk specialiser happy: minimum effort please
        simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        runWhen do_specialise CoreDoSpecialising,

        if full_laziness then
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas   = Just 0,
                                 floatOutConstants = True,
                                 floatOutOverSatApps = False,
                                 floatToTopLevelOnly = False }
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
        else
           -- Even with full laziness turned off, we still need to float static
           -- forms to the top level. See Note [Grand plan for static forms] in
           -- StaticPtrTable.
           static_ptrs_float_outwards,

        simpl_phases,

                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simplifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!
        simpl_phase 0 ["main"] (max max_iter 3),

        runWhen do_float_in CoreDoFloatInwards,
            -- Run float-inwards immediately before the strictness analyser
            -- Doing so pushes bindings nearer their use site and hence makes
            -- them more likely to be strict. These bindings might only show
            -- up after the inlining from simplification.  Example in fulsom,
            -- Csg.calc, where an arg of timesDouble thereby becomes strict.

        runWhen call_arity $ CoreDoPasses
            [ CoreDoCallArity
            , simpl_phase 0 ["post-call-arity"] max_iter
            ],

        runWhen strictness demand_analyser,

        runWhen exitification CoreDoExitify,
            -- See note [Placement of the exitification pass]

        runWhen full_laziness $
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas     = floatLamArgs dflags,
                                 floatOutConstants   = True,
                                 floatOutOverSatApps = True,
                                 floatToTopLevelOnly = False },
                -- nofib/spectral/hartel/wang doubles in speed if you
                -- do full laziness late in the day.  It only happens
                -- after fusion and other stuff, so the early pass doesn't
                -- catch it.  For the record, the redex is
                --        f_el22 (f_el21 r_midblock)


        runWhen cse CoreCSE,
                -- We want CSE to follow the final full-laziness pass, because it may
                -- succeed in commoning up things floated out by full laziness.
                -- CSE used to rely on the no-shadowing invariant, but it doesn't any more

        runWhen do_float_in CoreDoFloatInwards,

        maybe_rule_check (Phase 0),

                -- Case-liberation for -O2.  This should be after
                -- strictness analysis and the simplification which follows it.
        runWhen liberate_case (CoreDoPasses [
            CoreLiberateCase,
            simpl_phase 0 ["post-liberate-case"] max_iter
            ]),         -- Run the simplifier after LiberateCase to vastly
                        -- reduce the possibility of shadowing
                        -- Reason: see Note [Shadowing] in SpecConstr.hs

        runWhen spec_constr CoreDoSpecConstr,

        maybe_rule_check (Phase 0),

        runWhen late_specialise
          (CoreDoPasses [ CoreDoSpecialising
                        , simpl_phase 0 ["post-late-spec"] max_iter]),

        -- LiberateCase can yield new CSE opportunities because it peels
        -- off one layer of a recursive function (concretely, I saw this
        -- in wheel-sieve1), and I'm guessing that SpecConstr can too
        -- And CSE is a very cheap pass. So it seems worth doing here.
        runWhen ((liberate_case || spec_constr) && cse) CoreCSE,

        -- Final clean-up simplification:
        simpl_phase 0 ["final"] max_iter,

        runWhen late_dmd_anal $ CoreDoPasses (
            strictness_pass ++
            [simpl_phase 0 ["post-late-ww"] max_iter]
          ),

        -- Final run of the demand_analyser, ensures that one-shot thunks are
        -- really really one-shot thunks. Only needed if the demand analyser
        -- has run at all. See Note [Final Demand Analyser run] in DmdAnal
        -- It is EXTREMELY IMPORTANT to run this pass, otherwise execution
        -- can become /exponentially/ more expensive. See Trac #11731, #12996.
        runWhen (strictness || late_dmd_anal) CoreDoStrictness,

        maybe_rule_check (Phase 0)
     ]

    -- Remove 'CoreDoNothing' and flatten 'CoreDoPasses' for clarity.
    flatten_todos [] = []
    flatten_todos (CoreDoNothing : rest) = flatten_todos rest
    flatten_todos (CoreDoPasses passes : rest) =
      flatten_todos passes ++ flatten_todos rest
    flatten_todos (todo : rest) = todo : flatten_todos rest

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

Note [RULEs enabled in SimplGently]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RULES are enabled when doing "gentle" simplification.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.

************************************************************************
*                                                                      *
                  The CoreToDo interpreter
*                                                                      *
************************************************************************
-}

runCorePasses :: [CoreToDo] -> ModGuts -> CoreM ModGuts
runCorePasses passes guts
  = foldM do_pass guts passes
  where
    do_pass guts CoreDoNothing = return guts
    do_pass guts (CoreDoPasses ps) = runCorePasses ps guts
    do_pass guts pass
       = withTiming getDynFlags
                    (ppr pass <+> brackets (ppr mod))
                    (const ()) $ do
            { guts' <- lintAnnots (ppr pass) (doCorePass pass) guts
            ; endPass pass (mg_binds guts') (mg_rules guts')
            ; return guts' }

    mod = mg_module guts

doCorePass :: CoreToDo -> ModGuts -> CoreM ModGuts
doCorePass pass@(CoreDoSimplify {})  = {-# SCC "Simplify" #-}
                                       simplifyPgm pass

doCorePass CoreCSE                   = {-# SCC "CommonSubExpr" #-}
                                       doPass cseProgram

doCorePass CoreLiberateCase          = {-# SCC "LiberateCase" #-}
                                       doPassD liberateCase

doCorePass CoreDoFloatInwards        = {-# SCC "FloatInwards" #-}
                                       floatInwards

doCorePass (CoreDoFloatOutwards f)   = {-# SCC "FloatOutwards" #-}
                                       doPassDUM (floatOutwards f)

doCorePass CoreDoStaticArgs          = {-# SCC "StaticArgs" #-}
                                       doPassU doStaticArgs

doCorePass CoreDoCallArity           = {-# SCC "CallArity" #-}
                                       doPassD callArityAnalProgram

doCorePass CoreDoExitify             = {-# SCC "Exitify" #-}
                                       doPass exitifyProgram

doCorePass CoreDoStrictness          = {-# SCC "NewStranal" #-}
                                       doPassDFM dmdAnalProgram

doCorePass CoreDoWorkerWrapper       = {-# SCC "WorkWrap" #-}
                                       doPassDFU wwTopBinds

doCorePass CoreDoSpecialising        = {-# SCC "Specialise" #-}
                                       specProgram

doCorePass CoreDoSpecConstr          = {-# SCC "SpecConstr" #-}
                                       specConstrProgram

doCorePass CoreDoPrintCore              = observe   printCore
doCorePass (CoreDoRuleCheck phase pat)  = ruleCheckPass phase pat
doCorePass CoreDoNothing                = return
doCorePass (CoreDoPasses passes)        = runCorePasses passes

#if defined(GHCI)
doCorePass (CoreDoPluginPass _ pass) = {-# SCC "Plugin" #-} pass
#else
doCorePass pass@CoreDoPluginPass {}  = pprPanic "doCorePass" (ppr pass)
#endif

doCorePass pass@CoreDesugar          = pprPanic "doCorePass" (ppr pass)
doCorePass pass@CoreDesugarOpt       = pprPanic "doCorePass" (ppr pass)
doCorePass pass@CoreTidy             = pprPanic "doCorePass" (ppr pass)
doCorePass pass@CorePrep             = pprPanic "doCorePass" (ppr pass)
doCorePass pass@CoreOccurAnal        = pprPanic "doCorePass" (ppr pass)

{-
************************************************************************
*                                                                      *
\subsection{Core pass combinators}
*                                                                      *
************************************************************************
-}

printCore :: DynFlags -> CoreProgram -> IO ()
printCore dflags binds
    = Err.dumpIfSet dflags True "Print Core" (pprCoreBindings binds)

ruleCheckPass :: CompilerPhase -> String -> ModGuts -> CoreM ModGuts
ruleCheckPass current_phase pat guts =
    withTiming getDynFlags
               (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
               (const ()) $ do
    { rb <- getRuleBase
    ; dflags <- getDynFlags
    ; vis_orphs <- getVisibleOrphanMods
    ; let rule_fn fn = getRules (RuleEnv rb vis_orphs) fn
                        ++ (mg_rules guts)
    ; liftIO $ putLogMsg dflags NoReason Err.SevDump noSrcSpan
                   (defaultDumpStyle dflags)
                   (ruleCheckProgram current_phase pat
                      rule_fn (mg_binds guts))
    ; return guts }

doPassDUM :: (DynFlags -> UniqSupply -> CoreProgram -> IO CoreProgram) -> ModGuts -> CoreM ModGuts
doPassDUM do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    us     <- getUniqueSupplyM
    liftIO $ do_pass dflags us binds

doPassDM :: (DynFlags -> CoreProgram -> IO CoreProgram) -> ModGuts -> CoreM ModGuts
doPassDM do_pass = doPassDUM (\dflags -> const (do_pass dflags))

doPassD :: (DynFlags -> CoreProgram -> CoreProgram) -> ModGuts -> CoreM ModGuts
doPassD do_pass = doPassDM (\dflags -> return . do_pass dflags)

doPassDU :: (DynFlags -> UniqSupply -> CoreProgram -> CoreProgram) -> ModGuts -> CoreM ModGuts
doPassDU do_pass = doPassDUM (\dflags us -> return . do_pass dflags us)

doPassU :: (UniqSupply -> CoreProgram -> CoreProgram) -> ModGuts -> CoreM ModGuts
doPassU do_pass = doPassDU (const do_pass)

doPassDFM :: (DynFlags -> FamInstEnvs -> CoreProgram -> IO CoreProgram) -> ModGuts -> CoreM ModGuts
doPassDFM do_pass guts = do
    dflags <- getDynFlags
    p_fam_env <- getPackageFamInstEnv
    let fam_envs = (p_fam_env, mg_fam_inst_env guts)
    doPassM (liftIO . do_pass dflags fam_envs) guts

doPassDFU :: (DynFlags -> FamInstEnvs -> UniqSupply -> CoreProgram -> CoreProgram) -> ModGuts -> CoreM ModGuts
doPassDFU do_pass guts = do
    dflags <- getDynFlags
    us     <- getUniqueSupplyM
    p_fam_env <- getPackageFamInstEnv
    let fam_envs = (p_fam_env, mg_fam_inst_env guts)
    doPass (do_pass dflags fam_envs us) guts

-- Most passes return no stats and don't change rules: these combinators
-- let us lift them to the full blown ModGuts+CoreM world
doPassM :: Monad m => (CoreProgram -> m CoreProgram) -> ModGuts -> m ModGuts
doPassM bind_f guts = do
    binds' <- bind_f (mg_binds guts)
    return (guts { mg_binds = binds' })

doPass :: (CoreProgram -> CoreProgram) -> ModGuts -> CoreM ModGuts
doPass bind_f guts = return $ guts { mg_binds = bind_f (mg_binds guts) }

-- Observer passes just peek; don't modify the bindings at all
observe :: (DynFlags -> CoreProgram -> IO a) -> ModGuts -> CoreM ModGuts
observe do_pass = doPassM $ \binds -> do
    dflags <- getDynFlags
    _ <- liftIO $ do_pass dflags binds
    return binds

{-
************************************************************************
*                                                                      *
        Gentle simplification
*                                                                      *
************************************************************************
-}

simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
             -> CoreExpr
             -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
--
-- Also used by Template Haskell
simplifyExpr dflags expr
  = withTiming (pure dflags) (text "Simplify [expr]") (const ()) $
    do  {
        ; us <-  mkSplitUniqSupply 's'

        ; let sz = exprSize expr

        ; (expr', counts) <- initSmpl dflags emptyRuleEnv
                               emptyFamInstEnvs us sz
                               (simplExprGently (simplEnvForGHCi dflags) expr)

        ; Err.dumpIfSet dflags (dopt Opt_D_dump_simpl_stats dflags)
                  "Simplifier statistics" (pprSimplCount counts)

        ; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl "Simplified expression"
                        (pprCoreExpr expr')

        ; return expr'
        }

simplExprGently :: SimplEnv -> CoreExpr -> SimplM CoreExpr
-- Simplifies an expression
--      does occurrence analysis, then simplification
--      and repeats (twice currently) because one pass
--      alone leaves tons of crud.
-- Used (a) for user expressions typed in at the interactive prompt
--      (b) the LHS and RHS of a RULE
--      (c) Template Haskell splices
--
-- The name 'Gently' suggests that the SimplMode is SimplGently,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

-- It's important that simplExprGently does eta reduction; see
-- Note [Simplifying the left-hand side of a RULE] above.  The
-- simplifier does indeed do eta reduction (it's in Simplify.completeLam)
-- but only if -O is on.

simplExprGently env expr = do
    expr1 <- simplExpr env (occurAnalyseExpr expr)
    simplExpr env (occurAnalyseExpr expr1)

{-
************************************************************************
*                                                                      *
\subsection{The driver for the simplifier}
*                                                                      *
************************************************************************
-}

simplifyPgm :: CoreToDo -> ModGuts -> CoreM ModGuts
simplifyPgm pass guts
  = do { hsc_env <- getHscEnv
       ; us <- getUniqueSupplyM
       ; rb <- getRuleBase
       ; liftIOWithCount $
         simplifyPgmIO pass hsc_env us rb guts }

simplifyPgmIO :: CoreToDo
              -> HscEnv
              -> UniqSupply
              -> RuleBase
              -> ModGuts
              -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgmIO pass@(CoreDoSimplify max_iterations mode)
              hsc_env us hpt_rule_base
              guts@(ModGuts { mg_module = this_mod
                            , mg_rdr_env = rdr_env
                            , mg_deps = deps
                            , mg_binds = binds, mg_rules = rules
                            , mg_fam_inst_env = fam_inst_env })
  = do { (termination_msg, it_count, counts_out, guts')
           <- do_iteration us 1 [] binds rules

        ; Err.dumpIfSet dflags (dopt Opt_D_verbose_core2core dflags &&
                                dopt Opt_D_dump_simpl_stats  dflags)
                  "Simplifier statistics for following pass"
                  (vcat [text termination_msg <+> text "after" <+> ppr it_count
                                              <+> text "iterations",
                         blankLine,
                         pprSimplCount counts_out])

        ; return (counts_out, guts')
    }
  where
    dflags       = hsc_dflags hsc_env
    print_unqual = mkPrintUnqualified dflags rdr_env
    simpl_env    = mkSimplEnv mode
    active_rule  = activeRule mode
    active_unf   = activeUnfolding mode

    do_iteration :: UniqSupply
                 -> Int          -- Counts iterations
                 -> [SimplCount] -- Counts from earlier iterations, reversed
                 -> CoreProgram  -- Bindings in
                 -> [CoreRule]   -- and orphan rules
                 -> IO (String, Int, SimplCount, ModGuts)

    do_iteration us iteration_no counts_so_far binds rules
        -- iteration_no is the number of the iteration we are
        -- about to begin, with '1' for the first
      | iteration_no > max_iterations   -- Stop if we've run out of iterations
      = WARN( debugIsOn && (max_iterations > 2)
            , hang (text "Simplifier bailing out after" <+> int max_iterations
                    <+> text "iterations"
                    <+> (brackets $ hsep $ punctuate comma $
                         map (int . simplCountN) (reverse counts_so_far)))
                 2 (text "Size =" <+> ppr (coreBindsStats binds)))

                -- Subtract 1 from iteration_no to get the
                -- number of iterations we actually completed
        return ( "Simplifier baled out", iteration_no - 1
               , totalise counts_so_far
               , guts { mg_binds = binds, mg_rules = rules } )

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds
      , () <- sz `seq` ()     -- Force it
      = do {
                -- Occurrence analysis
           let { tagged_binds = {-# SCC "OccAnal" #-}
                     occurAnalysePgm this_mod active_unf active_rule rules
                                     binds
               } ;
           Err.dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
                     (pprCoreBindings tagged_binds);

                -- Get any new rules, and extend the rule base
                -- See Note [Overall plumbing for rules] in Rules.hs
                -- We need to do this regularly, because simplification can
                -- poke on IdInfo thunks, which in turn brings in new rules
                -- behind the scenes.  Otherwise there's a danger we'll simply
                -- miss the rules for Ids hidden inside imported inlinings
           eps <- hscEPS hsc_env ;
           let  { rule_base1 = unionRuleBase hpt_rule_base (eps_rule_base eps)
                ; rule_base2 = extendRuleBaseList rule_base1 rules
                ; fam_envs = (eps_fam_inst_env eps, fam_inst_env)
                ; vis_orphs = this_mod : dep_orphs deps } ;

                -- Simplify the program
           ((binds1, rules1), counts1) <-
             initSmpl dflags (mkRuleEnv rule_base2 vis_orphs) fam_envs us1 sz $
               do { (floats, env1) <- {-# SCC "SimplTopBinds" #-}
                                      simplTopBinds simpl_env tagged_binds

                      -- Apply the substitution to rules defined in this module
                      -- for imported Ids.  Eg  RULE map my_f = blah
                      -- If we have a substitution my_f :-> other_f, we'd better
                      -- apply it to the rule to, or it'll never match
                  ; rules1 <- simplRules env1 Nothing rules Nothing

                  ; return (getTopFloatBinds floats, rules1) } ;

                -- Stop if nothing happened; don't dump output
                -- See Note [Which transformations are innocuous] in CoreMonad
           if isZeroSimplCount counts1 then
                return ( "Simplifier reached fixed point", iteration_no
                       , totalise (counts1 : counts_so_far)  -- Include "free" ticks
                       , guts { mg_binds = binds1, mg_rules = rules1 } )
           else do {
                -- Short out indirections
                -- We do this *after* at least one run of the simplifier
                -- because indirection-shorting uses the export flag on *occurrences*
                -- and that isn't guaranteed to be ok until after the first run propagates
                -- stuff from the binding site to its occurrences
                --
                -- ToDo: alas, this means that indirection-shorting does not happen at all
                --       if the simplifier does nothing (not common, I know, but unsavoury)
           let { binds2 = {-# SCC "ZapInd" #-} shortOutIndirections binds1 } ;

                -- Dump the result of this iteration
           dump_end_iteration dflags print_unqual iteration_no counts1 binds2 rules1 ;
           lintPassResult hsc_env pass binds2 ;

                -- Loop
           do_iteration us2 (iteration_no + 1) (counts1:counts_so_far) binds2 rules1
           } }
      | otherwise = panic "do_iteration"
      where
        (us1, us2) = splitUniqSupply us

        -- Remember the counts_so_far are reversed
        totalise :: [SimplCount] -> SimplCount
        totalise = foldr (\c acc -> acc `plusSimplCount` c)
                         (zeroSimplCount dflags)

simplifyPgmIO _ _ _ _ _ = panic "simplifyPgmIO"

-------------------
dump_end_iteration :: DynFlags -> PrintUnqualified -> Int
                   -> SimplCount -> CoreProgram -> [CoreRule] -> IO ()
dump_end_iteration dflags print_unqual iteration_no counts binds rules
  = dumpPassResult dflags print_unqual mb_flag hdr pp_counts binds rules
  where
    mb_flag | dopt Opt_D_dump_simpl_iterations dflags = Just Opt_D_dump_simpl_iterations
            | otherwise                               = Nothing
            -- Show details if Opt_D_dump_simpl_iterations is on

    hdr = text "Simplifier iteration=" <> int iteration_no
    pp_counts = vcat [ text "---- Simplifier counts for" <+> hdr
                     , pprSimplCount counts
                     , text "---- End of simplifier counts for" <+> hdr ]

{-
************************************************************************
*                                                                      *
                Shorting out indirections
*                                                                      *
************************************************************************

If we have this:

        x_local = <expression>
        ...bindings...
        x_exported = x_local

where x_exported is exported, and x_local is not, then we replace it with this:

        x_exported = <expression>
        x_local = x_exported
        ...bindings...

Without this we never get rid of the x_exported = x_local thing.  This
save a gratuitous jump (from \tr{x_exported} to \tr{x_local}), and
makes strictness information propagate better.  This used to happen in
the final phase, but it's tidier to do it here.

Note [Messing up the exported Id's RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must be careful about discarding (obviously) or even merging the
RULES on the exported Id. The example that went bad on me at one stage
was this one:

    iterate :: (a -> a) -> a -> [a]
        [Exported]
    iterate = iterateList

    iterateFB c f x = x `c` iterateFB c f (f x)
    iterateList f x =  x : iterateList f (f x)
        [Not exported]

    {-# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterateList
     #-}

This got shorted out to:

    iterateList :: (a -> a) -> a -> [a]
    iterateList = iterate

    iterateFB c f x = x `c` iterateFB c f (f x)
    iterate f x =  x : iterate f (f x)

    {-# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterate
     #-}

And now we get an infinite loop in the rule system
        iterate f x -> build (\cn -> iterateFB c f x)
                    -> iterateFB (:) f x
                    -> iterate f x

Old "solution":
        use rule switching-off pragmas to get rid
        of iterateList in the first place

But in principle the user *might* want rules that only apply to the Id
he says.  And inline pragmas are similar
   {-# NOINLINE f #-}
   f = local
   local = <stuff>
Then we do not want to get rid of the NOINLINE.

Hence hasShortableIdinfo.


Note [Rules and indirection-zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem: what if x_exported has a RULE that mentions something in ...bindings...?
Then the things mentioned can be out of scope!  Solution
 a) Make sure that in this pass the usage-info from x_exported is
        available for ...bindings...
 b) If there are any such RULES, rec-ify the entire top-level.
    It'll get sorted out next time round

Other remarks
~~~~~~~~~~~~~
If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we do one only:
\begin{verbatim}
        x_local = ....
        x_exported1 = x_local
        x_exported2 = x_local
==>
        x_exported1 = ....

        x_exported2 = x_exported1
\end{verbatim}

We rely on prior eta reduction to simplify things like
\begin{verbatim}
        x_exported = /\ tyvars -> x_local tyvars
==>
        x_exported = x_local
\end{verbatim}
Hence,there's a possibility of leaving unchanged something like this:
\begin{verbatim}
        x_local = ....
        x_exported1 = x_local Int
\end{verbatim}
By the time we've thrown away the types in STG land this
could be eliminated.  But I don't think it's very common
and it's dangerous to do this fiddling in STG land
because we might elminate a binding that's mentioned in the
unfolding for something.

Note [Indirection zapping and ticks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unfortunately this is another place where we need a special case for
ticks. The following happens quite regularly:

        x_local = <expression>
        x_exported = tick<x> x_local

Which we want to become:

        x_exported =  tick<x> <expression>

As it makes no sense to keep the tick and the expression on separate
bindings. Note however that that this might increase the ticks scoping
over the execution of x_local, so we can only do this for floatable
ticks. More often than not, other references will be unfoldings of
x_exported, and therefore carry the tick anyway.
-}

type IndEnv = IdEnv (Id, [Tickish Var]) -- Maps local_id -> exported_id, ticks

shortOutIndirections :: CoreProgram -> CoreProgram
shortOutIndirections binds
  | isEmptyVarEnv ind_env = binds
  | no_need_to_flatten    = binds'                      -- See Note [Rules and indirect-zapping]
  | otherwise             = [Rec (flattenBinds binds')] -- for this no_need_to_flatten stuff
  where
    ind_env            = makeIndEnv binds
    -- These exported Ids are the subjects  of the indirection-elimination
    exp_ids            = map fst $ nonDetEltsUFM ind_env
      -- It's OK to use nonDetEltsUFM here because we forget the ordering
      -- by immediately converting to a set or check if all the elements
      -- satisfy a predicate.
    exp_id_set         = mkVarSet exp_ids
    no_need_to_flatten = all (null . ruleInfoRules . idSpecialisation) exp_ids
    binds'             = concatMap zap binds

    zap (NonRec bndr rhs) = [NonRec b r | (b,r) <- zapPair (bndr,rhs)]
    zap (Rec pairs)       = [Rec (concatMap zapPair pairs)]

    zapPair (bndr, rhs)
        | bndr `elemVarSet` exp_id_set
        = []   -- Kill the exported-id binding

        | Just (exp_id, ticks) <- lookupVarEnv ind_env bndr
        , (exp_id', lcl_id') <- transferIdInfo exp_id bndr
        =      -- Turn a local-id binding into two bindings
               --    exp_id = rhs; lcl_id = exp_id
          [ (exp_id', mkTicks ticks rhs),
            (lcl_id', Var exp_id') ]

        | otherwise
        = [(bndr,rhs)]

makeIndEnv :: [CoreBind] -> IndEnv
makeIndEnv binds
  = foldl' add_bind emptyVarEnv binds
  where
    add_bind :: IndEnv -> CoreBind -> IndEnv
    add_bind env (NonRec exported_id rhs) = add_pair env (exported_id, rhs)
    add_bind env (Rec pairs)              = foldl' add_pair env pairs

    add_pair :: IndEnv -> (Id,CoreExpr) -> IndEnv
    add_pair env (exported_id, exported)
        | (ticks, Var local_id) <- stripTicksTop tickishFloatable exported
        , shortMeOut env exported_id local_id
        = extendVarEnv env local_id (exported_id, ticks)
    add_pair env _ = env

-----------------
shortMeOut :: IndEnv -> Id -> Id -> Bool
shortMeOut ind_env exported_id local_id
-- The if-then-else stuff is just so I can get a pprTrace to see
-- how often I don't get shorting out because of IdInfo stuff
  = if isExportedId exported_id &&              -- Only if this is exported

       isLocalId local_id &&                    -- Only if this one is defined in this
                                                --      module, so that we *can* change its
                                                --      binding to be the exported thing!

       not (isExportedId local_id) &&           -- Only if this one is not itself exported,
                                                --      since the transformation will nuke it

       not (local_id `elemVarEnv` ind_env)      -- Only if not already substituted for
    then
        if hasShortableIdInfo exported_id
        then True       -- See Note [Messing up the exported Id's IdInfo]
        else WARN( True, text "Not shorting out:" <+> ppr exported_id )
             False
    else
        False

-----------------
hasShortableIdInfo :: Id -> Bool
-- True if there is no user-attached IdInfo on exported_id,
-- so we can safely discard it
-- See Note [Messing up the exported Id's IdInfo]
hasShortableIdInfo id
  =  isEmptyRuleInfo (ruleInfo info)
  && isDefaultInlinePragma (inlinePragInfo info)
  && not (isStableUnfolding (unfoldingInfo info))
  where
     info = idInfo id

-----------------
{- Note [Transferring IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
     lcl_id = e; exp_id = lcl_id

and lcl_id has useful IdInfo, we don't want to discard it by going
     gbl_id = e; lcl_id = gbl_id

Instead, transfer IdInfo from lcl_id to exp_id, specifically
* (Stable) unfolding
* Strictness
* Rules
* Inline pragma

Overwriting, rather than merging, seems to work ok.

We also zap the InlinePragma on the lcl_id. It might originally
have had a NOINLINE, which we have now transferred; and we really
want the lcl_id to inline now that its RHS is trivial!
-}

transferIdInfo :: Id -> Id -> (Id, Id)
-- See Note [Transferring IdInfo]
transferIdInfo exported_id local_id
  = ( modifyIdInfo transfer exported_id
    , local_id `setInlinePragma` defaultInlinePragma )
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setStrictnessInfo`    strictnessInfo local_info
                                 `setUnfoldingInfo`     unfoldingInfo local_info
                                 `setInlinePragInfo`    inlinePragInfo local_info
                                 `setRuleInfo`          addRuleInfo (ruleInfo exp_info) new_info
    new_info = setRuleInfoHead (idName exported_id)
                               (ruleInfo local_info)
        -- Remember to set the function-name field of the
        -- rules as we transfer them from one function to another
