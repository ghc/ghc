%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
module SimplCore ( core2core, simplifyExpr ) where

#include "HsVersions.h"

import DynFlags
import CoreSyn
import CoreSubst
import HscTypes
import CSE              ( cseProgram )
import Rules            ( RuleBase, emptyRuleBase, mkRuleBase, unionRuleBase,
                          extendRuleBaseList, ruleCheckProgram, addSpecInfo, )
import PprCore          ( pprCoreBindings, pprCoreExpr )
import OccurAnal        ( occurAnalysePgm, occurAnalyseExpr )
import IdInfo
import CoreUtils        ( coreBindsSize, coreBindsStats, exprSize )
import Simplify         ( simplTopBinds, simplExpr )
import SimplUtils       ( simplEnvForGHCi, activeRule )
import SimplEnv
import SimplMonad
import CoreMonad
import qualified ErrUtils as Err
import FloatIn          ( floatInwards )
import FloatOut         ( floatOutwards )
import FamInstEnv
import Id
import BasicTypes       ( CompilerPhase(..), isDefaultInlinePragma )
import VarSet
import VarEnv
import LiberateCase     ( liberateCase )
import SAT              ( doStaticArgs )
import Specialise       ( specProgram)
import SpecConstr       ( specConstrProgram)
import DmdAnal       ( dmdAnalProgram )
import WorkWrap         ( wwTopBinds )
import Vectorise        ( vectorise )
import FastString
import SrcLoc
import Util

import Maybes
import UniqSupply       ( UniqSupply, mkSplitUniqSupply, splitUniqSupply )
import Outputable
import Control.Monad

#ifdef GHCI
import Type             ( mkTyConTy )
import RdrName          ( mkRdrQual )
import OccName          ( mkVarOcc )
import PrelNames        ( pluginTyConName )
import DynamicLoading   ( forceLoadTyCon, lookupRdrNameInModule, getValueSafely )
import Module           ( ModuleName )
import Panic
#endif
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The driver for the simplifier}
%*                                                                      *
%************************************************************************

\begin{code}
core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core hsc_env guts
  = do { us <- mkSplitUniqSupply 's'
       -- make sure all plugins are loaded

       ; let builtin_passes = getCoreToDo dflags
       ;
       ; (guts2, stats) <- runCoreM hsc_env hpt_rule_base us mod $
                        do { all_passes <- addPluginPasses dflags builtin_passes
                           ; runCorePasses all_passes guts }

{--
       ; Err.dumpIfSet_dyn dflags Opt_D_dump_core_pipeline
             "Plugin information" "" -- TODO FIXME: dump plugin info
--}
       ; Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags         = hsc_dflags hsc_env
    home_pkg_rules = hptRules hsc_env (dep_mods (mg_deps guts))
    hpt_rule_base  = mkRuleBase home_pkg_rules
    mod            = mg_module guts
    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.
\end{code}


%************************************************************************
%*                                                                      *
           Generating the main optimisation pipeline
%*                                                                      *
%************************************************************************

\begin{code}
getCoreToDo :: DynFlags -> [CoreToDo]
getCoreToDo dflags
  = core_todo
  where
    opt_level     = optLevel           dflags
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
    rule_check    = ruleCheck          dflags
    strictness    = gopt Opt_Strictness                   dflags
    full_laziness = gopt Opt_FullLaziness                 dflags
    do_specialise = gopt Opt_Specialise                   dflags
    do_float_in   = gopt Opt_FloatIn                      dflags
    cse           = gopt Opt_CSE                          dflags
    spec_constr   = gopt Opt_SpecConstr                   dflags
    liberate_case = gopt Opt_LiberateCase                 dflags
    late_dmd_anal = gopt Opt_LateDmdAnal                  dflags
    static_args   = gopt Opt_StaticArgumentTransformation dflags
    rules_on      = gopt Opt_EnableRewriteRules           dflags
    eta_expand_on = gopt Opt_DoLambdaEtaExpansion         dflags

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    maybe_strictness_before phase
      = runWhen (phase `elem` strictnessBefore dflags) CoreDoStrictness

    base_mode = SimplMode { sm_phase      = panic "base_mode"
                          , sm_names      = []
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

          -- Vectorisation can introduce a fair few common sub expressions involving
          --  DPH primitives. For example, see the Reverse test from dph-examples.
          --  We need to eliminate these common sub expressions before their definitions
          --  are inlined in phase 2. The CSE introduces lots of  v1 = v2 bindings,
          --  so we also run simpl_gently to inline them.
      ++  (if gopt Opt_Vectorise dflags && phase == 3
            then [CoreCSE, simpl_gently]
            else [])

    vectorisation
      = runWhen (gopt Opt_Vectorise dflags) $
          CoreDoPasses [ simpl_gently, CoreDoVectorisation ]

                -- By default, we have 2 phases before phase 0.

                -- Want to run with inline phase 2 after the specialiser to give
                -- maximum chance for fusion to work before we inline build/augment
                -- in phase 1.  This made a difference in 'ansi' where an
                -- overloaded function wasn't inlined till too late.

                -- Need phase 1 so that build/augment get
                -- inlined.  I found that spectral/hartel/genfft lost some useful
                -- strictness in the function sumcode' if augment is not inlined
                -- before strictness analysis runs
    simpl_phases = CoreDoPasses [ simpl_phase phase ["main"] max_iter
                                | phase <- [phases, phases-1 .. 1] ]


        -- initial simplify: mk specialiser happy: minimum effort please
    simpl_gently = CoreDoSimplify max_iter
                       (base_mode { sm_phase = InitialPhase
                                  , sm_names = ["Gentle"]
                                  , sm_rules = rules_on   -- Note [RULEs enabled in SimplGently]
                                  , sm_inline = False
                                  , sm_case_case = False })
                          -- Don't do case-of-case transformations.
                          -- This makes full laziness work better

    -- New demand analyser
    demand_analyser = (CoreDoPasses ([
                           CoreDoStrictness,
                           CoreDoWorkerWrapper,
                           simpl_phase 0 ["post-worker-wrapper"] max_iter
                           ]))

    core_todo =
     if opt_level == 0 then
       [ vectorisation
       , CoreDoSimplify max_iter
             (base_mode { sm_phase = Phase 0
                        , sm_names = ["Non-opt simplification"] })
       ]

     else {- opt_level >= 1 -} [

    -- We want to do the static argument transform before full laziness as it
    -- may expose extra opportunities to float things outwards. However, to fix
    -- up the output of the transformation we need at do at least one simplify
    -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- We run vectorisation here for now, but we might also try to run
        -- it later
        vectorisation,

        -- initial simplify: mk specialiser happy: minimum effort please
        simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        runWhen do_specialise CoreDoSpecialising,

        runWhen full_laziness $
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas   = Just 0,
                                 floatOutConstants = True,
                                 floatOutPartialApplications = False },
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
                -- Not doing floatOutPartialApplications yet, we'll do
                -- that later on when we've had a chance to get more
                -- accurate arity information.  In fact it makes no
                -- difference at all to performance if we do it here,
                -- but maybe we save some unnecessary to-and-fro in
                -- the simplifier.

        runWhen do_float_in CoreDoFloatInwards,

        simpl_phases,

                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simpifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!
        simpl_phase 0 ["main"] (max max_iter 3),

        runWhen strictness demand_analyser,

        runWhen full_laziness $
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas   = floatLamArgs dflags,
                                 floatOutConstants = True,
                                 floatOutPartialApplications = True },
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
                        -- reduce the possiblility of shadowing
                        -- Reason: see Note [Shadowing] in SpecConstr.lhs

        runWhen spec_constr CoreDoSpecConstr,

        maybe_rule_check (Phase 0),

        -- Final clean-up simplification:
        simpl_phase 0 ["final"] max_iter,

        runWhen late_dmd_anal $ CoreDoPasses [
            CoreDoStrictness,
            CoreDoWorkerWrapper,
            simpl_phase 0 ["post-late-ww"] max_iter
          ],

        maybe_rule_check (Phase 0)
     ]
\end{code}

Loading plugins

\begin{code}
addPluginPasses :: DynFlags -> [CoreToDo] -> CoreM [CoreToDo]
#ifndef GHCI
addPluginPasses _ builtin_passes = return builtin_passes
#else
addPluginPasses dflags builtin_passes
  = do { hsc_env <- getHscEnv
       ; named_plugins <- liftIO (loadPlugins hsc_env)
       ; foldM query_plug builtin_passes named_plugins }
  where
    query_plug todos (mod_nm, plug)
       = installCoreToDos plug options todos
       where
         options = [ option | (opt_mod_nm, option) <- pluginModNameOpts dflags
                            , opt_mod_nm == mod_nm ]

loadPlugins :: HscEnv -> IO [(ModuleName, Plugin)]
loadPlugins hsc_env
  = do { let to_load = pluginModNames (hsc_dflags hsc_env)
       ; plugins <- mapM (loadPlugin hsc_env) to_load
       ; return $ to_load `zip` plugins }

loadPlugin :: HscEnv -> ModuleName -> IO Plugin
loadPlugin hsc_env mod_name
  = do { let plugin_rdr_name = mkRdrQual mod_name (mkVarOcc "plugin")
             dflags = hsc_dflags hsc_env
       ; mb_name <- lookupRdrNameInModule hsc_env mod_name plugin_rdr_name
       ; case mb_name of {
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ ptext (sLit "The module"), ppr mod_name
                          , ptext (sLit "did not export the plugin name")
                          , ppr plugin_rdr_name ]) ;
            Just name ->

     do { plugin_tycon <- forceLoadTyCon hsc_env pluginTyConName
        ; mb_plugin <- getValueSafely hsc_env name (mkTyConTy plugin_tycon)
        ; case mb_plugin of
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ ptext (sLit "The value"), ppr name
                          , ptext (sLit "did not have the type")
                          , ppr pluginTyConName, ptext (sLit "as required")])
            Just plugin -> return plugin } } }
#endif
\end{code}

%************************************************************************
%*                                                                      *
                  The CoreToDo interpreter
%*                                                                      *
%************************************************************************

\begin{code}
runCorePasses :: [CoreToDo] -> ModGuts -> CoreM ModGuts
runCorePasses passes guts
  = foldM do_pass guts passes
  where
    do_pass guts CoreDoNothing = return guts
    do_pass guts (CoreDoPasses ps) = runCorePasses ps guts
    do_pass guts pass
       = do { dflags <- getDynFlags
            ; liftIO $ showPass dflags pass
            ; guts' <- doCorePass dflags pass guts
            ; liftIO $ endPass dflags pass (mg_binds guts') (mg_rules guts')
            ; return guts' }

doCorePass :: DynFlags -> CoreToDo -> ModGuts -> CoreM ModGuts
doCorePass _      pass@(CoreDoSimplify {})  = {-# SCC "Simplify" #-}
                                              simplifyPgm pass

doCorePass _      CoreCSE                   = {-# SCC "CommonSubExpr" #-}
                                              doPass cseProgram

doCorePass _      CoreLiberateCase          = {-# SCC "LiberateCase" #-}
                                              doPassD liberateCase

doCorePass dflags CoreDoFloatInwards        = {-# SCC "FloatInwards" #-}
                                              doPass (floatInwards dflags)

doCorePass _      (CoreDoFloatOutwards f)   = {-# SCC "FloatOutwards" #-}
                                              doPassDUM (floatOutwards f)

doCorePass _      CoreDoStaticArgs          = {-# SCC "StaticArgs" #-}
                                              doPassU doStaticArgs

doCorePass _      CoreDoStrictness          = {-# SCC "NewStranal" #-}
                                              doPassDM dmdAnalProgram

doCorePass dflags CoreDoWorkerWrapper       = {-# SCC "WorkWrap" #-}
                                              doPassU (wwTopBinds dflags)

doCorePass dflags CoreDoSpecialising        = {-# SCC "Specialise" #-}
                                              specProgram dflags

doCorePass _      CoreDoSpecConstr          = {-# SCC "SpecConstr" #-}
                                              specConstrProgram

doCorePass _      CoreDoVectorisation       = {-# SCC "Vectorise" #-}
                                              vectorise

doCorePass _      CoreDoPrintCore              = observe   printCore
doCorePass _      (CoreDoRuleCheck phase pat)  = ruleCheckPass phase pat
doCorePass _      CoreDoNothing                = return
doCorePass _      (CoreDoPasses passes)        = runCorePasses passes

#ifdef GHCI
doCorePass _      (CoreDoPluginPass _ pass) = {-# SCC "Plugin" #-} pass
#endif

doCorePass _      pass = pprPanic "doCorePass" (ppr pass)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Core pass combinators}
%*                                                                      *
%************************************************************************

\begin{code}
printCore :: DynFlags -> CoreProgram -> IO ()
printCore dflags binds
    = Err.dumpIfSet dflags True "Print Core" (pprCoreBindings binds)

ruleCheckPass :: CompilerPhase -> String -> ModGuts -> CoreM ModGuts
ruleCheckPass current_phase pat guts = do
    rb <- getRuleBase
    dflags <- getDynFlags
    liftIO $ Err.showPass dflags "RuleCheck"
    liftIO $ log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
                 (ruleCheckProgram current_phase pat rb (mg_binds guts))
    return guts


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
\end{code}


%************************************************************************
%*                                                                      *
        Gentle simplification
%*                                                                      *
%************************************************************************

\begin{code}
simplifyExpr :: DynFlags -- includes spec of what core-to-core passes to do
             -> CoreExpr
             -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
--
-- Also used by Template Haskell
simplifyExpr dflags expr
  = do  {
        ; Err.showPass dflags "Simplify"

        ; us <-  mkSplitUniqSupply 's'

        ; let sz = exprSize expr

        ; (expr', counts) <- initSmpl dflags emptyRuleBase emptyFamInstEnvs us sz $
                                 simplExprGently (simplEnvForGHCi dflags) expr

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
-- The name 'Gently' suggests that the SimplifierMode is SimplGently,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

-- It's important that simplExprGently does eta reduction; see
-- Note [Simplifying the left-hand side of a RULE] above.  The
-- simplifier does indeed do eta reduction (it's in Simplify.completeLam)
-- but only if -O is on.

simplExprGently env expr = do
    expr1 <- simplExpr env (occurAnalyseExpr expr)
    simplExpr env (occurAnalyseExpr expr1)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The driver for the simplifier}
%*                                                                      *
%************************************************************************

\begin{code}
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
                            , mg_binds = binds, mg_rules = rules
                            , mg_fam_inst_env = fam_inst_env })
  = do { (termination_msg, it_count, counts_out, guts')
           <- do_iteration us 1 [] binds rules

        ; Err.dumpIfSet dflags (dump_phase && dopt Opt_D_dump_simpl_stats dflags)
                  "Simplifier statistics for following pass"
                  (vcat [text termination_msg <+> text "after" <+> ppr it_count <+> text "iterations",
                         blankLine,
                         pprSimplCount counts_out])

        ; return (counts_out, guts')
    }
  where
    dflags      = hsc_dflags hsc_env
    dump_phase  = dumpSimplPhase dflags mode
    simpl_env   = mkSimplEnv mode
    active_rule = activeRule simpl_env

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
            , hang (ptext (sLit "Simplifier bailing out after") <+> int max_iterations
                    <+> ptext (sLit "iterations")
                    <+> (brackets $ hsep $ punctuate comma $
                         map (int . simplCountN) (reverse counts_so_far)))
                 2 (ptext (sLit "Size =") <+> ppr (coreBindsStats binds)))

                -- Subtract 1 from iteration_no to get the
                -- number of iterations we actually completed
        return ( "Simplifier baled out", iteration_no - 1
               , totalise counts_so_far
               , guts { mg_binds = binds, mg_rules = rules } )

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds
      , sz == sz     -- Force it
      = do {
                -- Occurrence analysis
           let {   -- Note [Vectorisation declarations and occurences]
                   -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                   -- During the 'InitialPhase' (i.e., before vectorisation), we need to make sure
                   -- that the right-hand sides of vectorisation declarations are taken into
                   -- account during occurrence analysis. After the 'InitialPhase', we need to ensure
                   -- that the binders representing variable vectorisation declarations are kept alive.
                   -- (In contrast to automatically vectorised variables, their unvectorised versions
                   -- don't depend on them.)
                 vectVars = mkVarSet $
                              catMaybes [ fmap snd $ lookupVarEnv (vectInfoVar (mg_vect_info guts)) bndr
                                        | Vect bndr _ <- mg_vect_decls guts]
                              ++
                              catMaybes [ fmap snd $ lookupVarEnv (vectInfoVar (mg_vect_info guts)) bndr
                                        | bndr <- bindersOfBinds binds]
                                        -- FIXME: This second comprehensions is only needed as long as we
                                        --        have vectorised bindings where we get "Could NOT call
                                        --        vectorised from original version".
              ;  (maybeVects, maybeVectVars)
                   = case sm_phase mode of
                       InitialPhase -> (mg_vect_decls guts, vectVars)
                       _            -> ([], vectVars)
               ; tagged_binds = {-# SCC "OccAnal" #-}
                     occurAnalysePgm this_mod active_rule rules maybeVects maybeVectVars binds
               } ;
           Err.dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
                     (pprCoreBindings tagged_binds);

                -- Get any new rules, and extend the rule base
                -- See Note [Overall plumbing for rules] in Rules.lhs
                -- We need to do this regularly, because simplification can
                -- poke on IdInfo thunks, which in turn brings in new rules
                -- behind the scenes.  Otherwise there's a danger we'll simply
                -- miss the rules for Ids hidden inside imported inlinings
           eps <- hscEPS hsc_env ;
           let  { rule_base1 = unionRuleBase hpt_rule_base (eps_rule_base eps)
                ; rule_base2 = extendRuleBaseList rule_base1 rules
                ; simpl_binds = {-# SCC "SimplTopBinds" #-}
                                simplTopBinds simpl_env tagged_binds
                ; fam_envs = (eps_fam_inst_env eps, fam_inst_env) } ;

                -- Simplify the program
           (env1, counts1) <- initSmpl dflags rule_base2 fam_envs us1 sz simpl_binds ;

           let  { binds1 = getFloatBinds env1
                ; rules1 = substRulesForImportedIds (mkCoreSubst (text "imp-rules") env1) rules
                } ;

                -- Stop if nothing happened; don't dump output
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
           end_iteration dflags pass iteration_no counts1 binds2 rules1 ;

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
end_iteration :: DynFlags -> CoreToDo -> Int
             -> SimplCount -> CoreProgram -> [CoreRule] -> IO ()
end_iteration dflags pass iteration_no counts binds rules
  = do { dumpPassResult dflags mb_flag hdr pp_counts binds rules
       ; lintPassResult dflags pass binds }
  where
    mb_flag | dopt Opt_D_dump_simpl_iterations dflags = Just Opt_D_dump_simpl_phases
            | otherwise                               = Nothing
            -- Show details if Opt_D_dump_simpl_iterations is on

    hdr = ptext (sLit "Simplifier iteration=") <> int iteration_no
    pp_counts = vcat [ ptext (sLit "---- Simplifier counts for") <+> hdr
                     , pprSimplCount counts
                     , ptext (sLit "---- End of simplifier counts for") <+> hdr ]
\end{code}


%************************************************************************
%*                                                                      *
                Shorting out indirections
%*                                                                      *
%************************************************************************

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

Note [Transferring IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to propagage any useful IdInfo on x_local to x_exported.

STRICTNESS: if we have done strictness analysis, we want the strictness info on
x_local to transfer to x_exported.  Hence the copyIdInfo call.

RULES: we want to *add* any RULES for x_local to x_exported.


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

\begin{code}
type IndEnv = IdEnv Id          -- Maps local_id -> exported_id

shortOutIndirections :: CoreProgram -> CoreProgram
shortOutIndirections binds
  | isEmptyVarEnv ind_env = binds
  | no_need_to_flatten    = binds'                      -- See Note [Rules and indirect-zapping]
  | otherwise             = [Rec (flattenBinds binds')] -- for this no_need_to_flatten stuff
  where
    ind_env            = makeIndEnv binds
    exp_ids            = varSetElems ind_env    -- These exported Ids are the subjects
    exp_id_set         = mkVarSet exp_ids       -- of the indirection-elimination
    no_need_to_flatten = all (null . specInfoRules . idSpecialisation) exp_ids
    binds'             = concatMap zap binds

    zap (NonRec bndr rhs) = [NonRec b r | (b,r) <- zapPair (bndr,rhs)]
    zap (Rec pairs)       = [Rec (concatMap zapPair pairs)]

    zapPair (bndr, rhs)
        | bndr `elemVarSet` exp_id_set             = []
        | Just exp_id <- lookupVarEnv ind_env bndr = [(transferIdInfo exp_id bndr, rhs),
                                                      (bndr, Var exp_id)]
        | otherwise                                = [(bndr,rhs)]

makeIndEnv :: [CoreBind] -> IndEnv
makeIndEnv binds
  = foldr add_bind emptyVarEnv binds
  where
    add_bind :: CoreBind -> IndEnv -> IndEnv
    add_bind (NonRec exported_id rhs) env = add_pair (exported_id, rhs) env
    add_bind (Rec pairs)              env = foldr add_pair env pairs

    add_pair :: (Id,CoreExpr) -> IndEnv -> IndEnv
    add_pair (exported_id, Var local_id) env
        | shortMeOut env exported_id local_id = extendVarEnv env local_id exported_id
    add_pair _ env = env

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
        else WARN( True, ptext (sLit "Not shorting out:") <+> ppr exported_id )
             False
    else
        False

-----------------
hasShortableIdInfo :: Id -> Bool
-- True if there is no user-attached IdInfo on exported_id,
-- so we can safely discard it
-- See Note [Messing up the exported Id's IdInfo]
hasShortableIdInfo id
  =  isEmptySpecInfo (specInfo info)
  && isDefaultInlinePragma (inlinePragInfo info)
  && not (isStableUnfolding (unfoldingInfo info))
  where
     info = idInfo id

-----------------
transferIdInfo :: Id -> Id -> Id
-- See Note [Transferring IdInfo]
-- If we have
--      lcl_id = e; exp_id = lcl_id
-- and lcl_id has useful IdInfo, we don't want to discard it by going
--      gbl_id = e; lcl_id = gbl_id
-- Instead, transfer IdInfo from lcl_id to exp_id
-- Overwriting, rather than merging, seems to work ok.
transferIdInfo exported_id local_id
  = modifyIdInfo transfer exported_id
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setStrictnessInfo`    strictnessInfo local_info
                                 `setUnfoldingInfo`     unfoldingInfo local_info
                                 `setInlinePragInfo`    inlinePragInfo local_info
                                 `setSpecInfo`          addSpecInfo (specInfo exp_info) new_info
    new_info = setSpecInfoHead (idName exported_id)
                               (specInfo local_info)
        -- Remember to set the function-name field of the
        -- rules as we transfer them from one function to another
\end{code}
