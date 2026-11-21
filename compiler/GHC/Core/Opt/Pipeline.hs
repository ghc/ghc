{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}

module GHC.Core.Opt.Pipeline ( core2core, core2coreRetained, simplifyExpr, pairObservationOcc, pairObservationType, scalarRecipeTableOcc ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Plugins ( withPlugins, installCoreToDos )
import GHC.Driver.Env
import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.Config.Core.Lint ( endPass )
import GHC.Driver.Config.Core.Opt.LiberateCase ( initLiberateCaseOpts )
import GHC.Driver.Config.Core.Opt.Simplify ( initSimplifyOpts, initSimplMode, initGentleSimplMode )
import GHC.Driver.Config.Core.Opt.WorkWrap ( initWorkWrapOpts )
import GHC.Driver.Config.Core.Rules ( initRuleOpts )
import GHC.Platform.Ways  ( hasWay, Way(WayProf) )

import GHC.Builtin.KnownKeys (knownKeyOccName)
import GHC.Builtin.Modules (gHC_INTERNAL_ERR, gHC_TYPES)
import GHC.Builtin.PrimOps (PrimOp(RaiseOp))
import GHC.Builtin.WiredIn.Types
  (boolTy, charTy, naturalTy, pairTyCon, stringTy, tupleDataCon)
import GHC.Core
import GHC.Core.Make (getMkStringIds, mkCharExpr, mkListExpr, mkStringExprWith)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type
  (Type, getTyVar_maybe, mkVisFunTyManyU, splitForAllForAllTyBinders,
   splitFunTys, splitTyConApp_maybe)
import GHC.Iface.Env (allocateGlobalBinder, lookupNameCache)
import GHC.Tc.Utils.Env (lookupGlobal_maybe)
import GHC.Data.Maybe (MaybeErr(..))
import GHC.Types.Basic (Boxity(Boxed), LeftOrRight(..))
import GHC.Types.Id (idName, idType, isExportedId, isGlobalId, isPrimOpId_maybe,
                     isDataConWorkId_maybe, mkExportedVanillaId, setInlinePragma)
import GHC.Types.Name (getOccName, nameModule_maybe, nameOccName, nameSrcSpan)
import GHC.Types.Name.Occurrence (OccName, mkVarOcc, occNameString)
import GHC.Types.TyThing (TyThing(AnId))
import GHC.Types.Var (Var, VarBndr(Bndr), isId)
import GHC.Types.Var.Set (emptyVarSet, elemVarSet, extendVarSet)
import GHC.Core.Dependent
import GHC.Core.SimpleOpt (simpleOptNativeProgram, simpleOptPgm)
import GHC.Core.Opt.CSE  ( cseProgramWithMode )
import GHC.Core.Rules   ( RuleBase, ruleCheckProgram, getRules )
import GHC.Core.Ppr     ( pprCoreBindings, pprRules )
import GHC.Core.Utils   ( dumpIdInfoOfProgram, diffBinds, stripTicksE, exprType )
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Opt.Simplify ( SimplifyExprOpts, SimplifyOpts(..), simplifyPgm )
import GHC.Core.Opt.Simplify.Env (SimplMode(..))
import qualified GHC.Core.Opt.Simplify as Simplify
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Monad
import GHC.Core.Opt.OccurAnal (SimplCoreMode(..))
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.FloatIn      ( floatInwards )
import GHC.Core.Opt.FloatOut     ( floatOutwards )
import GHC.Core.Opt.LiberateCase ( liberateCase )
import GHC.Core.Opt.StaticArgs   ( doStaticArgs )
import GHC.Core.Opt.Specialise   ( specProgram)
import GHC.Core.Opt.SpecConstr   ( specConstrProgram)
import GHC.Core.Opt.DmdAnal
import GHC.Core.Opt.CprAnal      ( cprAnalProgram )
import GHC.Core.Opt.CallArity    ( callArityAnalProgram )
import GHC.Core.Opt.Exitify      ( exitifyProgram )
import GHC.Core.Opt.WorkWrap     ( wwTopBinds )
import GHC.Core.Opt.CallerCC     ( addCallerCostCentres )
import GHC.Core.LateCC.TopLevelBinds (topLevelBindsCCMG)
import GHC.Core.Seq (seqBinds)
import GHC.Core.FamInstEnv

import GHC.Utils.Error  ( withTiming, showPass )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module.ModGuts
import GHC.Unit.External ( ExternalUnitCache )

import GHC.Types.Id.Info
import GHC.Types.Tickish ( tickishIsCode )
import GHC.Types.Var.Env ( mkRnEnv2, emptyInScopeSet, mkVarEnv, lookupVarEnv )
import GHC.Types.InlinePragma ( CompilerPhase(..), neverInlinePragma )
import GHC.Types.Demand ( zapDmdEnvSig )
import GHC.Types.Name.Ppr
import GHC.Types.Unique.Supply ( UniqueTag(..) )

import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Maybe (catMaybes, isJust)
import GHC.Unit.Module

{-
************************************************************************
*                                                                      *
\subsection{The driver for the simplifier}
*                                                                      *
************************************************************************
-}

-- | Reject native dependent Core before simplifying a standalone expression.
simplifyExpr :: Logger -> ExternalUnitCache -> SimplifyExprOpts -> CoreExpr -> IO CoreExpr
simplifyExpr logger euc opts expr = do
  rejectNativeDependentExpr "expression simplification" expr
  Simplify.simplifyExpr logger euc opts expr

core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core = core2coreFor OrdinaryCore

-- | Run the standard optimizer for a module that passed the retained gate.
core2coreRetained :: HscEnv -> ModGuts -> IO ModGuts
core2coreRetained = core2coreFor RetainedCore

core2coreFor :: SimplCoreMode -> HscEnv -> ModGuts -> IO ModGuts
core2coreFor mode hsc_env guts@(ModGuts { mg_module  = mod
                                , mg_loc     = loc
                                , mg_rdr_env = rdr_env })
  = do { hpt_rule_base <- home_pkg_rules
       ; let planned = getCoreToDo dflags hpt_rule_base extra_vars
             builtin_passes = case mode of
               OrdinaryCore -> planned
               RetainedCore -> foldr retainedPass [] planned
             uniq_tag = SimplTag

       ; (guts2, stats) <- runCoreM hsc_env hpt_rule_base uniq_tag mod
                                    name_ppr_ctx loc $
                           do { hsc_env' <- getHscEnv
                              ; all_passes <- case mode of
                                  RetainedCore -> pure builtin_passes
                                  OrdinaryCore -> withPlugins (hsc_plugins hsc_env')
                                                    installCoreToDos builtin_passes
                              ; optimized <- runCorePasses mode all_passes guts
                              ; let final
                                      | RetainedCore <- mode
                                      = optimized { mg_binds = simpleOptNativeProgram (mg_binds optimized) }
                                      | otherwise = optimized
                              ; withTiming logger (text "Pair field observations" <+> brackets (ppr mod))
                                  (const ()) (addPairObservations final) }

       ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             FormatText
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags         = hsc_dflags hsc_env
    logger         = hsc_logger hsc_env
    unit_env       = hsc_unit_env hsc_env
    extra_vars | RetainedCore <- mode = []
               | otherwise = interactiveInScope (hsc_IC hsc_env)
    home_pkg_rules = hugRulesBelow hsc_env (moduleUnitId mod)
                      (GWIB { gwib_mod = moduleName mod, gwib_isBoot = NotBoot })
    name_ppr_ctx   = mkNamePprCtx ptc unit_env rdr_env
    ptc            = initPromotionTickContext dflags
    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.

    -- The caller has already run restricted native desugaring.
    retainedPass CoreDesugarOpt rest = rest
    retainedPass (CoreDoFloatOutwards opts) rest =
      CoreDoFloatOutwards (opts { floatCoreMode = RetainedCore }) : rest
    retainedPass (CoreDoSimplify opts) rest =
      CoreDoSimplify (opts { so_mode = (so_mode opts) { sm_core_mode = RetainedCore } }) : rest
    retainedPass pass rest = pass : rest

{- Note [Executable pair field observations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Choose the ABI from the exported ordinary signature. A replacement body
must supply the same symbol even when its proof becomes false. The
pair ABI covers unrestricted arrows from a scalar codec to a pair of
scalar codecs. Relay entries support unrestricted arrows between the
same codecs. In the library error module, entries are also available
for functions with a universally quantified result. Their value arity
comes from their checked type. These restrictions bound code growth.

Read final Core after the user's RULES and plugins. Use the exact raise#
primitive as a proof of nonreturn. Serialize an imported call's exact
owner and value arity as a recipe. An older object can lack its hidden
entry without losing an ordinary symbol. The type checker resolves
the recipe in the interpreter, where each link is optional. The object
never refers to an imported proof closure from native code.

The first-field observer keeps its original name. The second-field
observer has a distinct name. A first-field-only object can still
supply the first observer after recompilation of its interface.

Publish one String table when the module can emit a scalar recipe.
An empty table keeps the same entry after an object-only replacement.
Only the interpreter looks up the table. Older objects can still provide
per-owner scalar entries.
-}

-- | Hidden executable entry with the ordinary owner's stable ABI arity.
pairObservationOcc :: Maybe LeftOrRight -> Int -> OccName -> OccName
pairObservationOcc side arity owner =
    mkVarOcc ("$readback2:" ++ side_tag ++ show arity ++ ":" ++ occNameString owner)
  where
    side_tag = case side of
      Just CRight -> "right:"
      _ -> ""

-- | Stable module entry for the scalar recipes. An empty table keeps the
-- publication marker across a later object-only replacement.
scalarRecipeTableOcc :: OccName
scalarRecipeTableOcc = mkVarOcc "$readback2:scalar-table"

-- | Recognize an ordinary producer supported by the first observation ABI.
pairObservationType :: Type -> Maybe (Type, Type)
pairObservationType ty = do
    (input, result) <- observationArrow ty
    (constructor, [left, right]) <- splitTyConApp_maybe result
    guard (constructor == pairTyCon && scalarCodec left && scalarCodec right)
    pure (input, result)

-- | One unrestricted argument with an existing scalar readback codec.
observationArrow :: Type -> Maybe (Type, Type)
observationArrow ty = do
    let (_, args, result) = splitFunTys ty
    [arg] <- pure args
    let input = scaledThing arg
    guard (scalarCodec input && ty `eqType` mkVisFunTyManyU input result)
    pure (input, result)

-- | Scalar codecs for the bounded native ABI.
scalarCodec :: Type -> Bool
scalarCodec ty = any (eqType ty) [boolTy, charTy, naturalTy]

-- | Publish hidden native entries for checked pair observations.
addPairObservations :: ModGuts -> CoreM ModGuts
addPairObservations guts = do
    env <- getHscEnv
    let emit occ owner rhs = do
          name <- liftIO $ allocateGlobalBinder (hsc_NC env) this_mod occ
                    Nothing (nameSrcSpan (idName owner))
          let entry = mkExportedVanillaId name (exprType rhs)
          pure (NonRec (setInlinePragma entry neverInlinePragma) rhs)

        typedEntry mod occ = do
          name <- lookupNameCache (hsc_NC env) mod occ
          thing <- lookupType env name
          pure $ case thing of
            Just (AnId entry) -> idType entry `eqType` stringTy
            _ -> False

        importedProof owner arity
          | Just mod <- nameModule_maybe (idName owner)
          , RealUnit _ <- moduleUnit mod
          , mod /= this_mod = liftIO $ do
              let occ = nameOccName (idName owner)
              legacy <- typedEntry mod (pairObservationOcc Nothing arity occ)
              table <- if relayArity mod owner == Just arity
                         then typedEntry mod scalarRecipeTableOcc else pure False
              pure $ if legacy || table
                then show (unitString (moduleUnit mod), moduleNameString (moduleName mod),
                           occNameString occ, arity)
                else ""
          | otherwise = pure ""

        proof seen expr = case collectArgs expr of
          (Var fn, args)
            | isPrimOpId_maybe fn == Just RaiseOp
            , valArgCount args == 1 -> pure "!"
            | null args
            , Just body <- lookupVarEnv bodies fn
            , not (fn `elemVarSet` seen) -> proof (extendVarSet seen fn) body
            | isGlobalId fn
            , all (not . isCoArg) args -> importedProof fn (valArgCount args)
          (Case _ _ _ [Alt _ _ body], []) -> proof seen body
          (Tick _ body, []) -> proof seen body
          _ -> pure ""

        addPair (owner, rhs)
          | isExportedId owner
          , isJust (pairObservationType (idType owner)) = do
              (left, right) <- case collectBinders rhs of
                ([_], body)
                  | (Var con, [Type _, Type _, x, y]) <- collectArgs body
                  , isDataConWorkId_maybe con == Just (tupleDataCon Boxed 2) ->
                      (,) <$> proof emptyVarSet x <*> proof emptyVarSet y
                _ -> pure ("", "")
              let entry side flag = emit (pairObservationOcc (Just side) 1 (getOccName owner))
                                         owner (recipe flag)
              sequence [entry CLeft right, entry CRight left]
          | otherwise = pure []

        addRelay (owner, rhs)
          | isExportedId owner
          , Just arity <- relayArity this_mod owner = do
              let (params, body) = collectBinders rhs
              flag <- if length (filter isId params) == arity
                        then proof emptyVarSet body
                        else pure ""
              pure (Just (owner, arity, flag))
          | otherwise = pure Nothing
    pair_entries <- concat <$> mapM addPair bindings
    relay_rows <- catMaybes <$> mapM addRelay bindings
    table_entry <- case relay_rows of
      [] -> pure []
      (owner, _, _):_ -> do
        let rows = [(occNameString (getOccName fn), arity, flag)
                   | (fn, arity, flag) <- relay_rows, not (null flag)]
        packed <- liftIO $ packTable env (show rows)
        (:[]) <$> emit scalarRecipeTableOcc owner packed
    pure guts{mg_binds = mg_binds guts ++ pair_entries ++ table_entry}
  where
    this_mod = mg_module guts
    bindings = flattenBinds (mg_binds guts)
    bodies = mkVarEnv bindings
    recipe = mkListExpr charTy . map mkCharExpr

    -- Use the standard String codec for compiler metadata.
    -- Early library builds can use the literal list when the codec is absent.
    packTable env text = do
      ids <- runMaybeT (getMkStringIds findUnpacker)
      pure (maybe (recipe text) (`mkStringExprWith` text) ids)
      where
        findUnpacker key = MaybeT $ do
          name <- lookupNameCache (hsc_NC env) gHC_TYPES (knownKeyOccName key)
          thing <- lookupGlobal_maybe env name
          pure $ case thing of
            Succeeded (AnId unpacker) -> Just unpacker
            _ -> Nothing

    relayArity mod owner
      | Just (_, result) <- observationArrow (idType owner)
      , scalarCodec result = Just 1
      | mod == gHC_INTERNAL_ERR
      , let (parameters, body) = splitForAllForAllTyBinders (idType owner)
            (_, args, result) = splitFunTys body
      , Just variable <- getTyVar_maybe result
      , variable `elem` [v | (_, Bndr v _) <- parameters] = Just (length args)
      | otherwise = Nothing

{-
************************************************************************
*                                                                      *
           Generating the main optimisation pipeline
*                                                                      *
************************************************************************
-}

getCoreToDo :: DynFlags -> RuleBase -> [Var] -> [CoreToDo]
-- This function builds the pipeline of optimisations
getCoreToDo dflags hpt_rule_base extra_vars
  = flatten_todos core_todo
  where
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
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
    profiling     = ways dflags `hasWay` WayProf

    do_presimplify = do_specialise -- TODO: any other optimizations benefit from pre-simplification?
    do_simpl3      = const_fold || rules_on -- TODO: any other optimizations benefit from three-phase simplification?

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    maybe_strictness_before (Phase phase)
      | phase `elem` strictnessBefore dflags = CoreDoDemand False
    maybe_strictness_before _
      = CoreDoNothing

    simpl_phase phase name iter
      = CoreDoPasses
      $   [ maybe_strictness_before phase
          , CoreDoSimplify $ initSimplifyOpts dflags extra_vars iter
                             (initSimplMode dflags phase name) hpt_rule_base
          , maybe_rule_check phase ]

    -- Run GHC's internal simplification phase, after all rules have run.
    -- See Note [Compiler phases] in GHC.Types.Basic
    simplify name = simpl_phase FinalPhase name max_iter

    -- initial simplify: mk specialiser happy: minimum effort please
    -- See Note [Inline in InitialPhase]
    -- See Note [RULEs enabled in InitialPhase]
    simpl_gently = CoreDoSimplify $ initSimplifyOpts dflags extra_vars max_iter
                                    (initGentleSimplMode dflags) hpt_rule_base

    dmd_cpr_ww = if ww_on then [CoreDoDemand True,CoreDoCpr,CoreDoWorkerWrapper]
                          else [CoreDoDemand False] -- NB: No CPR! See Note [Don't change boxity without worker/wrapper]


    demand_analyser = (CoreDoPasses (
                           dmd_cpr_ww ++
                           [simplify "post-worker-wrapper"]
                           ))

    add_caller_ccs =
        runWhen (profiling && not (null $ callerCcFilters dflags)) CoreAddCallerCcs

    add_late_ccs =
        runWhen (profiling && gopt Opt_ProfLateInlineCcs dflags) $ CoreAddLateCcs

    core_todo =
     [
        -- We always perform a run of the simple optimizer after desugaring to
        -- remove really bad code
        CoreDesugarOpt,

        -- We want to do the static argument transform before full laziness as it
        -- may expose extra opportunities to float things outwards. However, to fix
        -- up the output of the transformation we need at do at least one simplify
        -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- initial simplify: mk specialiser happy: minimum effort please
        runWhen do_presimplify simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        runWhen do_specialise CoreDoSpecialising,

        runWhen full_laziness
           (CoreDoFloatOutwards $ FloatOutSwitches
                { floatCoreMode       = OrdinaryCore
                , floatOutLambdas     = Just 0
                , floatOutConstants   = True
                , floatOutOverSatApps = False
                , floatToTopLevelOnly = False
                , floatJoinsToTop     = False  -- Initially, don't float join points at all
                }),
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

        -- Run the simplifier phases 2,1,0 to allow rewrite rules to fire
        runWhen do_simpl3
            (CoreDoPasses $ [ simpl_phase (Phase phase) "main" max_iter
                            | phase <- [phases, phases-1 .. 1] ] ++
                            [ simpl_phase (Phase 0) "main" (max max_iter 3) ]),
                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simplifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!

        runWhen do_float_in CoreDoFloatInwards,
            -- Run float-inwards immediately before the strictness analyser
            -- Doing so pushes bindings nearer their use site and hence makes
            -- them more likely to be strict. These bindings might only show
            -- up after the inlining from simplification.  Example in fulsom,
            -- Csg.calc, where an arg of timesDouble thereby becomes strict.

        runWhen call_arity $ CoreDoPasses
            [ CoreDoCallArity
            , simplify "post-call-arity"
            ],

        -- Strictness analysis
        runWhen strictness demand_analyser,

        runWhen exitification CoreDoExitify,
            -- See Note [Placement of the exitification pass]

        -- nofib/spectral/hartel/wang doubles in speed if you
        -- do full laziness late in the day.  It only happens
        -- after fusion and other stuff, so the early pass doesn't
        -- catch it.  For the record, the redex is
        --        f_el22 (f_el21 r_midblock)
        runWhen full_laziness $ CoreDoFloatOutwards $ FloatOutSwitches
               { floatCoreMode       = OrdinaryCore
               , floatOutLambdas     = floatLamArgs dflags
               , floatOutConstants   = True
               , floatOutOverSatApps = True
               , floatToTopLevelOnly = False
               , floatJoinsToTop     = True },
                -- floatJoinsToTop: floating joins to the top makes a huge difference to
                -- spectral/minimax; see XXX


        runWhen cse CoreCSE,
                -- We want CSE to follow the final full-laziness pass, because it may
                -- succeed in commoning up things floated out by full laziness.
                -- CSE used to rely on the no-shadowing invariant, but it doesn't any more

        runWhen do_float_in CoreDoFloatInwards,

        simplify "final",  -- Final tidy-up

        maybe_rule_check FinalPhase,

        --------  After this we have -O2 passes -----------------
        -- None of them run with -O

                -- Case-liberation for -O2.  This should be after
                -- strictness analysis and the simplification which follows it.
        runWhen liberate_case $ CoreDoPasses
           [ CoreLiberateCase, simplify "post-liberate-case" ],
           -- Run the simplifier after LiberateCase to vastly
           -- reduce the possibility of shadowing
           -- Reason: see Note [Shadowing in SpecConstr] in GHC.Core.Opt.SpecConstr

        runWhen spec_constr $ CoreDoPasses
           [ CoreDoSpecConstr, simplify "post-spec-constr"],
           -- See Note [Simplify after SpecConstr]

        maybe_rule_check FinalPhase,

        runWhen late_specialise $ CoreDoPasses
           [ CoreDoSpecialising, simplify "post-late-spec"],

        -- LiberateCase can yield new CSE opportunities because it peels
        -- off one layer of a recursive function (concretely, I saw this
        -- in wheel-sieve1), and I'm guessing that SpecConstr can too
        -- And CSE is a very cheap pass. So it seems worth doing here.
        runWhen ((liberate_case || spec_constr) && cse) $ CoreDoPasses
           [ CoreCSE, simplify "post-final-cse" ],

        ---------  End of -O2 passes --------------

        runWhen late_dmd_anal $ CoreDoPasses (
            dmd_cpr_ww ++ [simplify "post-late-ww"]
          ),

        -- Final run of the demand_analyser, ensures that one-shot thunks are
        -- really really one-shot thunks. Only needed if the demand analyser
        -- has run at all. See Note [Final Demand Analyser run] in GHC.Core.Opt.DmdAnal
        -- It is EXTREMELY IMPORTANT to run this pass, otherwise execution
        -- can become /exponentially/ more expensive. See #11731, #12996.
        runWhen (strictness || late_dmd_anal) (CoreDoDemand False),

        maybe_rule_check FinalPhase,

        add_caller_ccs,
        add_late_ccs
     ]

    -- Remove 'CoreDoNothing' and flatten 'CoreDoPasses' for clarity.
    flatten_todos [] = []
    flatten_todos (CoreDoNothing : rest) = flatten_todos rest
    flatten_todos (CoreDoPasses passes : rest) =
      flatten_todos passes ++ flatten_todos rest
    flatten_todos (todo : rest) = todo : flatten_todos rest

-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing

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
    simplification in between meant that the carefully constructed
    SpecConstr rule never got to fire.  (It was something like
          lvl = f a   -- Arity 1
          ....g lvl....
    SpecConstr specialised g for argument lvl; but Specialise then
    specialised lvl = f a to lvl = $sf, and inlined. Or something like
    that.)

2.  Specialise relies on unfoldings being available for top-level dictionary
    bindings; but SpecConstr kills them all!  The Simplifer restores them.

This extra run of the simplifier has a cost, but this is only with -O2.


************************************************************************
*                                                                      *
                  The CoreToDo interpreter
*                                                                      *
************************************************************************
-}

runCorePasses :: SimplCoreMode -> [CoreToDo] -> ModGuts -> CoreM ModGuts
runCorePasses mode passes guts
  = do { when (mode == OrdinaryCore) $ liftIO $
           rejectNativeDependentProgram "the Core optimization pipeline" (mg_binds guts) (mg_rules guts)
       ; foldM do_pass guts passes }
  where
    do_pass guts CoreDoNothing = return guts
    do_pass guts (CoreDoPasses ps) = foldM do_pass guts ps
    do_pass guts pass = do
      logger <- getLogger
      withTiming logger (ppr pass <+> brackets (ppr mod))
                   (const ()) $ do
            guts' <- lintAnnots (ppr pass) (doCorePassFor mode pass) guts
            case pass of
              CoreDoPluginPass{} -> liftIO $
                rejectNativeDependentProgram "the output of a Core plugin" (mg_binds guts') (mg_rules guts')
              _ -> pure ()
            endPass pass (mg_binds guts') (mg_rules guts')
            return guts'

    mod = mg_module guts

doCorePassFor :: SimplCoreMode -> CoreToDo -> ModGuts -> CoreM ModGuts
doCorePassFor core_mode pass guts = do
  logger    <- getLogger
  hsc_env   <- getHscEnv
  dflags    <- getDynFlags
  us        <- getUniqueSupplyM
  p_fam_env <- getPackageFamInstEnv
  let platform = targetPlatform dflags
  let fam_envs = (p_fam_env, mg_fam_inst_env guts)
  let updateBinds  f = return $ guts { mg_binds = f (mg_binds guts) }
  let updateBindsM f = f (mg_binds guts) >>= \b' -> return $ guts { mg_binds = b' }
  let updateBindsAndRulesM f = f (mg_binds guts) (mg_rules guts) >>= \(b',r') -> return $ guts { mg_binds = b', mg_rules = r' }
  -- Important to force this now as name_ppr_ctx lives through an entire phase in
  -- the optimiser and if it's not forced then the entire previous `ModGuts` will
  -- be retained until the end of the phase. (See #24328 for more analysis)
  let !rdr_env = mg_rdr_env guts
  let name_ppr_ctx =
        mkNamePprCtx
          (initPromotionTickContext dflags)
          (hsc_unit_env hsc_env)
          rdr_env


  case pass of
    CoreDesugarOpt            -> {-# SCC "DesugarOpt" #-}
                                 updateBindsAndRulesM (desugarOpt dflags logger (mg_module guts))

    CoreDoSimplify opts       -> {-# SCC "Simplify" #-}
                                 liftIOWithCount $ simplifyPgm logger (hsc_unit_env hsc_env) name_ppr_ctx opts guts

    CoreCSE                   -> {-# SCC "CommonSubExpr" #-}
                                 updateBinds (cseProgramWithMode core_mode)

    CoreLiberateCase          -> {-# SCC "LiberateCase" #-}
                                 updateBinds (liberateCase (initLiberateCaseOpts dflags))

    CoreDoFloatInwards        -> {-# SCC "FloatInwards" #-}
                                 updateBinds (floatInwards platform)

    CoreDoFloatOutwards f     -> {-# SCC "FloatOutwards" #-}
                                 updateBindsM (liftIO . floatOutwards logger f us)

    CoreDoStaticArgs          -> {-# SCC "StaticArgs" #-}
                                 updateBinds (doStaticArgs us)

    CoreDoCallArity           -> {-# SCC "CallArity" #-}
                                 updateBinds callArityAnalProgram

    CoreDoExitify             -> {-# SCC "Exitify" #-}
                                 updateBinds exitifyProgram

    CoreDoDemand before_ww    -> {-# SCC "DmdAnal" #-}
                                 updateBindsM (liftIO . dmdAnal logger before_ww dflags fam_envs (mg_rules guts))

    CoreDoCpr                 -> {-# SCC "CprAnal" #-}
                                 updateBindsM (liftIO . cprAnalProgram logger fam_envs)

    CoreDoWorkerWrapper       -> {-# SCC "WorkWrap" #-}
                                 updateBinds (wwTopBinds core_mode
                                               (initWorkWrapOpts (mg_module guts) dflags fam_envs)
                                               us)

    CoreDoSpecialising        -> {-# SCC "Specialise" #-}
                                 specProgram core_mode guts

    CoreDoSpecConstr          -> {-# SCC "SpecConstr" #-}
                                 specConstrProgram guts

    CoreAddCallerCcs          -> {-# SCC "AddCallerCcs" #-}
                                 addCallerCostCentres guts

    CoreAddLateCcs            -> {-# SCC "AddLateCcs" #-}
                                 topLevelBindsCCMG guts

    CoreDoPrintCore           -> {-# SCC "PrintCore" #-}
                                 liftIO $ printCore logger (mg_binds guts) >> return guts

    CoreDoRuleCheck phase pat -> {-# SCC "RuleCheck" #-}
                                 ruleCheckPass phase pat guts
    CoreDoNothing             -> return guts
    CoreDoPasses passes       -> runCorePasses core_mode passes guts

    CoreDoPluginPass _ p      -> {-# SCC "Plugin" #-} p guts

    CoreDesugar               -> pprPanic "doCorePass" (ppr pass)
    CoreTidy                  -> pprPanic "doCorePass" (ppr pass)
    CorePrep                  -> pprPanic "doCorePass" (ppr pass)

{-
************************************************************************
*                                                                      *
\subsection{Core pass combinators}
*                                                                      *
************************************************************************
-}

printCore :: Logger -> CoreProgram -> IO ()
printCore logger binds
    = Logger.logDumpMsg logger "Print Core" (pprCoreBindings binds)

ruleCheckPass :: CompilerPhase -> String -> ModGuts -> CoreM ModGuts
ruleCheckPass current_phase pat guts = do
    dflags <- getDynFlags
    logger <- getLogger
    withTiming logger (text "RuleCheck"<+>brackets (ppr $ mg_module guts))
                (const ()) $ do
        rule_env <- initRuleEnv guts
        let rule_fn fn = getRules rule_env fn
            ropts = initRuleOpts dflags
        liftIO $ logDumpMsg logger "Rule check"
                     (ruleCheckProgram ropts current_phase pat
                        rule_fn (mg_binds guts))
        return guts

dmdAnal :: Logger -> Bool -> DynFlags -> FamInstEnvs -> [CoreRule] -> CoreProgram -> IO CoreProgram
dmdAnal logger before_ww dflags fam_envs rules binds = do
  let !opts = DmdAnalOpts
               { dmd_strict_dicts    = gopt Opt_DictsStrict dflags
               , dmd_do_boxity       = before_ww -- only run Boxity Analysis immediately preceding WW
               , dmd_unbox_width     = dmdUnboxWidth dflags
               , dmd_max_worker_args = maxWorkerArgs dflags
               }
      binds_plus_dmds = dmdAnalProgram opts fam_envs rules binds
  Logger.putDumpFileMaybe logger Opt_D_dump_dmd_signatures "Demand signatures" FormatText $
    dumpIdInfoOfProgram (hasPprDebug dflags) (ppr . zapDmdEnvSig . dmdSigInfo) binds_plus_dmds
  -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
  seqBinds binds_plus_dmds `seq` return binds_plus_dmds


-- | Simple optimization after desugaring.
--
-- This performs some quick basic optimizations even with -O0.
-- See Note [The simple optimiser] for details.
--
-- We could call it directly in the desugarer but we implement it as the first
-- Core-to-Core pass to accomodate Core plugins that want to see Core even
-- before the first (simple) optimization took place. See #23337
desugarOpt :: DynFlags -> Logger -> Module -> CoreProgram -> [CoreRule] -> CoreM (CoreProgram,[CoreRule])
desugarOpt dflags logger mod binds rules = liftIO $ do
  rejectNativeDependentProgram "simpleOptPgm" binds rules
  let simpl_opts = initSimpleOpts dflags
  let !(ds_binds, ds_rules_for_imps, occ_anald_binds) = simpleOptPgm simpl_opts mod binds rules

  putDumpFileMaybe logger Opt_D_dump_occur_anal "Occurrence analysis"
    FormatCore (pprCoreBindings occ_anald_binds $$ pprRules ds_rules_for_imps )

  pure (ds_binds, ds_rules_for_imps)

{-
************************************************************************
*                                                                      *
\subsection{Annotation Linting}
*                                                                      *
************************************************************************
-}

-- | This checks whether a pass correctly looks through debug
-- annotations (@SourceNote@). This works a bit different from other
-- consistency checks: We check this by running the given task twice,
-- noting all differences between the results.
lintAnnots :: SDoc -> (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
lintAnnots pname pass guts = {-# SCC "lintAnnots" #-} do
  -- Run the pass as we normally would
  dflags <- getDynFlags
  logger <- getLogger
  when (gopt Opt_DoAnnotationLinting dflags) $
    liftIO $ showPass logger "Annotation linting - first run"
  -- If appropriate re-run it without debug annotations to make sure
  -- that they made no difference.
  if gopt Opt_DoAnnotationLinting dflags
    then do
      nguts <- pass guts
      liftIO $ showPass logger "Annotation linting - second run"
      nguts' <- withoutAnnots pass guts
      -- Finally compare the resulting bindings
      liftIO $ showPass logger "Annotation linting - comparison"
      let binds = flattenBinds $ mg_binds nguts
          binds' = flattenBinds $ mg_binds nguts'
          (diffs,_) = diffBinds True (mkRnEnv2 emptyInScopeSet) binds binds'
      when (not (null diffs)) $ putMsg $ vcat
        [ text "*** Annotation linting: the result of" <+> pname
            <+> text "depends on debug annotations ***"
        , text "Core changes with annotations:"
        , withPprStyle defaultDumpStyle $ nest 2 $ vcat diffs
        ]
      return nguts
    else
      pass guts

-- | Run the given pass without annotations. This means that we both
-- set the debugLevel setting to 0 in the environment as well as all
-- annotations from incoming modules.
withoutAnnots :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
withoutAnnots pass guts = do
  -- Remove debug flag from environment.
  -- TODO: supply tag here as well ?
  let withoutFlag = mapDynFlagsCoreM $ \(!dflags) -> dflags { debugLevel = 0 }
  -- Nuke existing ticks in module.
  -- TODO: Ticks in unfoldings. Maybe change unfolding so it removes
  -- them in absence of debugLevel > 0.
  let nukeTicks = stripTicksE (not . tickishIsCode)
      nukeAnnotsBind :: CoreBind -> CoreBind
      nukeAnnotsBind bind = case bind of
        Rec bs     -> Rec $ map (\(b,e) -> (b, nukeTicks e)) bs
        NonRec b e -> NonRec b $ nukeTicks e
      nukeAnnotsMod mg@ModGuts{mg_binds=binds}
        = mg{mg_binds = map nukeAnnotsBind binds}
  -- Perform pass with all changes applied. Drop the simple count so it doesn't
  -- effect the total also
  dropSimplCount $ withoutFlag $ pass (nukeAnnotsMod guts)
