{-# LANGUAGE CPP #-}

module GHC.Core.Opt.Simplify
  ( SimplifyExprOpts(..), SimplifyOpts(..)
  , simplifyExpr, simplifyPgm
  ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.FVs (ruleFreeVars)
import GHC.Core.Rules
import GHC.Core.Ppr     ( pprCoreBindings, pprCoreExpr )
import GHC.Core.Opt.CompUnit (coreCompUnitTimingDoc, forceCompUnit)
import GHC.Core.Opt.OccurAnal ( occurAnalyseCompUnit, occurAnalyseExpr )
import GHC.Core.Stats   ( coreBindsSize, coreBindsStats, exprSize )
import GHC.Core.Utils   ( mkTicks, stripTicksTop )
import GHC.Core.Lint    ( LintPassResultConfig, dumpPassResult, lintPassResult )
import GHC.Core.Opt.Simplify.Iteration ( simplTopBinds, simplExpr, simplImpRules )
import GHC.Core.Opt.Simplify.Utils  ( activeRule )
import GHC.Core.Opt.Simplify.Inline ( activeUnfolding )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats ( simplCountN )
import GHC.Core.FamInstEnv

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Panic (panic, pprPanic)

import GHC.Unit.Env ( UnitEnv, ueEPS )
import GHC.Unit.External
import GHC.Unit.Module (Module)
import GHC.Unit.Module.Deps (Dependencies)
import GHC.Unit.Module.ModGuts

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.InlinePragma
import GHC.Types.Var.Env
import GHC.Types.Tickish
import GHC.Types.Unique.FM
import GHC.Types.Var.Set

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad
import Data.Foldable ( for_ )

{-
************************************************************************
*                                                                      *
        Gentle simplification
*                                                                      *
************************************************************************
-}

-- | Configuration record for `simplifyExpr`.
-- The values of this datatype are /only/ driven by the demands of that function.
data SimplifyExprOpts = SimplifyExprOpts
  { se_fam_inst :: ![FamInst]
  , se_mode :: !SimplMode
  , se_top_env_cfg :: !TopEnvConfig
  }

simplifyExpr :: Logger
             -> ExternalUnitCache
             -> SimplifyExprOpts
             -> CoreExpr
             -> IO CoreExpr
-- ^ Simplify an expression using 'simplExprGently'.
--
-- See 'simplExprGently' for details.
simplifyExpr logger euc opts expr
  = withTiming logger (text "Simplify [expr]") (const ()) $
    do  { eps <- eucEPS euc ;
        ; let fam_envs = ( eps_fam_inst_env eps
                         , extendFamInstEnvList emptyFamInstEnv $ se_fam_inst opts
                         )
              simpl_env = mkSimplEnv (se_mode opts) fam_envs
              top_env_cfg = se_top_env_cfg opts
              read_eps_rules = eps_rule_base <$> eucEPS euc
              read_ruleenv = updExternalPackageRules emptyRuleEnv <$> read_eps_rules

        ; let sz = exprSize expr

        ; (expr', counts) <- initSmpl logger read_ruleenv top_env_cfg sz $
                             simplExprGently simpl_env expr

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
                  "Simplifier statistics" FormatText (pprSimplCount counts)

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl "Simplified expression"
                        FormatCore
                        (pprCoreExpr expr')

        ; return expr'
        }

simplExprGently :: SimplEnv -> CoreExpr -> SimplM CoreExpr
-- ^ Simplifies an expression by doing occurrence analysis, then simplification,
-- and repeating (twice currently), because one pass alone leaves tons of crud.
--
-- Used only:
--
--   1. for user expressions typed in at the interactive prompt (see 'GHC.Driver.Main.hscStmt'),
--   2. for Template Haskell splices (see 'GHC.Tc.Gen.Splice.runMeta').
--
-- The name 'Gently' suggests that the SimplMode is InitialPhase,
-- and in fact that is so.... but the 'Gently' in 'simplExprGently' doesn't
-- enforce that; it just simplifies the expression twice.
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

-- | Configuration record for `simplifyPgm`.
-- The values of this datatype are /only/ driven by the demands of that function.
data SimplifyOpts = SimplifyOpts
  { so_dump_core_sizes :: !Bool
  , so_iterations      :: !Int
  , so_mode            :: !SimplMode

  , so_pass_result_cfg :: !(Maybe LintPassResultConfig)
                          -- Nothing => Do not Lint
                          -- Just cfg => Lint like this

  , so_hpt_rules       :: !RuleBase
  , so_top_env_cfg     :: !TopEnvConfig
  }

data StaticSimplInput = StaticSimplInput
  { ssi_module       :: !Module
  , ssi_deps         :: !Dependencies
  , ssi_fam_inst_env :: !FamInstEnv
  }

data DynamicSimplData = DynamicSimplData
  { dsd_binds :: !CoreProgram
  , dsd_rules :: ![CoreRule]
  }

splitModGuts :: ModGuts -> (StaticSimplInput, DynamicSimplData)
splitModGuts guts =
  ( StaticSimplInput
      { ssi_module       = mg_module guts
      , ssi_deps         = mg_deps guts
      , ssi_fam_inst_env = mg_fam_inst_env guts
      }
  , DynamicSimplData
      { dsd_binds = mg_binds guts
      , dsd_rules = mg_rules guts
      }
  )

makeGuts :: ModGuts -> StaticSimplInput -> DynamicSimplData -> ModGuts
makeGuts guts _static dyn = guts { mg_binds = dsd_binds dyn, mg_rules = dsd_rules dyn }

simplifyPgm :: Logger
            -> UnitEnv
            -> NamePprCtx                -- For dumping
            -> SimplifyOpts
            -> ModGuts
            -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgm logger unit_env name_ppr_ctx opts guts = do
  let (static, dyn) = splitModGuts guts
  (count, dyn_out) <- simplifyPgm' logger unit_env name_ppr_ctx opts static dyn
  pure (count, makeGuts guts static dyn_out)

simplifyPgm' :: Logger
            -> UnitEnv
            -> NamePprCtx                -- For dumping
            -> SimplifyOpts
            -> StaticSimplInput
            -> DynamicSimplData
            -> IO (SimplCount, DynamicSimplData)  -- New bindings

simplifyPgm' logger unit_env name_ppr_ctx opts
            static dyn
  = do { let (shared_rules, units0)
                 | [_] <- dsd_binds dyn = (dsd_rules dyn, dsd_binds dyn)
                 | otherwise = distributeLocalRules (dsd_binds dyn) (dsd_rules dyn)
             hpt_rule_env = mkRuleEnv this_mod deps shared_rules units0
                                      emptyRuleBase (so_hpt_rules opts)

        ; eps <- ueEPS unit_env
        ; let fam_envs = (eps_fam_inst_env eps, fam_inst_env)
              simpl_env = mkSimplEnv mode fam_envs

        ; unit_results <- run_units (simplify_unit hpt_rule_env simpl_env shared_rules) units0
        ; let (units1, unit_infos, unit_counts) = unzip3 unit_results
              counts_out = foldr plusSimplCount zero_counts unit_counts
              dyn_out = dyn_no_binds { dsd_binds = units1, dsd_rules = shared_rules }

        ; when (logHasDumpFlag logger Opt_D_verbose_core2core
                && logHasDumpFlag logger Opt_D_dump_simpl_stats) $
          logDumpMsg logger
                  "Simplifier statistics for following pass"
                  (vcat [text "Simplifier finished per-unit iterations"
                            <+> ppr (map snd unit_infos),
                         blankLine,
                         pprSimplCount counts_out])

        ; return (counts_out, dyn_out)
    }
  where
    this_mod = ssi_module static
    deps = ssi_deps static
    fam_inst_env = ssi_fam_inst_env static
    dump_core_sizes = so_dump_core_sizes opts
    mode            = so_mode opts
    max_iterations  = so_iterations opts
    top_env_cfg     = so_top_env_cfg opts
    active_rule     = activeRule mode
    active_unf      = activeUnfolding mode
    -- Note the bang in !dyn_no_binds.  If you don't force `dyn_no_binds`
    -- the old bindings are retained until the end of all simplifier iterations
    !dyn_no_binds = DynamicSimplData { dsd_binds = [], dsd_rules = [] }

    zero_counts = zeroSimplCount $ logHasDumpFlag logger Opt_D_dump_simpl_stats

    run_units
      :: (CoreCompUnit -> IO (CoreCompUnit, (String, Int), SimplCount))
      -> [CoreCompUnit]
      -> IO [(CoreCompUnit, (String, Int), SimplCount)]
    run_units f units
      | parallel_units = mapParallelIO timed_f (zip [1 :: Int ..] units)
      | otherwise      = mapM f units
      where
        total_units = length units
        parallel_units = length units > 1 && not disable_parallel

        disable_parallel =
             logHasDumpFlag logger Opt_D_dump_occur_anal
          || logHasDumpFlag logger Opt_D_dump_simpl_iterations

        timed_f (unit_no, unit)
          | logHasDumpFlag logger Opt_D_dump_timings
          = withTiming logger
              (coreCompUnitTimingDoc "Simplify" unit_no total_units unit)
              force_unit_result
              (f unit)
          | otherwise
          = f unit

        force_unit_result (unit', _, count) =
          forceCompUnit unit' `seq` count `seq` ()

    mapParallelIO :: (a -> IO b) -> [a] -> IO [b]
    mapParallelIO f xs = mask $ \restore -> do
      workers <- forM xs $ \x -> do
        result_var <- newEmptyMVar
        tid <- forkIO $ do
          result <- try (restore (f x))
          putMVar result_var result
        pure (tid, result_var)

      let kill_workers = mapM_ (killThread . fst) workers

      let wait_workers [] = pure []
          wait_workers ((_, result_var):rest) = do
            result <- takeMVar result_var
            case result of
              Left err -> do
                kill_workers
                throwIO (err :: SomeException)
              Right y -> (y :) <$> wait_workers rest

      wait_workers workers

    simplify_unit
      :: RuleEnv
      -> SimplEnv
      -> [CoreRule]
      -> CoreCompUnit
      -> IO (CoreCompUnit, (String, Int), SimplCount)
    simplify_unit hpt_rule_env simpl_env shared_rules unit0
      = do_iteration 1 [] unit0
      where
        do_iteration :: Int -> [SimplCount] -> CoreCompUnit
                     -> IO (CoreCompUnit, (String, Int), SimplCount)
        do_iteration iteration_no counts_so_far unit
          | iteration_no > max_iterations
          = warnPprTrace (debugIsOn && (max_iterations > 2))
                "Simplifier bailing out"
                ( hang (ppr this_mod <> text ", after"
                        <+> int max_iterations <+> text "iterations"
                        <+> (brackets $ hsep $ punctuate comma $
                             map (int . simplCountN) (reverse counts_so_far)))
                     2 (text "Size =" <+> ppr (coreBindsStats bind_list))) $
            return ( unit
                   , ("Simplifier bailed out", iteration_no - 1)
                   , totalise counts_so_far )

          | let sz = coreBindsSize bind_list
          , () <- sz `seq` ()
          = do
              let CoreCompUnit tagged_binds tagged_rules =
                    {-# SCC "OccAnal" #-}
                    occurAnalyseCompUnit this_mod active_unf active_rule shared_rules unit

              Logger.putDumpFileMaybe logger Opt_D_dump_occur_anal "Occurrence analysis"
                        FormatCore
                        (pprCoreBindings tagged_binds)

              let !base_rule_env = updLocalRules hpt_rule_env (shared_rules ++ tagged_rules)
                  read_eps_rules = eps_rule_base <$> ueEPS unit_env
                  read_rule_env = updExternalPackageRules base_rule_env <$> read_eps_rules

              (unit1, counts1) <-
                initSmpl logger read_rule_env top_env_cfg sz $ do
                  (floats, env1) <- {-# SCC "SimplTopBindsUnit" #-}
                                    simplTopBinds simpl_env tagged_binds

                  unit_rules1 <- simplImpRules env1 tagged_rules

                  let unit_binds1 = getTopFloatBinds floats
                  pure (CoreCompUnit unit_binds1 unit_rules1)

              if isZeroSimplCount counts1 then
                return ( unit1
                       , ("Simplifier reached fixed point", iteration_no)
                       , totalise (counts1 : counts_so_far) )
              else do
                let unit2 =
                      CoreCompUnit (shortOutIndirections (coreCompUnitBinds unit1))
                                   (cu_rules unit1)

                dump_end_iteration logger dump_core_sizes name_ppr_ctx iteration_no counts1
                  [unit2] shared_rules

                for_ (so_pass_result_cfg opts) $ \pass_result_cfg ->
                  lintPassResult logger pass_result_cfg [unit2] shared_rules

                do_iteration (iteration_no + 1) (counts1 : counts_so_far) unit2
          where
            bind_list = coreCompUnitBinds unit

        totalise :: [SimplCount] -> SimplCount
        totalise = foldr (\c acc -> acc `plusSimplCount` c) zero_counts

    distributeLocalRules
      :: CoreProgram
      -> [CoreRule]
      -> ([CoreRule], CoreProgram)
    distributeLocalRules units rules = foldr distribute_rule ([], units) rules
      where
        binder_unit_map = foldr add_unit emptyVarEnv (zip [0 :: Int ..] units)

        add_unit (i, CoreCompUnit unit_binds _) env =
          foldr (\v env' -> extendVarEnv env' v i) env (bindersOfBinds unit_binds)

        distribute_rule rule (shared_rules, units_acc)
          | null unit_indices = (rule : shared_rules, units_acc)
          | [i] <- unit_indices = (shared_rules, addRuleToUnit i rule units_acc)
          | otherwise = pprPanic "simplifyPgm'"
              ( text "Local imported-head rule spans multiple compilation units"
             $$ text "rule:" <+> ppr rule
             $$ text "units:" <+> ppr unit_indices )
          where
            local_fvs = ruleFreeVars rule `intersectVarSet` all_local_bndrs
            unit_indices = nonDetStrictFoldVarSet add_hit [] local_fvs

            add_hit v hits = case lookupVarEnv binder_unit_map v of
              Nothing -> hits
              Just i
                | i `elem` hits -> hits
                | otherwise     -> i : hits

        addRuleToUnit 0 rule (CoreCompUnit unit_binds unit_rules : units_acc) =
          CoreCompUnit unit_binds (rule : unit_rules) : units_acc
        addRuleToUnit n rule (unit : units_acc) =
          unit : addRuleToUnit (n - 1) rule units_acc
        addRuleToUnit _ _ [] = panic "distributeLocalRules"

    all_local_bndrs :: VarSet
    all_local_bndrs = mkVarSet (bindersOfBinds (concatMap coreCompUnitBinds (dsd_binds dyn)))

dump_end_iteration :: Logger -> Bool -> NamePprCtx -> Int
                   -> SimplCount -> CoreProgram -> [CoreRule] -> IO ()
dump_end_iteration logger dump_core_sizes name_ppr_ctx iteration_no counts binds rules
  = dumpPassResult logger dump_core_sizes name_ppr_ctx mb_flag hdr pp_counts binds rules
  where
    mb_flag | logHasDumpFlag logger Opt_D_dump_simpl_iterations = Just Opt_D_dump_simpl_iterations
            | otherwise                                         = Nothing
            -- Show details if Opt_D_dump_simpl_iterations is on

    hdr = "Simplifier iteration=" ++ show iteration_no
    pp_counts = vcat [ text "---- Simplifier counts for" <+> text hdr
                     , pprSimplCount counts
                     , text "---- End of simplifier counts for" <+> text hdr ]

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
they say.  And inline pragmas are similar
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
because we might eliminate a binding that's mentioned in the
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
bindings. Note however that this might increase the ticks scoping
over the execution of x_local, so we can only do this for floatable
ticks. More often than not, other references will be unfoldings of
x_exported, and therefore carry the tick anyway.
-}

type IndEnv = IdEnv (Id, [CoreTickish]) -- Maps local_id -> exported_id, ticks

shortOutIndirections :: [CoreBind] -> [CoreBind]
shortOutIndirections binds
  | isEmptyVarEnv ind_env = binds
  | no_need_to_flatten    = binds'                      -- See Note [Rules and indirection-zapping]
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
        then True       -- See Note [Messing up the exported Id's RULES]
        else warnPprTrace True "Not shorting out" (ppr exported_id) False
    else
        False

hasShortableIdInfo :: Id -> Bool
-- True if there is no user-attached IdInfo on exported_id,
-- so we can safely discard it
-- See Note [Messing up the exported Id's RULES]
hasShortableIdInfo id
  =  isEmptyRuleInfo (ruleInfo info)
  && isDefaultInlinePragma (inlinePragInfo info)
  && not (isStableUnfolding (realUnfoldingInfo info))
  where
     info = idInfo id

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

For the lcl_id we

* Zap the InlinePragma. It might originally have had a NOINLINE, which
  we have now transferred; and we really want the lcl_id to inline now
  that its RHS is trivial!

* Zap any Stable unfolding.  agian, we want lcl_id = gbl_id to inline,
  replacing lcl_id by gbl_id. That won't happen if lcl_id has its original
  great big Stable unfolding
-}

transferIdInfo :: Id -> Id -> (Id, Id)
-- See Note [Transferring IdInfo]
transferIdInfo exported_id local_id
  = ( modifyIdInfo transfer exported_id
    , modifyIdInfo zap_info local_id )
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setDmdSigInfo`     dmdSigInfo local_info
                                 `setCprSigInfo`     cprSigInfo local_info
                                 `setUnfoldingInfo`  realUnfoldingInfo local_info
                                 `setInlinePragInfo` inlinePragInfo local_info
                                 `setRuleInfo`       addRuleInfo (ruleInfo exp_info) new_info
    new_info = setRuleInfoHead (idName exported_id)
                               (ruleInfo local_info)
        -- Remember to set the function-name field of the
        -- rules as we transfer them from one function to another

    zap_info lcl_info = lcl_info `setInlinePragInfo` defaultInlinePragma
                                 `setUnfoldingInfo`  noUnfolding
