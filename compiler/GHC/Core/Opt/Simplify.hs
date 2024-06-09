{-# LANGUAGE CPP #-}

module GHC.Core.Opt.Simplify
  ( SimplifyExprOpts(..), SimplifyOpts(..)
  , simplifyExpr, simplifyPgm
  ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.Rules
import GHC.Core.Ppr     ( pprCoreBindings, pprCoreExpr )
import GHC.Core.Opt.OccurAnal ( occurAnalysePgm, occurAnalyseExpr )
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

import GHC.Unit.Env ( UnitEnv, ueEPS )
import GHC.Unit.External
import GHC.Unit.Module.ModGuts

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Tickish
import GHC.Types.Unique.FM

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
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
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
-- Simplifies an expression
--      does occurrence analysis, then simplification
--      and repeats (twice currently) because one pass
--      alone leaves tons of crud.
-- Used (a) for user expressions typed in at the interactive prompt
--      (b) the LHS and RHS of a RULE
--      (c) Template Haskell splices
--
-- The name 'Gently' suggests that the SimplMode is InitialPhase,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

-- It's important that simplExprGently does eta reduction; see
-- Note [Simplify rule LHS] above.  The
-- simplifier does indeed do eta reduction (it's in GHC.Core.Opt.Simplify.completeLam)
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

simplifyPgm :: Logger
            -> UnitEnv
            -> NamePprCtx                -- For dumping
            -> SimplifyOpts
            -> ModGuts
            -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgm logger unit_env name_ppr_ctx opts
            guts@(ModGuts { mg_module = this_mod
                          , mg_binds = binds, mg_rules = local_rules
                          , mg_fam_inst_env = fam_inst_env })
  = do { (termination_msg, it_count, counts_out, guts')
            <- do_iteration 1 [] binds local_rules

        ; when (logHasDumpFlag logger Opt_D_verbose_core2core
                && logHasDumpFlag logger Opt_D_dump_simpl_stats) $
          logDumpMsg logger
                  "Simplifier statistics for following pass"
                  (vcat [text termination_msg <+> text "after" <+> ppr it_count
                                              <+> text "iterations",
                         blankLine,
                         pprSimplCount counts_out])

        ; return (counts_out, guts')
    }
  where
    dump_core_sizes = so_dump_core_sizes opts
    mode            = so_mode opts
    max_iterations  = so_iterations opts
    top_env_cfg     = so_top_env_cfg opts
    active_rule     = activeRule mode
    active_unf      = activeUnfolding mode
    -- Note the bang in !guts_no_binds.  If you don't force `guts_no_binds`
    -- the old bindings are retained until the end of all simplifier iterations
    !guts_no_binds = guts { mg_binds = [], mg_rules = [] }

    hpt_rule_env :: RuleEnv
    hpt_rule_env = mkRuleEnv guts emptyRuleBase (so_hpt_rules opts)
                   -- emptyRuleBase: no EPS rules yet; we will update
                   -- them on each iteration to pick up the most up to date set

    do_iteration :: Int -- Counts iterations
                 -> [SimplCount] -- Counts from earlier iterations, reversed
                 -> CoreProgram  -- Bindings
                 -> [CoreRule]   -- Local rules for imported Ids
                 -> IO (String, Int, SimplCount, ModGuts)

    do_iteration iteration_no counts_so_far binds local_rules
        -- iteration_no is the number of the iteration we are
        -- about to begin, with '1' for the first
      | iteration_no > max_iterations   -- Stop if we've run out of iterations
      = warnPprTrace (debugIsOn && (max_iterations > 2))
            "Simplifier bailing out"
            ( hang (ppr this_mod <> text ", after"
                    <+> int max_iterations <+> text "iterations"
                    <+> (brackets $ hsep $ punctuate comma $
                         map (int . simplCountN) (reverse counts_so_far)))
                 2 (text "Size =" <+> ppr (coreBindsStats binds))) $

                -- Subtract 1 from iteration_no to get the
                -- number of iterations we actually completed
        return ( "Simplifier bailed out", iteration_no - 1
               , totalise counts_so_far
               , guts_no_binds { mg_binds = binds, mg_rules = local_rules } )

      -- Try and force thunks off the binds; significantly reduces
      -- space usage, especially with -O.  JRS, 000620.
      | let sz = coreBindsSize binds
      , () <- sz `seq` ()     -- Force it
      = do {
                -- Occurrence analysis
           let { tagged_binds = {-# SCC "OccAnal" #-}
                     occurAnalysePgm this_mod active_unf active_rule
                                     local_rules binds
               } ;
           Logger.putDumpFileMaybe logger Opt_D_dump_occur_anal "Occurrence analysis"
                     FormatCore
                     (pprCoreBindings tagged_binds);

                -- read_eps_rules:
                -- We need to read rules from the EPS regularly because simplification can
                -- poke on IdInfo thunks, which in turn brings in new rules
                -- behind the scenes.  Otherwise there's a danger we'll simply
                -- miss the rules for Ids hidden inside imported inlinings
                -- Hence just before attempting to match a rule we read the EPS
                -- value (via read_rule_env) and then combine it with the existing rule base.
                -- See `GHC.Core.Opt.Simplify.Monad.getSimplRules`.
          eps <- ueEPS unit_env ;
           let  { -- base_rule_env contains
                  --    (a) home package rules, fixed across all iterations
                  --    (b) local rules (substituted) from `local_rules` arg to do_iteration
                  -- Forcing base_rule_env to avoid unnecessary allocations.
                  -- Not doing so results in +25.6% allocations of LargeRecord.
                ; !base_rule_env = updLocalRules hpt_rule_env local_rules

                ; read_eps_rules :: IO PackageRuleBase
                ; read_eps_rules = eps_rule_base <$> ueEPS unit_env

                ; read_rule_env :: IO RuleEnv
                ; read_rule_env = updExternalPackageRules base_rule_env <$> read_eps_rules

                ; fam_envs = (eps_fam_inst_env eps, fam_inst_env)
                ; simpl_env = mkSimplEnv mode fam_envs } ;

                -- Simplify the program
           ((binds1, rules1), counts1) <-
             initSmpl logger read_rule_env top_env_cfg sz $
               do { (floats, env1) <- {-# SCC "SimplTopBinds" #-}
                                      simplTopBinds simpl_env tagged_binds

                      -- Apply the substitution to rules defined in this module
                      -- for imported Ids.  Eg  RULE map my_f = blah
                      -- If we have a substitution my_f :-> other_f, we'd better
                      -- apply it to the rule to, or it'll never match
                  ; rules1 <- simplImpRules env1 local_rules

                  ; return (getTopFloatBinds floats, rules1) } ;

                -- Stop if nothing happened; don't dump output
                -- See Note [Which transformations are innocuous] in GHC.Core.Opt.Stats
           if isZeroSimplCount counts1 then
                return ( "Simplifier reached fixed point", iteration_no
                       , totalise (counts1 : counts_so_far)  -- Include "free" ticks
                       , guts_no_binds { mg_binds = binds1, mg_rules = rules1 } )
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
           dump_end_iteration logger dump_core_sizes name_ppr_ctx iteration_no counts1 binds2 rules1 ;

           for_ (so_pass_result_cfg opts) $ \pass_result_cfg ->
             lintPassResult logger pass_result_cfg binds2 ;

                -- Loop
           do_iteration (iteration_no + 1) (counts1:counts_so_far) binds2 rules1
           } }
      where
        -- Remember the counts_so_far are reversed
        totalise :: [SimplCount] -> SimplCount
        totalise = foldr (\c acc -> acc `plusSimplCount` c)
                         (zeroSimplCount $ logHasDumpFlag logger Opt_D_dump_simpl_stats)

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

shortOutIndirections :: CoreProgram -> CoreProgram
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
