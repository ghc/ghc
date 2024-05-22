{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The Desugarer: turning HsSyn into Core.
-}

module GHC.HsToCore (
    -- * Desugaring operations
    deSugar, deSugarExpr
    ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Config
import GHC.Driver.Config.Core.Lint ( endPassHscEnvIO )
import GHC.Driver.Config.HsToCore.Ticks
import GHC.Driver.Config.HsToCore.Usage
import GHC.Driver.Env
import GHC.Driver.Backend
import GHC.Driver.Plugins

import GHC.Hs

import GHC.HsToCore.Usage
import GHC.HsToCore.Monad
import GHC.HsToCore.Errors.Types
import GHC.HsToCore.Expr
import GHC.HsToCore.Binds
import GHC.HsToCore.Foreign.Decl
import GHC.HsToCore.Ticks
import GHC.HsToCore.Breakpoints
import GHC.HsToCore.Coverage
import GHC.HsToCore.Docs

import GHC.Tc.Types
import GHC.Tc.Types.Origin ( Position(..) )
import GHC.Tc.Utils.Monad  ( finalSafeMode, fixSafeInstances, initIfaceLoad )
import GHC.Tc.Module ( runTcInteractive )

import GHC.Core.Type
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.TyCon       ( tyConDataCons )
import GHC.Core
import GHC.Core.FVs       ( exprsSomeFreeVarsList, exprFreeVars )
import GHC.Core.SimpleOpt ( simpleOptPgm, simpleOptExpr )
import GHC.Core.Utils
import GHC.Core.Unfold.Make
import GHC.Core.Coercion
import GHC.Core.DataCon ( dataConWrapId )
import GHC.Core.Make
import GHC.Core.Rules
import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Core.Ppr

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim
import GHC.Builtin.Types

import GHC.Data.Maybe    ( expectJust )
import GHC.Data.OrdList
import GHC.Data.SizedSeq ( sizeSS )

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Logger

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Id.Make ( mkRepPolyIdConcreteTyVars )
import GHC.Types.ForeignStubs
import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.Var.Set
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import GHC.Types.TypeEnv
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Name.Ppr
import GHC.Types.HpcInfo

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import Data.List (partition)
import Data.IORef
import Data.Traversable (for)

{-
************************************************************************
*                                                                      *
*              The main function: deSugar
*                                                                      *
************************************************************************
-}

-- | Main entry point to the desugarer.
deSugar :: HscEnv -> ModLocation -> TcGblEnv -> IO (Messages DsMessage, Maybe ModGuts)
-- Can modify PCS by faulting in more declarations

deSugar hsc_env
        mod_loc
        tcg_env@(TcGblEnv { tcg_mod          = id_mod,
                            tcg_semantic_mod = mod,
                            tcg_src          = hsc_src,
                            tcg_type_env     = type_env,
                            tcg_imports      = imports,
                            tcg_exports      = exports,
                            tcg_keep         = keep_var,
                            tcg_th_splice_used = tc_splice_used,
                            tcg_rdr_env      = rdr_env,
                            tcg_fix_env      = fix_env,
                            tcg_inst_env     = inst_env,
                            tcg_fam_inst_env = fam_inst_env,
                            tcg_merged       = merged,
                            tcg_warns        = warns,
                            tcg_anns         = anns,
                            tcg_binds        = binds,
                            tcg_imp_specs    = imp_specs,
                            tcg_dependent_files = dependent_files,
                            tcg_ev_binds     = ev_binds,
                            tcg_th_foreign_files = th_foreign_files_var,
                            tcg_fords        = fords,
                            tcg_rules        = rules,
                            tcg_patsyns      = patsyns,
                            tcg_tcs          = tcs,
                            tcg_default_exports = defaults,
                            tcg_insts        = insts,
                            tcg_fam_insts    = fam_insts,
                            tcg_hpc          = other_hpc_info,
                            tcg_complete_matches = complete_matches,
                            tcg_self_boot    = self_boot
                            })

  = do { let dflags = hsc_dflags hsc_env
             logger = hsc_logger hsc_env
             ptc = initPromotionTickContext (hsc_dflags hsc_env)
             name_ppr_ctx = mkNamePprCtx ptc (hsc_unit_env hsc_env) rdr_env
        ; withTiming logger
                     (text "Desugar"<+>brackets (ppr mod))
                     (const ()) $
     do { -- Desugar the program
        ; let export_set = availsToNameSet exports
              bcknd      = backend dflags
              -- See Note [Named default declarations] in GHC.Tc.Gen.Default

        ; (binds_cvr, m_tickInfo)
                         <- if not (isHsBootOrSig hsc_src)
                              then addTicksToBinds
                                       (hsc_logger hsc_env)
                                       (initTicksConfig (hsc_dflags hsc_env))
                                       mod mod_loc
                                       export_set (typeEnvTyCons type_env) binds
                              else return (binds, Nothing)
        ; modBreaks <- for
           [ (i, s)
           | i <- hsc_interp hsc_env
           , (_, s) <- m_tickInfo
           , breakpointsAllowed dflags
           ]
           $ \(interp, specs) -> mkModBreaks interp mod specs

        ; ds_hpc_info <- case m_tickInfo of
            Just (orig_file2, ticks)
              | gopt Opt_Hpc $ hsc_dflags hsc_env
              -> do
              hashNo <- if gopt Opt_Hpc $ hsc_dflags hsc_env
                then writeMixEntries (hpcDir dflags) mod ticks orig_file2
                else return 0 -- dummy hash when none are written
              pure $ HpcInfo (fromIntegral $ sizeSS ticks) hashNo
            _ -> pure $ emptyHpcInfo other_hpc_info

        ; (msgs, mb_res) <- initDs hsc_env tcg_env $
                       do { dsEvBinds ev_binds $ \ ds_ev_binds -> do
                          { core_prs <- dsTopLHsBinds binds_cvr
                          ; core_prs <- patchMagicDefns core_prs
                          ; (spec_prs, spec_rules) <- dsImpSpecs imp_specs
                          ; (ds_fords, foreign_prs) <- dsForeigns fords
                          ; ds_rules <- mapMaybeM dsRule rules
                          ; let hpc_init
                                  | gopt Opt_Hpc dflags = hpcInitCode (targetPlatform $ hsc_dflags hsc_env) mod ds_hpc_info
                                  | otherwise = mempty
                          ; return ( ds_ev_binds
                                   , foreign_prs `appOL` core_prs `appOL` spec_prs
                                   , spec_rules ++ ds_rules
                                   , ds_fords `appendStubC` hpc_init) } }

        ; case mb_res of {
           Nothing -> return (msgs, Nothing) ;
           Just (ds_ev_binds, all_prs, all_rules, ds_fords) ->

     do {       -- Add export flags to bindings
          keep_alive <- readIORef keep_var
        ; let (rules_for_locals, rules_for_imps) = partition isLocalRule all_rules
              final_prs = addExportFlagsAndRules bcknd export_set keep_alive
                                                 rules_for_locals (fromOL all_prs)

              final_pgm = combineEvBinds ds_ev_binds final_prs
        -- Notice that we put the whole lot in a big Rec, even the foreign binds
        -- When compiling PrelFloat, which defines data Float = F# Float#
        -- we want F# to be in scope in the foreign marshalling code!
        -- You might think it doesn't matter, but the simplifier brings all top-level
        -- things into the in-scope set before simplifying; so we get no unfolding for F#!

        ; endPassHscEnvIO hsc_env name_ppr_ctx CoreDesugar final_pgm rules_for_imps
        ; let simpl_opts = initSimpleOpts dflags
        ; let (ds_binds, ds_rules_for_imps, occ_anald_binds)
                = simpleOptPgm simpl_opts mod final_pgm rules_for_imps
                         -- The simpleOptPgm gets rid of type
                         -- bindings plus any stupid dead code
        ; putDumpFileMaybe logger Opt_D_dump_occur_anal "Occurrence analysis"
            FormatCore (pprCoreBindings occ_anald_binds $$ pprRules ds_rules_for_imps )

        ; endPassHscEnvIO hsc_env name_ppr_ctx CoreDesugarOpt ds_binds ds_rules_for_imps

        ; let used_names = mkUsedNames tcg_env
              pluginModules = map lpModule (loadedPlugins (hsc_plugins hsc_env))
              home_unit = hsc_home_unit hsc_env
        ; let deps = mkDependencies home_unit
                                    (tcg_mod tcg_env)
                                    (tcg_imports tcg_env)
                                    (map mi_module pluginModules)

        ; used_th <- readIORef tc_splice_used
        ; dep_files <- readIORef dependent_files
        ; safe_mode <- finalSafeMode dflags tcg_env
        ; (needed_mods, needed_pkgs) <- readIORef (tcg_th_needed_deps tcg_env)

        ; let uc = initUsageConfig hsc_env
        ; let plugins = hsc_plugins hsc_env
        ; let fc = hsc_FC hsc_env
        ; let unit_env = hsc_unit_env hsc_env
        ; usages <- initIfaceLoad hsc_env $
                      mkUsageInfo uc plugins fc unit_env mod (imp_mods imports) used_names
                        dep_files merged needed_mods needed_pkgs
        -- id_mod /= mod when we are processing an hsig, but hsigs
        -- never desugared and compiled (there's no code!)
        -- Consequently, this should hold for any ModGuts that make
        -- past desugaring. See Note [Identity versus semantic module].
        ; massert (id_mod == mod)

        ; foreign_files <- readIORef th_foreign_files_var

        ; docs <- extractDocs dflags tcg_env

        ; let mod_guts = ModGuts {
                mg_module       = mod,
                mg_hsc_src      = hsc_src,
                mg_loc          = mkFileSrcSpan mod_loc,
                mg_exports      = exports,
                mg_usages       = usages,
                mg_deps         = deps,
                mg_used_th      = used_th,
                mg_rdr_env      = rdr_env,
                mg_fix_env      = fix_env,
                mg_warns        = warns,
                mg_anns         = anns,
                mg_tcs          = tcs,
                mg_defaults     = defaults,
                mg_insts        = fixSafeInstances safe_mode insts,
                mg_fam_insts    = fam_insts,
                mg_inst_env     = inst_env,
                mg_fam_inst_env = fam_inst_env,
                mg_boot_exports = bootExports self_boot,
                mg_patsyns      = patsyns,
                mg_rules        = ds_rules_for_imps,
                mg_binds        = ds_binds,
                mg_foreign      = ds_fords,
                mg_foreign_files = foreign_files,
                mg_hpc_info     = ds_hpc_info,
                mg_modBreaks    = modBreaks,
                mg_safe_haskell = safe_mode,
                mg_trust_pkg    = imp_trust_own_pkg imports,
                mg_complete_matches = complete_matches,
                mg_docs         = docs
              }
        ; return (msgs, Just mod_guts)
        }}}}

dsImpSpecs :: [LTcSpecPrag] -> DsM (OrdList (Id,CoreExpr), [CoreRule])
dsImpSpecs imp_specs
 = do { spec_prs <- mapMaybeM (dsSpec Nothing) imp_specs
      ; let (spec_binds, spec_rules) = unzip spec_prs
      ; return (concatOL spec_binds, spec_rules) }

combineEvBinds :: [CoreBind] -> [(Id,CoreExpr)] -> [CoreBind]
-- Top-level bindings can include coercion bindings, but not via superclasses
-- See Note [Top-level evidence]
combineEvBinds [] val_prs
  = [Rec val_prs]
combineEvBinds (NonRec b r : bs) val_prs
  | isId b    = combineEvBinds bs ((b,r):val_prs)
  | otherwise = NonRec b r : combineEvBinds bs val_prs
combineEvBinds (Rec prs : bs) val_prs
  = combineEvBinds bs (prs ++ val_prs)

{-
Note [Top-level evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level evidence bindings may be mutually recursive with the top-level value
bindings, so we must put those in a Rec.  But we can't put them *all* in a Rec
because the occurrence analyser doesn't take account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.
-}

deSugarExpr :: HscEnv -> LHsExpr GhcTc -> IO (Messages DsMessage, Maybe CoreExpr)
deSugarExpr hsc_env tc_expr = do
    let logger = hsc_logger hsc_env

    showPass logger "Desugar"

    -- Do desugaring
    (tc_msgs, mb_result) <- runTcInteractive hsc_env $
                            initDsTc $
                            dsLExpr tc_expr

    massert (isEmptyMessages tc_msgs)  -- the type-checker isn't doing anything here

      -- mb_result is Nothing only when a failure happens in the type-checker,
      -- but mb_core_expr is Nothing when a failure happens in the desugarer
    let (ds_msgs, mb_core_expr) = expectJust "deSugarExpr" mb_result

    case mb_core_expr of
       Nothing   -> return ()
       Just expr -> putDumpFileMaybe logger Opt_D_dump_ds "Desugared"
                    FormatCore (pprCoreExpr expr)

      -- callers (i.e. ioMsgMaybe) expect that no expression is returned if
      -- there are errors
    let final_res | errorsFound ds_msgs = Nothing
                  | otherwise           = mb_core_expr

    return (ds_msgs, final_res)

{-
************************************************************************
*                                                                      *
*              Add rules and export flags to binders
*                                                                      *
************************************************************************
-}

addExportFlagsAndRules
    :: Backend -> NameSet -> NameSet -> [CoreRule]
    -> [(Id, t)] -> [(Id, t)]
addExportFlagsAndRules bcknd exports keep_alive rules
  = mapFst (addRulesToId rule_base . add_export_flag)
        -- addRulesToId: see Note [Attach rules to local ids]
        -- NB: the binder might have some existing rules,
        -- arising from specialisation pragmas

  where

    ---------- Rules --------
    rule_base = extendRuleBaseList emptyRuleBase rules

    ---------- Export flag --------
    -- See Note [Adding export flags]
    add_export_flag bndr
        | dont_discard bndr = setIdExported bndr
        | otherwise         = bndr

    dont_discard :: Id -> Bool
    dont_discard bndr = is_exported name
                     || name `elemNameSet` keep_alive
       where
         name = idName bndr

        -- In interactive mode, we don't want to discard any top-level
        -- entities at all (eg. do not inline them away during
        -- simplification), and retain them all in the TypeEnv so they are
        -- available from the command line.
        --
        -- isExternalName separates the user-defined top-level names from those
        -- introduced by the type checker.
    is_exported :: Name -> Bool
    is_exported | backendWantsGlobalBindings bcknd = isExternalName
                | otherwise                       = (`elemNameSet` exports)

{-
Note [Adding export flags]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Set the no-discard flag if either
        a) the Id is exported
        b) it's mentioned in the RHS of an orphan rule
        c) it's in the keep-alive set

It means that the binding won't be discarded EVEN if the binding
ends up being trivial (v = w) -- the simplifier would usually just
substitute w for v throughout, but we don't apply the substitution to
the rules (maybe we should?), so this substitution would make the rule
bogus.

You might wonder why exported Ids aren't already marked as such;
it's just because the type checker is rather busy already and
I didn't want to pass in yet another mapping.

Note [Attach rules to local ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Find the rules for locally-defined Ids; then we can attach them
to the binders in the top-level bindings

Reason
  - It makes the rules easier to look up
  - It means that rewrite rules and specialisations for
    locally defined Ids are handled uniformly
  - It keeps alive things that are referred to only from a rule
    (the occurrence analyser knows about rules attached to Ids)
  - It makes sure that, when we apply a rule, the free vars
    of the RHS are more likely to be in scope
  - The imported rules are carried in the in-scope set
    which is extended on each iteration by the new wave of
    local binders; any rules which aren't on the binding will
    thereby get dropped


************************************************************************
*                                                                      *
*              Desugaring rewrite rules
*                                                                      *
************************************************************************
-}

dsRule :: LRuleDecl GhcTc -> DsM (Maybe CoreRule)
dsRule (L loc (HsRule { rd_name = name
                      , rd_act  = rule_act
                      , rd_tmvs = vars
                      , rd_lhs  = lhs
                      , rd_rhs  = rhs }))
  = putSrcSpanDs (locA loc) $
    do  { let bndrs' = [var | L _ (RuleBndr _ (L _ var)) <- vars]

        ; lhs' <- unsetGOptM Opt_EnableRewriteRules $
                  unsetWOptM Opt_WarnIdentities     $
                  zapUnspecables                    $
                  dsLExpr lhs   -- Note [Desugaring RULE left hand sides]

        ; rhs' <- dsLExpr rhs
        ; this_mod <- getModule

        ; (bndrs'', lhs'', rhs'') <- unfold_coerce bndrs' lhs' rhs'

        -- Substitute the dict bindings eagerly,
        -- and take the body apart into a (f args) form
        ; dflags <- getDynFlags
        ; case decomposeRuleLhs dflags bndrs'' lhs'' (exprFreeVars rhs'') of {
                Left msg -> do { diagnosticDs msg; return Nothing } ;
                Right (final_bndrs, fn_id, args) -> do

        { let is_local = isLocalId fn_id
                -- NB: isLocalId is False of implicit Ids.  This is good because
                -- we don't want to attach rules to the bindings of implicit Ids,
                -- because they don't show up in the bindings until just before code gen
              fn_name   = idName fn_id
              simpl_opts = initSimpleOpts dflags
              final_rhs = simpleOptExpr simpl_opts rhs''    -- De-crap it
              rule_name = unLoc name
              rule = mkRule this_mod False is_local rule_name rule_act
                            fn_name final_bndrs args final_rhs
        ; dsWarnOrphanRule rule
        ; dsWarnRuleShadowing fn_id rule

        ; return (Just rule)
        } } }

dsWarnRuleShadowing :: Id -> CoreRule -> DsM ()
-- See Note [Rules and inlining/other rules]
dsWarnRuleShadowing fn_id
    (Rule { ru_name = rule_name, ru_act = rule_act, ru_bndrs = bndrs, ru_args = args})
  = do { check False fn_id    -- We often have multiple rules for the same Id in a
                              -- module. Maybe we should check that they don't overlap
                              -- but currently we don't
       ; mapM_ (check True) arg_ids }
  where
    bndrs_set = mkVarSet bndrs
    arg_ids = filterOut (`elemVarSet` bndrs_set) $
              exprsSomeFreeVarsList isId args

    check check_rules_too lhs_id
      | isLocalId lhs_id || canUnfold (idUnfolding lhs_id)
                       -- If imported with no unfolding, no worries
      , idInlineActivation lhs_id `competesWith` rule_act
      = diagnosticDs (DsRuleMightInlineFirst rule_name lhs_id rule_act)
      | check_rules_too
      , bad_rule : _ <- get_bad_rules lhs_id
      = diagnosticDs (DsAnotherRuleMightFireFirst rule_name (ruleName bad_rule) lhs_id)
      | otherwise
      = return ()

    get_bad_rules lhs_id
      = [ rule | rule <- idCoreRules lhs_id
               , ruleActivation rule `competesWith` rule_act ]

dsWarnRuleShadowing _ _ = return () -- Not expecting built-in rules here

-- See Note [Desugaring coerce as cast]
unfold_coerce :: [Id] -> CoreExpr -> CoreExpr -> DsM ([Var], CoreExpr, CoreExpr)
unfold_coerce bndrs lhs rhs = do
    (bndrs', wrap) <- go bndrs
    return (bndrs', wrap lhs, wrap rhs)
  where
    go :: [Id] -> DsM ([Id], CoreExpr -> CoreExpr)
    go []     = return ([], id)
    go (v:vs)
        | Just (tc, [k, t1, t2]) <- splitTyConApp_maybe (idType v)
        , tc `hasKey` coercibleTyConKey = do
            u <- newUnique

            let ty' = mkTyConApp eqReprPrimTyCon [k, k, t1, t2]
                v'  = mkLocalCoVar
                        (mkDerivedInternalName mkRepEqOcc u (getName v)) ty'
                box = Var (dataConWrapId coercibleDataCon) `mkTyApps`
                      [k, t1, t2] `App`
                      Coercion (mkCoVarCo v')

            (bndrs, wrap) <- go vs
            return (v':bndrs, mkCoreLet (NonRec v box) . wrap)
        | otherwise = do
            (bndrs,wrap) <- go vs
            return (v:bndrs, wrap)

{- Note [Desugaring RULE left hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the LHS of a RULE we do *not* want to desugar
    [x]   to    build (\cn. x `c` n)
We want to leave explicit lists simply as chains
of cons's. We can achieve that slightly indirectly by
switching off EnableRewriteRules.  See GHC.HsToCore.Expr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.

Nor do we want to warn of conversion identities on the LHS;
the rule is precisely to optimise them:
  {-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}

Finally, the `zapUnspecables` is to implement (NC1) of
Note [Desugaring non-canonical evidence] in GHC.HsToCore.Expr

Note [Desugaring coerce as cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want the user to express a rule saying roughly “mapping a coercion over a
list can be replaced by a coercion”. But the cast operator of Core (▷) cannot
be written in Haskell. So we use `coerce` for that (#2110). The user writes
    map coerce = coerce
as a RULE, and this optimizes any kind of mapped' casts away, including `map
MkNewtype`.

For that we replace any forall'ed `c :: Coercible a b` value in a RULE by
corresponding `co :: a ~#R b` and wrap the LHS and the RHS in
`let c = MkCoercible co in ...`. This is later simplified to the desired form
by simpleOptExpr (for the LHS) resp. the simplifiers (for the RHS).
See also Note [Getting the map/coerce RULE to work] in GHC.Core.SimpleOpt.

Note [Rules and inlining/other rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have
  f x = ...
  g x = ...
  {-# RULES "rule-for-f" forall x. f (g x) = ... #-}
then there's a good chance that in a potential rule redex
    ...f (g e)...
then 'f' or 'g' will inline before the rule can fire.  Solution: add an
INLINE [n] or NOINLINE [n] pragma to 'f' and 'g'.

Note that this applies to all the free variables on the LHS, both the
main function and things in its arguments.

We also check if there are Ids on the LHS that have competing RULES.
In the above example, suppose we had
  {-# RULES "rule-for-g" forally. g [y] = ... #-}
Then "rule-for-f" and "rule-for-g" would compete.  Better to add phase
control, so "rule-for-f" has a chance to fire before "rule-for-g" becomes
active; or perhaps after "rule-for-g" has become inactive. This is checked
by 'competesWith'

Class methods have a built-in RULE to select the method from the dictionary,
so you can't change the phase on this.  That makes id very dubious to
match on class methods in RULE lhs's.   See #10595.   I'm not happy
about this. For example in Control.Arrow we have

{-# RULES "compose/arr"   forall f g .
                          (arr f) . (arr g) = arr (f . g) #-}

and similar, which will elicit exactly these warnings, and risk never
firing.  But it's not clear what to do instead.  We could make the
class method rules inactive in phase 2, but that would delay when
subsequent transformations could fire.
-}

{-
************************************************************************
*                                                                      *
*              Magic definitions
*                                                                      *
************************************************************************

Note [Patching magic definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We sometimes need to have access to defined Ids in pure contexts. Usually, we
simply "wire in" these entities, as we do for types in GHC.Builtin.Types and for Ids
in GHC.Types.Id.Make. See Note [Wired-in Ids] in GHC.Types.Id.Make.

However, it is sometimes *much* easier to define entities in Haskell,
even if we need pure access; note that wiring-in an Id requires all
entities used in its definition *also* to be wired in, transitively
and recursively.  This can be a huge pain.  The little trick
documented here allows us to have the best of both worlds.

Motivating example: unsafeCoerce#. See [Wiring in unsafeCoerce#] for the
details.

The trick is to

* Define the known-key Id in a library module, with a stub definition,
     unsafeCoerce# :: ..a suitable type signature..
     unsafeCoerce# = error "urk"

* Magically over-write its RHS here in the desugarer, in
  patchMagicDefns.  This update can be done with full access to the
  DsM monad, and hence, dsLookupGlobal. We thus do not have to wire in
  all the entities used internally, a potentially big win.

  This step should not change the Name or type of the Id.

Because an Id stores its unfolding directly (as opposed to in the second
component of a (Id, CoreExpr) pair), the patchMagicDefns function returns
a new Id to use.

Here are the moving parts:

- patchMagicDefns checks whether we're in a module with magic definitions;
  if so, patch the magic definitions. If not, skip.

- patchMagicDefn just looks up in an environment to find a magic defn and
  patches it in.

- magicDefns holds the magic definitions.

- magicDefnsEnv allows for quick access to magicDefns.

- magicDefnModules, built also from magicDefns, contains the modules that
  need careful attention.

Note [Wiring in unsafeCoerce#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want (Haskell)

  unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                          (a :: TYPE r1) (b :: TYPE r2).
                   a -> b
  unsafeCoerce# x = case unsafeEqualityProof @r1 @r2 of
    UnsafeRefl -> case unsafeEqualityProof @a @b of
      UnsafeRefl -> x

or (Core)

  unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                          (a :: TYPE r1) (b :: TYPE r2).
                   a -> b
  unsafeCoerce# = \ @r1 @r2 @a @b (x :: a).
    case unsafeEqualityProof @RuntimeRep @r1 @r2 of
      UnsafeRefl (co1 :: r1 ~# r2) ->
        case unsafeEqualityProof @(TYPE r2) @(a |> TYPE co1) @b of
          UnsafeRefl (co2 :: (a |> TYPE co1) ~# b) ->
            (x |> (GRefl :: a ~# (a |> TYPE co1)) ; co2)

It looks like we can write this in Haskell directly, but we can't:
the representation polymorphism checks defeat us. Note that `x` is a
representation-polymorphic variable. So we must wire it in with a
compulsory unfolding, like other representation-polymorphic primops.

The challenge is that UnsafeEquality is a GADT, and wiring in a GADT
is *hard*: it has a worker separate from its wrapper, with all manner
of complications. (Simon and Richard tried to do this. We nearly wept.)

The solution is documented in Note [Patching magic definitions]. We now
simply look up the UnsafeEquality GADT in the environment, leaving us
only to wire in unsafeCoerce# directly.

Wrinkle: see Note [Always expose compulsory unfoldings] in GHC.Iface.Tidy
-}


-- Postcondition: the returned Ids are in one-to-one correspondence as the
-- input Ids; each returned Id has the same type as the passed-in Id.
-- See Note [Patching magic definitions]
patchMagicDefns :: OrdList (Id,CoreExpr)
                -> DsM (OrdList (Id,CoreExpr))
patchMagicDefns pairs
  -- optimization: check whether we're in a magic module before looking
  -- at all the ids
  = do { this_mod <- getModule
       ; if this_mod `elemModuleSet` magicDefnModules
         then traverse patchMagicDefn pairs
         else return pairs }

patchMagicDefn :: (Id, CoreExpr) -> DsM (Id, CoreExpr)
patchMagicDefn orig_pair@(orig_id, orig_rhs)
  | Just mk_magic_pair <- lookupNameEnv magicDefnsEnv (getName orig_id)
  = do { magic_pair@(magic_id, _) <- mk_magic_pair orig_id orig_rhs

       -- Patching should not change the Name or the type of the Id
       ; massert (getUnique magic_id == getUnique orig_id)
       ; massert (varType magic_id `eqType` varType orig_id)

       ; return magic_pair }
  | otherwise
  = return orig_pair

magicDefns :: [(Name,    Id -> CoreExpr     -- old Id and RHS
                      -> DsM (Id, CoreExpr) -- new Id and RHS
               )]
magicDefns = [ (unsafeCoercePrimName, mkUnsafeCoercePrimPair) ]

magicDefnsEnv :: NameEnv (Id -> CoreExpr -> DsM (Id, CoreExpr))
magicDefnsEnv = mkNameEnv magicDefns

magicDefnModules :: ModuleSet
magicDefnModules = mkModuleSet $ map (nameModule . getName . fst) magicDefns

mkUnsafeCoercePrimPair :: Id -> CoreExpr -> DsM (Id, CoreExpr)
-- See Note [Wiring in unsafeCoerce#] for the defn we are creating here
mkUnsafeCoercePrimPair _old_id old_expr
  = do { unsafe_equality_proof_id <- dsLookupGlobalId unsafeEqualityProofName
       ; unsafe_equality_tc       <- dsLookupTyCon unsafeEqualityTyConName

       ; let [unsafe_refl_data_con] = tyConDataCons unsafe_equality_tc

             rhs = mkLams [ runtimeRep1TyVar, runtimeRep2TyVar
                          , openAlphaTyVar, openBetaTyVar
                          , x ] $
                   mkSingleAltCase scrut1
                                   (mkWildValBinder ManyTy scrut1_ty)
                                   (DataAlt unsafe_refl_data_con)
                                   [rr_cv] $
                   mkSingleAltCase scrut2
                                   (mkWildValBinder ManyTy scrut2_ty)
                                   (DataAlt unsafe_refl_data_con)
                                   [ab_cv] $
                   Var x `mkCast` x_co

             [x, rr_cv, ab_cv] = mkTemplateLocals
               [ openAlphaTy -- x :: a
               , rr_cv_ty    -- rr_cv :: r1 ~# r2
               , ab_cv_ty    -- ab_cv :: (alpha |> alpha_co ~# beta)
               ]

             -- Returns (scrutinee, scrutinee type, type of covar in AltCon)
             unsafe_equality k a b
               = ( mkTyApps (Var unsafe_equality_proof_id) [k,b,a]
                 , mkTyConApp unsafe_equality_tc [k,b,a]
                 , mkNomPrimEqPred k a b
                 )
             -- NB: UnsafeRefl :: (b ~# a) -> UnsafeEquality a b, so we have to
             -- carefully swap the arguments above

             (scrut1, scrut1_ty, rr_cv_ty) = unsafe_equality runtimeRepTy
                                                             runtimeRep1Ty
                                                             runtimeRep2Ty
             (scrut2, scrut2_ty, ab_cv_ty) = unsafe_equality (mkTYPEapp runtimeRep2Ty)
                                                             (openAlphaTy `mkCastTy` alpha_co)
                                                             openBetaTy

             -- alpha_co :: TYPE r1 ~# TYPE r2
             -- alpha_co = TYPE rr_cv
             alpha_co = mkTyConAppCo Nominal tYPETyCon [mkCoVarCo rr_cv]

             -- x_co :: alpha ~R# beta
             x_co = mkGReflCo Representational openAlphaTy (MCo alpha_co) `mkTransCo`
                    mkSubCo (mkCoVarCo ab_cv)


             info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                                `setUnfoldingInfo` mkCompulsoryUnfolding rhs
                                `setArityInfo`     arity

             ty = mkSpecForAllTys [ runtimeRep1TyVar, runtimeRep2TyVar
                                  , openAlphaTyVar, openBetaTyVar ] $
                  mkVisFunTyMany openAlphaTy openBetaTy

             arity = 1

             concs = mkRepPolyIdConcreteTyVars
                     [((mkTyVarTy openAlphaTyVar, Argument 1 Top), runtimeRep1TyVar)]
                     unsafeCoercePrimName

             id   = mkExportedLocalId (RepPolyId concs) unsafeCoercePrimName ty `setIdInfo` info
       ; return (id, old_expr) }
