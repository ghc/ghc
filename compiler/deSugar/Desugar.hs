{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The Desugarer: turning HsSyn into Core.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Desugar (
    -- * Desugaring operations
    deSugar, deSugarExpr
    ) where

#include "HsVersions.h"

import DsUsage
import DynFlags
import HscTypes
import HsSyn
import TcRnTypes
import TcRnMonad  ( finalSafeMode, fixSafeInstances )
import TcRnDriver ( runTcInteractive )
import Id
import Name
import Type
import InstEnv
import Class
import Avail
import CoreSyn
import CoreFVs     ( exprsSomeFreeVarsList )
import CoreOpt     ( simpleOptPgm, simpleOptExpr )
import PprCore
import DsMonad
import DsExpr
import DsBinds
import DsForeign
import PrelNames   ( coercibleTyConKey )
import TysPrim     ( eqReprPrimTyCon )
import Unique      ( hasKey )
import Coercion    ( mkCoVarCo )
import TysWiredIn  ( coercibleDataCon )
import DataCon     ( dataConWrapId )
import MkCore      ( mkCoreLet )
import Module
import NameSet
import NameEnv
import Rules
import BasicTypes       ( Activation(.. ), competesWith, pprRuleName )
import CoreMonad        ( CoreToDo(..) )
import CoreLint         ( endPassIO )
import VarSet
import FastString
import ErrUtils
import Outputable
import SrcLoc
import Coverage
import Util
import MonadUtils
import OrdList

import Data.List
import Data.IORef
import Control.Monad( when )

{-
************************************************************************
*                                                                      *
*              The main function: deSugar
*                                                                      *
************************************************************************
-}

-- | Main entry point to the desugarer.
deSugar :: HscEnv -> ModLocation -> TcGblEnv -> IO (Messages, Maybe ModGuts)
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
                            tcg_vects        = vects,
                            tcg_patsyns      = patsyns,
                            tcg_tcs          = tcs,
                            tcg_insts        = insts,
                            tcg_fam_insts    = fam_insts,
                            tcg_hpc          = other_hpc_info,
                            tcg_complete_matches = complete_matches
                            })

  = do { let dflags = hsc_dflags hsc_env
             print_unqual = mkPrintUnqualified dflags rdr_env
        ; withTiming (pure dflags)
                     (text "Desugar"<+>brackets (ppr mod))
                     (const ()) $
     do { -- Desugar the program
        ; let export_set = availsToNameSet exports
              target     = hscTarget dflags
              hpcInfo    = emptyHpcInfo other_hpc_info

        ; (binds_cvr, ds_hpc_info, modBreaks)
                         <- if not (isHsBootOrSig hsc_src)
                              then addTicksToBinds hsc_env mod mod_loc
                                       export_set (typeEnvTyCons type_env) binds
                              else return (binds, hpcInfo, Nothing)
        ; (msgs, mb_res) <- initDs hsc_env tcg_env $
                       do { ds_ev_binds <- dsEvBinds ev_binds
                          ; core_prs <- dsTopLHsBinds binds_cvr
                          ; (spec_prs, spec_rules) <- dsImpSpecs imp_specs
                          ; (ds_fords, foreign_prs) <- dsForeigns fords
                          ; ds_rules <- mapMaybeM dsRule rules
                          ; ds_vects <- mapM dsVect vects
                          ; let hpc_init
                                  | gopt Opt_Hpc dflags = hpcInitCode mod ds_hpc_info
                                  | otherwise = empty
                          ; return ( ds_ev_binds
                                   , foreign_prs `appOL` core_prs `appOL` spec_prs
                                   , spec_rules ++ ds_rules, ds_vects
                                   , ds_fords `appendStubC` hpc_init) }

        ; case mb_res of {
           Nothing -> return (msgs, Nothing) ;
           Just (ds_ev_binds, all_prs, all_rules, vects0, ds_fords) ->

     do {       -- Add export flags to bindings
          keep_alive <- readIORef keep_var
        ; let (rules_for_locals, rules_for_imps) = partition isLocalRule all_rules
              final_prs = addExportFlagsAndRules target export_set keep_alive
                                                 rules_for_locals (fromOL all_prs)

              final_pgm = combineEvBinds ds_ev_binds final_prs
        -- Notice that we put the whole lot in a big Rec, even the foreign binds
        -- When compiling PrelFloat, which defines data Float = F# Float#
        -- we want F# to be in scope in the foreign marshalling code!
        -- You might think it doesn't matter, but the simplifier brings all top-level
        -- things into the in-scope set before simplifying; so we get no unfolding for F#!

#if defined(DEBUG)
          -- Debug only as pre-simple-optimisation program may be really big
        ; endPassIO hsc_env print_unqual CoreDesugar final_pgm rules_for_imps
#endif
        ; (ds_binds, ds_rules_for_imps, ds_vects)
            <- simpleOptPgm dflags mod final_pgm rules_for_imps vects0
                         -- The simpleOptPgm gets rid of type
                         -- bindings plus any stupid dead code

        ; endPassIO hsc_env print_unqual CoreDesugarOpt ds_binds ds_rules_for_imps

        ; let used_names = mkUsedNames tcg_env
        ; deps <- mkDependencies tcg_env

        ; used_th <- readIORef tc_splice_used
        ; dep_files <- readIORef dependent_files
        ; safe_mode <- finalSafeMode dflags tcg_env
        ; usages <- mkUsageInfo hsc_env mod (imp_mods imports) used_names dep_files merged
        -- id_mod /= mod when we are processing an hsig, but hsigs
        -- never desugared and compiled (there's no code!)
        -- Consequently, this should hold for any ModGuts that make
        -- past desugaring. See Note [Identity versus semantic module].
        ; MASSERT( id_mod == mod )

        ; foreign_files <- readIORef th_foreign_files_var

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
                mg_insts        = fixSafeInstances safe_mode insts,
                mg_fam_insts    = fam_insts,
                mg_inst_env     = inst_env,
                mg_fam_inst_env = fam_inst_env,
                mg_patsyns      = patsyns,
                mg_rules        = ds_rules_for_imps,
                mg_binds        = ds_binds,
                mg_foreign      = ds_fords,
                mg_foreign_files = foreign_files,
                mg_hpc_info     = ds_hpc_info,
                mg_modBreaks    = modBreaks,
                mg_vect_decls   = ds_vects,
                mg_vect_info    = noVectInfo,
                mg_safe_haskell = safe_mode,
                mg_trust_pkg    = imp_trust_own_pkg imports,
                mg_complete_sigs = complete_matches
              }
        ; return (msgs, Just mod_guts)
        }}}}

mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan mod_loc
  = case ml_hs_file mod_loc of
      Just file_path -> mkGeneralSrcSpan (mkFastString file_path)
      Nothing        -> interactiveSrcSpan   -- Presumably

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
because the occurrence analyser doesn't teke account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.
-}

deSugarExpr :: HscEnv -> LHsExpr GhcTc -> IO (Messages, Maybe CoreExpr)

deSugarExpr hsc_env tc_expr = do {
         let dflags = hsc_dflags hsc_env

       ; showPass dflags "Desugar"

         -- Do desugaring
       ; (msgs, mb_core_expr) <- runTcInteractive hsc_env $ initDsTc $
                                 dsLExpr tc_expr

       ; case mb_core_expr of
            Nothing   -> return ()
            Just expr -> dumpIfSet_dyn dflags Opt_D_dump_ds "Desugared"
                         (pprCoreExpr expr)

       ; return (msgs, mb_core_expr) }

{-
************************************************************************
*                                                                      *
*              Add rules and export flags to binders
*                                                                      *
************************************************************************
-}

addExportFlagsAndRules
    :: HscTarget -> NameSet -> NameSet -> [CoreRule]
    -> [(Id, t)] -> [(Id, t)]
addExportFlagsAndRules target exports keep_alive rules prs
  = mapFst add_one prs
  where
    add_one bndr = add_rules name (add_export name bndr)
       where
         name = idName bndr

    ---------- Rules --------
        -- See Note [Attach rules to local ids]
        -- NB: the binder might have some existing rules,
        -- arising from specialisation pragmas
    add_rules name bndr
        | Just rules <- lookupNameEnv rule_base name
        = bndr `addIdSpecialisations` rules
        | otherwise
        = bndr
    rule_base = extendRuleBaseList emptyRuleBase rules

    ---------- Export flag --------
    -- See Note [Adding export flags]
    add_export name bndr
        | dont_discard name = setIdExported bndr
        | otherwise         = bndr

    dont_discard :: Name -> Bool
    dont_discard name = is_exported name
                     || name `elemNameSet` keep_alive

        -- In interactive mode, we don't want to discard any top-level
        -- entities at all (eg. do not inline them away during
        -- simplification), and retain them all in the TypeEnv so they are
        -- available from the command line.
        --
        -- isExternalName separates the user-defined top-level names from those
        -- introduced by the type checker.
    is_exported :: Name -> Bool
    is_exported | targetRetainsAllBindings target = isExternalName
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
  - It means that transformation rules and specialisations for
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
*              Desugaring transformation rules
*                                                                      *
************************************************************************
-}

dsRule :: LRuleDecl GhcTc -> DsM (Maybe CoreRule)
dsRule (L loc (HsRule name rule_act vars lhs _tv_lhs rhs _fv_rhs))
  = putSrcSpanDs loc $
    do  { let bndrs' = [var | L _ (RuleBndr (L _ var)) <- vars]

        ; lhs' <- unsetGOptM Opt_EnableRewriteRules $
                  unsetWOptM Opt_WarnIdentities $
                  dsLExpr lhs   -- Note [Desugaring RULE left hand sides]

        ; rhs' <- dsLExpr rhs
        ; this_mod <- getModule

        ; (bndrs'', lhs'', rhs'') <- unfold_coerce bndrs' lhs' rhs'

        -- Substitute the dict bindings eagerly,
        -- and take the body apart into a (f args) form
        ; case decomposeRuleLhs bndrs'' lhs'' of {
                Left msg -> do { warnDs NoReason msg; return Nothing } ;
                Right (final_bndrs, fn_id, args) -> do

        { let is_local = isLocalId fn_id
                -- NB: isLocalId is False of implicit Ids.  This is good because
                -- we don't want to attach rules to the bindings of implicit Ids,
                -- because they don't show up in the bindings until just before code gen
              fn_name   = idName fn_id
              final_rhs = simpleOptExpr rhs''    -- De-crap it
              rule_name = snd (unLoc name)
              final_bndrs_set = mkVarSet final_bndrs
              arg_ids = filterOut (`elemVarSet` final_bndrs_set) $
                        exprsSomeFreeVarsList isId args

        ; dflags <- getDynFlags
        ; rule <- dsMkUserRule this_mod is_local
                         rule_name rule_act fn_name final_bndrs args
                         final_rhs
        ; when (wopt Opt_WarnInlineRuleShadowing dflags) $
          warnRuleShadowing rule_name rule_act fn_id arg_ids

        ; return (Just rule)
        } } }


warnRuleShadowing :: RuleName -> Activation -> Id -> [Id] -> DsM ()
-- See Note [Rules and inlining/other rules]
warnRuleShadowing rule_name rule_act fn_id arg_ids
  = do { check False fn_id    -- We often have multiple rules for the same Id in a
                              -- module. Maybe we should check that they don't overlap
                              -- but currently we don't
       ; mapM_ (check True) arg_ids }
  where
    check check_rules_too lhs_id
      | isLocalId lhs_id || canUnfold (idUnfolding lhs_id)
                       -- If imported with no unfolding, no worries
      , idInlineActivation lhs_id `competesWith` rule_act
      = warnDs (Reason Opt_WarnInlineRuleShadowing)
               (vcat [ hang (text "Rule" <+> pprRuleName rule_name
                               <+> text "may never fire")
                            2 (text "because" <+> quotes (ppr lhs_id)
                               <+> text "might inline first")
                     , text "Probable fix: add an INLINE[n] or NOINLINE[n] pragma for"
                       <+> quotes (ppr lhs_id)
                     , ifPprDebug (ppr (idInlineActivation lhs_id) $$ ppr rule_act) ])

      | check_rules_too
      , bad_rule : _ <- get_bad_rules lhs_id
      = warnDs (Reason Opt_WarnInlineRuleShadowing)
               (vcat [ hang (text "Rule" <+> pprRuleName rule_name
                               <+> text "may never fire")
                            2 (text "because rule" <+> pprRuleName (ruleName bad_rule)
                               <+> text "for"<+> quotes (ppr lhs_id)
                               <+> text "might fire first")
                      , text "Probable fix: add phase [n] or [~n] to the competing rule"
                      , ifPprDebug (ppr bad_rule) ])

      | otherwise
      = return ()

    get_bad_rules lhs_id
      = [ rule | rule <- idCoreRules lhs_id
               , ruleActivation rule `competesWith` rule_act ]

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
switching off EnableRewriteRules.  See DsExpr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.

Nor do we want to warn of conversion identities on the LHS;
the rule is precisly to optimise them:
  {-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}

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
See also Note [Getting the map/coerce RULE to work] in CoreSubst.

Note [Rules and inlining/other rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have
  f x = ...
  g x = ...
  {-# RULES "rule-for-f" forall x. f (g x) = ... #-}
then there's a good chance that in a potential rule redex
    ...f (g e)...
then 'f' or 'g' will inline befor the rule can fire.  Solution: add an
INLINE [n] or NOINLINE [n] pragma to 'f' and 'g'.

Note that this applies to all the free variables on the LHS, both the
main function and things in its arguments.

We also check if there are Ids on the LHS that have competing RULES.
In the above example, suppose we had
  {-# RULES "rule-for-g" forally. g [y] = ... #-}
Then "rule-for-f" and "rule-for-g" would compete.  Better to add phase
control, so "rule-for-f" has a chance to fire before "rule-for-g" becomes
active; or perhpas after "rule-for-g" has become inactive. This is checked
by 'competesWith'

Class methods have a built-in RULE to select the method from the dictionary,
so you can't change the phase on this.  That makes id very dubious to
match on class methods in RULE lhs's.   See Trac #10595.   I'm not happy
about this. For example in Control.Arrow we have

{-# RULES "compose/arr"   forall f g .
                          (arr f) . (arr g) = arr (f . g) #-}

and similar, which will elicit exactly these warnings, and risk never
firing.  But it's not clear what to do instead.  We could make the
class methocd rules inactive in phase 2, but that would delay when
subsequent transformations could fire.


************************************************************************
*                                                                      *
*              Desugaring vectorisation declarations
*                                                                      *
************************************************************************
-}

dsVect :: LVectDecl GhcTc -> DsM CoreVect
dsVect (L loc (HsVect _ (L _ v) rhs))
  = putSrcSpanDs loc $
    do { rhs' <- dsLExpr rhs
       ; return $ Vect v rhs'
       }
dsVect (L _loc (HsNoVect _ (L _ v)))
  = return $ NoVect v
dsVect (L _loc (HsVectTypeOut isScalar tycon rhs_tycon))
  = return $ VectType isScalar tycon' rhs_tycon
  where
    tycon' | Just ty <- coreView $ mkTyConTy tycon
           , (tycon', []) <- splitTyConApp ty      = tycon'
           | otherwise                             = tycon
dsVect vd@(L _ (HsVectTypeIn _ _ _ _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectTypeIn'" (ppr vd)
dsVect (L _loc (HsVectClassOut cls))
  = return $ VectClass (classTyCon cls)
dsVect vc@(L _ (HsVectClassIn _ _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectClassIn'" (ppr vc)
dsVect (L _loc (HsVectInstOut inst))
  = return $ VectInst (instanceDFunId inst)
dsVect vi@(L _ (HsVectInstIn _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectInstIn'" (ppr vi)
