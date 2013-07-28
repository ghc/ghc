%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The Desugarer: turning HsSyn into Core.

\begin{code}
module Desugar ( deSugar, deSugarExpr ) where

import DynFlags
import HscTypes
import HsSyn
import TcRnTypes
import TcRnMonad ( finalSafeMode )
import MkIface
import Id
import Name
import Type
import InstEnv
import Class
import Avail
import CoreSyn
import CoreSubst
import PprCore
import DsMonad
import DsExpr
import DsBinds
import DsForeign
import Module
import RdrName
import NameSet
import NameEnv
import Rules
import BasicTypes       ( Activation(.. ) )
import CoreMonad        ( endPass, CoreToDo(..) )
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
\end{code}

%************************************************************************
%*                                                                      *
%*              The main function: deSugar
%*                                                                      *
%************************************************************************

\begin{code}
-- | Main entry point to the desugarer.
deSugar :: HscEnv -> ModLocation -> TcGblEnv -> IO (Messages, Maybe ModGuts)
-- Can modify PCS by faulting in more declarations

deSugar hsc_env
        mod_loc
        tcg_env@(TcGblEnv { tcg_mod          = mod,
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
                            tcg_warns        = warns,
                            tcg_anns         = anns,
                            tcg_binds        = binds,
                            tcg_imp_specs    = imp_specs,
                            tcg_dependent_files = dependent_files,
                            tcg_ev_binds     = ev_binds,
                            tcg_fords        = fords,
                            tcg_rules        = rules,
                            tcg_vects        = vects,
                            tcg_tcs          = tcs,
                            tcg_insts        = insts,
                            tcg_fam_insts    = fam_insts,
                            tcg_hpc          = other_hpc_info })

  = do { let dflags = hsc_dflags hsc_env
        ; showPass dflags "Desugar"

        -- Desugar the program
        ; let export_set = availsToNameSet exports
        ; let target = hscTarget dflags
        ; let hpcInfo = emptyHpcInfo other_hpc_info
        ; (msgs, mb_res) <- do

                     let want_ticks = gopt Opt_Hpc dflags
                                   || target == HscInterpreted
                                   || (gopt Opt_SccProfilingOn dflags
                                       && case profAuto dflags of
                                            NoProfAuto -> False
                                            _          -> True)

                     (binds_cvr,ds_hpc_info, modBreaks)
                         <- if want_ticks && not (isHsBoot hsc_src)
                              then addTicksToBinds dflags mod mod_loc export_set
                                          (typeEnvTyCons type_env) binds
                              else return (binds, hpcInfo, emptyModBreaks)

                     initDs hsc_env mod rdr_env type_env $ do
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
                                   , ds_fords `appendStubC` hpc_init
                                   , ds_hpc_info, modBreaks) }

        ; case mb_res of {
           Nothing -> return (msgs, Nothing) ;
           Just (ds_ev_binds, all_prs, all_rules, vects0, ds_fords, ds_hpc_info, modBreaks) -> do

        {       -- Add export flags to bindings
          keep_alive <- readIORef keep_var
        ; let (rules_for_locals, rules_for_imps)
                   = partition isLocalRule all_rules
              final_prs = addExportFlagsAndRules target
                              export_set keep_alive rules_for_locals (fromOL all_prs)

              final_pgm = combineEvBinds ds_ev_binds final_prs
        -- Notice that we put the whole lot in a big Rec, even the foreign binds
        -- When compiling PrelFloat, which defines data Float = F# Float#
        -- we want F# to be in scope in the foreign marshalling code!
        -- You might think it doesn't matter, but the simplifier brings all top-level
        -- things into the in-scope set before simplifying; so we get no unfolding for F#!

#ifdef DEBUG
          -- Debug only as pre-simple-optimisation program may be really big
        ; endPass dflags CoreDesugar final_pgm rules_for_imps
#endif
        ; (ds_binds, ds_rules_for_imps, ds_vects)
            <- simpleOptPgm dflags mod final_pgm rules_for_imps vects0
                         -- The simpleOptPgm gets rid of type
                         -- bindings plus any stupid dead code

        ; endPass dflags CoreDesugarOpt ds_binds ds_rules_for_imps

        ; let used_names = mkUsedNames tcg_env
        ; deps <- mkDependencies tcg_env

        ; used_th <- readIORef tc_splice_used
        ; dep_files <- readIORef dependent_files
        ; safe_mode <- finalSafeMode dflags tcg_env

        ; let mod_guts = ModGuts {
                mg_module       = mod,
                mg_boot         = isHsBoot hsc_src,
                mg_exports      = exports,
                mg_deps         = deps,
                mg_used_names   = used_names,
                mg_used_th      = used_th,
                mg_dir_imps     = imp_mods imports,
                mg_rdr_env      = rdr_env,
                mg_fix_env      = fix_env,
                mg_warns        = warns,
                mg_anns         = anns,
                mg_tcs          = tcs,
                mg_insts        = insts,
                mg_fam_insts    = fam_insts,
                mg_inst_env     = inst_env,
                mg_fam_inst_env = fam_inst_env,
                mg_rules        = ds_rules_for_imps,
                mg_binds        = ds_binds,
                mg_foreign      = ds_fords,
                mg_hpc_info     = ds_hpc_info,
                mg_modBreaks    = modBreaks,
                mg_vect_decls   = ds_vects,
                mg_vect_info    = noVectInfo,
                mg_safe_haskell = safe_mode,
                mg_trust_pkg    = imp_trust_own_pkg imports,
                mg_dependent_files = dep_files
              }
        ; return (msgs, Just mod_guts)
        }}}

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
\end{code}

Note [Top-level evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level evidence bindings may be mutually recursive with the top-level value
bindings, so we must put those in a Rec.  But we can't put them *all* in a Rec
because the occurrence analyser doesn't teke account of type/coercion variables
when computing dependencies.

So we pull out the type/coercion variables (which are in dependency order),
and Rec the rest.


\begin{code}
deSugarExpr :: HscEnv
            -> Module -> GlobalRdrEnv -> TypeEnv
            -> LHsExpr Id
            -> IO (Messages, Maybe CoreExpr)
-- Prints its own errors; returns Nothing if error occurred

deSugarExpr hsc_env this_mod rdr_env type_env tc_expr = do
    let dflags = hsc_dflags hsc_env
    showPass dflags "Desugar"

    -- Do desugaring
    (msgs, mb_core_expr) <- initDs hsc_env this_mod rdr_env type_env $
                                   dsLExpr tc_expr

    case mb_core_expr of
      Nothing   -> return (msgs, Nothing)
      Just expr -> do

        -- Dump output
        dumpIfSet_dyn dflags Opt_D_dump_ds "Desugared" (pprCoreExpr expr)

        return (msgs, Just expr)
\end{code}

%************************************************************************
%*                                                                      *
%*              Add rules and export flags to binders
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}


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


%************************************************************************
%*                                                                      *
%*              Desugaring transformation rules
%*                                                                      *
%************************************************************************

\begin{code}
dsRule :: LRuleDecl Id -> DsM (Maybe CoreRule)
dsRule (L loc (HsRule name act vars lhs _tv_lhs rhs _fv_rhs))
  = putSrcSpanDs loc $
    do  { let bndrs' = [var | RuleBndr (L _ var) <- vars]

        ; lhs' <- unsetGOptM Opt_EnableRewriteRules $
                  unsetWOptM Opt_WarnIdentities $
                  dsLExpr lhs   -- Note [Desugaring RULE left hand sides]

        ; rhs' <- dsLExpr rhs
        ; dflags <- getDynFlags

        -- Substitute the dict bindings eagerly,
        -- and take the body apart into a (f args) form
        ; case decomposeRuleLhs bndrs' lhs' of {
                Left msg -> do { warnDs msg; return Nothing } ;
                Right (final_bndrs, fn_id, args) -> do

        { let is_local = isLocalId fn_id
                -- NB: isLocalId is False of implicit Ids.  This is good because
                -- we don't want to attach rules to the bindings of implicit Ids,
                -- because they don't show up in the bindings until just before code gen
              fn_name   = idName fn_id
              final_rhs = simpleOptExpr rhs'    -- De-crap it
              rule      = mkRule False {- Not auto -} is_local
                                 name act fn_name final_bndrs args final_rhs

              inline_shadows_rule   -- Function can be inlined before rule fires
                | wopt Opt_WarnInlineRuleShadowing dflags
                = case (idInlineActivation fn_id, act) of
                    (NeverActive, _)    -> False
                    (AlwaysActive, _)   -> True
                    (ActiveBefore {}, _) -> True
                    (ActiveAfter {}, NeverActive)     -> True
                    (ActiveAfter n, ActiveAfter r)    -> r < n  -- Rule active strictly first
                    (ActiveAfter {}, AlwaysActive)    -> False
                    (ActiveAfter {}, ActiveBefore {}) -> False
                | otherwise = False

        ; when inline_shadows_rule $
          warnDs (vcat [ hang (ptext (sLit "Rule") <+> doubleQuotes (ftext name)
                               <+> ptext (sLit "may never fire"))
                            2 (ptext (sLit "because") <+> quotes (ppr fn_id)
                               <+> ptext (sLit "might inline first"))
                       , ptext (sLit "Probable fix: add an INLINE[n] or NOINLINE[n] pragma on")
                         <+> quotes (ppr fn_id) ])

        ; return (Just rule)
        } } }
\end{code}

Note [Desugaring RULE left hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the LHS of a RULE we do *not* want to desugar
    [x]   to    build (\cn. x `c` n)
We want to leave explicit lists simply as chains
of cons's. We can achieve that slightly indirectly by
switching off EnableRewriteRules.  See DsExpr.dsExplicitList.

That keeps the desugaring of list comprehensions simple too.



Nor do we want to warn of conversion identities on the LHS;
the rule is precisly to optimise them:
  {-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}


%************************************************************************
%*                                                                      *
%*              Desugaring vectorisation declarations
%*                                                                      *
%************************************************************************

\begin{code}
dsVect :: LVectDecl Id -> DsM CoreVect
dsVect (L loc (HsVect (L _ v) rhs))
  = putSrcSpanDs loc $
    do { rhs' <- dsLExpr rhs
       ; return $ Vect v rhs'
       }
dsVect (L _loc (HsNoVect (L _ v)))
  = return $ NoVect v
dsVect (L _loc (HsVectTypeOut isScalar tycon rhs_tycon))
  = return $ VectType isScalar tycon' rhs_tycon
  where
    tycon' | Just ty <- coreView $ mkTyConTy tycon
           , (tycon', []) <- splitTyConApp ty      = tycon'
           | otherwise                             = tycon
dsVect vd@(L _ (HsVectTypeIn _ _ _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectTypeIn'" (ppr vd)
dsVect (L _loc (HsVectClassOut cls))
  = return $ VectClass (classTyCon cls)
dsVect vc@(L _ (HsVectClassIn _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectClassIn'" (ppr vc)
dsVect (L _loc (HsVectInstOut inst))
  = return $ VectInst (instanceDFunId inst)
dsVect vi@(L _ (HsVectInstIn _))
  = pprPanic "Desugar.dsVect: unexpected 'HsVectInstIn'" (ppr vi)
\end{code}
