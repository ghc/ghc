%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
module Desugar ( deSugar, deSugarExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), dopt, opt_SccProfilingOn )
import HscTypes		( ModGuts(..), ModGuts, HscEnv(..), ExternalPackageState(..), 
			  PersistentCompilerState(..), Dependencies(..), TypeEnv, GlobalRdrEnv,
	  		  lookupType, unQualInScope )
import HsSyn		( MonoBinds, RuleDecl(..), RuleBndr(..), 
			  HsExpr(..), HsBinds(..), MonoBinds(..) )
import TcHsSyn		( TypecheckedRuleDecl, TypecheckedHsExpr )
import TcRnTypes	( TcGblEnv(..), ImportAvails(..) )
import MkIface		( mkUsageInfo )
import Id		( Id )
import CoreSyn
import PprCore		( pprIdRules, pprCoreExpr )
import Subst		( substExpr, mkSubst, mkInScopeSet )
import DsMonad
import DsExpr		( dsExpr )
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsForeign	( dsForeigns )
import DsExpr		()	-- Forces DsExpr to be compiled; DsBinds only
				-- depends on DsExpr.hi-boot.
import Module		( Module, moduleEnvElts )
import Id		( Id )
import NameEnv		( lookupNameEnv )
import VarEnv
import VarSet
import Bag		( isEmptyBag, mapBag )
import CoreLint		( showPass, endPass )
import ErrUtils		( doIfSet, dumpIfSet_dyn, pprBagOfWarnings, addShortWarnLocLine )
import Outputable
import qualified Pretty
import UniqSupply	( mkSplitUniqSupply )
import Maybes		( orElse )
import SrcLoc		( SrcLoc )
import FastString
import DATA_IOREF	( readIORef )
\end{code}

%************************************************************************
%*									*
%* 		The main function: deSugar
%*									*
%************************************************************************

\begin{code}
deSugar :: HscEnv -> PersistentCompilerState
        -> TcGblEnv -> IO ModGuts

deSugar hsc_env pcs
        (TcGblEnv { tcg_mod      = mod,
		    tcg_type_env = type_env,
		    tcg_usages   = usage_var,
		    tcg_imports  = imports,
		    tcg_exports  = exports,
		    tcg_rdr_env  = rdr_env,
		    tcg_fix_env  = fix_env,
	    	    tcg_deprecs  = deprecs,
		    tcg_insts    = insts,
		    tcg_binds	 = binds,
		    tcg_fords    = fords,
		    tcg_rules	 = rules })
  = do	{ showPass dflags "Desugar"
	; us <- mkSplitUniqSupply 'd'
	; usages <- readIORef usage_var 

	-- Do desugaring
	; let ((ds_binds, ds_rules, ds_fords), ds_warns) 
		= initDs dflags us lookup mod
			 (dsProgram binds rules fords)
	
	      warn_doc = pprBagOfWarnings (mapBag mk_warn ds_warns)

	-- Display any warnings
        ; doIfSet (not (isEmptyBag ds_warns))
		  (printErrs warn_doc)

	-- Lint result if necessary
        ; endPass dflags "Desugar" Opt_D_dump_ds ds_binds

	-- Dump output
	; doIfSet (dopt Opt_D_dump_ds dflags) 
		  (printDump (ppr_ds_rules ds_rules))

	; let 
	     deps = Deps { dep_mods = moduleEnvElts (imp_dep_mods imports), 
			   dep_pkgs = imp_dep_pkgs imports,
			   dep_orphs = imp_orphs imports }
	     mod_guts = ModGuts {	
		mg_module   = mod,
		mg_exports  = exports,
		mg_deps	    = deps,
		mg_usages   = mkUsageInfo hsc_env eps imports usages,
		mg_dir_imps = [m | (m,_) <- moduleEnvElts (imp_mods imports)],
	        mg_rdr_env  = rdr_env,
		mg_fix_env  = fix_env,
		mg_deprecs  = deprecs,
		mg_types    = type_env,
		mg_insts    = insts,
	        mg_rules    = ds_rules,
		mg_binds    = ds_binds,
		mg_foreign  = ds_fords }
	
        ; return mod_guts
	}

  where
    dflags       = hsc_dflags hsc_env
    print_unqual = unQualInScope rdr_env

	-- Desugarer warnings are SDocs; here we
	-- add the info about whether or not to print unqualified
    mk_warn :: (SrcLoc,SDoc) -> (SrcLoc, Pretty.Doc)
    mk_warn (loc, sdoc) = addShortWarnLocLine loc print_unqual sdoc

	-- The lookup function passed to initDs is used for well-known Ids, 
	-- such as fold, build, cons etc, so the chances are
	-- it'll be found in the package symbol table.  That's
	-- why we don't merge all these tables
    eps	     = pcs_EPS pcs
    pte      = eps_PTE eps
    hpt      = hsc_HPT hsc_env
    lookup n = case lookupType hpt pte n of {
		 Just v -> v ;
		 other  -> 
	       case lookupNameEnv type_env n of
		 Just v -> v ;
		 other	-> pprPanic "Desugar: lookup:" (ppr n)
               }

deSugarExpr :: HscEnv
	    -> PersistentCompilerState
	    -> Module -> GlobalRdrEnv -> TypeEnv 
 	    -> TypecheckedHsExpr
	    -> IO CoreExpr
deSugarExpr hsc_env pcs this_mod rdr_env type_env tc_expr
  = do	{ showPass dflags "Desugar"
	; us <- mkSplitUniqSupply 'd'

	-- Do desugaring
	; let (core_expr, ds_warns) = initDs dflags us lookup this_mod (dsExpr tc_expr)    
	      warn_doc = pprBagOfWarnings (mapBag mk_warn ds_warns)

	-- Display any warnings
        ; doIfSet (not (isEmptyBag ds_warns))
		  (printErrs warn_doc)

	-- Dump output
	; dumpIfSet_dyn dflags Opt_D_dump_ds "Desugared" (pprCoreExpr core_expr)

        ; return core_expr
	}
  where
    dflags   = hsc_dflags hsc_env
    hpt      = hsc_HPT hsc_env
    pte      = eps_PTE (pcs_EPS pcs)
    lookup n = lookupNameEnv type_env n	`orElse`	-- Look in the type env of the
							-- current module first
	       lookupType hpt pte n 	`orElse`	-- Then other modules
	       pprPanic "Desugar: lookup:" (ppr n)

    mk_warn :: (SrcLoc,SDoc) -> (SrcLoc, Pretty.Doc)
    mk_warn (loc,sdoc) = addShortWarnLocLine loc print_unqual sdoc

    print_unqual = unQualInScope rdr_env

dsProgram all_binds rules fo_decls
  = dsMonoBinds auto_scc all_binds []	`thenDs` \ core_prs ->
    dsForeigns fo_decls			`thenDs` \ (ds_fords, foreign_binds) ->
    let
	ds_binds      = [Rec (foreign_binds ++ core_prs)]
	-- Notice that we put the whole lot in a big Rec, even the foreign binds
	-- When compiling PrelFloat, which defines data Float = F# Float#
	-- we want F# to be in scope in the foreign marshalling code!
	-- You might think it doesn't matter, but the simplifier brings all top-level
	-- things into the in-scope set before simplifying; so we get no unfolding for F#!

	local_binders = mkVarSet (bindersOfBinds ds_binds)
    in
    mapDs (dsRule local_binders) rules	`thenDs` \ ds_rules ->
    returnDs (ds_binds, ds_rules, ds_fords)
  where
    auto_scc | opt_SccProfilingOn = TopLevel
	     | otherwise          = NoSccs

ppr_ds_rules [] = empty
ppr_ds_rules rules
  = text "" $$ text "-------------- DESUGARED RULES -----------------" $$
    pprIdRules rules
\end{code}



%************************************************************************
%*									*
%* 		Desugaring transformation rules
%*									*
%************************************************************************

\begin{code}
dsRule :: IdSet -> TypecheckedRuleDecl -> DsM (Id, CoreRule)
dsRule in_scope (IfaceRuleOut fun rule)	-- Built-in rules come this way
  = returnDs (fun, rule)

dsRule in_scope (HsRule name act vars lhs rhs loc)
  = putSrcLocDs loc		$
    ds_lhs all_vars lhs		`thenDs` \ (fn, args) ->
    dsExpr rhs			`thenDs` \ core_rhs ->
    returnDs (fn, Rule name act tpl_vars args core_rhs)
  where
    tpl_vars = [var | RuleBndr var <- vars]
    all_vars = mkInScopeSet (in_scope `unionVarSet` mkVarSet tpl_vars)

ds_lhs all_vars lhs
  = let
	(dict_binds, body) = case lhs of
		(HsLet (MonoBind dict_binds _ _) body) -> (dict_binds, body)
		other			 	       -> (EmptyMonoBinds, lhs)
    in
    ds_dict_binds dict_binds 	`thenDs` \ dict_binds' ->
    dsExpr body			`thenDs` \ body' ->

	-- Substitute the dict bindings eagerly,
	-- and take the body apart into a (f args) form
    let
	subst_env = mkSubstEnv [id		     | (id,rhs) <- dict_binds']
			       [ContEx subst_env rhs | (id,rhs) <- dict_binds']
			-- Note recursion here... substitution won't terminate
			-- if there is genuine recursion... which there isn't

	subst = mkSubst all_vars subst_env
	body'' = substExpr subst body'
    in
	
	-- Now unpack the resulting body
    let
	pair = case collectArgs body'' of
			(Var fn, args) -> (fn, args)
			other	       -> pprPanic "dsRule" (ppr lhs)
    in
    returnDs pair

ds_dict_binds EmptyMonoBinds 	   = returnDs []
ds_dict_binds (AndMonoBinds b1 b2) = ds_dict_binds b1 	`thenDs` \ env1 ->
				     ds_dict_binds b2 	`thenDs` \ env2 ->
				     returnDs (env1 ++ env2)
ds_dict_binds (VarMonoBind id rhs) = dsExpr rhs		`thenDs` \ rhs' ->
				     returnDs [(id,rhs')]
\end{code}
