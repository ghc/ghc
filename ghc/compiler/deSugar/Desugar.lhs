%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
module Desugar ( deSugar ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_ds )
import HsSyn		( MonoBinds, RuleDecl(..), RuleBndr(..), HsExpr(..), HsBinds(..), MonoBinds(..) )
import HsCore		( UfRuleBody(..) )
import TcHsSyn		( TypecheckedMonoBinds, TypecheckedForeignDecl, TypecheckedRuleDecl )
import TcModule		( TcResults(..) )
import CoreSyn
import Rules		( ProtoCoreRule(..), pprProtoCoreRule )
import Subst		( substExpr, mkSubst )
import DsMonad
import DsExpr		( dsExpr )
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsForeign	( dsForeigns )
import DsUtils
import DsExpr		()	-- Forces DsExpr to be compiled; DsBinds only
				-- depends on DsExpr.hi-boot.
import Module		( Module, moduleString )
import Id		( Id )
import Name		( isLocallyDefined )
import VarEnv
import VarSet
import Bag		( isEmptyBag, unionBags )
import CmdLineOpts	( opt_SccGroup, opt_SccProfilingOn )
import CoreLint		( beginPass, endPass )
import ErrUtils		( doIfSet, pprBagOfWarnings )
import Outputable
import UniqSupply	( splitUniqSupply, UniqSupply )
\end{code}

%************************************************************************
%*									*
%* 		The main function: deSugar
%*									*
%************************************************************************

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: Module 
	-> UniqSupply
        -> TcResults
	-> IO ([CoreBind], [ProtoCoreRule], SDoc, SDoc)

deSugar mod_name us (TcResults {tc_env = global_val_env,
			        tc_binds = all_binds,
				tc_rules = rules,
			        tc_fords = fo_decls})
  = do
	beginPass "Desugar"
	-- Do desugaring
	let (result, ds_warns) = initDs us global_val_env module_and_group 
				        (dsProgram mod_name all_binds rules fo_decls)    
	    (ds_binds, ds_rules, _, _) = result

	 -- Display any warnings
        doIfSet (not (isEmptyBag ds_warns))
		(printErrs (pprBagOfWarnings ds_warns))

	 -- Lint result if necessary
        endPass "Desugar" opt_D_dump_ds ds_binds

	doIfSet opt_D_dump_ds (printDump (ppr_ds_rules ds_rules))

        return result
  where
    module_and_group = (mod_name, grp_name)
    grp_name  = case opt_SccGroup of
	          Just xx -> _PK_ xx
	    	  Nothing -> _PK_ (moduleString mod_name) -- default: module name

dsProgram mod_name all_binds rules fo_decls
  = dsMonoBinds auto_scc all_binds []	`thenDs` \ core_prs ->
    dsForeigns mod_name fo_decls	`thenDs` \ (fi_binds, fe_binds, h_code, c_code) ->
    mapDs dsRule rules			`thenDs` \ rules' ->
    let 
	ds_binds = fi_binds ++ [Rec core_prs] ++ fe_binds
    in
    returnDs (ds_binds, rules', h_code, c_code)
  where
    auto_scc | opt_SccProfilingOn = TopLevel
	     | otherwise          = NoSccs

ppr_ds_rules [] = empty
ppr_ds_rules rules
  = text "" $$ text "-------------- DESUGARED RULES -----------------" $$
    vcat (map pprProtoCoreRule rules)
\end{code}


%************************************************************************
%*									*
%* 		Desugaring transformation rules
%*									*
%************************************************************************

\begin{code}
dsRule :: TypecheckedRuleDecl -> DsM ProtoCoreRule
dsRule (IfaceRuleDecl fn (CoreRuleBody name all_vars args rhs) loc)
  = returnDs (ProtoCoreRule False {- non-local -} fn 
			    (Rule name all_vars args rhs))
    
dsRule (RuleDecl name sig_tvs vars lhs rhs loc)
  = putSrcLocDs loc		$
    ds_lhs all_vars lhs		`thenDs` \ (fn, args) ->
    dsExpr rhs			`thenDs` \ core_rhs ->
    returnDs (ProtoCoreRule True {- local -} fn
			    (Rule name all_vars args core_rhs))
  where
    all_vars = sig_tvs ++ [var | RuleBndr var <- vars]

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

	subst = mkSubst (mkVarSet all_vars) subst_env
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
