%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Desugar]{@deSugar@: the main function}

\begin{code}
module Desugar ( deSugar, deSugarExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), dopt, opt_SccProfilingOn )
import HscTypes		( ModDetails(..) )
import HsSyn		( MonoBinds, RuleDecl(..), RuleBndr(..), 
			  HsExpr(..), HsBinds(..), MonoBinds(..) )
import TcHsSyn		( TypecheckedRuleDecl, TypecheckedHsExpr )
import TcModule		( TcResults(..) )
import Id		( Id )
import CoreSyn
import PprCore		( pprIdCoreRule, pprCoreExpr )
import Subst		( substExpr, mkSubst, mkInScopeSet )
import DsMonad
import DsExpr		( dsExpr )
import DsBinds		( dsMonoBinds, AutoScc(..) )
import DsForeign	( dsForeigns )
import DsExpr		()	-- Forces DsExpr to be compiled; DsBinds only
				-- depends on DsExpr.hi-boot.
import Module		( Module )
import Id		( Id )
import NameEnv		( lookupNameEnv )
import VarEnv
import VarSet
import Bag		( isEmptyBag )
import CoreLint		( showPass, endPass )
import ErrUtils		( doIfSet, dumpIfSet_dyn, pprBagOfWarnings )
import Outputable
import UniqSupply	( mkSplitUniqSupply )
import HscTypes		( HomeSymbolTable, PersistentCompilerState(..), TyThing(..), lookupType,  )
\end{code}

%************************************************************************
%*									*
%* 		The main function: deSugar
%*									*
%************************************************************************

The only trick here is to get the @DsMonad@ stuff off to a good
start.

\begin{code}
deSugar :: DynFlags
	-> PersistentCompilerState -> HomeSymbolTable
	-> Module -> PrintUnqualified
        -> TcResults
	-> IO (ModDetails, (SDoc, SDoc, [CoreBndr]))

deSugar dflags pcs hst mod_name unqual
        (TcResults {tc_env   = type_env,
		    tc_binds = all_binds,
		    tc_insts = insts,
		    tc_rules = rules,
		    tc_fords = fo_decls})
  = do	{ showPass dflags "Desugar"
	; us <- mkSplitUniqSupply 'd'

	-- Do desugaring
	; let (ds_result, ds_warns) = initDs dflags us lookup mod_name
					     (dsProgram mod_name all_binds rules fo_decls)    

	      (ds_binds, ds_rules, foreign_stuff) = ds_result
	
	      mod_details = ModDetails { md_types = type_env,
					 md_insts = insts,
					 md_rules = ds_rules,
					 md_binds = ds_binds }

	-- Display any warnings
        ; doIfSet (not (isEmptyBag ds_warns))
		  (printErrs unqual (pprBagOfWarnings ds_warns))

	-- Lint result if necessary
        ; endPass dflags "Desugar" Opt_D_dump_ds ds_binds

	-- Dump output
	; doIfSet (dopt Opt_D_dump_ds dflags) 
		(printDump (ppr_ds_rules ds_rules))

        ; return (mod_details, foreign_stuff)
	}

  where
	-- The lookup function passed to initDs is used for well-known Ids, 
	-- such as fold, build, cons etc, so the chances are
	-- it'll be found in the package symbol table.  That's
	-- why we don't merge all these tables
    pte      = pcs_PTE pcs
    lookup n = case lookupType hst pte n of {
		 Just (AnId v) -> v ;
		 other -> 
	       case lookupNameEnv type_env n of
		 Just (AnId v) -> v ;
		 other	       -> pprPanic "Desugar: lookup:" (ppr n)
               }

deSugarExpr :: DynFlags
	    -> PersistentCompilerState -> HomeSymbolTable
	    -> Module -> PrintUnqualified
 	    -> TypecheckedHsExpr
	    -> IO CoreExpr
deSugarExpr dflags pcs hst mod_name unqual tc_expr
  = do	{ showPass dflags "Desugar"
	; us <- mkSplitUniqSupply 'd'

	-- Do desugaring
	; let (core_expr, ds_warns) = initDs dflags us lookup mod_name (dsExpr tc_expr)    

	-- Display any warnings
        ; doIfSet (not (isEmptyBag ds_warns))
		  (printErrs unqual (pprBagOfWarnings ds_warns))

	-- Dump output
	; dumpIfSet_dyn dflags Opt_D_dump_ds "Desugared" (pprCoreExpr core_expr)

        ; return core_expr
	}
  where
    pte      = pcs_PTE pcs
    lookup n = case lookupType hst pte n of
		 Just (AnId v) -> v 
		 other	       -> pprPanic "Desugar: lookup:" (ppr n)

dsProgram mod_name all_binds rules fo_decls
  = dsMonoBinds auto_scc all_binds []	`thenDs` \ core_prs ->
    dsForeigns mod_name fo_decls	`thenDs` \ (fe_binders, foreign_binds, h_code, c_code) ->
    let
	ds_binds      = [Rec (foreign_binds ++ core_prs)]
	-- Notice that we put the whole lot in a big Rec, even the foreign binds
	-- When compiling PrelFloat, which defines data Float = F# Float#
	-- we want F# to be in scope in the foreign marshalling code!
	-- You might think it doesn't matter, but the simplifier brings all top-level
	-- things into the in-scope set before simplifying; so we get no unfolding for F#!

	local_binders = mkVarSet (bindersOfBinds ds_binds)
    in
    mapDs (dsRule local_binders) rules	`thenDs` \ rules' ->
    returnDs (ds_binds, rules', (h_code, c_code, fe_binders))
  where
    auto_scc | opt_SccProfilingOn = TopLevel
	     | otherwise          = NoSccs

ppr_ds_rules [] = empty
ppr_ds_rules rules
  = text "" $$ text "-------------- DESUGARED RULES -----------------" $$
    vcat (map pprIdCoreRule rules)
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
