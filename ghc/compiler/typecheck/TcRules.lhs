%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcRules]{Typechecking transformation rules}

\begin{code}
module TcRules ( tcRules ) where

#include "HsVersions.h"

import HsSyn		( RuleDecl(..), RuleBndr(..), collectRuleBndrSigTys )
import CoreSyn		( CoreRule(..) )
import RnHsSyn		( RenamedRuleDecl )
import TcHsSyn		( TypecheckedRuleDecl, mkHsLet )
import TcRnMonad
import TcSimplify	( tcSimplifyToDicts, tcSimplifyInferCheck )
import TcMType		( newTyVarTy )
import TcType		( tyVarsOfTypes, openTypeKind )
import TcIfaceSig	( tcCoreExpr, tcCoreLamBndrs )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..), tcAddScopedTyVars )
import TcExpr		( tcCheckRho )
import TcEnv		( tcExtendLocalValEnv, tcLookupGlobalId, tcLookupId )
import Inst		( instToId )
import Id		( idType, mkLocalId )
import Outputable
\end{code}

\begin{code}
tcRules :: [RenamedRuleDecl] -> TcM [TypecheckedRuleDecl]
tcRules decls = mappM tcRule decls

tcRule :: RenamedRuleDecl -> TcM TypecheckedRuleDecl
tcRule (IfaceRule name act vars fun args rhs src_loc)
  = addSrcLoc src_loc 		$
    addErrCtxt (ruleCtxt name)	$
    tcLookupGlobalId fun		`thenM` \ fun' ->
    tcCoreLamBndrs vars			$ \ vars' ->
    mappM tcCoreExpr args		`thenM` \ args' ->
    tcCoreExpr rhs			`thenM` \ rhs' ->
    returnM (IfaceRuleOut fun' (Rule name act vars' args' rhs'))

tcRule (IfaceRuleOut fun rule)	-- Built-in rules, and only built-in rules, 
				-- come this way.  Usually IfaceRuleOut is only
				-- used for the *output* of the type checker
  = tcLookupId fun		`thenM` \ fun' ->
	-- NB: tcLookupId, not tcLookupGlobalId
	-- Reason: when compiling GHC.Base, where eqString is defined,
	--	   we'll get the builtin rule for eqString, but eqString
	--	   will be in the *local* type environment.
	-- Seems like a bit of a hack
    returnM (IfaceRuleOut fun' rule)   

tcRule (HsRule name act vars lhs rhs src_loc)
  = addSrcLoc src_loc 				$
    addErrCtxt (ruleCtxt name)			$
    newTyVarTy openTypeKind				`thenM` \ rule_ty ->

	-- Deal with the tyvars mentioned in signatures
    tcAddScopedTyVars (collectRuleBndrSigTys vars) (

		-- Ditto forall'd variables
	mappM new_id vars			`thenM` \ ids ->
	tcExtendLocalValEnv ids			$
	
		-- Now LHS and RHS
	getLIE (tcCheckRho lhs rule_ty)	`thenM` \ (lhs', lhs_lie) ->
	getLIE (tcCheckRho rhs rule_ty)	`thenM` \ (rhs', rhs_lie) ->
	
	returnM (ids, lhs', rhs', lhs_lie, rhs_lie)
    )				`thenM` \ (ids, lhs', rhs', lhs_lie, rhs_lie) ->

		-- Check that LHS has no overloading at all
    getLIE (tcSimplifyToDicts lhs_lie)	`thenM` \ (lhs_binds, lhs_dicts) ->

	-- Gather the template variables and tyvars
    let
	tpl_ids = map instToId lhs_dicts ++ ids

	-- IMPORTANT!  We *quantify* over any dicts that appear in the LHS
	-- Reason: 
	--	a) The particular dictionary isn't important, because its value
	--	   depends only on the type
	--		e.g	gcd Int $fIntegralInt
	--         Here we'd like to match against (gcd Int any_d) for any 'any_d'
	--
	--	b) We'd like to make available the dictionaries bound 
	--	   on the LHS in the RHS, so quantifying over them is good
	--	   See the 'lhs_dicts' in tcSimplifyAndCheck for the RHS

	-- We initially quantify over any tyvars free in *either* the rule
	-- *or* the bound variables.  The latter is important.  Consider
	--	ss (x,(y,z)) = (x,z)
	--	RULE:  forall v. fst (ss v) = fst v
	-- The type of the rhs of the rule is just a, but v::(a,(b,c))
	--
	-- We also need to get the free tyvars of the LHS; but we do that
	-- during zonking (see TcHsSyn.zonkRule)
	--
	forall_tvs = tyVarsOfTypes (rule_ty : map idType tpl_ids)
    in
	-- RHS can be a bit more lenient.  In particular,
	-- we let constant dictionaries etc float outwards
	--
	-- NB: tcSimplifyInferCheck zonks the forall_tvs, and 
	--     knocks out any that are constrained by the environment
    tcSimplifyInferCheck (text "tcRule")
			 forall_tvs
			 lhs_dicts rhs_lie	`thenM` \ (forall_tvs1, rhs_binds) ->

    returnM (HsRule name act
		    (map RuleBndr (forall_tvs1 ++ tpl_ids))	-- yuk
		    (mkHsLet lhs_binds lhs')
		    (mkHsLet rhs_binds rhs')
		    src_loc)
  where
    new_id (RuleBndr var) 	   = newTyVarTy openTypeKind			`thenM` \ ty ->
		          	     returnM (mkLocalId var ty)
    new_id (RuleBndrSig var rn_ty) = tcHsSigType (RuleSigCtxt var) rn_ty	`thenM` \ ty ->
				     returnM (mkLocalId var ty)

ruleCtxt name = ptext SLIT("When checking the transformation rule") <+> 
		doubleQuotes (ftext name)
\end{code}




