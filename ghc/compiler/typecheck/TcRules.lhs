%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcRules]{Typechecking transformation rules}

\begin{code}
module TcRules ( tcIfaceRules, tcSourceRules ) where

#include "HsVersions.h"

import HsSyn		( RuleDecl(..), RuleBndr(..), collectRuleBndrSigTys )
import CoreSyn		( CoreRule(..) )
import RnHsSyn		( RenamedRuleDecl )
import HscTypes		( PackageRuleBase )
import TcHsSyn		( TypecheckedRuleDecl, mkHsLet )
import TcMonad
import TcSimplify	( tcSimplifyToDicts, tcSimplifyInferCheck )
import TcMType		( newTyVarTy )
import TcType		( tyVarsOfTypes, openTypeKind )
import TcIfaceSig	( tcCoreExpr, tcCoreLamBndrs, tcVar, tcDelay )
import TcMonoType	( kcHsSigTypes, tcHsSigType, UserTypeCtxt(..), tcAddScopedTyVars )
import TcExpr		( tcExpr )
import TcEnv		( RecTcEnv, tcExtendLocalValEnv, isLocalThing )
import Rules		( extendRuleBase )
import Inst		( LIE, plusLIEs, instToId )
import Id		( idName, idType, mkLocalId )
import Module		( Module )
import List		( partition )
import Outputable
\end{code}

\begin{code}
tcIfaceRules :: RecTcEnv -> PackageRuleBase -> Module -> [RenamedRuleDecl] 
	     -> TcM (PackageRuleBase, [TypecheckedRuleDecl])
tcIfaceRules unf_env pkg_rule_base mod decls 
  = tcDelay unf_env doc [] (
	-- We need the recursive env because the built-in rules show up as
	-- IfaceOut rules, sot they get typechecked by tcIfaceRules 
	mapTc tcIfaceRule decls
    )				`thenTc` \ new_rules ->
    let
	(local_rules, imported_rules) = partition is_local new_rules
	new_rule_base = foldl add pkg_rule_base imported_rules
    in
    returnTc (new_rule_base, local_rules)
  where
    doc = text "tcIfaceRules"
    add rule_base (IfaceRuleOut id rule) = extendRuleBase rule_base (id, rule)

	-- When relinking this module from its interface-file decls
	-- we'll have IfaceRules that are in fact local to this module
    is_local (IfaceRuleOut n _) = isLocalThing mod n
    is_local other		= True

tcIfaceRule :: RenamedRuleDecl -> TcM TypecheckedRuleDecl
  -- No zonking necessary!
tcIfaceRule (IfaceRule name act vars fun args rhs src_loc)
  = tcAddSrcLoc src_loc 		$
    tcAddErrCtxt (ruleCtxt name)	$
    tcVar fun				`thenTc` \ fun' ->
    tcCoreLamBndrs vars			$ \ vars' ->
    mapTc tcCoreExpr args		`thenTc` \ args' ->
    tcCoreExpr rhs			`thenTc` \ rhs' ->
    returnTc (IfaceRuleOut fun' (Rule name act vars' args' rhs'))

tcIfaceRule (IfaceRuleOut fun rule)	-- Built-in rules come this way
  = tcVar fun				`thenTc` \ fun' ->
    returnTc (IfaceRuleOut fun' rule)   

tcSourceRules :: [RenamedRuleDecl] -> TcM (LIE, [TypecheckedRuleDecl])
tcSourceRules decls
  = mapAndUnzipTc tcSourceRule decls	`thenTc` \ (lies, decls') ->
    returnTc (plusLIEs lies, decls')

tcSourceRule (HsRule name act vars lhs rhs src_loc)
  = tcAddSrcLoc src_loc 				$
    tcAddErrCtxt (ruleCtxt name)			$
    newTyVarTy openTypeKind				`thenNF_Tc` \ rule_ty ->

	-- Deal with the tyvars mentioned in signatures
    tcAddScopedTyVars (collectRuleBndrSigTys vars) (

		-- Ditto forall'd variables
	mapNF_Tc new_id vars					`thenNF_Tc` \ ids ->
	tcExtendLocalValEnv [(idName id, id) | id <- ids]	$
	
		-- Now LHS and RHS
	tcExpr lhs rule_ty					`thenTc` \ (lhs', lhs_lie) ->
	tcExpr rhs rule_ty					`thenTc` \ (rhs', rhs_lie) ->
	
	returnTc (ids, lhs', rhs', lhs_lie, rhs_lie)
    )						`thenTc` \ (ids, lhs', rhs', lhs_lie, rhs_lie) ->

		-- Check that LHS has no overloading at all
    tcSimplifyToDicts lhs_lie			`thenTc` \ (lhs_dicts, lhs_binds) ->

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
	-- It's still conceivable that there may be type variables mentioned
	-- in the LHS, but not in the type of the lhs, nor in the binders.
	-- They'll get zapped to (), but that's over-constraining really.
	-- Let's see if we get a problem.
	forall_tvs = tyVarsOfTypes (rule_ty : map idType tpl_ids)
    in

	-- RHS can be a bit more lenient.  In particular,
	-- we let constant dictionaries etc float outwards
	--
	-- 
    tcSimplifyInferCheck (text "tcRule")
			 forall_tvs
			 lhs_dicts rhs_lie	`thenTc` \ (forall_tvs1, lie', rhs_binds) ->

    returnTc (lie', HsRule	name act
				(map RuleBndr (forall_tvs1 ++ tpl_ids))	-- yuk
				(mkHsLet lhs_binds lhs')
				(mkHsLet rhs_binds rhs')
				src_loc)
  where
    new_id (RuleBndr var) 	   = newTyVarTy openTypeKind			`thenNF_Tc` \ ty ->
		          	     returnNF_Tc (mkLocalId var ty)
    new_id (RuleBndrSig var rn_ty) = tcHsSigType (RuleSigCtxt var) rn_ty	`thenTc` \ ty ->
				     returnNF_Tc (mkLocalId var ty)

ruleCtxt name = ptext SLIT("When checking the transformation rule") <+> 
		doubleQuotes (ptext name)
\end{code}




