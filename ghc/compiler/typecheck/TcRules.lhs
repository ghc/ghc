%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcRules]{Typechecking transformation rules}

\begin{code}
module TcRules ( tcRules ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), RuleDecl(..), RuleBndr(..), HsTyVarBndr(..) )
import CoreSyn		( CoreRule(..) )
import RnHsSyn		( RenamedHsDecl )
import TcHsSyn		( TypecheckedRuleDecl, mkHsLet )
import TcMonad
import TcSimplify	( tcSimplifyToDicts, tcSimplifyAndCheck )
import TcType		( zonkTcTypes, zonkTcTyVarToTyVar, newTyVarTy )
import TcIfaceSig	( tcCoreExpr, tcCoreLamBndrs, tcVar )
import TcMonoType	( kcHsSigType, tcHsSigType, tcTyVars, checkSigTyVars )
import TcExpr		( tcExpr )
import TcEnv		( tcExtendLocalValEnv, tcExtendTyVarEnv	)
import Inst		( LIE, emptyLIE, plusLIEs, instToId )
import Id		( idType, idName, mkVanillaId )
import VarSet
import Type		( tyVarsOfTypes, openTypeKind )
import Bag		( bagToList )
import Outputable
\end{code}

\begin{code}
tcRules :: [RenamedHsDecl] -> TcM s (LIE, [TypecheckedRuleDecl])
tcRules decls = mapAndUnzipTc tcRule [rule | RuleD rule <- decls]	`thenTc` \ (lies, rules) ->
		returnTc (plusLIEs lies, rules)

tcRule (IfaceRule name vars fun args rhs src_loc)
  = tcAddSrcLoc src_loc 		$
    tcAddErrCtxt (ruleCtxt name)	$
    tcVar fun				`thenTc` \ fun' ->
    tcCoreLamBndrs vars			$ \ vars' ->
    mapTc tcCoreExpr args		`thenTc` \ args' ->
    tcCoreExpr rhs			`thenTc` \ rhs' ->
    returnTc (emptyLIE, IfaceRuleOut fun' (Rule name vars' args' rhs'))

tcRule (IfaceRuleOut fun rule)
  = tcVar fun				`thenTc` \ fun' ->
    returnTc (emptyLIE, IfaceRuleOut fun' rule)

tcRule (HsRule name sig_tvs vars lhs rhs src_loc)
  = tcAddSrcLoc src_loc 				$
    tcAddErrCtxt (ruleCtxt name)			$
    newTyVarTy openTypeKind				`thenNF_Tc` \ rule_ty ->

	-- Deal with the tyvars mentioned in signatures
    tcTyVars sig_tvs (mapTc_ kcHsSigType sig_tys) 	`thenTc` \ sig_tyvars ->
    tcExtendTyVarEnv sig_tyvars (

		-- Ditto forall'd variables
	mapNF_Tc new_id vars					`thenNF_Tc` \ ids ->
	tcExtendLocalValEnv [(idName id, id) | id <- ids]	$
	
		-- Now LHS and RHS
	tcExpr lhs rule_ty					`thenTc` \ (lhs', lhs_lie) ->
	tcExpr rhs rule_ty					`thenTc` \ (rhs', rhs_lie) ->
	
	returnTc (sig_tyvars, ids, lhs', rhs', lhs_lie, rhs_lie)
    )						`thenTc` \ (sig_tyvars, ids, lhs', rhs', lhs_lie, rhs_lie) ->

		-- Check that LHS has no overloading at all
    tcSimplifyToDicts lhs_lie				`thenTc` \ (lhs_dicts, lhs_binds) ->
    checkSigTyVars sig_tyvars emptyVarSet		`thenTc_`

	-- Gather the template variables and tyvars
    let
	tpl_ids = map instToId (bagToList lhs_dicts) ++ ids

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
    in

	-- Gather type variables to quantify over
	-- and turn them into real TyVars (just as in TcBinds.tcBindWithSigs)
    zonkTcTypes (rule_ty : map idType tpl_ids)				`thenNF_Tc` \ zonked_tys ->
    mapTc zonkTcTyVarToTyVar (varSetElems (tyVarsOfTypes zonked_tys))	`thenTc` \ tvs ->

	-- RHS can be a bit more lenient.  In particular,
	-- we let constant dictionaries etc float outwards
    tcSimplifyAndCheck (text "tcRule") (mkVarSet tvs)
		       lhs_dicts rhs_lie		`thenTc` \ (lie', rhs_binds) ->

    returnTc (lie', HsRule	name tvs
				(map RuleBndr tpl_ids)	-- yuk
				(mkHsLet lhs_binds lhs')
				(mkHsLet rhs_binds rhs')
				src_loc)
  where
    sig_tys = [t | RuleBndrSig _ t <- vars]

    new_id (RuleBndr var) 	   = newTyVarTy openTypeKind	`thenNF_Tc` \ ty ->
		          	     returnNF_Tc (mkVanillaId var ty)
    new_id (RuleBndrSig var rn_ty) = tcHsSigType rn_ty	`thenTc` \ ty ->
				     returnNF_Tc (mkVanillaId var ty)

ruleCtxt name = ptext SLIT("When checking the transformation rule") <+> 
		doubleQuotes (ptext name)
\end{code}
