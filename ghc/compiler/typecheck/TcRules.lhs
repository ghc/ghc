%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcRules]{Typechecking transformation rules}

\begin{code}
module TcRules ( tcRules ) where

#include "HsVersions.h"

import HsSyn		( RuleDecl(..), RuleBndr(..), HsExpr(..), collectRuleBndrSigTys )
import CoreSyn		( CoreRule(..) )
import RnHsSyn		( RenamedRuleDecl )
import TcHsSyn		( TypecheckedRuleDecl, TcExpr, mkHsLet )
import TcRnMonad
import TcSimplify	( tcSimplifyToDicts, tcSimplifyInferCheck )
import TcMType		( newTyVarTy )
import TcType		( TcTyVarSet, tyVarsOfTypes, tyVarsOfType, openTypeKind )
import TcIfaceSig	( tcCoreExpr, tcCoreLamBndrs, tcVar )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..), tcAddScopedTyVars )
import TcExpr		( tcMonoExpr )
import TcEnv		( tcExtendLocalValEnv )
import Inst		( instToId )
import Id		( idType, mkLocalId )
import VarSet
import Outputable
\end{code}

\begin{code}
tcRules :: [RenamedRuleDecl] -> TcM [TypecheckedRuleDecl]
tcRules decls = mappM tcRule decls

tcRule :: RenamedRuleDecl -> TcM TypecheckedRuleDecl
tcRule (IfaceRule name act vars fun args rhs src_loc)
  = addSrcLoc src_loc 		$
    addErrCtxt (ruleCtxt name)	$
    tcVar fun				`thenM` \ fun' ->
    tcCoreLamBndrs vars			$ \ vars' ->
    mappM tcCoreExpr args		`thenM` \ args' ->
    tcCoreExpr rhs			`thenM` \ rhs' ->
    returnM (IfaceRuleOut fun' (Rule name act vars' args' rhs'))

tcRule (IfaceRuleOut fun rule)	-- Built-in rules come this way
  = tcVar fun				`thenM` \ fun' ->
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
	getLIE (tcMonoExpr lhs rule_ty)		`thenM` \ (lhs', lhs_lie) ->
	getLIE (tcMonoExpr rhs rule_ty)		`thenM` \ (rhs', rhs_lie) ->
	
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
	-- We also need to get the free tyvars of the LHS; see notes 
	-- below with ruleLhsTvs.
	--
	forall_tvs = tyVarsOfTypes (rule_ty : map idType tpl_ids)
			`unionVarSet`
		     ruleLhsTvs lhs'
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

ruleLhsTvs :: TcExpr -> TcTyVarSet
-- We need to gather the type variables mentioned on the LHS so we can 
-- quantify over them.  Example:
--   data T a = C
-- 
--   foo :: T a -> Int
--   foo C = 1
--
--   {-# RULES "myrule"  foo C = 1 #-}
-- 
-- After type checking the LHS becomes (foo a (C a))
-- and we do not want to zap the unbound tyvar 'a' to (), because
-- that limits the applicability of the rule.  Instead, we
-- want to quantify over it!  
--
-- Fortunately the form of the LHS is pretty limited (see RnSource.validRuleLhs)
-- so we don't need to deal with the whole of HsSyn.
--
-- Uh oh!  validRuleLhs only checks the function part of rule LHSs!

ruleLhsTvs (HsPar e)     = ruleLhsTvs e
ruleLhsTvs (HsLit e)     = emptyVarSet
ruleLhsTvs (HsOverLit e) = emptyVarSet
ruleLhsTvs (HsVar v)     = emptyVarSet	-- I don't think we need the tyvars of the Id

ruleLhsTvs (OpApp e1 op _ e2)   = ruleLhsTvs e1 `unionVarSet` ruleLhsTvs op 
				  `unionVarSet` ruleLhsTvs e2
ruleLhsTvs (HsApp e1 e2)   	= ruleLhsTvs e1 `unionVarSet` ruleLhsTvs e2
ruleLhsTvs (TyApp e1 tys)  	= ruleLhsTvs e1 `unionVarSet` tyVarsOfTypes tys
ruleLhsTvs (DictApp e ds)  	= ruleLhsTvs e
ruleLhsTvs (NegApp e _)    	= ruleLhsTvs e
ruleLhsTvs (ExplicitList ty es) = tyVarsOfType ty `unionVarSet` ruleLhsTvs_s es
ruleLhsTvs (ExplicitTuple es _) = ruleLhsTvs_s es

-- Type abstractions can occur in rules like 
--	"foldr k z (build g) = g k z"
ruleLhsTvs (TyLam tvs e)   = ruleLhsTvs e `delVarSetList` tvs
ruleLhsTvs (DictLam ids e) = ruleLhsTvs e
ruleLhsTvs e = pprPanic "ruleLhsTvs" (ppr e)

ruleLhsTvs_s es = foldr (unionVarSet . ruleLhsTvs) emptyVarSet es

ruleCtxt name = ptext SLIT("When checking the transformation rule") <+> 
		doubleQuotes (ftext name)
\end{code}




