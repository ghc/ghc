%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreRules]{Transformation rules}

\begin{code}
module Rules (
	RuleBase, emptyRuleBase, 
	extendRuleBaseList, 
	ruleBaseIds, pprRuleBase, ruleCheckProgram,

        lookupRule, addRule, addRules, addIdSpecialisations
    ) where

#include "HsVersions.h"

import CoreSyn		-- All of it
import OccurAnal	( occurAnalyseRule )
import CoreFVs		( exprFreeVars, exprsFreeVars, ruleRhsFreeVars )
import CoreUnfold	( isCheapUnfolding, unfoldingTemplate )
import CoreUtils	( tcEqExprX )
import Type		( Type )
import CoreTidy		( pprTidyIdRules )
import Id		( Id, idUnfolding, isLocalId, idSpecialisation, setIdSpecialisation ) 
import Var		( Var )
import VarSet
import VarEnv
import Unify 		( tcMatchTyX, MatchEnv(..) )
import BasicTypes	( Activation, CompilerPhase, isActive )

import Outputable
import FastString
import Maybe		( isJust, fromMaybe )
import Bag
import List		( isPrefixOf )
\end{code}


%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

A @CoreRule@ holds details of one rule for an @Id@, which
includes its specialisations.

For example, if a rule for @f@ contains the mapping:
\begin{verbatim}
	forall a b d. [Type (List a), Type b, Var d]  ===>  f' a b
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
	f (List Int) Bool dict ===>  f' Int Bool
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
Rule contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

	pi :: forall a. Num a => a

might have a specialisation

	[Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.


%************************************************************************
%*									*
\subsection{Matching}
%*									*
%************************************************************************

\begin{code}
matchRules :: (Activation -> Bool) -> InScopeSet
	   -> [CoreRule] -> [CoreExpr] -> Maybe (RuleName, CoreExpr)
-- See comments on matchRule
matchRules is_active in_scope [] args = Nothing
matchRules is_active in_scope (rule:rules) args
  = case matchRule is_active in_scope rule args of
	Just result -> Just result
	Nothing	    -> matchRules is_active in_scope rules args

noBlackList :: Activation -> Bool
noBlackList act = False		-- Nothing is black listed

matchRule :: (Activation -> Bool) -> InScopeSet
	  -> CoreRule -> [CoreExpr] -> Maybe (RuleName, CoreExpr)

-- If (matchRule rule args) returns Just (name,rhs)
-- then (f args) matches the rule, and the corresponding
-- rewritten RHS is rhs
--
-- The bndrs and rhs is occurrence-analysed
--
-- 	Example
--
-- The rule
--	forall f g x. map f (map g x) ==> map (f . g) x
-- is stored
--	CoreRule "map/map" 
--		 [f,g,x]		-- tpl_vars
--		 [f,map g x]		-- tpl_args
--		 map (f.g) x)		-- rhs
--	  
-- Then the call: matchRule the_rule [e1,map e2 e3]
--	  = Just ("map/map", (\f,g,x -> rhs) e1 e2 e3)
--
-- Any 'surplus' arguments in the input are simply put on the end
-- of the output.

matchRule is_active in_scope rule@(BuiltinRule name match_fn) args
  = case match_fn args of
	Just expr -> Just (name,expr)
	Nothing   -> Nothing

matchRule is_active in_scope rule@(Rule rn act tpl_vars tpl_args rhs) args
  | not (is_active act)
  = Nothing
  | otherwise
  = case matchN in_scope tpl_vars tpl_args args of
	Just (tpl_vals, leftovers) -> Just (rn, mkLams tpl_vars rhs `mkApps` tpl_vals `mkApps` leftovers)
	Nothing			   -> Nothing
\end{code}

\begin{code}
matchN	:: InScopeSet
	-> [Var]		-- Template tyvars
	-> [CoreExpr]		-- Template
	-> [CoreExpr]		-- Target; can have more elts than template
	-> Maybe ([CoreExpr], 	-- What is substituted for each template var
		  [CoreExpr])	-- Leftover target exprs

matchN in_scope tmpl_vars tmpl_es target_es
  = do	{ (subst, leftover_es) <- go init_menv emptySubstEnv tmpl_es target_es
	; return (map (lookup_tmpl subst) tmpl_vars, leftover_es) }
  where
    init_menv = ME { me_tmpls = mkVarSet tmpl_vars, me_env = init_rn_env }
    init_rn_env = mkRnEnv2 (extendInScopeSetList in_scope tmpl_vars)
		
    go menv subst []     es 	= Just (subst, es)
    go menv subst ts     [] 	= Nothing	-- Fail if too few actual args
    go menv subst (t:ts) (e:es) = do { subst1 <- match menv subst t e 
				     ; go menv subst1 ts es }

    lookup_tmpl :: (TvSubstEnv, IdSubstEnv) -> Var -> CoreExpr
    lookup_tmpl (tv_subst, id_subst) tmpl_var
	| isTyVar tmpl_var = case lookupVarEnv tv_subst tmpl_var of
				Just ty 	-> Type ty
				Nothing 	-> unbound tmpl_var
	| otherwise	   = case lookupVarEnv id_subst tmpl_var of
				Just e -> e
				other  -> unbound tmpl_var
 
    unbound var = pprPanic "Template variable unbound in rewrite rule" (ppr var)
\end{code}


	---------------------------------------------
		The inner workings of matching
	---------------------------------------------

\begin{code}
-- These two definitions are not the same as in Subst,
-- but they simple and direct, and purely local to this module
-- The third, for TvSubstEnv, is the same as in VarEnv, but repeated here
-- for uniformity with IdSubstEnv
type SubstEnv   = (TvSubstEnv, IdSubstEnv)	
type IdSubstEnv = IdEnv    CoreExpr		
type TvSubstEnv = TyVarEnv Type

emptySubstEnv :: SubstEnv
emptySubstEnv = (emptyVarEnv, emptyVarEnv)


--	At one stage I tried to match even if there are more 
--	template args than real args.

--	I now think this is probably a bad idea.
--	Should the template (map f xs) match (map g)?  I think not.
--	For a start, in general eta expansion wastes work.
--	SLPJ July 99


match :: MatchEnv
      -> SubstEnv
      -> CoreExpr		-- Template
      -> CoreExpr		-- Target
      -> Maybe SubstEnv

-- See the notes with Unify.match, which matches types
-- Everything is very similar for terms

-- Interesting examples:
-- Consider matching
--	\x->f 	   against    \f->f
-- When we meet the lambdas we must remember to rename f to f' in the
-- second expresion.  The RnEnv2 does that.
--
-- Consider matching 
--	forall a. \b->b	   against   \a->3
-- We must rename the \a.  Otherwise when we meet the lambdas we 
-- might substitute [a/b] in the template, and then erroneously 
-- succeed in matching what looks like the template variable 'a' against 3.

-- The Var case follows closely what happens in Unify.match
match menv subst@(tv_subst, id_subst) (Var v1) e2 
  | v1 `elemVarSet` me_tmpls menv
  = case lookupVarEnv id_subst v1' of
	Nothing	| any (inRnEnvR rn_env) (varSetElems (exprFreeVars e2))
		-> Nothing	-- Occurs check failure
		-- e.g. match forall a. (\x-> a x) against (\y. y y)

		| otherwise
		-> Just (tv_subst, extendVarEnv id_subst v1 e2)

	Just e2' | tcEqExprX (nukeRnEnvL rn_env) e2' e2 
		 -> Just subst

	other -> Nothing

  | otherwise	-- v1 is not a template variable
  = case e2 of
	Var v2 | v1' == rnOccR rn_env v2 -> Just subst
	other				 -> Nothing
  where
    rn_env = me_env menv
    v1'    = rnOccL rn_env v1

-- Here is another important rule: if the term being matched is a
-- variable, we expand it so long as its unfolding is a WHNF
-- (Its occurrence information is not necessarily up to date,
--  so we don't use it.)
match menv subst e1 (Var v2)
  | isCheapUnfolding unfolding
  = match menv subst e1 (unfoldingTemplate unfolding)
  where
    unfolding = idUnfolding v2

match menv subst (Lit lit1) (Lit lit2)
  | lit1 == lit2
  = Just subst

match menv subst (App f1 a1) (App f2 a2)
  = do 	{ subst' <- match menv subst f1 f2
	; match menv subst' a1 a2 }

match menv subst (Lam x1 e1) (Lam x2 e2)
  = match menv' subst e1 e2
  where
    menv' = menv { me_env = rnBndr2 (me_env menv) x1 x2 }

-- This rule does eta expansion
--		(\x.M)  ~  N 	iff	M  ~  N x
match menv subst (Lam x1 e1) e2
  = match menv' subst e1 (App e2 (varToCoreExpr new_x))
  where
    (rn_env', new_x) = rnBndrL (me_env menv) x1
    menv' = menv { me_env = rn_env' }

-- Eta expansion the other way
--	M  ~  (\y.N)	iff   M	y     ~  N
match menv subst e1 (Lam x2 e2)
  = match menv' subst (App e1 (varToCoreExpr new_x)) e2
  where
    (rn_env', new_x) = rnBndrR (me_env menv) x2
    menv' = menv { me_env = rn_env' }

match menv subst (Case e1 x1 ty1 alts1) (Case e2 x2 ty2 alts2)
  = do	{ subst1 <- match_ty menv subst ty1 ty2
	; subst2 <- match menv subst1 e1 e2
	; let menv' = menv { me_env = rnBndr2 (me_env menv) x2 x2 }
	; match_alts menv' subst2 alts1 alts2	-- Alts are both sorted
	}

match menv subst (Type ty1) (Type ty2)
  = match_ty menv subst ty1 ty2

match menv subst (Note (Coerce to1 from1) e1) (Note (Coerce to2 from2) e2)
  = do	{ subst1 <- match_ty menv subst  to1   to2
	; subst2 <- match_ty menv subst1 from1 from2
	; match menv subst2 e1 e2 }

-- This is an interesting rule: we simply ignore lets in the 
-- term being matched against!  The unfolding inside it is (by assumption)
-- already inside any occurrences of the bound variables, so we'll expand
-- them when we encounter them.
match menv subst e1 (Let (NonRec x2 r2) e2)
  = match menv' subst e1 e2
  where
    menv' = menv { me_env = fst (rnBndrR (me_env menv) x2) }
	-- It's important to do this renaming. For example:
	-- Matching
	--	forall f,x,xs. f (x:xs)
	--   against
	--	f (let y = e in (y:[]))
	-- We must not get success with x->y!  Instead, we 
	-- need an occurs check.

-- Everything else fails
match menv subst e1 e2 = Nothing

------------------------------------------
match_alts :: MatchEnv
      -> SubstEnv
      -> [CoreAlt]		-- Template
      -> [CoreAlt]		-- Target
      -> Maybe SubstEnv
match_alts menv subst [] []
  = return subst
match_alts menv subst ((c1,vs1,r1):alts1) ((c2,vs2,r2):alts2)
  | c1 == c2
  = do	{ subst1 <- match menv' subst r1 r2
	; match_alts menv subst1 alts1 alts2 }
  where
    menv' :: MatchEnv
    menv' = menv { me_env = rnBndrs2 (me_env menv) vs1 vs2 }

match_alts menv subst alts1 alts2 
  = Nothing
\end{code}

Matching Core types: use the matcher in TcType.
Notice that we treat newtypes as opaque.  For example, suppose 
we have a specialised version of a function at a newtype, say 
	newtype T = MkT Int
We only want to replace (f T) with f', not (f Int).

\begin{code}
------------------------------------------
match_ty menv (tv_subst, id_subst) ty1 ty2
  = do	{ tv_subst' <- Unify.tcMatchTyX menv tv_subst ty1 ty2
	; return (tv_subst', id_subst) }
\end{code}


%************************************************************************
%*									*
\subsection{Adding a new rule}
%*									*
%************************************************************************

\begin{code}
addRules :: Id -> CoreRules -> [CoreRule] -> CoreRules
addRule  :: Id -> CoreRules -> CoreRule -> CoreRules

-- Add a new rule to an existing bunch of rules.
-- The rules are for the given Id; the Id argument is needed only
-- so that we can exclude the Id from its own RHS free-var set

-- Insert the new rule just before a rule that is *less specific*
-- than the new one; or at the end if there isn't such a one.
-- In this way we make sure that when looking up, the first match
-- is the most specific.
--
-- We make no check for rules that unify without one dominating
-- the other.   Arguably this would be a bug.

addRules id rules rule_list = foldl (addRule id) rules rule_list

addRule id (Rules rules rhs_fvs) rule@(BuiltinRule _ _)
  = Rules (rule:rules) rhs_fvs
	-- Put it at the start for lack of anything better

addRule id (Rules rules rhs_fvs) rule
  = Rules (insertRule rules new_rule) (rhs_fvs `unionVarSet` new_rhs_fvs)
  where
    new_rule    = occurAnalyseRule rule
    new_rhs_fvs = ruleRhsFreeVars new_rule `delVarSet` id
	-- Hack alert!
	-- Don't include the Id in its own rhs free-var set.
	-- Otherwise the occurrence analyser makes bindings recursive
	-- that shoudn't be.  E.g.
	--	RULE:  f (f x y) z  ==>  f x (f y z)

insertRule rules new_rule@(Rule _ _ tpl_vars tpl_args _)
  = go rules
  where
    tpl_var_set = mkInScopeSet (mkVarSet tpl_vars)
	-- Actually we should probably include the free vars of tpl_args,
	-- but I can't be bothered

    go []					= [new_rule]
    go (rule:rules) | new_is_more_specific rule = (new_rule:rule:rules)
		    | otherwise		        = rule : go rules

    new_is_more_specific rule = isJust (matchRule noBlackList tpl_var_set rule tpl_args)

addIdSpecialisations :: Id -> [CoreRule] -> Id
addIdSpecialisations id rules
  = setIdSpecialisation id new_specs
  where
    new_specs = addRules id (idSpecialisation id) rules
\end{code}


%************************************************************************
%*									*
\subsection{Looking up a rule}
%*									*
%************************************************************************

\begin{code}
lookupRule :: (Activation -> Bool) 
	   -> InScopeSet
	   -> RuleBase		-- Ids from other modules
	   -> Id -> [CoreExpr] -> Maybe (RuleName, CoreExpr)
lookupRule is_active in_scope rules fn args
  = case idSpecialisation fn' of
	Rules rules _ -> matchRules is_active in_scope rules args
  where
    fn' | isLocalId fn					     = fn
	| Just ext_fn <- lookupVarSet (ruleBaseIds rules) fn = ext_fn
	| otherwise					     = fn
\end{code}


%************************************************************************
%*									*
\subsection{Checking a program for failing rule applications}
%*									*
%************************************************************************

-----------------------------------------------------
			Game plan
-----------------------------------------------------

We want to know what sites have rules that could have fired but didn't.
This pass runs over the tree (without changing it) and reports such.

NB: we assume that this follows a run of the simplifier, so every Id
occurrence (including occurrences of imported Ids) is decorated with
all its (active) rules.  No need to construct a rule base or anything
like that.

\begin{code}
ruleCheckProgram :: CompilerPhase -> String -> [CoreBind] -> SDoc
-- Report partial matches for rules beginning 
-- with the specified string
ruleCheckProgram phase rule_pat binds 
  | isEmptyBag results
  = text "Rule check results: no rule application sites"
  | otherwise
  = vcat [text "Rule check results:",
	  line,
	  vcat [ p $$ line | p <- bagToList results ]
	 ]
  where
    results = unionManyBags (map (ruleCheckBind (phase, rule_pat)) binds)
    line = text (replicate 20 '-')
	  
type RuleCheckEnv = (CompilerPhase, String) 	-- Phase and Pattern

ruleCheckBind :: RuleCheckEnv -> CoreBind -> Bag SDoc
   -- The Bag returned has one SDoc for each call site found
ruleCheckBind env (NonRec b r) = ruleCheck env r
ruleCheckBind env (Rec prs)    = unionManyBags [ruleCheck env r | (b,r) <- prs]

ruleCheck :: RuleCheckEnv -> CoreExpr -> Bag SDoc
ruleCheck env (Var v) 	    = emptyBag
ruleCheck env (Lit l) 	    = emptyBag
ruleCheck env (Type ty)     = emptyBag
ruleCheck env (App f a)     = ruleCheckApp env (App f a) []
ruleCheck env (Note n e)    = ruleCheck env e
ruleCheck env (Let bd e)    = ruleCheckBind env bd `unionBags` ruleCheck env e
ruleCheck env (Lam b e)     = ruleCheck env e
ruleCheck env (Case e _ _ as) = ruleCheck env e `unionBags` 
			        unionManyBags [ruleCheck env r | (_,_,r) <- as]

ruleCheckApp env (App f a) as = ruleCheck env a `unionBags` ruleCheckApp env f (a:as)
ruleCheckApp env (Var f) as   = ruleCheckFun env f as
ruleCheckApp env other as     = ruleCheck env other
\end{code}

\begin{code}
ruleCheckFun :: RuleCheckEnv -> Id -> [CoreExpr] -> Bag SDoc
-- Produce a report for all rules matching the predicate
-- saying why it doesn't match the specified application

ruleCheckFun (phase, pat) fn args
  | null name_match_rules = emptyBag
  | otherwise		  = unitBag (ruleAppCheck_help phase fn args name_match_rules)
  where
    name_match_rules = case idSpecialisation fn of
			  Rules rules _ -> filter match rules
    match rule = pat `isPrefixOf` unpackFS (ruleName rule)

ruleAppCheck_help :: CompilerPhase -> Id -> [CoreExpr] -> [CoreRule] -> SDoc
ruleAppCheck_help phase fn args rules
  = 	-- The rules match the pattern, so we want to print something
    vcat [text "Expression:" <+> ppr (mkApps (Var fn) args),
	  vcat (map check_rule rules)]
  where
    n_args = length args
    i_args = args `zip` [1::Int ..]

    check_rule rule = rule_herald rule <> colon <+> rule_info rule

    rule_herald (BuiltinRule name _) = 
	ptext SLIT("Builtin rule") <+> doubleQuotes (ftext name)
    rule_herald (Rule name _ _ _ _)  = 
	ptext SLIT("Rule") <+> doubleQuotes (ftext name)

    rule_info rule
	| Just (name,_) <- matchRule noBlackList emptyInScopeSet rule args
	= text "matches (which is very peculiar!)"

    rule_info (BuiltinRule name fn) = text "does not match"

    rule_info (Rule name act rule_bndrs rule_args _)
	| not (isActive phase act)    = text "active only in later phase"
	| n_args < n_rule_args	      = text "too few arguments"
	| n_mismatches == n_rule_args = text "no arguments match"
	| n_mismatches == 0	      = text "all arguments match (considered individually), but rule as a whole does not"
	| otherwise		      = text "arguments" <+> ppr mismatches <+> text "do not match (1-indexing)"
	where
	  n_rule_args  = length rule_args
	  n_mismatches = length mismatches
	  mismatches   = [i | (rule_arg, (arg,i)) <- rule_args `zip` i_args,
			      not (isJust (match_fn rule_arg arg))]

	  lhs_fvs = exprsFreeVars rule_args	-- Includes template tyvars
	  match_fn rule_arg arg = match menv emptySubstEnv rule_arg arg
		where
		  in_scope = lhs_fvs `unionVarSet` exprFreeVars arg
		  menv = ME { me_env   = mkRnEnv2 (mkInScopeSet in_scope)
			    , me_tmpls = mkVarSet rule_bndrs }
\end{code}


%************************************************************************
%*									*
\subsection{Getting the rules ready}
%*									*
%************************************************************************

\begin{code}
data RuleBase = RuleBase
		    IdSet	-- Ids with their rules in their specialisations
				-- Held as a set, so that it can simply be the initial
				-- in-scope set in the simplifier
	-- This representation is a bit cute, and I wonder if we should
	-- change it to use (IdEnv CoreRule) which seems a bit more natural

ruleBaseIds (RuleBase ids) = ids
emptyRuleBase = RuleBase emptyVarSet

extendRuleBaseList :: RuleBase -> [IdCoreRule] -> RuleBase
extendRuleBaseList rule_base new_guys
  = foldl extendRuleBase rule_base new_guys

extendRuleBase :: RuleBase -> IdCoreRule -> RuleBase
extendRuleBase (RuleBase rule_ids) (IdCoreRule id _ rule)
  = RuleBase (extendVarSet rule_ids new_id)
  where
    new_id    = setIdSpecialisation id (addRule id old_rules rule)
    old_rules = idSpecialisation (fromMaybe id (lookupVarSet rule_ids id))
	-- Get the old rules from rule_ids if the Id is already there, but
	-- if not, use the Id from the incoming rule.  If may be a PrimOpId,
	-- in which case it may have rules in its belly already.  Seems
	-- dreadfully hackoid.

pprRuleBase :: RuleBase -> SDoc
pprRuleBase (RuleBase rules) = vcat [ pprTidyIdRules id | id <- varSetElems rules ]
\end{code}
